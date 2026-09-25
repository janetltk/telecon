library(dplyr)
library(readxl)
library(janitor)
library(ggplot2)
library(scales)

df <- readRDS("~/Telemed/0922_clean.rds")

# Basic characteristics
summary(df$age)
summary(df$gender)
summary(df$edu)
summary(df$x1st_dx_group)
summary(df$year_enter_lsch)
summary(df$year_enter_lsch, na.rm = TRUE)
summary(df$year_fu_tmmhc, na.rm = TRUE)
summary(df$bl_sat)


# Make a pie chart of how many teleconsultations were given-----
# 1. Create a summary data frame
tele_df <- df %>%
  group_by(tele_num) %>%
  summarise(n = n(), .groups = "drop")

tele_df

# 2. Compute percentages
tele_df <- tele_df %>%
  mutate(
    perc = 100 * n / sum(n),
    label = ifelse(perc >= 5,
                   paste0(n, " (", sprintf("%.1f", perc), "%)"),
                   "")
  )

# 3. Plot as a pie chart
ggplot(tele_df, aes(x = "", y = n, fill = factor(tele_num))) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(
    title = "Number of teleconsultations received by each study participant",
    fill = "Number of teleconsultations"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_manual(values = c("#c9706a", "#d9b39c", "#cfd8dc", "#90a4ae", "#607d8b")
  ) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 4)

### Alternative: use bar chart
tele_df <- df %>%
  count(tele_num, name = "n") %>%
  mutate(
    tele_num = factor(tele_num, levels = c(0, 1, 2, 3, 4)),
    perc = n / sum(n)
  )

ggplot(
  tele_df,
  aes(
    x = reorder(tele_num, -as.numeric(tele_num)),
    y = n,
    fill = tele_num
  )
) +
  geom_col(width = 0.7, show.legend = FALSE, colour = "black",
           linewidth = 0.3) +
  geom_text(
    aes(label = paste0(n, " (", scales::percent(perc, accuracy = 0.1), ")")),
    hjust = -0.1,
    size = 4
  ) +
  coord_flip() +
  scale_fill_manual(values = c("#C9706A", "#D9B39C", "#CFD8DC", "#90A4AE", "#607D8B")) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "Number of teleconsultations received by each study participant \n during the 8-month study period (N = 140)",
    x = "Number of teleconsultations",
    y = "Number of participants"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(colour = "grey60", linewidth = 0.6),
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.6)
  )

# Comparison of n group-----

#1. Age: numeric variable
#“Do patients who received teleconsultation have a different mean or median age compared with those who never received teleconsultation?”
#Step 1 – descriptive stats

age_summary <- df %>%
  group_by(tele_group_factor) %>%
  summarise(
    n      = sum(!is.na(age)),
    mean   = mean(age, na.rm = TRUE),
    sd     = sd(age, na.rm = TRUE),
    median = median(age, na.rm = TRUE),
    iqr    = IQR(age, na.rm = TRUE)
  )

age_summary

#Check distribution quickly:

hist(df$age[df$tele_group_factor == "No_tele"],
     main = "Age - No tele", xlab = "Age")
hist(df$age[df$tele_group_factor == "At_least_1_tele"],
     main = "Age - ≥1 tele", xlab = "Age")

shapiro.test(df$age)

#Step 3 – t‑test (if distribution looks ok)

t_age <- t.test(age ~ tele_group_factor, data = df)
t_age

# Wilcoxon rank-sum test (Mann-Whitney U)
w_age <- wilcox.test(age ~ tele_group_factor, data = df, exact = FALSE)
w_age

#2. Gender: categorical variable
tab_gender <- table(df$tele_group_factor, df$gender)
tab_gender

#Step 2 – check expected counts
chisq.test(tab_gender)$expected

chi_gender <- chisq.test(tab_gender, correct = FALSE)
chi_gender

fisher_gender <- fisher.test(tab_gender)
fisher_gender

#3. Educational level (edu)

tab_edu <- table(df$tele_group_factor, df$edu)
tab_edu
chisq.test(tab_edu)$expected

# Fisher as expected counts not ok
fisher_edu <- fisher.test(tab_edu)
fisher_edu

#4. Duration of FU in MHS / TMMHC / year entering LSCH
#FU MHS
fu_mhs_summary <- df %>%
  group_by(tele_group_factor) %>%
  summarise(
    n      = sum(!is.na(year_fu_mhs)),
    mean   = mean(year_fu_mhs, na.rm = TRUE),
    sd     = sd(year_fu_mhs, na.rm = TRUE),
    median = median(year_fu_mhs, na.rm = TRUE),
    iqr    = IQR(year_fu_mhs, na.rm = TRUE)
  )

fu_mhs_summary

# quick histogram to eyeball distribution
hist(df$year_fu_mhs[df$tele_group == 0], main = "Years FU MHS - No tele", xlab = "Year followed up in MHS")
hist(df$year_fu_mhs[df$tele_group == 1], main = "Years FU MHS - ≥1 tele", xlab = "Year followed up in MHS")

shapiro.test(df$year_fu_mhs)

#t test if normally distributed
t_fu_mhs <- t.test(year_fu_mhs ~ tele_group_factor, data = df)
t_fu_mhs

#mann-whitney U test if not normally distributed
w_fu_mhs <- wilcox.test(year_fu_mhs ~ tele_group_factor, data = df, exact = FALSE)
w_fu_mhs

#FU TMMHC
fu_tmmhc_summary <- df %>%
  group_by(tele_group_factor) %>%
  summarise(
    n      = sum(!is.na(year_fu_tmmhc)),
    mean   = mean(year_fu_tmmhc, na.rm = TRUE),
    sd     = sd(year_fu_tmmhc, na.rm = TRUE),
    median = median(year_fu_tmmhc, na.rm = TRUE),
    iqr    = IQR(year_fu_tmmhc, na.rm = TRUE)
  )

fu_tmmhc_summary

# quick histogram to eyeball distribution
hist(df$year_fu_tmmhc[df$tele_group == 0], main = "Years FU TMMHC - No tele", xlab = "Years followed up in TMMHC")
hist(df$year_fu_tmmhc[df$tele_group == 1], main = "Years FU TMMHC - ≥1 tele", xlab = "Years followed up in TMMHC")

shapiro.test(df$year_fu_tmmhc)


#t test if normally distributed
t_fu_tmmhc <- t.test(year_fu_tmmhc ~ tele_group_factor, data = df)
t_fu_tmmhc

#mann-whitney U test if not normally distributed
w_fu_tmmhc <- wilcox.test(year_fu_tmmhc ~ tele_group_factor, data = df, exact = FALSE)
w_fu_tmmhc

#Year enter LSCH
enter_lsch_summary <- df %>%
  group_by(tele_group_factor) %>%
  summarise(
    n      = sum(!is.na(year_enter_lsch)),
    mean   = mean(year_enter_lsch, na.rm = TRUE),
    sd     = sd(year_enter_lsch, na.rm = TRUE),
    median = median(year_enter_lsch, na.rm = TRUE),
    iqr    = IQR(year_enter_lsch, na.rm = TRUE)
  )

enter_lsch_summary

# quick histogram to eyeball distribution
hist(df$year_enter_lsch[df$tele_group == 0], main = "Years entered LSCH - No tele", xlab = "Years entered LSCH")
hist(df$year_enter_lsch[df$tele_group == 1], main = "Years entered LSCH - ≥1 tele", xlab = "Years entered LSCH")

#t test if normally distributed
t_enter_lsch <- t.test(year_enter_lsch ~ tele_group_factor, data = df)
t_enter_lsch

#mann-whitney U test if not normally distributed
w_enter_lsch <- wilcox.test(year_enter_lsch ~ tele_group_factor, data = df, exact = FALSE)
w_enter_lsch

#5. Compare diagnoses
# 1st diagnosis
table(df$x1st_dx_group, useNA = "ifany")

# 2nd diagnosis
table(df$x2nd_dx_group, useNA = "ifany")

# 1st Dx vs tele group - Chi sq test if expected counts ok
tab_dx1 <- table(df$tele_group_factor, df$x1st_dx_group)
tab_dx1

chisq.test(tab_dx1)$expected
chisq.test(tab_dx1) 

# Fisher if not
fisher_dx1 <- fisher.test(tab_dx1)
fisher_dx1


# 2nd Dx vs tele group - Chi sq test if expected counts ok
tab_dx2 <- table(df$tele_group_factor, df$x2nd_dx_group)
tab_dx2

chisq.test(tab_dx2)$expected
chisq.test(tab_dx2) 

# Fisher if not
fisher_dx2 <- fisher.test(tab_dx2)
fisher_dx2

#6. LSCH: categorical variable
tab_hostel <- table(df$tele_group_factor, df$hostel)
tab_hostel

#Step 2 – check expected counts
chisq.test(tab_hostel)$expected

chi_hostel <- chisq.test(tab_hostel, correct = FALSE)
chi_hostel

fisher_hostel <- fisher.test(tab_hostel)
fisher_hostel


## repeat everything for sensitivity analysis

#1. Age: numeric variable
#“Do patients who received teleconsultation have a different mean or median age compared with those who never received teleconsultation?”
#Step 1 – descriptive stats

age_sens_summary <- df %>%
  group_by(tele_sens_factor) %>%
  summarise(
    n      = sum(!is.na(age)),
    mean   = mean(age, na.rm = TRUE),
    sd     = sd(age, na.rm = TRUE),
    median = median(age, na.rm = TRUE),
    iqr    = IQR(age, na.rm = TRUE)
  )

age_sens_summary

#Check distribution quickly:

hist(df$age[df$tele_sens_factor == "Tele_sensitivity"],
     main = "Age - tele (sens)", xlab = "Age")
hist(df$age[df$tele_sens_factor == "No_tele_sensitivity"],
     main = "Age - No tele (sens)", xlab = "Age")

#Step 3 – t‑test (if distribution looks ok)

t_sens_age <- t.test(age ~ tele_sens_factor, data = df)
t_sens_age

# Wilcoxon rank-sum test (Mann-Whitney U)
w_sens_age <- wilcox.test(age ~ tele_sens_factor, data = df, exact = FALSE)
w_sens_age

#2. Gender: categorical variable
tab_sens_gender <- table(df$tele_sens_factor, df$gender)
tab_sens_gender

#Step 2 – check expected counts
chisq.test(tab_sens_gender)$expected

chi_sens_gender <- chisq.test(tab_sens_gender, correct = FALSE)
chi_sens_gender

fisher_sens_gender <- fisher.test(tab_sens_gender)
fisher_sens_gender

#3. Educational level (edu)

tab_sens_edu <- table(df$tele_sens_factor, df$edu)
tab_sens_edu
chisq.test(tab_sens_edu)$expected

# Chi-square if expected counts ok
chi_sens_edu <- chisq.test(tab_sens_edu)
chi_sens_edu

# Fisher if not
fisher_sens_edu <- fisher.test(tab_sens_edu)
fisher_sens_edu

#4. Duration of FU in MHS / TMMHC / year entering LSCH
#FU MHS
fu_mhs_sens_summary <- df %>%
  group_by(tele_sens_factor) %>%
  summarise(
    n      = sum(!is.na(year_fu_mhs)),
    mean   = mean(year_fu_mhs, na.rm = TRUE),
    sd     = sd(year_fu_mhs, na.rm = TRUE),
    median = median(year_fu_mhs, na.rm = TRUE),
    iqr    = IQR(year_fu_mhs, na.rm = TRUE)
  )

fu_mhs_sens_summary

# quick histogram to eyeball distribution
hist(df$year_fu_mhs[df$tele_sens_factor == "Tele_sensitivity"], main = "Year FU MHS - tele (sens)", xlab = "Year followed up in MHS")
hist(df$year_fu_mhs[df$tele_sens_factor == "No_tele_sensitivity"], main = "Year FU MHS - no tele (sens)", xlab = "Year followed up in MHS")

#t test if normally distributed
t_sens_fu_mhs <- t.test(year_fu_mhs ~ tele_sens_factor, data = df)
t_sens_fu_mhs

#mann-whitney U test if not normally distributed
w_sens_fu_mhs <- wilcox.test(year_fu_mhs ~ tele_sens_factor, data = df, exact = FALSE)
w_sens_fu_mhs

#FU TMMHC
fu_tmmhc_sens_summary <- df %>%
  group_by(tele_sens_factor) %>%
  summarise(
    n      = sum(!is.na(year_fu_tmmhc)),
    mean   = mean(year_fu_tmmhc, na.rm = TRUE),
    sd     = sd(year_fu_tmmhc, na.rm = TRUE),
    median = median(year_fu_tmmhc, na.rm = TRUE),
    iqr    = IQR(year_fu_tmmhc, na.rm = TRUE)
  )

fu_tmmhc_sens_summary

# quick histogram to eyeball distribution
hist(df$year_fu_tmmhc[df$tele_sens_factor == "Tele_sensitivity"], main = "Year FU TMMHC - tele (sens)", xlab = "Year followed up in TMMHC")
hist(df$year_fu_tmmhc[df$tele_sens_factor == "No_tele_sensitivity"], main = "Year FU TMMHC - no tele (sens)", xlab = "Year followed up in TMMHC")

#t test if normally distributed
t_sens_fu_tmmhc <- t.test(year_fu_tmmhc ~ tele_sens_factor, data = df)
t_sens_fu_tmmhc

#mann-whitney U test if not normally distributed
w_sens_fu_tmmhc <- wilcox.test(year_fu_tmmhc ~ tele_sens_factor, data = df, exact = FALSE)
w_sens_fu_tmmhc

#Year enter LSCH
enter_lsch_sens_summary <- df %>%
  group_by(tele_sens_factor) %>%
  summarise(
    n      = sum(!is.na(year_enter_lsch)),
    mean   = mean(year_enter_lsch, na.rm = TRUE),
    sd     = sd(year_enter_lsch, na.rm = TRUE),
    median = median(year_enter_lsch, na.rm = TRUE),
    iqr    = IQR(year_enter_lsch, na.rm = TRUE)
  )

enter_lsch_sens_summary

# quick histogram to eyeball distribution
hist(df$year_enter_lsch[df$tele_sens_factor == "Tele_sensitivity"], main = "Year entered LSCH - tele (sens)", xlab = "Years Entered LSCH")
hist(df$year_enter_lsch[df$tele_sens_factor == "No_tele_sensitivity"], main = "Year entered LSCH - no tele (sens)", xlab = "Years Entered LSCH")

#t test if normally distributed
t_sens_enter_lsch <- t.test(year_enter_lsch ~ tele_sens_factor, data = df)
t_sens_enter_lsch

#mann-whitney U test if not normally distributed
w_sens_enter_lsch <- wilcox.test(year_enter_lsch ~ tele_sens_factor, data = df, exact = FALSE)
w_sens_enter_lsch

#5. Compare diagnoses

# 1st Dx vs tele group - Chi sq test if expected counts ok
tab_sens_dx1 <- table(df$tele_sens_factor, df$x1st_dx_group)
tab_sens_dx1

chisq.test(tab_sens_dx1)$expected
chisq.test(tab_sens_dx1) 

# Fisher if not
fisher_sens_dx1 <- fisher.test(tab_sens_dx1)
fisher_sens_dx1

# 2nd Dx vs tele group - Chi sq test if expected counts ok
tab_sens_dx2 <- table(df$tele_sens_factor, df$x2nd_dx_group)
tab_sens_dx2

chisq.test(tab_sens_dx2)$expected
chisq.test(tab_sens_dx2) 

# Fisher if not
fisher_sens_dx2 <- fisher.test(tab_sens_dx2)

###################
# Use logistic regression model to see which factors predict telemedicine uptake-----

# outcome numeric
df$tele_group <- as.numeric(df$tele_group)  # 0/1 if not already
df$tele_sens <- as.numeric(df$tele_sens)

# demographic
m_age    <- glm(tele_group ~ age, data = df, family = binomial)
summary(m_age)
exp(coef(m_age))        # odds ratio per year ↑
exp(confint(m_age))     # 95% CI

m_gender <- glm(tele_group ~ gender, data = df, family = binomial)
summary(m_gender)
exp(coef(m_gender))
exp(confint(m_gender))

m_edu    <- glm(tele_group ~ edu, data = df, family = binomial)
summary(m_edu)
exp(coef(m_edu))
exp(confint(m_edu))

m_hostel <- glm(tele_group ~ hostel, data = df, family = binomial)
summary(m_hostel)
exp(coef(m_hostel))
exp(confint(m_hostel))

m_escort <- glm(tele_group ~ escort, data = df, family = binomial)
summary(m_escort)

# clinical
m_fu_mhs     <- glm(tele_group ~ year_fu_mhs, data = df, family = binomial)
m_fu_tmmhc   <- glm(tele_group ~ year_fu_tmmhc, data = df, family = binomial)
m_enter_lsch <- glm(tele_group ~ year_enter_lsch, data = df, family = binomial)

summary(m_fu_mhs);     exp(coef(m_fu_mhs));     exp(confint(m_fu_mhs))
summary(m_fu_tmmhc);   exp(coef(m_fu_tmmhc));   exp(confint(m_fu_tmmhc))
summary(m_enter_lsch); exp(coef(m_enter_lsch)); exp(confint(m_enter_lsch))


m_dx1        <- glm(tele_group ~ x1st_dx_group, data = df, family = binomial)
m_dx2        <- glm(tele_group ~ x2nd_dx_group, data = df, family = binomial)

summary(m_dx1); exp(coef(m_dx1)); exp(confint(m_dx1))
summary(m_dx2); exp(coef(m_dx2)); exp(confint(m_dx2))

m_swemwbs    <- glm(tele_group ~ bl_swemwbs, data = df, family = binomial)
summary(m_swemwbs); exp(coef(m_swemwbs)); exp(confint(m_swemwbs))


m_honos      <- glm(tele_group ~ bl_honos, data = df, family = binomial)
summary(m_honos);   exp(coef(m_honos));   exp(confint(m_honos))


m_cgi        <- glm(tele_group ~ bl_cgi, data = df, family = binomial)
summary(m_cgi);     exp(coef(m_cgi));     exp(confint(m_cgi))


# SOPEQ questions

df$bl_sat <- as.numeric(as.character(df$bl_sat))
m_bl_sat <- glm(tele_group ~ bl_sat, data = df, family = binomial)
summary(m_bl_sat)
exp(coef(m_bl_sat)); exp(confint(m_bl_sat))

df$sopeq_b_2_num <- as.numeric(as.character(df$sopeq_b_2))
m_sopeq2 <- glm(tele_group ~ sopeq_b_2_num, data = df, family = binomial)
summary(m_sopeq2)
exp(coef(m_sopeq2)); exp(confint(m_sopeq2))


df$sopeq_b_3_num <- as.numeric(as.character(df$sopeq_b_3))
m_sopeq3 <- glm(tele_group ~ sopeq_b_3_num, data = df, family = binomial)
summary(m_sopeq3)
exp(coef(m_sopeq3)); exp(confint(m_sopeq3))

df$sopeq_b_4_num <- as.numeric(as.character(df$sopeq_b_4))
m_sopeq4 <- glm(tele_group ~ sopeq_b_4_num, data = df, family = binomial)
summary(m_sopeq4)
exp(coef(m_sopeq4)); exp(confint(m_sopeq4))

## Create numeric SOPEQ baseline variables 5–28
df$sopeq_b_5_num  <- as.numeric(as.character(df$sopeq_b_5))
df$sopeq_b_6_num  <- as.numeric(as.character(df$sopeq_b_6))
df$sopeq_b_7_num  <- as.numeric(as.character(df$sopeq_b_7))
df$sopeq_b_8_num  <- as.numeric(as.character(df$sopeq_b_8))
df$sopeq_b_9_num  <- as.numeric(as.character(df$sopeq_b_9))
df$sopeq_b_10_num <- as.numeric(as.character(df$sopeq_b_10))
df$sopeq_b_11_num <- as.numeric(as.character(df$sopeq_b_11))
df$sopeq_b_12_num <- as.numeric(as.character(df$sopeq_b_12))
df$sopeq_b_13_num <- as.numeric(as.character(df$sopeq_b_13))
df$sopeq_b_14_num <- as.numeric(as.character(df$sopeq_b_14))
df$sopeq_b_15_num <- as.numeric(as.character(df$sopeq_b_15))
df$sopeq_b_16_num <- as.numeric(as.character(df$sopeq_b_16))
df$sopeq_b_17_num <- as.numeric(as.character(df$sopeq_b_17))
df$sopeq_b_18_num <- as.numeric(as.character(df$sopeq_b_18))
df$sopeq_b_19_num <- as.numeric(as.character(df$sopeq_b_19))
df$sopeq_b_20_num <- as.numeric(as.character(df$sopeq_b_20))
df$sopeq_b_21_num <- as.numeric(as.character(df$sopeq_b_21))
df$sopeq_b_22_num <- as.numeric(as.character(df$sopeq_b_22))
df$sopeq_b_23_num <- as.numeric(as.character(df$sopeq_b_23))
df$sopeq_b_24_num <- as.numeric(as.character(df$sopeq_b_24))
df$sopeq_b_25_num <- as.numeric(as.character(df$sopeq_b_25))
df$sopeq_b_26_num <- as.numeric(as.character(df$sopeq_b_26))
df$sopeq_b_27_num <- as.numeric(as.character(df$sopeq_b_27))
df$sopeq_b_28_num <- as.numeric(as.character(df$sopeq_b_28))

## Univariate logistic regressions for each numeric SOPEQ item

# sopeq_b_5
m_sopeq5 <- glm(tele_group ~ sopeq_b_5_num, data = df, family = binomial)
summary(m_sopeq5)
exp(coef(m_sopeq5)); exp(confint(m_sopeq5))

# sopeq_b_6
m_sopeq6 <- glm(tele_group ~ sopeq_b_6_num, data = df, family = binomial)
summary(m_sopeq6)
exp(coef(m_sopeq6)); exp(confint(m_sopeq6))

# sopeq_b_7
m_sopeq7 <- glm(tele_group ~ sopeq_b_7_num, data = df, family = binomial)
summary(m_sopeq7)
exp(coef(m_sopeq7)); exp(confint(m_sopeq7))

# sopeq_b_8
m_sopeq8 <- glm(tele_group ~ sopeq_b_8_num, data = df, family = binomial)
summary(m_sopeq8)
exp(coef(m_sopeq8)); exp(confint(m_sopeq8))

# sopeq_b_9
m_sopeq9 <- glm(tele_group ~ sopeq_b_9_num, data = df, family = binomial)
summary(m_sopeq9)
exp(coef(m_sopeq9)); exp(confint(m_sopeq9))

# sopeq_b_10
m_sopeq10 <- glm(tele_group ~ sopeq_b_10_num, data = df, family = binomial)
summary(m_sopeq10)
exp(coef(m_sopeq10)); exp(confint(m_sopeq10))

# sopeq_b_11
m_sopeq11 <- glm(tele_group ~ sopeq_b_11_num, data = df, family = binomial)
summary(m_sopeq11)
exp(coef(m_sopeq11)); exp(confint(m_sopeq11))

# sopeq_b_12
m_sopeq12 <- glm(tele_group ~ sopeq_b_12_num, data = df, family = binomial)
summary(m_sopeq12)
exp(coef(m_sopeq12)); exp(confint(m_sopeq12))

# sopeq_b_13
m_sopeq13 <- glm(tele_group ~ sopeq_b_13_num, data = df, family = binomial)
summary(m_sopeq13)
exp(coef(m_sopeq13)); exp(confint(m_sopeq13))

# sopeq_b_14
m_sopeq14 <- glm(tele_group ~ sopeq_b_14_num, data = df, family = binomial)
summary(m_sopeq14)
exp(coef(m_sopeq14)); exp(confint(m_sopeq14))

# sopeq_b_15
m_sopeq15 <- glm(tele_group ~ sopeq_b_15_num, data = df, family = binomial)
summary(m_sopeq15)
exp(coef(m_sopeq15)); exp(confint(m_sopeq15))

# sopeq_b_16
m_sopeq16 <- glm(tele_group ~ sopeq_b_16_num, data = df, family = binomial)
summary(m_sopeq16)
exp(coef(m_sopeq16)); exp(confint(m_sopeq16))

# sopeq_b_17
m_sopeq17 <- glm(tele_group ~ sopeq_b_17_num, data = df, family = binomial)
summary(m_sopeq17)
exp(coef(m_sopeq17)); exp(confint(m_sopeq17))

# sopeq_b_18
m_sopeq18 <- glm(tele_group ~ sopeq_b_18_num, data = df, family = binomial)
summary(m_sopeq18)
exp(coef(m_sopeq18)); exp(confint(m_sopeq18))

# sopeq_b_19
m_sopeq19 <- glm(tele_group ~ sopeq_b_19_num, data = df, family = binomial)
summary(m_sopeq19)
exp(coef(m_sopeq19)); exp(confint(m_sopeq19))

# sopeq_b_20
m_sopeq20 <- glm(tele_group ~ sopeq_b_20_num, data = df, family = binomial)
summary(m_sopeq20)
exp(coef(m_sopeq20)); exp(confint(m_sopeq20))

# sopeq_b_21
m_sopeq21 <- glm(tele_group ~ sopeq_b_21_num, data = df, family = binomial)
summary(m_sopeq21)
exp(coef(m_sopeq21)); exp(confint(m_sopeq21))

# sopeq_b_22
m_sopeq22 <- glm(tele_group ~ sopeq_b_22_num, data = df, family = binomial)
summary(m_sopeq22)
exp(coef(m_sopeq22)); exp(confint(m_sopeq22))

# sopeq_b_23
m_sopeq23 <- glm(tele_group ~ sopeq_b_23_num, data = df, family = binomial)
summary(m_sopeq23)
exp(coef(m_sopeq23)); exp(confint(m_sopeq23))

# sopeq_b_24
m_sopeq24 <- glm(tele_group ~ sopeq_b_24_num, data = df, family = binomial)
summary(m_sopeq24)
exp(coef(m_sopeq24)); exp(confint(m_sopeq24))

# sopeq_b_25
m_sopeq25 <- glm(tele_group ~ sopeq_b_25_num, data = df, family = binomial)
summary(m_sopeq25)
exp(coef(m_sopeq25)); exp(confint(m_sopeq25))

# sopeq_b_26
m_sopeq26 <- glm(tele_group ~ sopeq_b_26_num, data = df, family = binomial)
summary(m_sopeq26)
exp(coef(m_sopeq26)); exp(confint(m_sopeq26))

# sopeq_b_27
m_sopeq27 <- glm(tele_group ~ sopeq_b_27_num, data = df, family = binomial)
summary(m_sopeq27)
exp(coef(m_sopeq27)); exp(confint(m_sopeq27))

# sopeq_b_28
m_sopeq28 <- glm(tele_group ~ sopeq_b_28_num, data = df, family = binomial)
summary(m_sopeq28)
exp(coef(m_sopeq28)); exp(confint(m_sopeq28))


# Multivariate logistic model
model_uptake <- glm(
  tele_group ~ hostel + year_fu_mhs,  # pick a few key SOPEQ items
  data = df,
  family = binomial
)
summary(model_uptake)

model_final <- glm(
  tele_group ~ hostel + escort + sopeq_b_4_num + year_fu_mhs + year_enter_lsch,
  data = df, family = binomial
)
summary(model_final)
exp(cbind(OR = coef(model_final), confint(model_final)))

# sensitivity analysis-----


# demographic
m_age    <- glm(tele_sens ~ age, data = df, family = binomial)
summary(m_age)
exp(coef(m_age))        # odds ratio per year ↑
exp(confint(m_age))     # 95% CI

m_gender <- glm(tele_sens ~ gender, data = df, family = binomial)
summary(m_gender)
exp(coef(m_gender))
exp(confint(m_gender))

m_edu    <- glm(tele_sens ~ edu, data = df, family = binomial)
summary(m_edu)
exp(coef(m_edu))
exp(confint(m_edu))

m_hostel <- glm(tele_sens ~ hostel, data = df, family = binomial)
summary(m_hostel)
exp(coef(m_hostel))
exp(confint(m_hostel))

m_escort <- glm(tele_sens ~ escort, data = df, family = binomial)
summary(m_escort)
exp(coef(m_escort))
exp(confint(m_escort))

# clinical
m_fu_mhs     <- glm(tele_sens ~ year_fu_mhs, data = df, family = binomial)
m_fu_tmmhc   <- glm(tele_sens ~ year_fu_tmmhc, data = df, family = binomial)
m_enter_lsch <- glm(tele_sens ~ year_enter_lsch, data = df, family = binomial)

summary(m_fu_mhs);     exp(coef(m_fu_mhs));     exp(confint(m_fu_mhs))
summary(m_fu_tmmhc);   exp(coef(m_fu_tmmhc));   exp(confint(m_fu_tmmhc))
summary(m_enter_lsch); exp(coef(m_enter_lsch)); exp(confint(m_enter_lsch))

m_dx1        <- glm(tele_sens ~ x1st_dx_group, data = df, family = binomial)
m_dx2        <- glm(tele_sens ~ x2nd_dx_group, data = df, family = binomial)

summary(m_dx1); exp(coef(m_dx1)); exp(confint(m_dx1))
summary(m_dx2); exp(coef(m_dx2)); exp(confint(m_dx2))

m_swemwbs    <- glm(tele_sens ~ bl_swemwbs, data = df, family = binomial)
summary(m_swemwbs); exp(coef(m_swemwbs)); exp(confint(m_swemwbs))


m_honos      <- glm(tele_sens ~ bl_honos, data = df, family = binomial)
summary(m_honos);   exp(coef(m_honos));   exp(confint(m_honos))


m_cgi        <- glm(tele_sens ~ bl_cgi, data = df, family = binomial)
summary(m_cgi);     exp(coef(m_cgi));     exp(confint(m_cgi))


# SOPEQ questions

df$bl_sat <- as.numeric(as.character(df$bl_sat))
m_bl_sat <- glm(tele_sens ~ bl_sat, data = df, family = binomial)
summary(m_bl_sat)
exp(coef(m_bl_sat)); exp(confint(m_bl_sat))

df$sopeq_b_2_num <- as.numeric(as.character(df$sopeq_b_2))
m_sopeq2 <- glm(tele_sens ~ sopeq_b_2_num, data = df, family = binomial)
summary(m_sopeq2)
exp(coef(m_sopeq2)); exp(confint(m_sopeq2))


df$sopeq_b_3_num <- as.numeric(as.character(df$sopeq_b_3))
m_sopeq3 <- glm(tele_sens ~ sopeq_b_3_num, data = df, family = binomial)
summary(m_sopeq3)
exp(coef(m_sopeq3)); exp(confint(m_sopeq3))

df$sopeq_b_4_num <- as.numeric(as.character(df$sopeq_b_4))
m_sopeq4 <- glm(tele_sens ~ sopeq_b_4_num, data = df, family = binomial)
summary(m_sopeq4)
exp(coef(m_sopeq4)); exp(confint(m_sopeq4))

## Univariate logistic regressions for each numeric SOPEQ item

# sopeq_b_5
m_sopeq5 <- glm(tele_sens ~ sopeq_b_5_num, data = df, family = binomial)
summary(m_sopeq5)
exp(coef(m_sopeq5)); exp(confint(m_sopeq5))

# sopeq_b_6
m_sopeq6 <- glm(tele_sens ~ sopeq_b_6_num, data = df, family = binomial)
summary(m_sopeq6)
exp(coef(m_sopeq6)); exp(confint(m_sopeq6))

# sopeq_b_7
m_sopeq7 <- glm(tele_sens ~ sopeq_b_7_num, data = df, family = binomial)
summary(m_sopeq7)
exp(coef(m_sopeq7)); exp(confint(m_sopeq7))

# sopeq_b_8
m_sopeq8 <- glm(tele_sens ~ sopeq_b_8_num, data = df, family = binomial)
summary(m_sopeq8)
exp(coef(m_sopeq8)); exp(confint(m_sopeq8))

# sopeq_b_9
m_sopeq9 <- glm(tele_sens ~ sopeq_b_9_num, data = df, family = binomial)
summary(m_sopeq9)
exp(coef(m_sopeq9)); exp(confint(m_sopeq9))

# sopeq_b_10
m_sopeq10 <- glm(tele_sens ~ sopeq_b_10_num, data = df, family = binomial)
summary(m_sopeq10)
exp(coef(m_sopeq10)); exp(confint(m_sopeq10))

# sopeq_b_11
m_sopeq11 <- glm(tele_sens ~ sopeq_b_11_num, data = df, family = binomial)
summary(m_sopeq11)
exp(coef(m_sopeq11)); exp(confint(m_sopeq11))

# sopeq_b_12
m_sopeq12 <- glm(tele_sens ~ sopeq_b_12_num, data = df, family = binomial)
summary(m_sopeq12)
exp(coef(m_sopeq12)); exp(confint(m_sopeq12))

# sopeq_b_13
m_sopeq13 <- glm(tele_sens ~ sopeq_b_13_num, data = df, family = binomial)
summary(m_sopeq13)
exp(coef(m_sopeq13)); exp(confint(m_sopeq13))

# sopeq_b_14
m_sopeq14 <- glm(tele_sens ~ sopeq_b_14_num, data = df, family = binomial)
summary(m_sopeq14)
exp(coef(m_sopeq14)); exp(confint(m_sopeq14))

# sopeq_b_15
m_sopeq15 <- glm(tele_sens ~ sopeq_b_15_num, data = df, family = binomial)
summary(m_sopeq15)
exp(coef(m_sopeq15)); exp(confint(m_sopeq15))

# sopeq_b_16
m_sopeq16 <- glm(tele_sens ~ sopeq_b_16_num, data = df, family = binomial)
summary(m_sopeq16)
exp(coef(m_sopeq16)); exp(confint(m_sopeq16))

# sopeq_b_17
m_sopeq17 <- glm(tele_sens ~ sopeq_b_17_num, data = df, family = binomial)
summary(m_sopeq17)
exp(coef(m_sopeq17)); exp(confint(m_sopeq17))

# sopeq_b_18
m_sopeq18 <- glm(tele_sens ~ sopeq_b_18_num, data = df, family = binomial)
summary(m_sopeq18)
exp(coef(m_sopeq18)); exp(confint(m_sopeq18))

# sopeq_b_19
m_sopeq19 <- glm(tele_sens ~ sopeq_b_19_num, data = df, family = binomial)
summary(m_sopeq19)
exp(coef(m_sopeq19)); exp(confint(m_sopeq19))

# sopeq_b_20
m_sopeq20 <- glm(tele_sens ~ sopeq_b_20_num, data = df, family = binomial)
summary(m_sopeq20)
exp(coef(m_sopeq20)); exp(confint(m_sopeq20))

# sopeq_b_21
m_sopeq21 <- glm(tele_sens ~ sopeq_b_21_num, data = df, family = binomial)
summary(m_sopeq21)
exp(coef(m_sopeq21)); exp(confint(m_sopeq21))

# sopeq_b_22
m_sopeq22 <- glm(tele_sens ~ sopeq_b_22_num, data = df, family = binomial)
summary(m_sopeq22)
exp(coef(m_sopeq22)); exp(confint(m_sopeq22))

# sopeq_b_23
m_sopeq23 <- glm(tele_sens ~ sopeq_b_23_num, data = df, family = binomial)
summary(m_sopeq23)
exp(coef(m_sopeq23)); exp(confint(m_sopeq23))

# sopeq_b_24
m_sopeq24 <- glm(tele_sens ~ sopeq_b_24_num, data = df, family = binomial)
summary(m_sopeq24)
exp(coef(m_sopeq24)); exp(confint(m_sopeq24))

# sopeq_b_25
m_sopeq25 <- glm(tele_sens ~ sopeq_b_25_num, data = df, family = binomial)
summary(m_sopeq25)
exp(coef(m_sopeq25)); exp(confint(m_sopeq25))

# sopeq_b_26
m_sopeq26 <- glm(tele_sens ~ sopeq_b_26_num, data = df, family = binomial)
summary(m_sopeq26)
exp(coef(m_sopeq26)); exp(confint(m_sopeq26))

# sopeq_b_27
m_sopeq27 <- glm(tele_sens ~ sopeq_b_27_num, data = df, family = binomial)
summary(m_sopeq27)
exp(coef(m_sopeq27)); exp(confint(m_sopeq27))

# sopeq_b_28
m_sopeq28 <- glm(tele_sens ~ sopeq_b_28_num, data = df, family = binomial)
summary(m_sopeq28)
exp(coef(m_sopeq28)); exp(confint(m_sopeq28))


# Multivariate logistic model
model_uptake <- glm(
  tele_sens ~ hostel + year_fu_mhs,  # pick a few key SOPEQ items
  data = df,
  family = binomial
)
summary(model_multi)

model_final <- glm(
  tele_sens ~ hostel + year_fu_mhs+ year_enter_lsch + sopeq_b_4_num ,
  data = df, family = binomial
)
summary(model_final)

library(dplyr)
library(readxl)
library(janitor)

df <- readRDS("~/Telemed/0623_clean.rds")

# Basic characteristics
summary(df$age)
summary(df$gender)
summary(df$edu)
summary(df$x1st_dx_group)
summary(df$year_enter_lsch)
summary(df$year_enter_lsch, na.rm = TRUE)
summary(df$year_fu_tmmhc, na.rm = TRUE)

# Comparison of n group

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

# Chi-square if expected counts ok
chi_edu <- chisq.test(tab_edu)
chi_edu

# Fisher if not
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
     main = "Age - No tele", xlab = "Age")
hist(df$age[df$tele_sens_factor == "No_tele_sensitivity"],
     main = "Age - ≥1 tele", xlab = "Age")

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

chi_sens_gender <- chisq.test(tab_gender, correct = FALSE)
chi_sens_gender

fisher_sens_gender <- fisher.test(tab_gender)
fisher_sens_gender

#3. Educational level (edu)

tab_sens_edu <- table(df$tele_sens_factor, df$edu)
tab_sens_edu
chisq.test(tab_edu)$expected

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
hist(df$year_fu_mhs[df$tele_sens_factor == "Tele_sensitivity"], main = "Year FU MHS - No tele (sens)", xlab = "Year followed up in MHS")
hist(df$year_fu_mhs[df$tele_sens_factor == "No_tele_sensitivity"], main = "Year FU MHS - ≥1 tele (sens)", xlab = "Year followed up in MHS")

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
hist(df$year_fu_tmmhc[df$tele_sens_factor == "Tele_sensitivity"], main = "Year FU TMMHC - No tele (sens)", xlab = "Year followed up in TMMHC")
hist(df$year_fu_tmmhc[df$tele_sens_factor == "No_tele_sensitivity"], main = "Year FU TMMHC - ≥1 tele (sens)", xlab = "Year followed up in TMMHC")

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
hist(df$year_enter_lsch[df$tele_sens_factor == "Tele_sensitivity"], main = "Year entered LSCH - No tele (sens)", xlab = "Years Entered LSCH")
hist(df$year_enter_lsch[df$tele_sens_factor == "No_tele_sensitivity"], main = "Year entered LSCH - ≥1 tele (sens)", xlab = "Years Entered LSCH")

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
fisher_sens_dx2
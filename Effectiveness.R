
# Load packages
library(dplyr)    # for data manipulation
library(ggplot2)
library(tidyr)
library(DescTools)


# 1. Read data-----

# Set your path to the Excel file

df <- readRDS("~/Telemed/0922_clean.rds")

# Keep only teleconsultation group
tele_df <- df %>%
  filter(tele_num >= 1)
summary(tele_df)
tele_df$bl_cgi <- as.numeric(tele_df$bl_cgi)
tele_df$f_cgi <- as.numeric(tele_df$f_cgi)
tele_df$bl_sat <- as.numeric(tele_df$bl_sat)
tele_df$f_sat <- as.numeric(tele_df$f_sat)
tele_df$bl_honos <- as.numeric(tele_df$bl_honos)
tele_df$f_honos <- as.numeric(tele_df$f_honos)

saveRDS(tele_df, file = "~/Telemed/0922_tele.rds")

summary(tele_df$f_honos - tele_df$bl_honos)
sd(tele_df$f_honos - tele_df$bl_honos, na.rm = TRUE)

summary(tele_df$f_cgi - tele_df$bl_cgi)
sd(tele_df$f_cgi - tele_df$bl_cgi, na.rm = TRUE)


# Simple graphs
# function

# tele_df should already be your Tele_num >= 1 dataset

plot_bl_f_hist <- function(data, bl_var, f_var,
                           binwidth = 1,
                           x_label = NULL,
                           title  = NULL) {
  # bl_var and f_var should be strings with column names
  
  # Default labels if not supplied
  if (is.null(x_label)) x_label <- "Value"
  if (is.null(title)) {
    title <- paste0("Distribution of ", bl_var, " and ", f_var,
                    "\n(Teleconsultation group)")
  }
  
  plot_df <- data %>%
    select(
      Baseline  = all_of(bl_var),
      Final = all_of(f_var)
    ) %>%
    pivot_longer(cols = c(Baseline, Final),
                 names_to  = "Time",
                 values_to = "Value")
  
  ggplot(plot_df, aes(x = Value)) +
    geom_histogram(binwidth = binwidth,
                   color = "black",
                   fill  = "pink3") +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    stat_bin(binwidth = binwidth,
             geom = "text",
             aes(label = after_stat(count)),
             vjust = -0.3,
             color = "black") +
    facet_wrap(~ Time, nrow = 1) +
    labs(
      title = title,
      x     = x_label,
      y     = "Number of participants"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      strip.text = element_text(face = "bold")
    )
}

plot_bl_f_hist(
  data    = tele_df,
  bl_var  = "bl_opd",
  f_var   = "f_opd",
  binwidth = 1,
  x_label = "Number of OPD attendances",
  title   = "Number of OPD attendances in the 8 months pre-teleconsultation vs 8 months after incorporation of teleconsultation \n(Teleconsultation group, n = 96)"
)

plot_bl_f_hist(
  data    = tele_df,
  bl_var  = "bl_cgi",
  f_var   = "f_cgi",
  binwidth = 1,
  x_label = "CGI score",
  title   = "Clinical Global Impressions (Severity) Scores in the \n 8 months pre-teleconsultation vs 8 months after incorporation of teleconsultation \n(Teleconsultation group having completed both baseline and final assessments, n = 90)"
)

plot_bl_f_hist(
  data    = tele_df,
  bl_var  = "bl_honos",
  f_var   = "f_honos",
  binwidth = 1,
  x_label = "HoNOS score",
  title   = "Health of Nations Outcomes Scale Scores in the \n 8 months pre-teleconsultation vs 8 months after incorporation of teleconsultation \n(Teleconsultation group having completed both baseline and final assessments, n = 90)"
)

plot_bl_f_hist(
  data    = tele_df,
  bl_var  = "bl_swemwbs",
  f_var   = "f_swemwbs",
  binwidth = 1,
  x_label = "SWEMWBS Score",
  title   = "Short Warwick Edinburgh Mental Well-being Scale Scores in the \n 8 months pre-teleconsultation vs 8 months after incorporation of teleconsultation \n(Teleconsultation group having completed both baseline and final assessments, n = 87)"
)
     

plot_bl_f_hist(
  data    = tele_df,
  bl_var  = "bl_sat",
  f_var   = "f_sat",
  binwidth = 1,
  x_label = "Satisfaction score",
  title   = "Overall satisfaction score reflecting the whole outpatient experience \n among participants receiving at least 1 teleconsultation in the 8-month study period"
)




# descriptive data------

tele_df$bl_bd_cat <- as.factor(tele_df$bl_bd)
summary(tele_df$bl_bd_cat)
tele_df$f_bd_cat <- as.factor(tele_df$f_bd)
summary(tele_df$f_bd_cat)

tele_df$bl_aed_cat <- as.factor(tele_df$bl_aed)
summary(tele_df$bl_aed_cat)
tele_df$f_aed_cat <- as.factor(tele_df$f_aed)
summary(tele_df$f_aed_cat)

tele_df$bl_ip
tele_df$f_ip

tele_df$bl_cgi <- as.factor(tele_df$bl_cgi)
summary(tele_df$bl_cgi)
tele_df$f_cgi <- as.factor(tele_df$f_cgi)
summary(tele_df$f_cgi)

# Stuard-Maxwell test for categorical data

cgi_df <- tele_df %>%
  filter(
    !is.na(f_cgi)
  )
summary(cgi_df$bl_cgi)

cgi_table <- table(
  Baseline = cgi_df$bl_cgi,
  Follow_up = cgi_df$f_cgi
)

StuartMaxwellTest(cgi_table)

##

tele_df$bl_honos <- as.factor(tele_df$bl_honos)
tele_df$f_honos <- as.factor(tele_df$f_honos)
summary(tele_df$f_honos)

honos_df <- tele_df %>%
  filter(
    !is.na(f_honos)
  )
summary(honos_df$bl_honos)

honos_table <- table(
  Baseline = honos_df$bl_honos,
  Follow_up = honos_df$f_honos
)

StuartMaxwellTest(honos_table)

##

tele_df$bl_sat <- as.factor(tele_df$bl_sat)
tele_df$f_sat <- as.factor(tele_df$f_sat)
summary(tele_df$f_sat)

sat_df <- tele_df %>%
  filter(
    !is.na(f_sat)
  ) %>%
  filter(
    !is.na(bl_sat)
  )
summary(sat_df$bl_sat)
summary(sat_df$f_sat)

mean(as.numeric(sat_df$bl_sat))

all_levels <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
sat_df$bl_sat <- factor(sat_df$bl_sat, levels = all_levels)
sat_df$f_sat <- factor(sat_df$f_sat, levels = all_levels)

sat_table <- table(
  Baseline = sat_df$bl_sat,
  Follow_up = sat_df$f_sat
)

StuartMaxwellTest(sat_table)

sat_df$bl_sat <- as.numeric(sat_df$bl_sat)
sat_df$f_sat <- as.numeric(sat_df$f_sat)


plot_bl_f_hist(
  data    = sat_df,
  bl_var  = "bl_sat",
  f_var   = "f_sat",
  binwidth = 1,
  x_label = "Satisfaction score",
  title   = "Overall satisfaction score reflecting the whole outpatient experience \n among participants receiving at least 1 teleconsultation in the 8-month study period"
)

# 2. Check normality-----
# Function: compute paired differences and Shapiro–Wilk

check_normality <- function(data, bl_var, f_var, make_plots = TRUE) {
  sub <- data %>%
    select(all_of(c(bl_var, f_var))) %>%
    filter(!is.na(.data[[bl_var]]), !is.na(.data[[f_var]]))
  
  n <- nrow(sub)
  if (n <= 2) {
    message(paste("Too few cases for", bl_var, "vs", f_var, "- skipping."))
    return(NULL)
  }
  
  bl_vals <- sub[[bl_var]]
  f_vals  <- sub[[f_var]]
  diff    <- f_vals - bl_vals
  
  # Shapiro–Wilk test on differences
  sh <- shapiro.test(diff)
  
  cat("\n===========================================\n")
  cat("Normality check for", bl_var, "vs", f_var, "\n")
  cat("n =", n, "\n")
  cat("Shapiro-Wilk W =", round(sh$statistic, 3),
      "p =", round(sh$p.value, 3), "\n")
  
  # Optional plots
  if (make_plots) {
    par(mfrow = c(1, 2))
    hist(diff,
         main = paste("Histogram of differences:", f_var, "-", bl_var),
         xlab = "Difference (F - BL)")
    qqnorm(diff,
           main = paste("Q-Q plot:", f_var, "-", bl_var))
    qqline(diff, col = "red")
    par(mfrow = c(1, 1))
  }
  
  invisible(list(
    n = n,
    W = sh$statistic,
    p_value = sh$p.value,
    diffs = diff
  ))
}

# Run normality checks for each pair

norm_OPD <- check_normality(tele_df, "bl_opd", "f_opd", make_plots = FALSE)
norm_BD  <- check_normality(tele_df, "bl_bd",  "f_bd",  make_plots = FALSE)
norm_AED <- check_normality(tele_df, "bl_aed", "f_aed", make_plots = FALSE)
norm_IP  <- check_normality(tele_df, "bl_ip",  "f_ip",  make_plots = FALSE)

norm_HONOS <- check_normality(tele_df, "bl_honos", "f_honos",  make_plots = FALSE)
norm_SWEMWBS <- check_normality(tele_df, "bl_swemwbs", "f_swemwbs",  make_plots = FALSE)
norm_sat <- check_normality(sat_df, "bl_sat", "f_sat",  make_plots = FALSE)

# 3. Paired t-tests for BL vs F-----

# Function to run paired t-test and return a small summary data frame
paired_summary <- function(data, bl_var, f_var) {
  # Keep only complete cases for the two variables
  sub <- data %>%
    select(all_of(c(bl_var, f_var))) %>%
    filter(!is.na(.data[[bl_var]]), !is.na(.data[[f_var]]))
  
  n <- nrow(sub)
  
  if (n <= 1) {
    return(
      data.frame(
        comparison = paste(bl_var, "vs", f_var),
        n = n,
        mean_BL = NA_real_,
        mean_F  = NA_real_,
        mean_diff_F_minus_BL = NA_real_,
        sd_diff = NA_real_,
        t       = NA_real_,
        p_value = NA_real_
      )
    )
  }
  
  bl_vals <- sub[[bl_var]]
  f_vals  <- sub[[f_var]]
  
  # Paired t-test
  tt <- t.test(f_vals, bl_vals, paired = TRUE)
  
  # Differences
  diff_vals <- f_vals - bl_vals
  
  data.frame(
    comparison = paste(bl_var, "vs", f_var),
    n = n,
    mean_BL = mean(bl_vals, na.rm = TRUE),
    mean_F  = mean(f_vals,  na.rm = TRUE),
    mean_diff_F_minus_BL = mean(diff_vals, na.rm = TRUE),
    sd_diff = sd(diff_vals, na.rm = TRUE),
    t       = unname(tt$statistic),
    p_value = tt$p.value
  )
}

# Run for each pair

res_OPD <- paired_summary(tele_df, "bl_opd", "f_opd")
res_BD  <- paired_summary(tele_df, "bl_bd",  "f_bd")
res_AED <- paired_summary(tele_df, "bl_aed", "f_aed")
res_EMW <- paired_summary(tele_df, "bl_emw", "f_emw")
res_IP  <- paired_summary(tele_df, "bl_ip",  "f_ip")


swemwbs_df <- tele_df %>%
  filter(
    !is.na(f_swemwbs)
  )
sum(is.na(swemwbs_df$f_swemwbs))

res_SWEMWBS <- paired_summary(swemwbs_df, "bl_swemwbs", "f_swemwbs")
res_SWEMWBS

sum(swemwbs_df$bl_swemwbs >= 7 & swemwbs_df$bl_swemwbs <= 19.4, na.rm = TRUE)

swemwbs_bl <- c(
  sum(swemwbs_df$bl_swemwbs >= 7 & swemwbs_df$bl_swemwbs <= 19.4, na.rm = TRUE), 
  sum(swemwbs_df$bl_swemwbs >= 19.5 & swemwbs_df$bl_swemwbs <= 27.4, na.rm = TRUE),
  sum(swemwbs_df$bl_swemwbs >= 27.5, na.rm = TRUE)
)
swemwbs_bl

swemwbs_f <- c(
  sum(swemwbs_df$f_swemwbs >= 7 & swemwbs_df$f_swemwbs <= 19.4, na.rm = TRUE), 
  sum(swemwbs_df$f_swemwbs >= 19.5 & swemwbs_df$f_swemwbs <= 27.4, na.rm = TRUE),
  sum(swemwbs_df$f_swemwbs >= 27.5, na.rm = TRUE)
)
swemwbs_f

res_sat <- paired_summary(tele_df, "bl_sat", "f_sat")

# Combine all results into one table
results <- bind_rows(res_OPD, res_BD, res_AED, res_EMW, res_IP, res_CGI, res_HONOS, res_SWEMWBS, res_sat)
results

# 4. Wilcoxon signed‑rank tests-----
wilcoxon_summary <- function(data, bl_var, f_var) {
sub <- data %>%
  select(all_of(c(bl_var, f_var))) %>%
  filter(!is.na(.data[[bl_var]]), !is.na(.data[[f_var]]))

n <- nrow(sub)
if (n <= 0) {
  return(
    data.frame(
      comparison = paste(bl_var, "vs", f_var),
      n = n,
      V = NA_real_,
      p_value = NA_real_
    )
  )
}

bl_vals <- sub[[bl_var]]
f_vals  <- sub[[f_var]]

wt <- wilcox.test(f_vals, bl_vals, paired = TRUE, exact = FALSE)

data.frame(
  comparison = paste(bl_var, "vs", f_var),
  n = n,
  V = unname(wt$statistic),
  p_value = wt$p.value
)
}

table(tele_df$bl_opd, useNA = "ifany")
table(tele_df$f_opd, useNA = "ifany")
tele_df$diff_opd <- tele_df$f_opd - tele_df$bl_opd
summary(tele_df$diff_opd, useNA = "ifany")
res_OPD_w <- wilcoxon_summary(tele_df, "bl_opd", "f_opd")


table(tele_df$bl_bd, useNA = "ifany")
table(tele_df$f_bd, useNA = "ifany")
tele_df$diff_bd <- tele_df$f_bd - tele_df$bl_bd
summary(tele_df$diff_bd, useNA = "ifany")
res_BD_w  <- wilcoxon_summary(tele_df, "bl_bd",  "f_bd")

table(tele_df$bl_aed, useNA = "ifany")
table(tele_df$f_aed, useNA = "ifany")
tele_df$diff_aed <- tele_df$f_aed - tele_df$bl_aed
summary(tele_df$diff_aed, useNA = "ifany")
res_AED_w <- wilcoxon_summary(tele_df, "bl_aed", "f_aed")

table(tele_df$bl_emw, useNA = "ifany")
table(tele_df$f_emw, useNA = "ifany")
tele_df$diff_emw <- tele_df$f_emw - tele_df$bl_emw
summary(tele_df$diff_emw, useNA = "ifany")
res_EMW_w <- wilcoxon_summary(tele_df, "bl_emw", "f_emw")

table(tele_df$bl_ip, useNA = "ifany")
table(tele_df$f_ip, useNA = "ifany")
tele_df$diff_ip <- tele_df$f_ip - tele_df$bl_ip
summary(tele_df$diff_ip, useNA = "ifany")
res_IP_w  <- wilcoxon_summary(tele_df, "bl_ip",  "f_ip")

table(tele_df$bl_cgi, useNA = "ifany")
table(tele_df$f_cgi, useNA = "ifany")
tele_df$diff_cgi <- tele_df$f_cgi - tele_df$bl_cgi
summary(tele_df$diff_cgi, useNA = "ifany")
res_CGI_w  <- wilcoxon_summary(tele_df, "bl_cgi",  "f_cgi")

table(tele_df$bl_honos, useNA = "ifany")
table(tele_df$f_honos, useNA = "ifany")
tele_df$diff_honos <- tele_df$f_honos - tele_df$bl_honos
summary(tele_df$diff_honos, useNA = "ifany")
res_HONOS_w  <- wilcoxon_summary(tele_df, "bl_honos",  "f_honos")



table(swemwbs_df$bl_swemwbs, useNA = "ifany")
table(swemwbs_df$f_swemwbs, useNA = "ifany")
swemwbs_df$diff_swemwbs <- swemwbs_df$f_swemwbs - swemwbs_df$bl_swemwbs
summary(swemwbs_df$diff_swemwbs, useNA = "ifany")
res_SWEMWBS_w  <- wilcoxon_summary(swemwbs_df, "bl_swemwbs",  "f_swemwbs")
res_SWEMWBS_w

table(sat_df$bl_sat, useNA = "ifany")
table(sat_df$f_sat, useNA = "ifany")
sat_df$diff_sat <- sat_df$f_sat - sat_df$bl_sat
summary(sat_df$bl_sat)
summary(sat_df$f_sat)
summary(sat_df$diff_sat, useNA = "ifany")
res_sat_w  <- wilcoxon_summary(sat_df, "bl_sat",  "f_sat")

wilcoxon_results <- bind_rows(res_OPD_w, res_BD_w, res_AED_w, res_EMW_w, res_IP_w, res_CGI_w, res_HONOS_w, res_SWEMWBS_w, res_sat_w)
wilcoxon_results

res_SWEMWBS_w  <- wilcoxon_summary(tele_df, "bl_swemwbs",  "f_swemwbs")

####
res_sat_w  <- chisq.test(tele_df, "sopeq_b_2",  "sopeq_f_2")
wilcoxon_results_SOPEQ <- bind_rows()


summary
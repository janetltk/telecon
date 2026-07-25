# ---- 1. Setup ------

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)

# Load data and keep only teleconsultation-group participants
# who also completed the follow-up assessment
df <- readRDS("~/Telemed/0719_clean.rds")

tele_df <- df %>%
  filter(tele_num >= 1, !is.na(f_ax_date))

summary(tele_df)


# ---- 2. Core helper functions -----

# Coerce a column to plain numeric no matter whether it arrives as
# numeric, factor, character, or haven_labelled (common after SPSS/Excel
# import). This is what fixes the "'x' must be numeric" error from
# wilcox.test().
to_numeric_safe <- function(x) {
  if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
  suppressWarnings(as.numeric(as.character(x)))
}

# Keep only paired baseline/follow-up values that fall within the valid
# ordinal scale (keep_levels). Anything outside keep_levels (e.g. "not
# applicable" codes) is set to NA, and unpaired rows are dropped.
recode_sopeq_item <- function(data, bl_var, f_var, keep_levels) {
  out <- data %>%
    select(all_of(c(bl_var, f_var))) %>%
    mutate(across(everything(), to_numeric_safe))
  
  out[[bl_var]][!out[[bl_var]] %in% keep_levels] <- NA
  out[[f_var]][!out[[f_var]] %in% keep_levels] <- NA
  
  drop_na(out, all_of(c(bl_var, f_var)))
}

# Paired Wilcoxon signed-rank test + descriptive stats for one item
analyze_sopeq_item <- function(bl_var, f_var, keep_levels, data = tele_df) {
  item_df <- recode_sopeq_item(data, bl_var, f_var, keep_levels)
  
  if (nrow(item_df) == 0) {
    return(tibble::tibble(
      n_paired = 0, median_BL = NA_real_, median_F = NA_real_,
      V = NA_real_, p_value = NA_real_
    ))
  }
  
  wt <- suppressWarnings(
    wilcox.test(item_df[[f_var]], item_df[[bl_var]], paired = TRUE, exact = FALSE)
  )
  
  tibble::tibble(
    n_paired  = nrow(item_df),
    median_BL = median(item_df[[bl_var]], na.rm = TRUE),
    median_F  = median(item_df[[f_var]], na.rm = TRUE),
    V         = unname(wt$statistic),
    p_value   = wt$p.value
  )
}


# ---- 3. Wilcoxon tests across all 27 SOPEQ items ------------

sopeq_specs <- tibble::tibble(
  item_name = c(
    "Q2_wait_time", "Q3_dr_awareness", "Q4_duration", "Q5_enough_time",
    "Q6_explain_reasons", "Q7_listen_views", "Q8_clear_ans", "Q9_trust",
    "Q10_explain", "Q11_involved", "Q12_introduce", "Q13_information",
    "Q14_fear", "Q15_family_info", "Q16_family_talk", "Q17_explain_meds",
    "Q18_explain_med_purpose", "Q19_med_SE", "Q20_med_info", "Q21_danger_sign",
    "Q22_sat", "Q23_manage", "Q24_respect", "Q25_privacy", "Q26_care",
    "Q27_dr_rate", "Q28_complain"
  ),
  bl_var = c(
    "sopeq_b_2", "sopeq_b_3", "sopeq_b_4", "sopeq_b_5", "sopeq_b_6",
    "sopeq_b_7", "sopeq_b_8", "sopeq_b_9", "sopeq_b_10", "sopeq_b_11",
    "sopeq_b_12", "sopeq_b_13", "sopeq_b_14", "sopeq_b_15", "sopeq_b_16",
    "sopeq_b_17", "sopeq_b_18", "sopeq_b_19", "sopeq_b_20", "sopeq_b_21",
    "sopeq_b_22", "sopeq_b_23", "sopeq_b_24", "sopeq_b_25", "sopeq_b_26",
    "sopeq_b_27", "sopeq_b_28"
  ),
  f_var = c(
    "sopeq_f_2", "sopeq_f_3", "sopeq_f_4", "sopeq_f_5", "sopeq_f_6",
    "sopeq_f_7", "sopeq_f_8", "sopeq_f_9", "sopeq_f_10", "sopeq_f_11",
    "sopeq_f_12", "sopeq_f_13", "sopeq_f_14", "sopeq_f_15", "sopeq_f_16",
    "sopeq_f_17", "sopeq_f_18", "sopeq_f_19", "sopeq_f_20", "sopeq_f_21",
    "sopeq_f_22", "sopeq_f_23", "sopeq_f_24", "sopeq_f_25", "sopeq_f_26",
    "sopeq_f_27", "sopeq_f_28"
  ),
  # Numeric codes that belong to the ordered satisfaction scale for each
  # item; any other code (e.g. "not applicable") is treated as missing.
  keep_levels = list(
    1:4, 1:3, 1:4, 1:4, 1:3, 1:3, 1:3, 1:3, 1:4, 1:3, 1:3, 1:4, 1:3, 1:3,
    1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:5, 1:3
  )
)

sopeq_results <- sopeq_specs %>%
  mutate(res = pmap(list(bl_var, f_var, keep_levels), analyze_sopeq_item)) %>%
  unnest(res)

print(sopeq_results, n = nrow(sopeq_results))

# ---- 4. Generic histogram helper (numeric-scale items) ------

plot_bl_f_hist <- function(data, bl_var, f_var,
                           binwidth = 1,
                           x_label = "Value",
                           title = NULL) {
  if (is.null(title)) {
    title <- paste0("Distribution of ", bl_var, " and ", f_var,
                    "\n(Teleconsultation group)")
  }
  
  plot_df <- data %>%
    select(Baseline = all_of(bl_var), Final = all_of(f_var)) %>%
    pivot_longer(cols = c(Baseline, Final), names_to = "Time", values_to = "Value")
  
  ggplot(plot_df, aes(x = Value)) +
    geom_histogram(binwidth = binwidth, color = "black", fill = "pink3") +
    scale_x_continuous(breaks = pretty_breaks()) +
    stat_bin(binwidth = binwidth, geom = "text",
             aes(label = after_stat(count)), vjust = -0.3, color = "black") +
    facet_wrap(~ Time, nrow = 1) +
    labs(title = title, x = x_label, y = "Number of participants") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          strip.text = element_text(face = "bold"))
}


# ---- 5. Categorical items: bar chart + % stacked chart ------

# One row per item you want to visualise as labelled categories.
# response_labels must be in the same order as numeric codes 1, 2, 3, ...
plot_specs <- tibble::tibble(
  item_id = c("Q2", "Q3", "Q4", "Q13", "Q17", "Q18"),
  bl_var  = c("sopeq_b_2", "sopeq_b_3","sopeq_b_4", "sopeq_b_13", "sopeq_b_17", "sopeq_b_18"),
  f_var   = c("sopeq_f_2", "sopeq_f_3", "sopeq_f_4", "sopeq_f_13", "sopeq_f_17", "sopeq_f_18"),
  response_labels = list(
    c("Less than half an hour", "Half an hour to 1 hour",
      "1 to 2 hours", "More than 2 hours"),
    c("He/she knew enough", "He/she knew part of my medical history", "He/she knew little to nothing"),
    c("Less than 5 minutes", "5 to 10 minutes",
      "10 to 20 minutes", "More than 20 minutes"),
    c("Right amount", "Too much", "Not enough",
      "I was not given any information about my treatment or condition"),
    c("Yes, completely", "Yes, to some extent", "No",
      "I did not need an explanation"),
    c("Yes, completely", "Yes, to some extent", "No",
      "I did not need an explanation")
  ),
  topic = c(
    "Patients' perceived time difference between the stated time and the actual time of seeing doctor",
    "Patients' perceived doctor's awareness of own medical history",
    "Patients' perceived duration of consultation",
    "Patients' perceived amount of information given about own condition or treatment",
    "Patients' perceived amount of explanation given on how to take medications",
    "Patients' perceived amount of information given on the purpose of medications"
  ),
  x_lab = c(
    "Duration of time difference",
    "Awareness of medical history",
    "Duration of consultation time",
    "Information given",
    "Explanation given",
    "Explanation given"
  )
)

# Turn one item's baseline/follow-up pair into a long data frame of
# labelled responses, ready for plotting.
prepare_item_for_plot <- function(bl_var, f_var, response_labels, data = tele_df) {
  n_levels <- length(response_labels)
  
  item_df <- recode_sopeq_item(data, bl_var, f_var, keep_levels = seq_len(n_levels))
  
  item_df %>%
    transmute(
      Baseline = factor(.data[[bl_var]], levels = seq_len(n_levels),
                        labels = response_labels, ordered = TRUE),
      Final    = factor(.data[[f_var]], levels = seq_len(n_levels),
                        labels = response_labels, ordered = TRUE)
    ) %>%
    pivot_longer(everything(), names_to = "Time", values_to = "Response") %>%
    mutate(Time = factor(Time, levels = c("Baseline", "Final")))
}

# Grouped bar chart of raw counts
plot_item_bar <- function(long_df, topic, x_lab, n_paired) {
  ggplot(long_df, aes(x = Response)) +
    geom_bar(fill = "pink2", color = "black") +
    facet_wrap(~ Time, nrow = 1) +
    labs(
      title = paste0(topic,
                     "\n8 months pre-teleconsultation vs 8 months after ",
                     "(teleconsultation group, n = ", n_paired, ")"),
      x = x_lab,
      y = "Number of respondents"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          strip.text = element_text(face = "bold"))
}

# 100% stacked bar chart of proportions, with respondent counts shown
# on the chart itself: each segment is labelled with its raw count (n),
# and the total number of respondents is printed above each bar.
plot_item_stacked <- function(long_df, topic, n_paired) {
  prop_df <- long_df %>%
    count(Time, Response, name = "n") %>%
    group_by(Time) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()
  
  total_df <- long_df %>%
    count(Time, name = "n_total")
  
  ggplot(prop_df, aes(x = Time, y = prop, fill = Response)) +
    geom_col(color = "black") +
    # count label inside each segment (skip labelling segments with n = 0)
    geom_text(
      aes(label = ifelse(n > 0, n, "")),
      position = position_stack(vjust = 0.5),
      size = 3.2, color = "black"
    ) +
    scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.08))) +
    scale_fill_brewer(palette = "Reds") +
    labs(
      title = paste0(topic,
                     "\n8 months pre-teleconsultation vs 8 months after ",
                     "incorporation of teleconsultation",
                     "\n(teleconsultation group having completed both ",
                     "baseline and final assessments, n = ", n_paired, ")"),
      x = "", y = "Percentage of respondents", fill = "Legend"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

# Run the whole pipeline for one item and return both plots + the data
make_item_plots <- function(bl_var, f_var, response_labels, topic, x_lab, data = tele_df) {
  long_df  <- prepare_item_for_plot(bl_var, f_var, response_labels, data)
  n_paired <- nrow(long_df) / 2  # each respondent contributes 2 rows (Baseline + Final)
  
  list(
    data    = long_df,
    bar     = plot_item_bar(long_df, topic, x_lab, n_paired),
    stacked = plot_item_stacked(long_df, topic, n_paired)
  )
}

# Generate plots for every item in plot_specs in one go
item_plots <- plot_specs %>%
  pmap(function(item_id, bl_var, f_var, response_labels, topic, x_lab) {
    make_item_plots(bl_var, f_var, response_labels, topic, x_lab)
  }) %>%
  set_names(plot_specs$item_id)

# Access any item's plots like this:
item_plots$Q2$bar
item_plots$Q2$stacked

item_plots$Q3$bar
item_plots$Q3$stacked

item_plots$Q4$bar
item_plots$Q4$stacked

item_plots$Q13$bar
item_plots$Q13$stacked

item_plots$Q17$bar
item_plots$Q17$stacked

item_plots$Q18$bar
item_plots$Q18$stacked

# Sensitivity analysis - see if the same pattern in Q3, 13, 17, 18 occurs with just TM LSCH==================

# who also completed the follow-up assessment
df <- readRDS("~/Telemed/0719_clean.rds")

tele_df <- df %>%
  filter(tele_num >= 1 & hostel == "TM", !is.na(f_ax_date))

summary(tele_df)

sopeq_specs <- tibble::tibble(
  item_name = c(
    "Q2_wait_time", "Q3_dr_awareness", "Q4_duration", "Q5_enough_time",
    "Q6_explain_reasons", "Q7_listen_views", "Q8_clear_ans", "Q9_trust",
    "Q10_explain", "Q11_involved", "Q12_introduce", "Q13_information",
    "Q14_fear", "Q15_family_info", "Q16_family_talk", "Q17_explain_meds",
    "Q18_explain_med_purpose", "Q19_med_SE", "Q20_med_info", "Q21_danger_sign",
    "Q22_sat", "Q23_manage", "Q24_respect", "Q25_privacy", "Q26_care",
    "Q27_dr_rate", "Q28_complain"
  ),
  bl_var = c(
    "sopeq_b_2", "sopeq_b_3", "sopeq_b_4", "sopeq_b_5", "sopeq_b_6",
    "sopeq_b_7", "sopeq_b_8", "sopeq_b_9", "sopeq_b_10", "sopeq_b_11",
    "sopeq_b_12", "sopeq_b_13", "sopeq_b_14", "sopeq_b_15", "sopeq_b_16",
    "sopeq_b_17", "sopeq_b_18", "sopeq_b_19", "sopeq_b_20", "sopeq_b_21",
    "sopeq_b_22", "sopeq_b_23", "sopeq_b_24", "sopeq_b_25", "sopeq_b_26",
    "sopeq_b_27", "sopeq_b_28"
  ),
  f_var = c(
    "sopeq_f_2", "sopeq_f_3", "sopeq_f_4", "sopeq_f_5", "sopeq_f_6",
    "sopeq_f_7", "sopeq_f_8", "sopeq_f_9", "sopeq_f_10", "sopeq_f_11",
    "sopeq_f_12", "sopeq_f_13", "sopeq_f_14", "sopeq_f_15", "sopeq_f_16",
    "sopeq_f_17", "sopeq_f_18", "sopeq_f_19", "sopeq_f_20", "sopeq_f_21",
    "sopeq_f_22", "sopeq_f_23", "sopeq_f_24", "sopeq_f_25", "sopeq_f_26",
    "sopeq_f_27", "sopeq_f_28"
  ),
  # Numeric codes that belong to the ordered satisfaction scale for each
  # item; any other code (e.g. "not applicable") is treated as missing.
  keep_levels = list(
    1:4, 1:3, 1:4, 1:4, 1:3, 1:3, 1:3, 1:3, 1:4, 1:3, 1:3, 1:4, 1:3, 1:3,
    1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:5, 1:3
  )
)

sopeq_results <- sopeq_specs %>%
  mutate(res = pmap(list(bl_var, f_var, keep_levels), analyze_sopeq_item)) %>%
  unnest(res)

print(sopeq_results, n = nrow(sopeq_results))



# ---- 5. Categorical items: bar chart + % stacked chart ------

# One row per item you want to visualise as labelled categories.
# response_labels must be in the same order as numeric codes 1, 2, 3, ...
plot_specs <- tibble::tibble(
  item_id = c("Q2", "Q3", "Q4", "Q13", "Q17", "Q18"),
  bl_var  = c("sopeq_b_2", "sopeq_b_3","sopeq_b_4", "sopeq_b_13", "sopeq_b_17", "sopeq_b_18"),
  f_var   = c("sopeq_f_2", "sopeq_f_3", "sopeq_f_4", "sopeq_f_13", "sopeq_f_17", "sopeq_f_18"),
  response_labels = list(
    c("Less than half an hour", "Half an hour to 1 hour",
      "1 to 2 hours", "More than 2 hours"),
    c("He/she knew enough", "He/she knew part of my medical history", "He/she knew little to nothing"),
    c("Less than 5 minutes", "5 to 10 minutes",
      "10 to 20 minutes", "More than 20 minutes"),
    c("Right amount", "Too much", "Not enough",
      "I was not given any information about my treatment or condition"),
    c("Yes, completely", "Yes, to some extent", "No",
      "I did not need an explanation"),
    c("Yes, completely", "Yes, to some extent", "No",
      "I did not need an explanation")
  ),
  topic = c(
    "Patients' perceived time difference between the stated time and the actual time of seeing doctor",
    "Patients' perceived doctor's awareness of own medical history",
    "Patients' perceived duration of consultation",
    "Patients' perceived amount of information given about own condition or treatment",
    "Patients' perceived amount of explanation given on how to take medications",
    "Patients' perceived amount of information given on the purpose of medications"
  ),
  x_lab = c(
    "Duration of time difference",
    "Awareness of medical history",
    "Duration of consultation time",
    "Information given",
    "Explanation given",
    "Explanation given"
  )
)

# Generate plots for every item in plot_specs in one go
item_plots <- plot_specs %>%
  pmap(function(item_id, bl_var, f_var, response_labels, topic, x_lab) {
    make_item_plots(bl_var, f_var, response_labels, topic, x_lab)
  }) %>%
  set_names(plot_specs$item_id)

item_plots$Q2$bar
item_plots$Q2$stacked

item_plots$Q3$bar
item_plots$Q3$stacked

item_plots$Q4$bar
item_plots$Q4$stacked

item_plots$Q13$bar
item_plots$Q13$stacked

item_plots$Q17$bar
item_plots$Q17$stacked

item_plots$Q18$bar
item_plots$Q18$stacked



# Sensitivity analysis - see if the same pattern in Q3, 13, 17, 18 occurs with just TM LSCH==================

df <- readRDS("~/Telemed/0719_clean.rds")

tele_df <- df %>%
  filter(tele_num >= 1 & hostel == "TM", !is.na(f_ax_date))

summary(tele_df)

sopeq_specs <- tibble::tibble(
  item_name = c(
    "Q2_wait_time", "Q3_dr_awareness", "Q4_duration", "Q5_enough_time",
    "Q6_explain_reasons", "Q7_listen_views", "Q8_clear_ans", "Q9_trust",
    "Q10_explain", "Q11_involved", "Q12_introduce", "Q13_information",
    "Q14_fear", "Q15_family_info", "Q16_family_talk", "Q17_explain_meds",
    "Q18_explain_med_purpose", "Q19_med_SE", "Q20_med_info", "Q21_danger_sign",
    "Q22_sat", "Q23_manage", "Q24_respect", "Q25_privacy", "Q26_care",
    "Q27_dr_rate", "Q28_complain"
  ),
  bl_var = c(
    "sopeq_b_2", "sopeq_b_3", "sopeq_b_4", "sopeq_b_5", "sopeq_b_6",
    "sopeq_b_7", "sopeq_b_8", "sopeq_b_9", "sopeq_b_10", "sopeq_b_11",
    "sopeq_b_12", "sopeq_b_13", "sopeq_b_14", "sopeq_b_15", "sopeq_b_16",
    "sopeq_b_17", "sopeq_b_18", "sopeq_b_19", "sopeq_b_20", "sopeq_b_21",
    "sopeq_b_22", "sopeq_b_23", "sopeq_b_24", "sopeq_b_25", "sopeq_b_26",
    "sopeq_b_27", "sopeq_b_28"
  ),
  f_var = c(
    "sopeq_f_2", "sopeq_f_3", "sopeq_f_4", "sopeq_f_5", "sopeq_f_6",
    "sopeq_f_7", "sopeq_f_8", "sopeq_f_9", "sopeq_f_10", "sopeq_f_11",
    "sopeq_f_12", "sopeq_f_13", "sopeq_f_14", "sopeq_f_15", "sopeq_f_16",
    "sopeq_f_17", "sopeq_f_18", "sopeq_f_19", "sopeq_f_20", "sopeq_f_21",
    "sopeq_f_22", "sopeq_f_23", "sopeq_f_24", "sopeq_f_25", "sopeq_f_26",
    "sopeq_f_27", "sopeq_f_28"
  ),
  # Numeric codes that belong to the ordered satisfaction scale for each
  # item; any other code (e.g. "not applicable") is treated as missing.
  keep_levels = list(
    1:4, 1:3, 1:4, 1:4, 1:3, 1:3, 1:3, 1:3, 1:4, 1:3, 1:3, 1:4, 1:3, 1:3,
    1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:5, 1:3
  )
)

sopeq_results <- sopeq_specs %>%
  mutate(res = pmap(list(bl_var, f_var, keep_levels), analyze_sopeq_item)) %>%
  unnest(res)

print(sopeq_results, n = nrow(sopeq_results))

plot_specs <- tibble::tibble(
  item_id = c("Q2", "Q3", "Q4", "Q13", "Q17", "Q18"),
  bl_var  = c("sopeq_b_2", "sopeq_b_3","sopeq_b_4", "sopeq_b_13", "sopeq_b_17", "sopeq_b_18"),
  f_var   = c("sopeq_f_2", "sopeq_f_3", "sopeq_f_4", "sopeq_f_13", "sopeq_f_17", "sopeq_f_18"),
  response_labels = list(
    c("Less than half an hour", "Half an hour to 1 hour",
      "1 to 2 hours", "More than 2 hours"),
    c("He/she knew enough", "He/she knew part of my medical history", "He/she knew little to nothing"),
    c("Less than 5 minutes", "5 to 10 minutes",
      "10 to 20 minutes", "More than 20 minutes"),
    c("Right amount", "Too much", "Not enough",
      "I was not given any information about my treatment or condition"),
    c("Yes, completely", "Yes, to some extent", "No",
      "I did not need an explanation"),
    c("Yes, completely", "Yes, to some extent", "No",
      "I did not need an explanation")
  ),
  topic = c(
    "Patients' perceived time difference between the stated time and the actual time of seeing doctor",
    "Patients' perceived doctor's awareness of own medical history",
    "Patients' perceived duration of consultation",
    "Patients' perceived amount of information given about own condition or treatment",
    "Patients' perceived amount of explanation given on how to take medications",
    "Patients' perceived amount of information given on the purpose of medications"
  ),
  x_lab = c(
    "Duration of time difference",
    "Awareness of medical history",
    "Duration of consultation time",
    "Information given",
    "Explanation given",
    "Explanation given"
  )
)

# Generate plots for every item in plot_specs in one go
item_plots <- plot_specs %>%
  pmap(function(item_id, bl_var, f_var, response_labels, topic, x_lab) {
    make_item_plots(bl_var, f_var, response_labels, topic, x_lab)
  }) %>%
  set_names(plot_specs$item_id)

item_plots$Q2$bar
item_plots$Q2$stacked

item_plots$Q3$bar
item_plots$Q3$stacked

item_plots$Q4$bar
item_plots$Q4$stacked

item_plots$Q13$bar
item_plots$Q13$stacked

item_plots$Q17$bar
item_plots$Q17$stacked

item_plots$Q18$bar
item_plots$Q18$stacked



## sensitivity analysis using only SL LSCH =============

df <- readRDS("~/Telemed/0719_clean.rds")

tele_df <- df %>%
  filter(tele_num >= 1 & hostel == "SL", !is.na(f_ax_date))

summary(tele_df)

sopeq_specs <- tibble::tibble(
  item_name = c(
    "Q2_wait_time", "Q3_dr_awareness", "Q4_duration", "Q5_enough_time",
    "Q6_explain_reasons", "Q7_listen_views", "Q8_clear_ans", "Q9_trust",
    "Q10_explain", "Q11_involved", "Q12_introduce", "Q13_information",
    "Q14_fear", "Q15_family_info", "Q16_family_talk", "Q17_explain_meds",
    "Q18_explain_med_purpose", "Q19_med_SE", "Q20_med_info", "Q21_danger_sign",
    "Q22_sat", "Q23_manage", "Q24_respect", "Q25_privacy", "Q26_care",
    "Q27_dr_rate", "Q28_complain"
  ),
  bl_var = c(
    "sopeq_b_2", "sopeq_b_3", "sopeq_b_4", "sopeq_b_5", "sopeq_b_6",
    "sopeq_b_7", "sopeq_b_8", "sopeq_b_9", "sopeq_b_10", "sopeq_b_11",
    "sopeq_b_12", "sopeq_b_13", "sopeq_b_14", "sopeq_b_15", "sopeq_b_16",
    "sopeq_b_17", "sopeq_b_18", "sopeq_b_19", "sopeq_b_20", "sopeq_b_21",
    "sopeq_b_22", "sopeq_b_23", "sopeq_b_24", "sopeq_b_25", "sopeq_b_26",
    "sopeq_b_27", "sopeq_b_28"
  ),
  f_var = c(
    "sopeq_f_2", "sopeq_f_3", "sopeq_f_4", "sopeq_f_5", "sopeq_f_6",
    "sopeq_f_7", "sopeq_f_8", "sopeq_f_9", "sopeq_f_10", "sopeq_f_11",
    "sopeq_f_12", "sopeq_f_13", "sopeq_f_14", "sopeq_f_15", "sopeq_f_16",
    "sopeq_f_17", "sopeq_f_18", "sopeq_f_19", "sopeq_f_20", "sopeq_f_21",
    "sopeq_f_22", "sopeq_f_23", "sopeq_f_24", "sopeq_f_25", "sopeq_f_26",
    "sopeq_f_27", "sopeq_f_28"
  ),
  # Numeric codes that belong to the ordered satisfaction scale for each
  # item; any other code (e.g. "not applicable") is treated as missing.
  keep_levels = list(
    1:4, 1:3, 1:4, 1:4, 1:3, 1:3, 1:3, 1:3, 1:4, 1:3, 1:3, 1:4, 1:3, 1:3,
    1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:3, 1:5, 1:3
  )
)

sopeq_results <- sopeq_specs %>%
  mutate(res = pmap(list(bl_var, f_var, keep_levels), analyze_sopeq_item)) %>%
  unnest(res)

print(sopeq_results, n = nrow(sopeq_results))


##################################
##################################
##################################


#=== OLD CODES FOR REFERENCE ONLY==============
# Helper: recode one SOPEQ item (baseline + follow-up) to numeric (ordinal) and NA out 'not applicable'
recode_sopeq_item <- function(data, bl_var, f_var, keep_levels) {
  # keep_levels: numeric vector of codes that are part of the ordered satisfaction scale
  # other codes will be set to NA for the Wilcoxon analysis
  
  out <- data %>%
    select(all_of(c(bl_var, f_var)))
  
  # Set to NA if not in keep_levels
  out[[bl_var]][!out[[bl_var]] %in% keep_levels] <- NA
  out[[f_var]][!out[[f_var]] %in% keep_levels] <- NA
  
  # Return only rows with paired non-NA
  out <- out %>% drop_na(all_of(c(bl_var, f_var)))
  
  out
} 

# Wilcoxon test for all questions-----
# Specification for each SOPEQ item you want to test
# (Fill in with the correct variable names and keep_levels per item)
sopeq_specs <- tibble::tibble(
  item_name  = c("Q2_wait_time",
                 "Q19_dr_awareness",
                 "Q4_duration",
                 "Q21_enough_time",
                 "Q22_explain_reasons",
                 "Q23_listen_views",
                 "Q24_clear_ans",
                 "Q25_trust",
                 "Q29_explain",
                 "Q30_involved",
                 "Q32_introduce",
                 "Q33_information",
                 "Q34_fear",
                 "Q35_family_info",
                 "Q36_family_talk",
                 "Q38_explain_meds",
                 "Q39_explain_med_purpose",
                 "Q40_med_SE",
                 "Q41_med_info",
                 "Q42_danger_sign",
                 "Q47_sat",
                 "Q48_manage",
                 "Q49_respect",
                 "Q50_privacy",
                 "Q51_care",
                 "Q52_dr_rate",
                 "Q53_complain"
  ),
  bl_var     = c("sopeq_b_2",
                 "sopeq_b_3",
                 "sopeq_b_4",
                 "sopeq_b_5",
                 "sopeq_b_6",
                 "sopeq_b_7",
                 "sopeq_b_8",
                 "sopeq_b_9",
                 "sopeq_b_10",
                 "sopeq_b_11",
                 "sopeq_b_12",
                 "sopeq_b_13",
                 "sopeq_b_14",
                 "sopeq_b_15",
                 "sopeq_b_16",
                 "sopeq_b_17",
                 "sopeq_b_18",
                 "sopeq_b_19",
                 "sopeq_b_20",
                 "sopeq_b_21",
                 "sopeq_b_22",
                 "sopeq_b_23",
                 "sopeq_b_24",
                 "sopeq_b_25",
                 "sopeq_b_26",
                 "sopeq_b_27",
                 "sopeq_b_28"
  ),
  f_var      = c("sopeq_f_2",
                 "sopeq_f_3",
                 "sopeq_f_4",
                 "sopeq_f_5",
                 "sopeq_f_6",
                 "sopeq_f_7",
                 "sopeq_f_8",
                 "sopeq_f_9",
                 "sopeq_f_10",
                 "sopeq_f_11",
                 "sopeq_f_12",
                 "sopeq_f_13",
                 "sopeq_f_14",
                 "sopeq_f_15",
                 "sopeq_f_16",
                 "sopeq_f_17",
                 "sopeq_f_18",
                 "sopeq_f_19",
                 "sopeq_f_20",
                 "sopeq_f_21",
                 "sopeq_f_22",
                 "sopeq_f_23",
                 "sopeq_f_24",
                 "sopeq_f_25",
                 "sopeq_f_26",
                 "sopeq_f_27",
                 "sopeq_f_28"
  ),
  # For each item, define which numeric codes are part of the ordinal satisfaction scale
  # Example: 1=best, 2=middle, 3=worst; codes 4+ are 'not applicable'
  keep_levels = list(
    1:4, # Q2
    1:3, # Q3/19
    1:4, # Q4
    1:4, # Q5/21 (if 5 is NA)
    1:3, # Q6/22
    1:3, # Q7/23
    1:3, # Q8/24
    1:3, # Q9/25
    1:4, # Q10/29
    1:3, # Q11/30
    1:3, # Q12/32
    1:4, # Q13/33
    1:3, # Q14/34
    1:3, # Q15/35
    1:3, # Q16/36
    1:3, # Q17/38
    1:3, # Q18/39
    1:3, # Q19/40
    1:3, # Q20/41
    1:3, # Q21/42
    1:3, # Q22/47
    1:3, # Q23/48
    1:3, # Q24/49
    1:3, # Q25/50
    1:3, # Q26/51
    1:5, # Q27/52
    1:3 # Q28/56
  )
)

# Function: run Wilcoxon and compute medians
analyze_sopeq_item <- function(bl_var, f_var, keep_levels) {
  item_df <- recode_sopeq_item(tele_df, bl_var, f_var, keep_levels)
  
  if (nrow(item_df) == 0) {
    return(
      tibble::tibble(
        n_paired   = 0,
        median_BL  = NA_real_,
        median_F   = NA_real_,
        V          = NA_real_,
        p_value    = NA_real_
      )
    )
  }
  
  wt <- suppressWarnings(
    wilcox.test(item_df[[f_var]],
                item_df[[bl_var]],
                paired = TRUE,
                exact  = FALSE)
  )
  
  tibble::tibble(
    n_paired   = nrow(item_df),
    median_BL  = median(item_df[[bl_var]], na.rm = TRUE),
    median_F   = median(item_df[[f_var]], na.rm = TRUE),
    V          = unname(wt$statistic),
    p_value    = wt$p.value
  )
}

# Apply to all items
sopeq_results <- sopeq_specs %>%
  mutate(
    res = pmap(
      list(bl_var, f_var, keep_levels),
      ~ analyze_sopeq_item(..1, ..2, ..3)
    )
  ) %>%
  unnest(res)

print(sopeq_results, n = 60)

### Graphical representation------

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
  

### Q2
item_data <- recode_sopeq_item(
  data        = tele_df,
  bl_var      = "sopeq_b_2",
  f_var       = "sopeq_f_2",
  keep_levels = 1:4
)

# Optional: recode to ordered labels for tables/plots
item_data <- item_data %>%
  mutate(
    Q2_BL = factor(sopeq_b_2,
                    levels  = c(1, 2, 3, 4),
                    labels  = c("Less than half an hour",
                                "Half an hour to 1 hour",
                                "1 to 2 hours", "More than 2 hours"),
                    ordered = TRUE),
    Q2_F  = factor(sopeq_f_2,
                   levels  = c(1, 2, 3, 4),
                   labels  = c("Less than half an hour",
                               "Half an hour to 1 hour",
                               "1 to 2 hours", "More than 2 hours"),
                   ordered = TRUE),
  )

plot_q2 <- item_data %>%
  select(Q2_BL, Q2_F) %>%
  pivot_longer(cols = c(Q2_BL, Q2_F),
               names_to  = "Time",
               values_to = "Response") %>%
  mutate(
    Time = recode(Time,
                  Q2_BL = "Baseline",
                  Q2_F  = "Final")
  )

ggplot(plot_q2, aes(x = Response)) +
  geom_bar(fill = "pink2", color = "black") +
  facet_wrap(~ Time, nrow = 1) +
  labs(
    title = "Duration of waiting time before seeing doctor \n 8 months pre-teleconsultation vs 8 months after (teleconsultation group)",
    x     = "Duration of waiting time",
    y     = "Number of respondents"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold")
  )


# stacked presentation
plot_q2_prop <- plot_q2 %>%
  group_by(Time, Response) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Time) %>%
  mutate(prop = n / sum(n))

ggplot(plot_q2_prop, aes(x = Time, y = prop, fill = Response)) +
  geom_col(color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Self-reported duration of waiting time before seeing doctor \n 8 months pre-teleconsultation vs 8 months after incorporation of teleconsultation \n (Teleconsultation group having completed both baseline and final assessments, n = 79)",
    x     = "",
    y     = "Percentage of respondents",
    fill  = "Legend"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

  ### Q4

item_data <- recode_sopeq_item(
  data        = tele_df,
  bl_var      = "sopeq_b_4",
  f_var       = "sopeq_f_4",
  keep_levels = 1:4
)

# Optional: recode to ordered labels for tables/plots
item_data <- item_data %>%
  mutate(
    Q4_BL = factor(sopeq_b_4,
                   levels  = c(1, 2, 3, 4),
                   labels  = c("Less than 5 minutes",
                               "5 to 10 minutes",
                               "10 to 20 minutes", "More than 20 minutes"),
                   ordered = TRUE),
    Q4_F  = factor(sopeq_f_4,
                   levels  = c(1, 2, 3, 4),
                   labels  = c("Less than 5 minutes",
                               "5 to 10 minutes",
                               "10 to 20 minutes", "More than 20 minutes"),
                   ordered = TRUE),
  )

plot_q4 <- item_data %>%
  select(Q4_BL, Q4_F) %>%
  pivot_longer(cols = c(Q4_BL, Q4_F),
               names_to  = "Time",
               values_to = "Response") %>%
  mutate(
    Time = recode(Time,
                  Q4_BL = "Baseline",
                  Q4_F  = "Final")
  )


ggplot(plot_q4, aes(x = Response)) +
  geom_bar(fill = "pink2", color = "black") +
  facet_wrap(~ Time, nrow = 1) +
  labs(
    title = "Self-reported uration of consultation time \n 8 months pre-teleconsultation vs 8 months after (teleconsultation group)",
    x     = "Duration of consultation time",
    y     = "Number of respondents"
  ) +
  scale_fill_brewer(palette = "Pastel") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold"))
  
# stacked presentation
plot_q4_prop <- plot_q4 %>%
  group_by(Time, Response) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Time) %>%
  mutate(prop = n / sum(n))


ggplot(plot_q4_prop, aes(x = Time, y = prop, fill = Response)) +
  geom_col(color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Self-reported duration of consultation time \n 8 months pre-teleconsultation vs 8 months after incorporation of teleconsultation \n (Teleconsultation group having completed both baseline and final assessments, n = 88)",
    x     = "",
    y     = "Percentage of respondents",
    fill  = "Legend"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


### Q13
item_data <- recode_sopeq_item(
  data        = tele_df,
  bl_var      = "sopeq_b_13",
  f_var       = "sopeq_f_13",
  keep_levels = 1:4
)

# recode to ordered labels for tables/plots
item_data <- item_data %>%
  mutate(
    Q13_BL = factor(sopeq_b_13,
                   levels  = c(1, 2, 3, 4),
                   labels  = c("Right amount",
                               "Too much",
                               "Not enough", "I was not given any information about my treatment or condition"),
                   ordered = TRUE),
    Q13_F  = factor(sopeq_f_13,
                   levels  = c(1, 2, 3, 4),
                   labels  = c("Right amount",
                               "Too much",
                               "Not enough", "I was not given any information about my treatment or condition"),
                   ordered = TRUE),
  )

plot_q13 <- item_data %>%
  select(Q13_BL, Q13_F) %>%
  pivot_longer(cols = c(Q13_BL, Q13_F),
               names_to  = "Time",
               values_to = "Response") %>%
  mutate(
    Time = recode(Time,
                  Q13_BL = "Baseline",
                  Q13_F  = "Final")
  )

ggplot(plot_q13, aes(x = Response)) +
  geom_bar(fill = "pink2", color = "black") +
  facet_wrap(~ Time, nrow = 1) +
  labs(
    title = "Information given about own condition or treatment \n 8 months pre-teleconsultation vs 8 months after (teleconsultation group)",
    x     = "Information given",
    y     = "Number of respondents"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold")
  )


# stacked presentation
plot_q13_prop <- plot_q13 %>%
  group_by(Time, Response) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Time) %>%
  mutate(prop = n / sum(n))

ggplot(plot_q13_prop, aes(x = Time, y = prop, fill = Response)) +
  geom_col(color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Information given about own condition or treatment \n 8 months pre-teleconsultation vs 8 months after incorporation of teleconsultation \n (Teleconsultation group having completed both baseline and final assessments, n = 80)",
    x     = "Information given",
    y     = "Number of respondents",
    fill  = "Legend"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )



### Q13
item_data <- recode_sopeq_item(
  data        = tele_df,
  bl_var      = "sopeq_b_13",
  f_var       = "sopeq_f_13",
  keep_levels = 1:4
)

# recode to ordered labels for tables/plots
item_data <- item_data %>%
  mutate(
    Q13_BL = factor(sopeq_b_13,
                    levels  = c(1, 2, 3, 4),
                    labels  = c("Right amount",
                                "Too much",
                                "Not enough", "I was not given any information about my treatment or condition"),
                    ordered = TRUE),
    Q13_F  = factor(sopeq_f_13,
                    levels  = c(1, 2, 3, 4),
                    labels  = c("Right amount",
                                "Too much",
                                "Not enough", "I was not given any information about my treatment or condition"),
                    ordered = TRUE),
  )

plot_q13 <- item_data %>%
  select(Q13_BL, Q13_F) %>%
  pivot_longer(cols = c(Q13_BL, Q13_F),
               names_to  = "Time",
               values_to = "Response") %>%
  mutate(
    Time = recode(Time,
                  Q13_BL = "Baseline",
                  Q13_F  = "Final")
  )

ggplot(plot_q13, aes(x = Response)) +
  geom_bar(fill = "pink2", color = "black") +
  facet_wrap(~ Time, nrow = 1) +
  labs(
    title = "Information given about own condition or treatment \n 8 months pre-teleconsultation vs 8 months after incorporation of teleconsultation \n (Teleconsultation group having completed both baseline and final assessments, n = 80)",
    x     = "Information given",
    y     = "Number of respondents"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold")
  )


# stacked presentation
plot_q13_prop <- plot_q13 %>%
  group_by(Time, Response) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Time) %>%
  mutate(prop = n / sum(n))

ggplot(plot_q13_prop, aes(x = Time, y = prop, fill = Response)) +
  geom_col(color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Information given about own condition or treatment \n 8 months pre-teleconsultation vs 8 months after incorporation of teleconsultation \n (Teleconsultation group having completed both baseline and final assessments, n = 80)",
    x     = "Information given",
    y     = "Number of respondents",
    fill  = "Legend"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )



### Q17/38
item_data <- recode_sopeq_item(
  data        = tele_df,
  bl_var      = "sopeq_b_17",
  f_var       = "sopeq_f_17",
  keep_levels = 1:4
)

# recode to ordered labels for tables/plots
item_data <- item_data %>%
  mutate(
    Q17_BL = factor(sopeq_b_17,
                    levels  = c(1, 2, 3, 4),
                    labels  = c("Yes, completely",
                                "Yes, to some extent",
                                "No", "I did not need an explanation"),
                    ordered = TRUE),
    Q17_F  = factor(sopeq_f_17,
                    levels  = c(1, 2, 3, 4),
                    labels  = c("Yes, completely",
                                "Yes, to some extent",
                                "No", "I did not need an explanation"),
                    ordered = TRUE),
  )

plot_q17 <- item_data %>%
  select(Q17_BL, Q17_F) %>%
  pivot_longer(cols = c(Q17_BL, Q17_F),
               names_to  = "Time",
               values_to = "Response") %>%
  mutate(
    Time = recode(Time,
                  Q17_BL = "Baseline",
                  Q17_F  = "Final")
  )

ggplot(plot_q17, aes(x = Response)) +
  geom_bar(fill = "pink2", color = "black") +
  facet_wrap(~ Time, nrow = 1) +
  labs(
    title = "Explanation given on how to take medications \n 8 months pre-teleconsultation vs 8 months after incorporation of teleconsultation \n (Teleconsultation group having completed both baseline and final assessments, n = 80)",
    x     = "Explanation given",
    y     = "Number of respondents"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold")
  )



# stacked presentation
plot_q17_prop <- plot_q17 %>%
  group_by(Time, Response) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Time) %>%
  mutate(prop = n / sum(n))

ggplot(plot_q17_prop, aes(x = Time, y = prop, fill = Response)) +
  geom_col(color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Explanation given on how to take medications \n 8 months pre-teleconsultation vs 8 months after incorporation of teleconsultation \n (Teleconsultation group having completed both baseline and final assessments, n = 80)",
    x     = "Explanation given",
    y     = "Number of respondents",
    fill  = "Legend"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


### Q18/39
item_data <- recode_sopeq_item(
  data        = tele_df,
  bl_var      = "sopeq_b_18",
  f_var       = "sopeq_f_18",
  keep_levels = 1:4
)

# recode to ordered labels for tables/plots
item_data <- item_data %>%
  mutate(
    Q18_BL = factor(sopeq_b_18,
                    levels  = c(1, 2, 3, 4),
                    labels  = c("Yes, completely",
                                "Yes, to some extent",
                                "No", "I did not need an explanation"),
                    ordered = TRUE),
    Q18_F  = factor(sopeq_f_18,
                    levels  = c(1, 2, 3, 4),
                    labels  = c("Yes, completely",
                                "Yes, to some extent",
                                "No", "I did not need an explanation"),
                    ordered = TRUE),
  )

plot_q18 <- item_data %>%
  select(Q18_BL, Q18_F) %>%
  pivot_longer(cols = c(Q18_BL, Q18_F),
               names_to  = "Time",
               values_to = "Response") %>%
  mutate(
    Time = recode(Time,
                  Q18_BL = "Baseline",
                  Q18_F  = "Final")
  )

ggplot(plot_q18, aes(x = Response)) +
  geom_bar(fill = "pink2", color = "black") +
  facet_wrap(~ Time, nrow = 1) +
  labs(
    title = "Explanation given on the purpose of medications \n 8 months pre-teleconsultation vs 8 months after incorporation of teleconsultation \n (Teleconsultation group having completed both baseline and final assessments, n = 80)",
    x     = "Explanation given",
    y     = "Number of respondents"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold")
  )


# stacked presentation
plot_q18_prop <- plot_q18 %>%
  group_by(Time, Response) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Time) %>%
  mutate(prop = n / sum(n))

ggplot(plot_q18_prop, aes(x = Time, y = prop, fill = Response)) +
  geom_col(color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Explanation given on the purpose of medications \n 8 months pre-teleconsultation vs 8 months after incorporation of teleconsultation \n (Teleconsultation group having completed both baseline and final assessments, n = 78)",
    x     = "Explanation given",
    y     = "Number of respondents",
    fill  = "Legend"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

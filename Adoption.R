library(dplyr)
library(ggplot2)
library(readr)

# Load cleaned data
df <- readRDS("0701_clean.rds")

# Check hostel and tele_group_factor
tab_hostel_tele <- table(df$hostel, df$tele_group_factor, useNA = "ifany")
tab_hostel_tele

# Summarise counts
tele_hostel_counts <- df %>%
  group_by(hostel, tele_group_factor) %>%
  summarise(n = n(), .groups = "drop")

# Grouped bar chart
ggplot(tele_hostel_counts,
       aes(x = hostel, y = n, fill = tele_group_factor)) +
  geom_col(position = "dodge") +
  labs(
    title = "Tele vs non-tele participants by hostel",
    x = "Hostel",
    y = "Number of participants",
    fill = "Tele group"
  ) +
  theme_minimal()

# Check expected counts
chisq.test(tab_hostel_tele)$expected

# If all expected counts >= 5: chi-square test
chisq_res <- chisq.test(tab_hostel_tele)
chisq_res

# If some expected counts < 5: Fisher's exact test
fisher_res <- fisher.test(tab_hostel_tele)
fisher_res
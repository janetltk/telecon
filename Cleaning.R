library(dplyr)
library(readxl)
library(janitor)
library(readxl)

df <- read_excel("Data Collection Form 0623.xlsx")
View(df)

# (A) Data Cleaning
#clean names
df <- df %>% clean_names()

# How many rows do we have?
nrow(df)

# Look at the last 6 rows for a few key columns
tail(df, 6)

#delete empty rows
df <- df %>%
  filter(!is.na(participant_code) & participant_code != "")

# Quick overview of types for the first part of the data
str(df[, 1:80])

# Convert core baseline variables to numeric
df <- df %>%
  mutate(
    age        = as.numeric(age),
    fu_mhs     = as.numeric(fu_mhs),
    fu_tmmhc   = as.numeric(fu_tmmhc),
    enter_lsch = as.numeric(enter_lsch),
    tele_num   = as.numeric(tele_num),  # count of teleconsultations
    year_fu_mhs = as.numeric(2026 - fu_mhs),
    year_fu_tmmhc = as.numeric(2026 - fu_tmmhc),
    year_enter_lsch = as.numeric(2026 - enter_lsch)
  )

# Convert baseline and final clinical scales to numeric (if any came in as character)
df <- df %>%
  mutate(
    bl_sat      = as.numeric(bl_sat),
    bl_swemwbs  = as.numeric(bl_swemwbs),
    bl_cgi      = as.numeric(bl_cgi),
    bl_honos    = as.numeric(bl_honos),
    f_sat       = as.numeric(f_sat),
    f_swemwbs   = as.numeric(f_swemwbs),
    f_cgi       = as.numeric(f_cgi),
    f_honos     = as.numeric(f_honos)
  )

# Convert categorical variables to factors
df <- df %>%
  mutate(
    gender          = factor(gender),
    x1st_dx         = factor(x1st_dx),
    x1st_dx_group   = factor(x1st_dx_group),
    x2nd_dx         = factor(x2nd_dx),
    x2nd_dx_group   = factor(x2nd_dx_group),
    edu             = factor(edu),
    hostel          = factor(hostel),
    escort          = factor(escort),
    tele_after      = factor(tele_after),
    no_tele_reason  = factor(no_tele_reason)
  )

# Convert date variables
df <- df %>%
  mutate(
    bl_ax_date   = as.Date(bl_ax_date),
    bl_start_date = as.Date(bl_start_date),
    f_end_date   = as.Date(f_end_date),
    f_ax_date    = as.Date(f_ax_date),
  )

# Check that the key variables now look right
str(df[, c("age", "gender", "x1st_dx_group", "x2nd_dx_group", "edu",
           "year_fu_mhs", "year_fu_tmmhc", "year_enter_lsch",
           "tele_num", "tele_after")])
table(df$x1st_dx_group, useNA = "ifany")

# (B) Create tele vs non-tele groups
# Create binary tele group (0 vs ≥1)
df <- df %>%
  mutate(
    tele_group = ifelse(tele_num >= 1, 1, 0),
    tele_group_factor = factor(
      tele_group,
      levels = c(0, 1),
      labels = c("No_tele", "At_least_1_tele")
    )
  )

# Check the distribution of tele groups
table(df$tele_group_factor, useNA = "ifany")

# Create sensitivity group
df <- df %>%
mutate(
tele_after_chr = trimws(as.character(tele_after)),
tele_after_is_yes = tele_after_chr == "Yes",
tele_after_is_yes = ifelse(is.na(tele_after_is_yes), FALSE, tele_after_is_yes),
pt_wish_chr = trimws(as.character(pt_wish)),
pt_wish_is_yes = pt_wish_chr == "Yes",
pt_wish_is_yes = ifelse(is.na(pt_wish_is_yes), FALSE, pt_wish_is_yes),

tele_sens = case_when(
tele_num >= 1 | tele_after_is_yes | pt_wish_is_yes ~ 1,
tele_num == 0 & !tele_after_is_yes & !pt_wish_is_yes ~ 0,
TRUE                              ~ NA_real_
),
tele_sens_factor = factor(
tele_sens,
levels = c(0, 1),
labels = c("No_tele_sensitivity", "Tele_sensitivity")
)
)

table(df$tele_after_is_yes)
table(df$pt_wish_is_yes)
table(df$tele_sens_factor)

# Save the cleaned data as an RDS (R internal format)
saveRDS(df, file = "0623_clean.rds")
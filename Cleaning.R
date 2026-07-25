library(dplyr)
library(readxl)
library(janitor)

df <- read_excel("Data Collection Form 0719.xlsx")
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
str(df[, 81:174])

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
    bl_swemwbs  = as.numeric(bl_swemwbs),
    bl_honos    = as.numeric(bl_honos),
    f_swemwbs   = as.numeric(f_swemwbs),
    f_honos     = as.numeric(f_honos),
    bl_cgi      = as.numeric(bl_cgi),
    f_cgi       = as.numeric(f_cgi)
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
    no_tele_reason  = factor(no_tele_reason),
  )

# Convert date variables
df <- df %>%
  mutate(
    bl_ax_date   = as.Date(bl_ax_date),
    bl_start_date = as.Date(bl_start_date),
    f_end_date   = as.Date(f_end_date),
    f_ax_date    = as.Date(f_ax_date)
  )

# Convert ordinal variables
df <- df %>%
  mutate(
    bl_sat = factor(as.integer(bl_sat), levels = c(1:10), ordered = TRUE),
    f_sat = factor(as.integer(f_sat), levels = c(1:10), ordered = TRUE),
    bl_cgi = factor(bl_cgi, levels = c(1:7), ordered = TRUE),
    f_cgi = factor(f_cgi, levels = c(1:7), ordered = TRUE),
    sopeq_b_2 = factor(sopeq_b_2, levels = c(1:4), ordered = TRUE),
    sopeq_f_2 = factor(sopeq_f_2, levels = c(1:4), ordered = TRUE),
    sopeq_b_3 = factor(sopeq_b_3, levels = c(1:3), ordered = TRUE),
    sopeq_f_3 = factor(sopeq_f_3, levels = c(1:3), ordered = TRUE),
    sopeq_b_4 = factor(sopeq_b_4, levels = c(1:4), ordered = TRUE),
    sopeq_f_4 = factor(sopeq_f_4, levels = c(1:4), ordered = TRUE),
    sopeq_b_5 = ifelse(sopeq_b_5 == "5", NA, sopeq_b_5),
    sopeq_b_5 = factor(sopeq_b_5, levels = c(1:4), ordered = TRUE),
    sopeq_f_5 = ifelse(sopeq_f_5 == "5", NA, sopeq_f_5),
    sopeq_f_5 = factor(sopeq_f_5, levels = c(1:4), ordered = TRUE),  
    sopeq_b_6 = ifelse(sopeq_b_6 == "4", NA, sopeq_b_6),
    sopeq_b_6 = factor(sopeq_b_6, levels = c(1:3), ordered = TRUE),  
    sopeq_f_6 = ifelse(sopeq_f_6 == "4", NA, sopeq_f_6),
    sopeq_f_6 = factor(sopeq_f_6, levels = c(1:3), ordered = TRUE),  
    sopeq_b_7 = factor(sopeq_b_7, levels = c(1:3), ordered = TRUE),
    sopeq_f_7 = factor(sopeq_f_7, levels = c(1:3), ordered = TRUE),    
    sopeq_b_8 = ifelse(sopeq_b_8 == "5", NA, sopeq_b_8),
    sopeq_b_8 = factor(sopeq_b_8, levels = c(1:4), ordered = TRUE),  
    sopeq_f_8 = ifelse(sopeq_f_8 == "5", NA, sopeq_f_8),
    sopeq_f_8 = factor(sopeq_f_8, levels = c(1:4), ordered = TRUE),  
    sopeq_b_9 = factor(sopeq_b_9, levels = c(1:3), ordered = TRUE),
    sopeq_f_9 = factor(sopeq_f_9, levels = c(1:3), ordered = TRUE),    
    sopeq_b_10 = ifelse(sopeq_b_10 %in% c("5", "6"), NA, sopeq_b_10),
    sopeq_b_10 = factor(sopeq_b_10, levels = c(1:4), ordered = TRUE),
    sopeq_f_10 = ifelse(sopeq_f_10 %in% c("5", "6"), NA, sopeq_f_10),
    sopeq_f_10 = factor(sopeq_f_10, levels = c(1:4), ordered = TRUE),
    sopeq_b_11 = factor(sopeq_b_11, levels = c(1:3), ordered = TRUE),
    sopeq_f_11 = factor(sopeq_f_11, levels = c(1:3), ordered = TRUE),     
    sopeq_b_12 = factor(sopeq_b_12, levels = c(1:3), ordered = TRUE),
    sopeq_f_12 = factor(sopeq_f_12, levels = c(1:3), ordered = TRUE),     
    sopeq_b_13 = factor(sopeq_b_13, levels = c(1:4), ordered = TRUE),
    sopeq_f_13 = factor(sopeq_f_13, levels = c(1:4), ordered = TRUE),     
    sopeq_b_14 = ifelse(sopeq_b_14 == "4", NA, sopeq_b_14),
    sopeq_b_14 = factor(sopeq_b_14, levels = c(1:3), ordered = TRUE),  
    sopeq_f_14 = ifelse(sopeq_f_14 == "4", NA, sopeq_f_14),
    sopeq_f_14 = factor(sopeq_f_14, levels = c(1:3), ordered = TRUE),   
    sopeq_b_15 = ifelse(sopeq_b_15 %in% c("4", "5", "6"), NA, sopeq_b_15),
    sopeq_b_15 = factor(sopeq_b_15, levels = c(1:4), ordered = TRUE),
    sopeq_f_15 = ifelse(sopeq_f_15 %in% c("4", "5", "6"), NA, sopeq_f_15),
    sopeq_f_15 = factor(sopeq_f_15, levels = c(1:4), ordered = TRUE),
    sopeq_b_16 = ifelse(sopeq_b_16 %in% c("4", "5", "6"), NA, sopeq_b_16),
    sopeq_b_16 = factor(sopeq_b_16, levels = c(1:3), ordered = TRUE),
    sopeq_f_16 = ifelse(sopeq_f_16 %in% c("4", "5", "6"), NA, sopeq_f_16),
    sopeq_f_16 = factor(sopeq_f_16, levels = c(1:3), ordered = TRUE),
    sopeq_b_17 = ifelse(sopeq_b_17 == "4", NA, sopeq_b_17),
    sopeq_b_17 = factor(sopeq_b_17, levels = c(1:3), ordered = TRUE),
    sopeq_f_17 = ifelse(sopeq_f_17 == "4", NA, sopeq_f_17),
    sopeq_f_17 = factor(sopeq_f_17, levels = c(1:3), ordered = TRUE),
    sopeq_b_18 = ifelse(sopeq_b_18 == "4", NA, sopeq_b_18),
    sopeq_b_18 = factor(sopeq_b_18, levels = c(1:3), ordered = TRUE),
    sopeq_f_18 = ifelse(sopeq_f_18 == "4", NA, sopeq_f_18),
    sopeq_f_18 = factor(sopeq_f_18, levels = c(1:3), ordered = TRUE),    
    sopeq_b_19 = ifelse(sopeq_b_19 == "4", NA, sopeq_b_19),
    sopeq_b_19 = factor(sopeq_b_19, levels = c(1:3), ordered = TRUE),
    sopeq_f_19 = ifelse(sopeq_f_19 == "4", NA, sopeq_f_19),
    sopeq_f_19 = factor(sopeq_f_19, levels = c(1:3), ordered = TRUE),    
    sopeq_b_20 = factor(sopeq_b_20, levels = c(1:3), ordered = TRUE),
    sopeq_f_20 = factor(sopeq_f_20, levels = c(1:3), ordered = TRUE),    
    sopeq_b_21 = ifelse(sopeq_b_21 == "4", NA, sopeq_b_21),
    sopeq_b_21 = factor(sopeq_b_21, levels = c(1:3), ordered = TRUE),
    sopeq_f_21 = ifelse(sopeq_f_21 == "4", NA, sopeq_f_21),
    sopeq_f_21 = factor(sopeq_f_21, levels = c(1:3), ordered = TRUE),  
    sopeq_b_22 = factor(sopeq_b_22, levels = c(1:3), ordered = TRUE),
    sopeq_f_22 = factor(sopeq_f_22, levels = c(1:3), ordered = TRUE),    
    sopeq_b_23 = factor(sopeq_b_23, levels = c(1:3), ordered = TRUE),
    sopeq_f_23 = factor(sopeq_f_23, levels = c(1:3), ordered = TRUE),      
    sopeq_b_24 = factor(sopeq_b_24, levels = c(1:3), ordered = TRUE),
    sopeq_f_24 = factor(sopeq_f_24, levels = c(1:3), ordered = TRUE),    
    sopeq_b_25 = factor(sopeq_b_25, levels = c(1:3), ordered = TRUE),
    sopeq_f_25 = factor(sopeq_f_25, levels = c(1:3), ordered = TRUE),  
    sopeq_b_26 = factor(sopeq_b_26, levels = c(1:3), ordered = TRUE),
    sopeq_f_26 = factor(sopeq_f_26, levels = c(1:3), ordered = TRUE),  
    sopeq_b_27 = factor(sopeq_b_27, levels = c(1:3), ordered = TRUE),
    sopeq_f_27 = factor(sopeq_f_27, levels = c(1:3), ordered = TRUE),  
    sopeq_b_28 = ifelse(sopeq_b_28 == "4", NA, sopeq_b_28),
    sopeq_b_28 = factor(sopeq_b_28, levels = c(1:3), ordered = TRUE),
    sopeq_f_28 = ifelse(sopeq_f_28 == "4", NA, sopeq_f_28),
    sopeq_f_28 = factor(sopeq_f_28, levels = c(1:3), ordered = TRUE),  
    teleq_1 = factor(teleq_1,
                     levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), ordered = TRUE),
    teleq_2 = factor(teleq_2,
                     levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), ordered = TRUE),
    teleq_3 = factor(teleq_3,
                     levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), ordered = TRUE),
    teleq_4 = factor(teleq_4,
                     levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), ordered = TRUE),
    teleq_5 = factor(teleq_5,
                     levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), ordered = TRUE),
    teleq_6 = factor(teleq_6,
                     levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), ordered = TRUE),
    teleq_7 = factor(teleq_7,
                     levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), ordered = TRUE)
      )

# Check that the key variables now look right
str(df[, 1:80])
str(df[, 81:174])
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
TRUE ~ NA_real_),
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
saveRDS(df, file = "0719_clean.rds")

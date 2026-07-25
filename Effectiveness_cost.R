# Set your path to the Excel file
df <- readRDS("~/Telemed/0719_clean.rds")

tele_df_tm_escort <- df %>%
  filter(tele_num >= 1 & hostel == "TM" & escort == "Escort")
sum(tele_df_tm_escort$tele_num)

tele_df_sl <- df %>%
  filter(tele_num >= 1 & hostel == "SL")
summary(tele_df_sl)

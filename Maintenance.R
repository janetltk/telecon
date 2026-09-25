library(dplyr)
library(readxl)
library(ggplot2)
library(janitor)
library(tidyr)
library(forcats)
library(scales)

# Load cleaned data
df <- read_excel("Data Collection Form 0725.xlsx")
df <- df %>% clean_names()
nrow(df)

# Keep only participants who actually had at least 1 teleconsultation
tele_df <- df |>
  filter(tele_num >= 1) |>
  select(teleq_7, teleq_6, teleq_5)

tele_long_nomiss <- df |>
  filter(tele_num >= 1) |>
  select(teleq_7, teleq_6, teleq_5) |>
  pivot_longer(
    cols = everything(),
    names_to = "item",
    values_to = "response"
  ) |>
  filter(!is.na(response)) |>
  mutate(
    response = factor(
      as.character(response),
      levels = c("1", "2", "3", "4", "5"),
      labels = c(
        "1 Totally disagree",
        "2",
        "3",
        "4",
        "5 Totally agree"
      )
    ),
    item = recode(
      item,
      teleq_7 = "Recommend telepsychiatry to others",
      teleq_6 = "Want telepsychiatry next follow-up",
      teleq_5 = "Telepsychiatry met my medical care needs"
    )
  ) |>
  mutate(
    item = factor(
      item,
      levels = c(
        "Recommend telepsychiatry to others",
        "Want telepsychiatry next follow-up",
        "Telepsychiatry met my medical care needs"
      )
    )
  )

tele_props_nomiss <- tele_long_nomiss |>
  count(item, response, name = "n") |>
  group_by(item) |>
  mutate(
    prop  = n / sum(n),
    label = percent(prop, accuracy = 1)
  ) |>
  ungroup()

likert_palette_nomiss <- c(
  "1 Totally disagree" = "#c9706a",
  "2"                  = "#d9b39c",
  "3"                  = "#cfd8dc",
  "4"                  = "#90a4ae",
  "5 Totally agree"    = "#607d8b"
)

ggplot(tele_long_nomiss, aes(x = item, fill = response)) +
  geom_bar(position = position_fill(reverse = TRUE), colour = "black", width = 0.6) +
  geom_text(
    data = tele_props_nomiss,
    aes(x = item, y = prop, label = label),
    position = position_fill(vjust = 0.5, reverse = TRUE),
    colour = "black",
    size = 3
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = likert_palette_nomiss, name = NULL) +
  labs(
    x = NULL,
    y = "Percentage of respondents",
    title = "Patients' opinions on maintaining teleconsultation-incorporated outpatient care"
  ) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    axis.text.y = element_text(hjust = 1)
  )

levels(tele_long_nomiss$item)

##########################
#Tele after study ends======

df <- read_excel("Data Collection Form 0725.xlsx")
df <- df %>% clean_names()
nrow(df)
df$tele_after_study_completion <- as.numeric(df$tele_after_study_completion)

df <- df |>
  filter(
    tele_after_study_completion >= 1,
    !is.na(tele_after_study_completion)
  )

###
summary(df$age)
df$edu <- as.factor(df$edu)
summary(df$edu)
summary(df$fu_mhs)
sd(df$fu_mhs)
summary(df$fu_tmmhc)
sd(df$fu_tmmhc)
summary(df$enter_lsch)

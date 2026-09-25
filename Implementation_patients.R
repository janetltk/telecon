library(dplyr)
library(readxl)
library(ggplot2)
library(janitor)
library(tidyr)
library(forcats)
library(scales)

# Load data
df <- read_excel("~/Telemed/Data Collection Form 0725.xlsx")
df <- df %>% clean_names()
nrow(df)

# Keep only participants who actually had at least 1 teleconsultation
tele_df <- df |>
  filter(tele_num >= 1) |>
  select(teleq_1, teleq_2, teleq_3, teleq_4)

tele_long <- df |>
  filter(tele_num >= 1) |>
  select(teleq_1, teleq_2, teleq_3, teleq_4) |>
  pivot_longer(
    cols = everything(),
    names_to = "item",
    values_to = "response"
  ) |>
  mutate(
    response = as.character(response),
    response = replace_na(response, "Missing"),
    response = factor(
      response,
      levels = c("Missing", "1", "2", "3", "4", "5"),
      labels = c(
        "Missing / No response",
        "1 Totally disagree",
        "2",
        "3",
        "4",
        "5 Totally agree"
      )
    ),
    item = recode(
      item,
      teleq_1 = "Staff explained teleconsultation clearly",
      teleq_2 = "Staff respected me & privacy",
      teleq_3 = "Satisfied with video quality",
      teleq_4 = "Satisfied with audio quality"
    )
  )

levels(tele_long$response)
table(tele_long$response, useNA = "always")

# Summarise counts and proportions by item and response
tele_props <- tele_long |>
  count(item, response, name = "n") |>
  group_by(item) |>
  mutate(
    prop = n / sum(n),
    label = percent(prop, accuracy = 1),
    label = if_else(prop >= 0.05, label, "")
  ) |>
  ungroup()

# Plot with NAs as a separate segment
likert_palette <- c(
  "Missing / No response" = "grey70",
  "1 Totally disagree"    = "#c9706a",
  "2"                     = "#d9b39c",
  "3"                     = "#cfd8dc",
  "4"                     = "#90a4ae",
  "5 Totally agree"       = "#607d8b"
)

ggplot(tele_long, aes(x = item, fill = response)) +
  geom_bar(position = position_fill(reverse = TRUE), colour = "black") +
  geom_text(
    data = tele_props,
    aes(x = item, y = prop, label = label),
    position = position_fill(vjust = 0.5, reverse = TRUE),
    colour = "black",
    size = 3
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = likert_palette, name = NULL) +
  labs(
    x = NULL,
    y = "Percentage of respondents",
    title = "Patient-reported quality of teleconsultation"
  ) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    axis.text.y = element_text(hjust = 1)
  )

# version ignoring the NA=====================
tele_long_nomiss <- df |>
  filter(tele_num >= 1) |>
  select(teleq_1, teleq_2, teleq_3, teleq_4) |>
  pivot_longer(
    cols = everything(),
    names_to = "item",
    values_to = "response"
  ) |>
  filter(!is.na(response)) |>                       # drop NA rows
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
      teleq_1 = "Staff explained teleconsultation clearly",
      teleq_2 = "Staff respected me & privacy",
      teleq_3 = "Satisfied with video quality",
      teleq_4 = "Satisfied with audio quality"
    )
  )

tele_props_nomiss <- tele_long_nomiss |>
  count(item, response, name = "n") |>
  group_by(item) |>
  mutate(
    prop  = n / sum(n),
    label = percent(prop, accuracy = 1),
    label = if_else(prop >= 0.05, label, "")
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
  geom_bar(position = position_fill(reverse = TRUE), colour = "black") +
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
    title = "Patient-reported quality of teleconsultation"
  ) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    axis.text.y = element_text(hjust = 1)
  )

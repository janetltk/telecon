library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load Excel-----
data <- read_excel("Clinician Response 0725.xlsx")
str(data)  # quick check of column names / types
data <- data %>% clean_names()

# Select Q9a–Q9d
q9 <- data %>%
  select(x9a, x9b, x9c, x9d)

# Add respondent ID and pivot longer
q9_long <- q9 %>%
  mutate(Respondent = row_number()) %>%
  pivot_longer(
    cols = c(x9a, x9b, x9c, x9d),
    names_to = "Question",
    values_to = "Score"
  ) %>%
  filter(!is.na(Score))

# Question text for axis labels
q9_labels <- c(
  "9a" = "9a Clinician-side technical problems",
  "9b" = "9b Patient-side technical problems",
  "9c" = "9c Problems not resolvable by clinician",
  "9d" = "9d Nobody available to help resolve problems"
)

# Recode numeric scores to ordered factor with full legend labels
q9_long <- q9_long %>%
  mutate(
    Question = factor(Question,
                      levels = c("x9a", "x9b", "x9c", "x9d"),
                      labels = q9_labels[c("9a", "9b", "9c", "9d")]),
    Score = factor(
      Score,
      levels = 1:5,
      labels = c(
        "1 = Never",
        "2 = Rarely",
        "3 = Sometimes",
        "4 = Often",
        "5 = Always"
      ),
      ordered = TRUE
    )
  )

q9_dist <- q9_long %>%
  group_by(Question, Score) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Question) %>%
  mutate(
    Percent = 100 * n / sum(n),
    Label = ifelse(Percent >= 5, paste0(round(Percent), "%"), "")  # label only >=5%
  ) %>%
  ungroup()

# Colour palette (from low to high frequency)
likert_cols <- c(
  "1 = Never"    = "#607d8b",
  "2 = Rarely"   = "#90a4ae",
  "3 = Sometimes"= "#cfd8dc",
  "4 = Often"    = "#d9b39c",
  "5 = Always"   = "#c9706a"
)

ggplot(q9_dist, aes(x = Question, y = Percent, fill = Score)) +
  geom_col(
    width = 0.6,
    position = position_stack(reverse = TRUE),
    show.legend = TRUE
  ) +
  geom_text(
    aes(
      label = ifelse(Percent < 5, "", paste0(round(Percent), "%")),
      group = Score
    ),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    size = 3,
    colour = "black"
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = likert_cols,
    limits = c(
      "1 = Never",
      "2 = Rarely",
      "3 = Sometimes",
      "4 = Often",
      "5 = Always"
    ),
    drop = FALSE,
    name = "Response"
  ) +
  labs(
    title = "Clinicians-reported technical issues during consultation",
    x = NULL,
    y = "Percentage of responses"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

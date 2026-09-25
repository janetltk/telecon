library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(janitor)
library(stringr)

# Load Excel-----
data <- read_excel("~/Telemed/Clinician Response 0922.xlsx")
str(data)  # quick check of column names / types
data <- data %>% clean_names()
data <- data[1:11, ]

# (1) Technical issues ==================

# Select Q9a–Q9d
q9 <- data %>%
  select(x9a, x9b, x9c, x9d, x11c)

# Add respondent ID and pivot longer
q9_long <- q9 %>%
  mutate(Respondent = row_number()) %>%
  pivot_longer(
    cols = c(x9a, x9b, x9c, x9d, x11c),
    names_to = "Question",
    values_to = "Score"
  ) %>%
  filter(!is.na(Score))

# Question text for axis labels
q9_labels <- c(
  "9a" = "Clinician-side technical problems",
  "9b" = "Patient-side technical problems",
  "9c" = "Problems not resolvable by clinician",
  "9d" = "Nobody available to help resolve problems",
  "11c" = "More time used due to technical problems"
)

# Recode numeric scores to ordered factor with full legend labels
q9_long <- q9_long %>%
  mutate(
    Question = factor(Question,
                      levels = c("x11c", "x9d", "x9c", "x9b", "x9a"),
                      labels = q9_labels[c("11c", "9d", "9c", "9b", "9a")]),
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
    show.legend = TRUE,
    colour = "black",   # add this
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
    title = "Clinicians-reported occurrence of technical issues during teleconsultation",
    x = NULL,
    y = "Percentage of responses"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# (2) Clinical management issues ==============

# Select Q10a–Q10e
q10 <- data %>%
  select(x10a, x10b, x10c, x10d, x10e, x10f)

# Add respondent ID and pivot longer
q10_long <- q10 %>%
  mutate(Respondent = row_number()) %>%
  pivot_longer(
    cols = c(x10a, x10b, x10c, x10d, x10e, x10f),
    names_to = "Question",
    values_to = "Score"
  ) %>%
  filter(!is.na(Score))

# Question text for axis labels
q10_labels <- c(
  "10a" = "Difficulty accessing information",
  "10b" = "Difficulty understanding patients",
  "10c" = "Difficulty performing mental state examination",
  "10d" = "Difficulty observing patient's physical condition",
  "10e" = "Difficulty making diagnosis or treatment plan",
  "10f" = "Difficulty explaining to patients"
)

# Recode numeric scores to ordered factor with full legend labels
q10_long <- q10_long %>%
  mutate(
    Question = factor(Question,
                      levels = c("x10f", "x10e", "x10d", "x10c", "x10b", "x10a"),
                      labels = q10_labels[c("10f", "10e", "10d", "10c", "10b", "10a")]),
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

q10_dist <- q10_long %>%
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

ggplot(q10_dist, aes(x = Question, y = Percent, fill = Score)) +
  geom_col(
    width = 0.6,
    position = position_stack(reverse = TRUE),
    show.legend = TRUE,
    colour = "black",   # add this
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
    title = "Clinicians-reported occurrence of clinical management issues during teleconsultation",
    x = NULL,
    y = "Percentage of responses"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# (3) Administrative issues ==================

# Select Q11a–Q11d
q11 <- data %>%
  select(x11a, x11c, x11d)

# Add respondent ID and pivot longer
q11_long <- q11 %>%
  mutate(Respondent = row_number()) %>%
  pivot_longer(
    cols = c(x11a, x11c, x11d),
    names_to = "Question",
    values_to = "Score"
  ) %>%
  filter(!is.na(Score))

# Question text for axis labels
q11_labels <- c(
  "11a" = "Increased workload for patient selection",
  "11c" = "Need for ad-hoc face-to-face consultation",
  "11d" = "Difficulty delivering hard copies"
)

# Recode numeric scores to ordered factor with full legend labels
q11_long <- q11_long %>%
  mutate(
    Question = factor(Question,
                      levels = c("x11d", "x11c", "x11a"),
                      labels = q11_labels[c("11d", "11c", "11a")]),
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

q11_dist <- q11_long %>%
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

ggplot(q11_dist, aes(x = Question, y = Percent, fill = Score)) +
  geom_col(
    width = 0.6,
    position = position_stack(reverse = TRUE),
    show.legend = TRUE,
    colour = "black",   # add this
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
    title = "Clinicians-reported occurrence of administrative issues during teleconsultation",
    x = NULL,
    y = "Percentage of responses"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

###########################
# (4) Combine all 3 graphs above to a single faceted graph

## ---------- Q9: Technical issues ----------
q9 <- data %>%
  select(x9a, x9b, x9c, x9d, x11c)

q9_long <- q9 %>%
  mutate(Respondent = row_number()) %>%
  pivot_longer(
    cols = c(x9a, x9b, x9c, x9d, x11c),
    names_to = "Item_raw",
    values_to = "Score"
  ) %>%
  filter(!is.na(Score)) %>%
  mutate(
    Domain = "Technical issues",
    Item_label = recode(Item_raw,
                        "x9a"  = "Clinician-side technical problems",
                        "x9b"  = "Patient-side technical problems",
                        "x9c"  = "Problems not resolvable by clinician",
                        "x9d"  = "Nobody available to help resolve problems",
                        "x11c" = "More time used due to technical problems"
    )
  )

## ---------- Q10: Clinical management issues ----------
q10 <- data %>%
  select(x10a, x10b, x10c, x10d, x10e, x10f)

q10_long <- q10 %>%
  mutate(Respondent = row_number()) %>%
  pivot_longer(
    cols = c(x10a, x10b, x10c, x10d, x10e, x10f),
    names_to = "Item_raw",
    values_to = "Score"
  ) %>%
  filter(!is.na(Score)) %>%
  mutate(
    Domain = "Clinical management issues",
    Item_label = recode(Item_raw,
                        "x10a" = "Difficulty accessing information",
                        "x10b" = "Difficulty understanding patients",
                        "x10c" = "Difficulty performing mental state examination",
                        "x10d" = "Difficulty observing patient's physical condition",
                        "x10e" = "Difficulty making diagnosis or treatment plan",
                        "x10f" = "Difficulty explaining to patients"
    )
  )

## ---------- Q11: Administrative issues ----------
q11 <- data %>%
  select(x11a, x11c, x11d)

q11_long <- q11 %>%
  mutate(Respondent = row_number()) %>%
  pivot_longer(
    cols = c(x11a, x11c, x11d),
    names_to = "Item_raw",
    values_to = "Score"
  ) %>%
  filter(!is.na(Score)) %>%
  mutate(
    Domain = "Administrative issues",
    Item_label = recode(Item_raw,
                        "x11a" = "Increased workload for patient selection",
                        "x11c" = "Need for ad-hoc face-to-face consultation",
                        "x11d" = "Difficulty delivering hard copies"
    )
  )

## ---------- Combine all domains ----------
issues_long <- bind_rows(q9_long, q10_long, q11_long) %>%
  mutate(
    Domain = factor(
      Domain,
      levels = c(
        "Technical issues",
        "Clinical management issues",
        "Administrative issues"
      )
    ),
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
  ) %>%
  filter(!is.na(Score), !is.na(Item_label))

# 2. Compute percentages
issues_dist <- issues_long %>%
  group_by(Domain, Item_label, Score) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Domain, Item_label) %>%
  mutate(
    Percent = 100 * n / sum(n),
    Label = ifelse(Percent >= 5, paste0(round(Percent), "%"), "")
  ) %>%
  ungroup()

# 3. Plot with facets and similar bar thickness
likert_cols <- c(
  "1 = Never"    = "#607d8b",
  "2 = Rarely"   = "#90a4ae",
  "3 = Sometimes"= "#cfd8dc",
  "4 = Often"    = "#d9b39c",
  "5 = Always"   = "#c9706a"
)

ggplot(issues_dist, aes(x = Item_label, y = Percent, fill = Score)) +
  geom_col(
    width = 0.5,
    position = position_stack(reverse = TRUE),
    show.legend = TRUE,
    colour = "black"
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
  facet_wrap(
    ~ Domain,
    ncol = 1,
    scales = "free_y",
    strip.position = "top",
    labeller = labeller(
      Domain = c(
        "Technical issues"            = "Technological issues",
        "Clinical management issues"  = "Clinical management difficulties",
        "Administrative issues"       = "Administrative troubles"
      )
    )
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = likert_cols,
    limits = names(likert_cols),
    drop = FALSE,
    name = "Response"
  ) +
  labs(
    title = "Clinicians-reported frequency of problems encountered during teleconsultation",
    x = NULL,
    y = "Percentage of responses"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey95", colour = NA)
  )

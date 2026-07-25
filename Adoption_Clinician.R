library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)

raw_data <- read_excel("Clinician Response 0725.xlsx") %>%
  clean_names()

aim_iam_fim <- raw_data %>%
  select(
    x4a_aim_1, x4b_aim_2, x4c_aim_3, x4d_aim_4,
    x5a_iam_1, x5b_iam_2, x5c_iam_3, x5d_iam_4,
    x6a_fim_1, x6b_fim_2, x6c_fim_3, x6d_fim_4
  )

likert_long <- aim_iam_fim %>%
  mutate(Respondent = row_number()) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Item_raw",
    values_to = "Score"
  ) %>%
  mutate(
    Measure = case_when(
      grepl("^x4", Item_raw) ~ "AIM",
      grepl("^x5", Item_raw) ~ "IAM",
      grepl("^x6", Item_raw) ~ "FIM",
      TRUE ~ NA_character_
    ),
    Item_num = sub(".*_(\\d+)$", "\\1", Item_raw),
    Item_label = case_when(
      Measure == "AIM" & Item_num == "1" ~ "Meets my approval",
      Measure == "AIM" & Item_num == "2" ~ "Is appealing to me",
      Measure == "AIM" & Item_num == "3" ~ "I like it",
      Measure == "AIM" & Item_num == "4" ~ "I welcome it",
      
      Measure == "IAM" & Item_num == "1" ~ "Seems fitting",
      Measure == "IAM" & Item_num == "2" ~ "Seems suitable",
      Measure == "IAM" & Item_num == "3" ~ "Seems applicable",
      Measure == "IAM" & Item_num == "4" ~ "Seems like a good match",
      
      Measure == "FIM" & Item_num == "1" ~ "Seems implementable",
      Measure == "FIM" & Item_num == "2" ~ "Seems possible",
      Measure == "FIM" & Item_num == "3" ~ "Seems doable",
      Measure == "FIM" & Item_num == "4" ~ "Seems easy to use",
      
      TRUE ~ NA_character_
    ),
    # reverse levels so that after coord_flip() they appear 1 -> 4 top to bottom
    Item_label = factor(
      Item_label,
      levels = c(
        "I welcome it",
        "I like it",
        "Is appealing to me",
        "Meets my approval",
        
        "Seems like a good match",
        "Seems applicable",
        "Seems suitable",
        "Seems fitting",
        
        "Seems easy to use",
        "Seems doable",
        "Seems possible",
        "Seems implementable"
      )
    ),
    Measure = factor(Measure, levels = c("AIM", "IAM", "FIM")),
    Score = as.integer(Score)
  ) %>%
  filter(!is.na(Measure), !is.na(Score), !is.na(Item_label))

dist_item <- likert_long %>%
  group_by(Measure, Item_label, Score) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Measure, Item_label) %>%
  mutate(Percent = 100 * n / sum(n)) %>%
  ungroup() %>%
  filter(!is.na(Percent), Percent >= 0, Percent <= 100)

dist_item$Score <- factor(
  dist_item$Score,
  levels = 1:5,
  labels = c(
    "1 = Strongly disagree",
    "2 = Disagree",
    "3 = Neither agree nor disagree",
    "4 = Agree",
    "5 = Strongly agree"
  ),
  ordered = TRUE
)

likert_cols <- c(
  "1 = Strongly disagree" = "#c9706a",
  "2 = Disagree" = "#d9b39c",
  "3 = Neither agree nor disagree" = "#cfd8dc",
  "4 = Agree" = "#90a4ae",
  "5 = Strongly agree" = "#607d8b"
)

ggplot(dist_item, aes(x = Item_label, y = Percent, fill = Score)) +
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
  facet_wrap(
    ~ Measure,
    ncol = 1,
    scales = "free_y",
    strip.position = "top",
    labeller = labeller(
      Measure = c(
        AIM = "Acceptability of Intervention Measure (AIM)",
        IAM = "Intervention Appropriateness Measure (IAM)",
        FIM = "Feasibility of Intervention Measure (FIM)"
      )
    )
  ) +
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
    title = "Clinician ratings for individual items under the  Acceptability, Appropriateness \n and Feasibility of Intervention Measures (n = 14)",
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


# Legal concerns =======================

data <- read_excel("Clinician Response 0725.xlsx") %>%
  clean_names()

# Select Q8
q8 <- data %>%
  select(x8a, x8b, x8c, x8d)

# Add respondent ID and pivot longer
q8_long <- q8 %>%
  mutate(Respondent = row_number()) %>%
  pivot_longer(
    cols = c(x8a, x8b, x8c, x8d),
    names_to = "Question",
    values_to = "Score"
  ) %>%
  filter(!is.na(Score))

# Question text for axis labels
q8_labels <- c(
  "8d" = "Legal issues",
  "8c" = "Patient confidentiality",
  "8b" = "Data security",
  "8a" = "Ethical issues"
)

# Recode numeric scores to ordered factor with full legend labels
q8_long <- q8_long %>%
  mutate(
    Question = factor(Question,
                      levels = c("x8a", "x8b", "x8c", "x8d"),
                      labels = q8_labels[c("8a", "8b", "8c", "8d")]),
    Score = factor(
      Score,
      levels = 1:5,
      labels = c(
        "1 = Not concerned at all",
        "2",
        "3",
        "4",
        "5 = Very concerned"
      ),
      ordered = TRUE
    )
  )

q8_dist <- q8_long %>%
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
  "1 = Not concerned at all"    = "#607d8b",
  "2"   = "#90a4ae",
  "3"= "#cfd8dc",
  "4"    = "#d9b39c",
  "5 = Very concerned"   = "#c9706a"
)

ggplot(q8_dist, aes(x = Question, y = Percent, fill = Score)) +
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
      "1 = Not concerned at all",
      "2",
      "3",
      "4",
      "5 = Very concerned"
    ),
    drop = FALSE,
    name = "Response"
  ) +
  labs(
    title = "Clinicians-reported legal and ethical concerns about teleconsultation",
    x = NULL,
    y = "Percentage of responses"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# technological concerns ==============

# Select Q8
q8 <- data %>%
  select(x8g, x8l)

# Add respondent ID and pivot longer
q8_long <- q8 %>%
  mutate(Respondent = row_number()) %>%
  pivot_longer(
    cols = c(x8g, x8l),
    names_to = "Question",
    values_to = "Score"
  ) %>%
  filter(!is.na(Score))

# Question text for axis labels
q8_labels <- c(
  "8g" = "Unfamiliarity with tech requirements",
  "8l" = "Tech issues interrupting patient care"
)

# Recode numeric scores to ordered factor with full legend labels
q8_long <- q8_long %>%
  mutate(
    Question = factor(Question,
                      levels = c("x8g", "x8l"),
                      labels = q8_labels[c("8g", "8l")]),
    Score = factor(
      Score,
      levels = 1:5,
      labels = c(
        "1 = Not concerned at all",
        "2",
        "3",
        "4",
        "5 = Very concerned"
      ),
      ordered = TRUE
    )
  )

q8_dist <- q8_long %>%
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
  "1 = Not concerned at all"    = "#607d8b",
  "2"   = "#90a4ae",
  "3"= "#cfd8dc",
  "4"    = "#d9b39c",
  "5 = Very concerned"   = "#c9706a"
)

ggplot(q8_dist, aes(x = Question, y = Percent, fill = Score)) +
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
      "1 = Not concerned at all",
      "2",
      "3",
      "4",
      "5 = Very concerned"
    ),
    drop = FALSE,
    name = "Response"
  ) +
  labs(
    title = "Clinicians-reported technological concerns about teleconsultation",
    x = NULL,
    y = "Percentage of responses"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# clinical management difficulties=============

# Select Q8
q8 <- data %>%
  select(x8e, x8f, x8h, x8j, x8k)

# Add respondent ID and pivot longer
q8_long <- q8 %>%
  mutate(Respondent = row_number()) %>%
  pivot_longer(
    cols = c(x8e, x8f, x8h, x8j, x8k),
    names_to = "Question",
    values_to = "Score"
  ) %>%
  filter(!is.na(Score))

# Question text for axis labels
q8_labels <- c(
  "8e" = "Lack professional guidance",
  "8f" = "Lack training",
  "8h" = "Need to adjust interview styles",
  "8j" = "Ability to diagnose or treat",
  "8k" = "Risk management concerns"
)

# Recode numeric scores to ordered factor with full legend labels
q8_long <- q8_long %>%
  mutate(
    Question = factor(Question,
                      levels = c("x8e", "x8f", "x8h", "x8j", "x8k"),
                      labels = q8_labels[c("8e", "8f", "8h", "8j", "8k")]),
    Score = factor(
      Score,
      levels = 1:5,
      labels = c(
        "1 = Not concerned at all",
        "2",
        "3",
        "4",
        "5 = Very concerned"
      ),
      ordered = TRUE
    )
  )

q8_dist <- q8_long %>%
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
  "1 = Not concerned at all"    = "#607d8b",
  "2"   = "#90a4ae",
  "3"= "#cfd8dc",
  "4"    = "#d9b39c",
  "5 = Very concerned"   = "#c9706a"
)

ggplot(q8_dist, aes(x = Question, y = Percent, fill = Score)) +
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
      "1 = Not concerned at all",
      "2",
      "3",
      "4",
      "5 = Very concerned"
    ),
    drop = FALSE,
    name = "Response"
  ) +
  labs(
    title = "Clinicians-reported clinical management concerns about teleconsultation",
    x = NULL,
    y = "Percentage of responses"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# doctor-patient relationships ==========
data <- read_excel("Clinician Response 0725.xlsx")

# If columns are named 7a, 7b, 7c, 7d
q7 <- data %>%
  select(x7a = `7a`, x7b = `7b`, x7c = `7c`, x7d = `7d`)

# If columns are already named x7a, x7b, x7c, x7d, use this instead:
# q7 <- data %>%
#   select(x7a, x7b, x7c, x7d)

# Convert to long format
q7_long <- q7 %>%
  mutate(Respondent = row_number()) %>%
  pivot_longer(
    cols = c(x7a, x7b, x7c, x7d),
    names_to = "Question",
    values_to = "Score"
  ) %>%
  filter(!is.na(Score)) %>%
  mutate(Score = as.numeric(Score))

# Question labels
q7_labels <- c(
  "x7a" = "Sharing rapport",
  "x7b" = "Listen to patients better",
  "x7c" = "Take patients' perspectives better",
  "x7d" = "Empathise with patients"
)

q7_long <- q7_long %>%
  mutate(
    Question = factor(
      Question,
      levels = c("x7a", "x7b", "x7c", "x7d"),
      labels = q7_labels[c("x7a", "x7b", "x7c", "x7d")]
    )
  )

# Count responses and fill missing categories with 0
q7_dist <- q7_long %>%
  count(Question, Score, name = "n") %>%
  complete(
    Question,
    Score = 1:5,
    fill = list(n = 0)
  ) %>%
  group_by(Question) %>%
  mutate(
    Percent = 100 * n / sum(n)
  ) %>%
  ungroup()

# Add display labels for Likert categories
q7_dist <- q7_dist %>%
  mutate(
    Score_f = factor(
      Score,
      levels = c(5, 4, 3, 2, 1),
      labels = c(
        "5 = Teleconsultation was better",
        "4",
        "3 = No difference",
        "2",
        "1 = Teleconsultation was worse"
      )
    )
  )

# Build diverging bar positions
# 5 and 4 = left
# 3 = centre
# 2 and 1 = right
q7_plot <- q7_dist %>%
  group_by(Question) %>%
  mutate(
    p1 = Percent[Score == 1],
    p2 = Percent[Score == 2],
    p3 = Percent[Score == 3],
    p4 = Percent[Score == 4],
    p5 = Percent[Score == 5]
  ) %>%
  ungroup() %>%
  mutate(
    xmin = case_when(
      Score == 5 ~ -(p5 + p4 + p3 / 2),
      Score == 4 ~ -(p4 + p3 / 2),
      Score == 3 ~ -(p3 / 2),
      Score == 2 ~  (p3 / 2),
      Score == 1 ~  (p3 / 2 + p2)
    ),
    xmax = case_when(
      Score == 5 ~ -(p4 + p3 / 2),
      Score == 4 ~ -(p3 / 2),
      Score == 3 ~  (p3 / 2),
      Score == 2 ~  (p3 / 2 + p2),
      Score == 1 ~  (p3 / 2 + p2 + p1)
    ),
    label_x = (xmin + xmax) / 2,
    label = ifelse(Percent >= 5, paste0(round(Percent), "%"), "")
  )

# Colours
likert_cols <- c(
  "5 = Teleconsultation was better" = "#607d8b",
  "4" = "#90a4ae",
  "3 = No difference" = "#cfd8dc",
  "2" = "#d9b39c",
  "1 = Teleconsultation was worse" = "#c9706a"
)

# Plot
ggplot(q7_plot) +
  geom_rect(
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = as.numeric(Question) - 0.3,
      ymax = as.numeric(Question) + 0.3,
      fill = Score_f
    ),
    colour = "white"
  ) +
  geom_text(
    aes(
      x = label_x,
      y = as.numeric(Question),
      label = label
    ),
    size = 3,
    colour = "black"
  ) +
  scale_y_continuous(
    breaks = seq_along(levels(q7_plot$Question)),
    labels = levels(q7_plot$Question),
    expand = expansion(add = c(0.5, 0.5))
  ) +
  scale_x_continuous(
    breaks = seq(-100, 100, 20),
    labels = function(x) paste0(abs(x), "%"),
    limits = c(-100, 100),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = likert_cols,
    limits = c(
      "5 = Teleconsultation was better",
      "4",
      "3 = No difference",
      "2",
      "1 = Teleconsultation was worse"
    ),
    drop = FALSE,
    name = "Response"
  ) +
  labs(
    title = "Clinicians-reported opinions about teleconsultation versus face-to-face consultation",
    x = "Percentage of responses",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )


#######################
# Put all the graphs together


# -------------------------
# 1) LEGAL / ETHICAL
# -------------------------
q8_legal <- data %>%
  select(x8a, x8b, x8c, x8d) %>%
  mutate(Respondent = row_number()) %>%
  pivot_longer(
    cols = c(x8a, x8b, x8c, x8d),
    names_to = "Item",
    values_to = "Score"
  ) %>%
  mutate(
    Domain = "Legal or ethical issues",
    Item_label = recode(Item,
                        "x8a" = "Ethical issues",
                        "x8b" = "Data security",
                        "x8c" = "Patient confidentiality",
                        "x8d" = "Legal issues")
  )

# -------------------------
# 2) TECHNOLOGICAL
# -------------------------
q8_tech <- data %>%
  select(x8g, x8l) %>%
  mutate(Respondent = row_number()) %>%
  pivot_longer(
    cols = c(x8g, x8l),
    names_to = "Item",
    values_to = "Score"
  ) %>%
  mutate(
    Domain = "Technological issues",
    Item_label = recode(Item,
                        "x8g" = "Unfamiliarity with tech requirements",
                        "x8l" = "Tech issues interrupting patient care")
  )

# -------------------------
# 3) CLINICAL MANAGEMENT
# -------------------------
q8_clin <- data %>%
  select(x8e, x8f, x8h, x8j, x8k) %>%
  mutate(Respondent = row_number()) %>%
  pivot_longer(
    cols = c(x8e, x8f, x8h, x8j, x8k),
    names_to = "Item",
    values_to = "Score"
  ) %>%
  mutate(
    Domain = "Clinical management difficulties",
    Item_label = recode(Item,
                        "x8e" = "Lack professional guidance",
                        "x8f" = "Lack training",
                        "x8h" = "Need to adjust interview styles",
                        "x8j" = "Ability to diagnose or treat",
                        "x8k" = "Risk management concerns")
  )

# -------------------------
# 4) COMBINE ALL
# -------------------------
q8_all <- bind_rows(q8_legal, q8_tech, q8_clin) %>%
  filter(!is.na(Score)) %>%
  mutate(
    Score = factor(
      Score,
      levels = 1:5,
      labels = c(
        "1 = Not concerned at all",
        "2",
        "3",
        "4",
        "5 = Very concerned"
      ),
      ordered = TRUE
    ),
    Domain = factor(Domain,
                    levels = c("Legal or ethical issues", "Technological issues", "Clinical management difficulties"))
  )

q8_dist_all <- q8_all %>%
  group_by(Domain, Item_label, Score) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Domain, Item_label) %>%
  mutate(
    Percent = 100 * n / sum(n),
    Label = ifelse(Percent >= 5, paste0(round(Percent), "%"), "")
  ) %>%
  ungroup()

likert_cols <- c(
  "1 = Not concerned at all" = "#607d8b",
  "2"                       = "#90a4ae",
  "3"                       = "#cfd8dc",
  "4"                       = "#d9b39c",
  "5 = Very concerned"      = "#c9706a"
)


ggplot(q8_dist_all, aes(x = Item_label, y = Percent, fill = Score)) +
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
  facet_wrap(
    ~ Domain,
    ncol = 1,
    scales = "free_y",
    space = "free_y",
    strip.position = "top"
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 28)) +
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
    title = "Clinician-reported areas of concern about teleconsultation",
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

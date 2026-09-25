library(readxl)
library(dplyr)
library(purrr)
library(tibble)
library(janitor)
library(DescTools)

# Load data and keep only teleconsultation-group participants
# who also completed the follow-up assessment

dat <- readRDS("~/Telemed/0922_tele.rds")
summary(dat)

#q2--------------
# Keep the two paired variables
q2 <- dat[, c("sopeq_b_2", "sopeq_f_2")]

# Rename for a clearer transition table
names(q2) <- c("baseline", "followup")

# Keep only complete baseline-follow-up pairs
q2 <- q2[!is.na(q2$baseline) & !is.na(q2$followup), ]

# Number of paired observations included in Question 2
nrow(q2)

# Obtain the response categories that remain
q2_categories <- sort(unique(c(q2$baseline, q2$followup)))

# Create the square matched-pairs transition table
q2_table <- table(
  factor(q2$baseline, levels = q2_categories),
  factor(q2$followup, levels = q2_categories)
)

# Display the transition table
q2_table

# Stuart-Maxwell test
q2_test <- StuartMaxwellTest(q2_table)

# Display result
q2_test

#q3--------------
# Keep the two paired variables
q3 <- dat[, c("sopeq_b_3", "sopeq_f_3")]

# Rename for a clearer transition table
names(q3) <- c("baseline", "followup")

# Keep only complete baseline-follow-up pairs
q3 <- q3[!is.na(q3$baseline) & !is.na(q3$followup), ]

# Number of paired observations included in Question 2
nrow(q3)

# Obtain the response categories that remain
q3_categories <- sort(unique(c(q3$baseline, q3$followup)))

# Create the square matched-pairs transition table
q3_table <- table(
  factor(q3$baseline, levels = q3_categories),
  factor(q3$followup, levels = q3_categories)
)

# Display the transition table
q3_table

# Stuart-Maxwell test
q3_test <- StuartMaxwellTest(q3_table)

# Display result
q3_test

#q4--------------
# Keep the two paired variables
q4 <- dat[, c("sopeq_b_4", "sopeq_f_4")]

# Rename for a clearer transition table
names(q4) <- c("baseline", "followup")

# Keep only complete baseline-follow-up pairs
q4 <- q4[!is.na(q3$baseline) & !is.na(q4$followup), ]

# Number of paired observations included in Question 2
nrow(q4)

# Obtain the response categories that remain
q4_categories <- sort(unique(c(q4$baseline, q4$followup)))

# Create the square matched-pairs transition table
q4_table <- table(
  factor(q4$baseline, levels = q4_categories),
  factor(q4$followup, levels = q4_categories)
)

# Display the transition table
q4_table

# Stuart-Maxwell test
q4_test <- StuartMaxwellTest(q4_table)

# Display result
q4_test

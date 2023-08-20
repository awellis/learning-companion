# Load necessary libraries
library(tidyverse)
library(MASS) # For mvrnorm function

# Set seed for reproducibility
set.seed(1234)

# Parameters
n_students <- 60
n_assessments <- 6
n_items <- 20

# Generate group-level effects
group_effects <- tibble(
  Group = c("Control", "Self-Regulation"),
  group_initial_ability_mean = c(0.4, 0.42),
  group_improvement_rate_mean = c(0.05, 0.07)
)

# Generate student-level data with parameters drawn from the group-level effects
students <- tibble(
  StudentID = 1:n_students,
  Group = if_else(StudentID <= n_students / 2, "Control", "Self-Regulation")
) |>
  left_join(group_effects, by = "Group") |>
  rowwise() |>
  mutate(
    # Assume a correlation matrix and covariance matrix for our two random effects
    correlation_matrix = matrix(c(1, 0.6, 0.6, 1), 2), # Example correlation of 0.6 between effects
    sigma = matrix(c(0.05, 0, 0, 0.01), 2), # Variances for the random effects
    covariance_matrix = sigma %*% correlation_matrix %*% sigma,

    # Draw from a multivariate normal distribution
    effects = mvrnorm(1, c(group_initial_ability_mean, group_improvement_rate_mean), covariance_matrix),

    # Extract individual parameters
    individual_initial_ability = effects[1],
    individual_improvement_rate = effects[2]
  ) |>
  select(-group_initial_ability_mean, -group_improvement_rate_mean)

# Expand to create dataset with each assessment and item for each student
data <- students |>
  crossing(Assessment = 1:n_assessments, Item = 1:n_items) |>
  rowwise() |>
  mutate(
    prob = individual_initial_ability + Assessment * individual_improvement_rate,
    Score = rbinom(1, 1, prob)
  ) |>
  select(StudentID, Assessment, Item, Group, Score)

# View the first few rows of the data
head(data)

# Optional: Analysis can be done using multilevel modeling packages like `brms` or `lme4`.



# Compute average scores for each assessment and group
average_scores <- data |>
  group_by(Group, Assessment) |>
  summarize(avg_score = mean(Score), .groups = "drop")

# Plot the data using ggplot
ggplot(average_scores, aes(x = Assessment, y = avg_score, color = Group, group = Group)) +
  geom_line(aes(linetype = Group), size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Average Score by Assessment and Group",
    x = "Assessment",
    y = "Average Score",
    color = "Group"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Control" = "blue", "Self-Regulation" = "red"))

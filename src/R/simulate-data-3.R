library(tidyverse)

# Set seed for reproducibility
set.seed(1234)

# Parameters
n_students <- 60
n_assessments <- 6
n_items <- 20
practice_amount <- 5 # Example value: amount of practice every assessment
group_multiplier <- 1.2 # Self-regulation group practices more effectively

# Create student-level data
students <- tibble(
    StudentID = 1:n_students,
    Group = if_else(StudentID <= n_students / 2, "Control", "Self-Regulation"),
    PracticeEffectiveness = if_else(Group == "Self-Regulation", group_multiplier, 1),
    Ability = rnorm(n_students, 10, 2) # Initialize abilities with some random values
)

# Generate the state-space model data for each student and each assessment
data <- expand.grid(StudentID = 1:n_students, Assessment = 1:n_assessments) |>
    left_join(students, by = "StudentID") |>
    arrange(StudentID, Assessment) |>
    group_by(StudentID) |>
    mutate(
        # State transition model
        Ability = if_else(Assessment == 1, Ability, lag(Ability) + practice_amount * PracticeEffectiveness + rnorm(n(), 0, 1)),
        # Observation model
        Score = rbinom(n(), n_items, pnorm(Ability, mean = 15, sd = 3)) # Example values for mean and sd
    ) |>
    ungroup() |>
    select(StudentID, Assessment, Group, Score)

# View the first few rows of the data
head(data)

# Plot the data using ggplot
ggplot(data, aes(x = Assessment, y = Score, color = Group, group = interaction(StudentID, Group))) +
    geom_line(aes(linetype = Group), alpha = 0.7) +
    geom_point(size = 2) +
    labs(
        title = "Score by Assessment and Group",
        x = "Assessment",
        y = "Score",
        color = "Group"
    ) +
    theme_minimal() +
    scale_color_manual(values = c("Control" = "blue", "Self-Regulation" = "red"))

## using accumulate

library(tidyverse)

# Set seed for reproducibility
set.seed(1234)

# Parameters
n_students <- 60
n_assessments <- 6
n_items <- 20
practice_amount <- 5 # Example value: amount of practice every assessment
group_multiplier <- 1.2 # Self-regulation group practices more effectively

# Create student-level data
students <- tibble(
    StudentID = 1:n_students,
    Group = if_else(StudentID <= n_students / 2, "Control", "Self-Regulation"),
    PracticeEffectiveness = if_else(Group == "Self-Regulation", group_multiplier, 1),
    InitialAbility = rnorm(n_students, 10, 2) # Initialize abilities with some random values
)

# Define state transition function
transition_ability <- function(prev_ability, practice_effectiveness) {
    prev_ability + practice_amount * practice_effectiveness + rnorm(1, 0, 1)
}

# Generate the state-space model data for each student and each assessment
data <- students %>%
    rowwise() %>%
    mutate(
        Abilities = list(accumulate(rep(PracticeEffectiveness, n_assessments - 1),
            .f = transition_ability,
            .init = InitialAbility
        )),
        Scores = map_dbl(Abilities, ~ rbinom(1, n_items, pnorm(.x, mean = 15, sd = 3)))
    ) %>%
    select(-InitialAbility) %>%
    unnest(cols = c(Abilities, Scores))

# Transform data for plotting
data_for_plot <- data %>%
    group_by(StudentID) %>%
    mutate(Assessment = row_number()) %>%
    ungroup()

# Plot the data using ggplot
ggplot(data_for_plot, aes(x = Assessment, y = Scores, color = Group, group = interaction(StudentID, Group))) +
    geom_line(aes(linetype = Group), alpha = 0.7) +
    geom_point(size = 2) +
    labs(
        title = "Score by Assessment and Group",
        x = "Assessment",
        y = "Score",
        color = "Group"
    ) +
    theme_minimal() +
    scale_color_manual(values = c("Control" = "blue", "Self-Regulation" = "red"))

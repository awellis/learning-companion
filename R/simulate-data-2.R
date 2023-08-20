library(tidyverse)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(faux)

data <- add_random(uni = 2) |>
    add_random(class = c(2, 2), .nested_in = "uni") |>
    add_random(subject = 2, item = 3) |>
    add_within("subject",
        time = c("pre", "mid", "post")
    ) |>
    add_between("subject",
        group = c("control", "sel-regulation")
    ) |>
    add_ranef("subject", u0s = 1.3) |>
    # add by-item random intercept and slope
    add_ranef("item", u0i = 1.5, u1i = 1.5, .cors = 0.3) |>
    # add by-subject:item random intercept
    add_ranef(c("subject", "item"), u0si = 1.7) |>
    # add error term (by observation)
    add_ranef(sigma = 2.2)



# define parameters
subj_n <- 10 # number of subjects
item_n <- 10 # number of items
b0 <- 0 # intercept
b1 <- 0 # fixed effect of condition
u0s_sd <- 1 # random intercept SD for subjects
u0i_sd <- 1 # random intercept SD for items
u1i_sd <- 1 # random b1 slope SD for items
r01i <- 0 # correlation between random effects 0 and 1 for items
sigma_sd <- 2 # error SD

# set up data structure
data <- add_random(subj = subj_n, item = item_n) |>
    # add and recode categorical variables
    add_between("subj", cond = c("control", "test")) |>
    add_recode("cond", "cond.t", control = 0, test = 1) |>
    # add random effects
    add_ranef("subj", u0s = u0s_sd) |>
    add_ranef("item", u0i = u0i_sd, u1i = u1i_sd, .cors = r01i) |>
    add_ranef(sigma = sigma_sd) |>
    # calculate DV
    mutate(dv = b0 + u0s + u0i + (b1 + u1i) * cond.t + sigma)

m <- lmer(dv ~ cond.t + (1 | subj) + (1 + cond.t | item), data = data)

summary(m)



## Power simulation

sim <- function(subj_n = 10, item_n = 10,
                b0 = 0, b1 = 0, # fixed effects
                u0s_sd = 1, u0i_sd = 1, # random intercepts
                u1i_sd = 1, r01i = 0, # random slope and cor
                sigma_sd = 2, # error term
                ... # helps the function work with pmap() below
) {
    # set up data structure
    data <- add_random(subj = subj_n, item = item_n) |>
        # add and recode categorical variables
        add_between("subj", cond = c("control", "test")) |>
        add_recode("cond", "cond.t", control = 0, test = 1) |>
        # add random effects
        add_ranef("subj", u0s = u0s_sd) |>
        add_ranef("item", u0i = u0i_sd, u1i = u1i_sd, .cors = r01i) |>
        add_ranef(sigma = sigma_sd) |>
        # calculate DV
        mutate(dv = b0 + u0s + u0i + (b1 + u1i) * cond.t + sigma)

    # run mixed effect model and return relevant values
    m <- lmer(dv ~ cond.t + (1 | subj) + (1 + cond.t | item), data = data)

    broom.mixed::tidy(m)
}

sim(subj_n = 50, item_n = 40, b1 = 0.5, r01i = 0.2)

x <- crossing(
    rep = 1:50, # number of replicates
    subj_n = c(50, 100), # range of subject N
    item_n = 25, # fixed item N
    b1 = c(0.25, 0.5, 0.75), # range of effects
    r01i = 0.2 # fixed correlation
) |>
    mutate(analysis = pmap(., sim)) |>
    unnest(analysis)


# calculate power for alpha = 0.05
filter(x, effect == "fixed", term == "cond.t") |>
    group_by(b1, subj_n) |>
    summarise(
        power = mean(p.value < .05),
        .groups = "drop"
    ) |>
    ggplot(aes(b1, subj_n, fill = power)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.2f", power)), color = "white", size = 10) +
    scale_fill_viridis_c(limits = c(0, 1))

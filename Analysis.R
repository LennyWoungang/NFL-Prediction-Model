############################# Loading libraries and data
library(dplyr)
library(geepack)
library(MuMIn)
library(ggplot2)
library(broom)
library(geeM)
Training_data <- read.csv("Training_data.csv")
Test_data <- read.csv("Test_data.csv")
Final_data <- read.csv("Final_data.csv")

############### Descriptive stats
predictors_by_season <- Training_data %>%
  group_by(season) %>%
  summarise(
    avg_age_mean = mean(avg_age, na.rm = TRUE),
    avg_age_sd   = sd(avg_age, na.rm = TRUE),
    avg_age_min  = min(avg_age, na.rm = TRUE),
    avg_age_max  = max(avg_age, na.rm = TRUE),
    
    tenure_mean = mean(coach_tenure_year, na.rm = TRUE),
    tenure_sd   = sd(coach_tenure_year, na.rm = TRUE),
    tenure_min  = min(coach_tenure_year, na.rm = TRUE),
    tenure_max  = max(coach_tenure_year, na.rm = TRUE)
  )

playoffs_by_season <- Training_data %>%
  group_by(season) %>%
  summarise(
    n_teams = n(),
    playoffs_prev_n = sum(made_playoffs_prev == 1, na.rm = TRUE),
    playoffs_prev_pct = mean(made_playoffs_prev == 1, na.rm = TRUE) * 100
  )

offense_cat_by_season <- Training_data %>%
  group_by(season, rank_offense_cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(season) %>%
  mutate(percent = n / sum(n) * 100)

defense_cat_by_season <- Training_data %>%
  group_by(season, rank_defence_cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(season) %>%
  mutate(percent = n / sum(n) * 100)


############### Making predictors into factors
Training_data$team <- factor(Training_data$team)
Training_data$rank_offense_cat <- factor(
  Training_data$rank_offense_cat,
  levels = c(1, 2, 3, 4)
)
Training_data$rank_defence_cat <- factor(
  Training_data$rank_defence_cat,
  levels = c(1, 2, 3, 4)
)
Training_data$made_playoffs_prev <- factor(
  Training_data$made_playoffs_prev,
  levels = c(0, 1)
)


############## Checking Model Assumptions for Poisson
### Linearity
par(mfrow = c(1, 2))
plot(
  Training_data$avg_age,
  log(Training_data$wins + 0.5),
  xlab = "Average age",
  ylab = "log(wins + 0.5)",
  main = "Log wins vs Average Age"
)
lines(
  lowess(Training_data$avg_age, log(Training_data$wins + 0.5)),
  col = "red",
  lwd = 2
)
plot(
  Training_data$coach_tenure_year,
  log(Training_data$wins + 0.5),
  xlab = "Coach tenure (years)",
  ylab = "log(wins + 0.5)",
  main = "Log wins vs Coach Tenure"
)
lines(
  lowess(Training_data$coach_tenure_year, log(Training_data$wins + 0.5)),
  col = "red",
  lwd = 2
)

#### Overdispersion check
overdisperison_check <- Training_data %>%
  group_by(season) %>%
  summarise(
    mean_wins = mean(wins, na.rm = TRUE),
    var_wins  = var(wins, na.rm = TRUE),
    n_teams   = n()
  )


############### Interaction variables diagnosis
## Offense/defense
Training_data %>%
  group_by(rank_offense_cat, rank_defence_cat) %>%
  summarise(mean_wins = mean(wins, na.rm = TRUE),
            n = n(),
            .groups = "drop")
Training_data %>%
  group_by(rank_offense_cat, rank_defence_cat) %>%
  summarise(mean_wins = mean(wins, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = rank_offense_cat,
             y = mean_wins,
             color = factor(rank_defence_cat),
             group = rank_defence_cat)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Offense Rank Category",
    y = "Mean Wins",
    color = "Defense Rank Category",
    title = "Interaction Plot: Offense × Defense"
  ) +
  theme_minimal()

## Offense/made_playoffs_last_year
Training_data %>%
  group_by(rank_offense_cat, made_playoffs_prev) %>%
  summarise(
    mean_wins = mean(wins, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
Training_data %>%
  group_by(rank_offense_cat, made_playoffs_prev) %>%
  summarise(
    mean_wins = mean(wins, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = rank_offense_cat,
             y = mean_wins,
             color = factor(made_playoffs_prev),
             group = made_playoffs_prev)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Offense Rank Category",
    y = "Mean Wins",
    color = "Made Playoffs Previous Year",
    title = "Interaction Plot: Offense × Previous Playoffs"
  ) +
  theme_minimal()

## Defense/made_playoffs_last_year
Training_data %>%
  group_by(rank_defence_cat, made_playoffs_prev) %>%
  summarise(
    mean_wins = mean(wins, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
Training_data %>%
  group_by(rank_defence_cat, made_playoffs_prev) %>%
  summarise(
    mean_wins = mean(wins, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = rank_defence_cat,
             y = mean_wins,
             color = factor(made_playoffs_prev),
             group = made_playoffs_prev)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Defense Rank Category",
    y = "Mean Wins",
    color = "Made Playoffs Previous Year",
    title = "Interaction Plot: Defense × Previous Playoffs"
  ) +
  theme_minimal()

## Tenure/offense
Training_data %>%
  group_by(rank_offense_cat) %>%
  do(tidy(lm(wins ~ coach_tenure_year, data = .))) %>%
  filter(term == "coach_tenure_year") %>%
  select(rank_offense_cat, estimate, std.error)

## Tenure/defence
Training_data %>%
  group_by(rank_defence_cat) %>%
  do(tidy(lm(wins ~ coach_tenure_year, data = .))) %>%
  filter(term == "coach_tenure_year") %>%
  select(rank_defence_cat, estimate, std.error)

## Tenure/playoffs
Training_data %>%
  group_by(made_playoffs_prev) %>%
  do(tidy(lm(wins ~ coach_tenure_year, data = .))) %>%
  filter(term == "coach_tenure_year") %>%
  select(made_playoffs_prev, estimate, std.error)

## Age/offense
Training_data %>%
  group_by(rank_offense_cat) %>%
  do(tidy(lm(wins ~ avg_age, data = .))) %>%
  filter(term == "avg_age") %>%
  select(rank_offense_cat, estimate, std.error)

## Age/defence
Training_data %>%
  group_by(rank_defence_cat) %>%
  do(tidy(lm(wins ~ avg_age, data = .))) %>%
  filter(term == "avg_age") %>%
  select(rank_defence_cat, estimate, std.error)

## Age/playoffs
Training_data %>%
  group_by(made_playoffs_prev) %>%
  do(tidy(lm(wins ~ avg_age, data = .))) %>%
  filter(term == "avg_age") %>%
  select(made_playoffs_prev, estimate, std.error)

############# Model selection (Poisson)
##### full interaction
cor_structs <- c("independence", "exchangeable", "ar1")
results <- list()
interactions <- c(
  "rank_offense_cat:made_playoffs_prev",
  "avg_age:rank_defence_cat",
  "avg_age:made_playoffs_prev",
  "avg_age:coach_tenure_year"
)
for (cor in cor_structs) {
  full_model <- geeglm(
    wins ~
      rank_offense_cat +
      rank_defence_cat +
      made_playoffs_prev +
      avg_age +
      coach_tenure_year +
      offset(log(games_played))+
      rank_offense_cat:made_playoffs_prev +
      avg_age:rank_defence_cat +
      avg_age:made_playoffs_prev +
      avg_age:coach_tenure_year,
    data = Training_data,
    family = poisson,
    id = team,
    waves = season,
    corstr = cor,
    scale.fix = TRUE)
  qic_full <- QIC(full_model)
  qic_table <- data.frame(
    corstr = cor,
    removed_interaction = c("none (full model)", interactions),
    QIC = NA_real_
  )
  qic_table$QIC[1] <- qic_full
  for (i in seq_along(interactions)) {
    reduced_formula <- update(
      formula(full_model),
      paste(". ~ . -", interactions[i])
    )
    reduced_model <- geeglm(
      formula = reduced_formula,
      data = Training_data,
      family = poisson,
      id = team,
      corstr = cor,
      waves = season,
      scale.fix = TRUE
    )
    qic_table$QIC[i + 1] <- QIC(reduced_model)
  }
  best_row <- qic_table[which.min(qic_table$QIC), ]
  results[[cor]] <- list(
    qic_table = qic_table,
    best_removal = best_row$removed_interaction,
    best_qic = best_row$QIC
  )
}
results
### stop(keep model)

########### Model selection (Quasi Poisson)
##### full interaction
results <- list()
interactions <- c(
  "rank_offense_cat:made_playoffs_prev",
  "avg_age:rank_defence_cat",
  "avg_age:made_playoffs_prev",
  "avg_age:coach_tenure_year"
)
for (cor in cor_structs) {
  full_model <- geeglm(
    wins ~
      rank_offense_cat +
      rank_defence_cat +
      made_playoffs_prev +
      avg_age +
      coach_tenure_year +
      offset(log(games_played))+
      rank_offense_cat:made_playoffs_prev +
      avg_age:rank_defence_cat +
      avg_age:made_playoffs_prev +
      avg_age:coach_tenure_year,
    data = Training_data,
    waves = season,
    family = poisson,
    id = team,
    corstr = cor,
    scale.fix = FALSE)
  qic_full <- QIC(full_model)
  qic_table <- data.frame(
    corstr = cor,
    removed_interaction = c("none (full model)", interactions),
    QIC = NA_real_
  )
  qic_table$QIC[1] <- qic_full
  for (i in seq_along(interactions)) {
    reduced_formula <- update(
      formula(full_model),
      paste(". ~ . -", interactions[i])
    )
    reduced_model <- geeglm(
      formula = reduced_formula,
      data = Training_data,
      family = poisson,
      id = team,
      corstr = cor,
      waves = season,
      scale.fix = FALSE
    )
    qic_table$QIC[i + 1] <- QIC(reduced_model)
  }
  best_row <- qic_table[which.min(qic_table$QIC), ]
  results[[cor]] <- list(
    qic_table = qic_table,
    best_removal = best_row$removed_interaction,
    best_qic = best_row$QIC
  )
}
results

#### Removed Age/Defense
interactions <- c(
  "rank_offense_cat:made_playoffs_prev",
  "avg_age:made_playoffs_prev",
  "avg_age:coach_tenure_year"
)
results <- list()
for (cor in cor_structs) {
  full_model <- geeglm(
    wins ~
      rank_offense_cat +
      rank_defence_cat +
      made_playoffs_prev +
      avg_age +
      coach_tenure_year +
      offset(log(games_played))+
      rank_offense_cat:made_playoffs_prev +
      avg_age:made_playoffs_prev +
      avg_age:coach_tenure_year,
    data = Training_data,
    family = poisson,
    waves = season,
    id = team,
    corstr = cor,
    scale.fix = FALSE
  )
  qic_full <- QIC(full_model)
  qic_table <- data.frame(
    corstr = cor,
    removed_interaction = c("none (full model)", interactions),
    QIC = NA_real_
  )
  qic_table$QIC[1] <- qic_full
  for (i in seq_along(interactions)) {
    reduced_formula <- update(
      formula(full_model),
      paste(". ~ . -", interactions[i])
    )
    reduced_model <- geeglm(
      formula = reduced_formula,
      data = Training_data,
      family = poisson,
      id = team,
      waves = season,
      corstr = cor,
      scale.fix = FALSE
    )
    qic_table$QIC[i + 1] <- QIC(reduced_model)
  }
  best_row <- qic_table[which.min(qic_table$QIC), ]
  results[[cor]] <- list(
    qic_table = qic_table,
    best_removal = best_row$removed_interaction,
    best_qic = best_row$QIC
  )
}
results

####  Removed Age/Defense + Offense/playoffs
interactions <- c(
  "avg_age:made_playoffs_prev",
  "avg_age:coach_tenure_year"
)
results <- list()
for (cor in cor_structs) {
  full_model <- geeglm(
    wins ~
      rank_offense_cat +
      rank_defence_cat +
      made_playoffs_prev +
      avg_age +
      coach_tenure_year +
      offset(log(games_played))+
      avg_age:made_playoffs_prev +
      avg_age:coach_tenure_year,
    data = Training_data,
    family = poisson,
    id = team,
    waves = season,
    corstr = cor,
    scale.fix = FALSE
  )
  qic_full <- QIC(full_model)
  qic_table <- data.frame(
    corstr = cor,
    removed_interaction = c("none (full model)", interactions),
    QIC = NA_real_
  )
  qic_table$QIC[1] <- qic_full
  for (i in seq_along(interactions)) {
    reduced_formula <- update(
      formula(full_model),
      paste(". ~ . -", interactions[i])
    )
    reduced_model <- geeglm(
      formula = reduced_formula,
      data = Training_data,
      family = poisson,
      id = team,
      corstr = cor,
      waves = season,
      scale.fix = FALSE
    )
    qic_table$QIC[i + 1] <- QIC(reduced_model)
  }
  best_row <- qic_table[which.min(qic_table$QIC), ]
  results[[cor]] <- list(
    qic_table    = qic_table,
    best_removal = best_row$removed_interaction,
    best_qic     = best_row$QIC
  )
}
results

#### Removed Age/Defence + Offense/playoffs + Age/playoffs
interactions <- c(
  "avg_age:coach_tenure_year"
)
results <- list()
for (cor in cor_structs) {
  full_model <- geeglm(
    wins ~
      rank_offense_cat +
      rank_defence_cat +
      made_playoffs_prev +
      avg_age +
      coach_tenure_year +
      offset(log(games_played))+
      avg_age:coach_tenure_year,
    data = Training_data,
    family = poisson,
    id = team,
    waves = season,
    corstr = cor,
    scale.fix = FALSE
  )
  qic_full <- QIC(full_model)
  qic_table <- data.frame(
    corstr = cor,
    removed_interaction = c("none (full model)", interactions),
    QIC = NA_real_
  )
  qic_table$QIC[1] <- qic_full
  reduced_formula <- update(
    formula(full_model),
    ". ~ . - avg_age:coach_tenure_year"
  )
  reduced_model <- geeglm(
    formula = reduced_formula,
    data = Training_data,
    family = poisson,
    id = team,
    corstr = cor,
    waves = season,
    scale.fix = FALSE
  )
  qic_table$QIC[2] <- QIC(reduced_model)
  best_row <- qic_table[which.min(qic_table$QIC), ]
  results[[cor]] <- list(
    qic_table    = qic_table,
    best_removal = best_row$removed_interaction,
    best_qic     = best_row$QIC
  )
}
results
### stop(keep model)

############# model selection (NB)
##### full interaction
results <- list()
interactions <- c(
  "rank_offense_cat:made_playoffs_prev",
  "avg_age:rank_defence_cat",
  "avg_age:made_playoffs_prev",
  "avg_age:coach_tenure_year"
)
for (cor in cor_structs) {
  full_model <- geem(
    wins ~
      rank_offense_cat +
      rank_defence_cat +
      made_playoffs_prev +
      avg_age +
      coach_tenure_year +
      offset(log(games_played))+
      rank_offense_cat:made_playoffs_prev +
      avg_age:rank_defence_cat +
      avg_age:made_playoffs_prev +
      avg_age:coach_tenure_year,
    data = Training_data,
    family = MASS::negative.binomial(2),
    id = team,
    corstr = cor,
    waves = season,
    scale.fix = TRUE)
  qic_full <- QIC(full_model)
  qic_table <- data.frame(
    corstr = cor,
    removed_interaction = c("none (full model)", interactions),
    QIC = NA_real_
  )
  qic_table$QIC[1] <- qic_full
  for (i in seq_along(interactions)) {
    reduced_formula <- update(
      formula(full_model),
      paste(". ~ . -", interactions[i])
    )
    reduced_model <- geem(
      formula = reduced_formula,
      data = Training_data,
      family = MASS::negative.binomial(2),
      id = team,
      corstr = cor,
      waves = season,
      scale.fix = TRUE
    )
    qic_table$QIC[i + 1] <- QIC(reduced_model)
  }
  best_row <- qic_table[which.min(qic_table$QIC), ]
  results[[cor]] <- list(
    qic_table = qic_table,
    best_removal = best_row$removed_interaction,
    best_qic = best_row$QIC
  )
}
results

#### Removed Age/playoffs
results <- list()
interactions <- c(
  "rank_offense_cat:made_playoffs_prev",
  "avg_age:rank_defence_cat",
  "avg_age:coach_tenure_year"
)
for (cor in cor_structs) {
  full_model <- geem(
    wins ~
      rank_offense_cat +
      rank_defence_cat +
      made_playoffs_prev +
      avg_age +
      coach_tenure_year +
      offset(log(games_played))+
      rank_offense_cat:made_playoffs_prev +
      avg_age:rank_defence_cat +
      avg_age:coach_tenure_year,
    data = Training_data,
    family = MASS::negative.binomial(2),
    id = team,
    waves = season,
    corstr = cor,
    scale.fix = TRUE
  )
  qic_full <- QIC(full_model)
  qic_table <- data.frame(
    corstr = cor,
    removed_interaction = c("none (full model)", interactions),
    QIC = NA_real_
  )
  qic_table$QIC[1] <- qic_full
  for (i in seq_along(interactions)) {
    reduced_formula <- update(
      formula(full_model),
      paste(". ~ . -", interactions[i])
    )
    reduced_model <- geem(
      formula = reduced_formula,
      data = Training_data,
      family = MASS::negative.binomial(2),
      id = team,
      waves = season,
      corstr = cor,
      scale.fix = TRUE
    )
    qic_table$QIC[i + 1] <- QIC(reduced_model)
  }
  
  best_row <- qic_table[which.min(qic_table$QIC), ]
  results[[cor]] <- list(
    qic_table    = qic_table,
    best_removal = best_row$removed_interaction,
    best_qic     = best_row$QIC
  )
}
results

#### Removed Age/playoffs and Age/defense 
interactions <- c(
  "rank_offense_cat:made_playoffs_prev",
  "avg_age:coach_tenure_year"
)
results <- list()
for (cor in cor_structs) {
  full_model <- geem(
    wins ~
      rank_offense_cat +
      rank_defence_cat +
      made_playoffs_prev +
      avg_age +
      coach_tenure_year +
      offset(log(games_played))+
      rank_offense_cat:made_playoffs_prev +
      avg_age:coach_tenure_year,
    data = Training_data,
    family = MASS::negative.binomial(2),
    id = team,
    waves = season,
    corstr = cor,
    scale.fix = TRUE
  )
  qic_full <- QIC(full_model)
  qic_table <- data.frame(
    corstr = cor,
    removed_interaction = c("none (full model)", interactions),
    QIC = NA_real_
  )
  qic_table$QIC[1] <- qic_full
  for (i in seq_along(interactions)) {
    reduced_formula <- update(
      formula(full_model),
      paste(". ~ . -", interactions[i])
    )
    reduced_model <- geem(
      formula = reduced_formula,
      data = Training_data,
      family = MASS::negative.binomial(2),
      id = team,
      waves = season,
      corstr = cor,
      scale.fix = TRUE
    )
    qic_table$QIC[i + 1] <- QIC(reduced_model)
  }
  best_row <- qic_table[which.min(qic_table$QIC), ]
  results[[cor]] <- list(
    qic_table    = qic_table,
    best_removal = best_row$removed_interaction,
    best_qic     = best_row$QIC
  )
}
results
## stop (Keep full model)



####### Final models
### Poisson
Poisson_model <- geeglm(
  wins ~
    rank_offense_cat +
    rank_defence_cat +
    made_playoffs_prev +
    avg_age +
    coach_tenure_year +
    offset(log(games_played))+
    rank_offense_cat:made_playoffs_prev +
    avg_age:rank_defence_cat +
    avg_age:made_playoffs_prev +
    avg_age:coach_tenure_year,
  data = Training_data,
  family = poisson,
  id = team,
  waves = season,
  corstr = cor,
  scale.fix = TRUE)

### Quasi Poisson
Quasi_model <- geeglm(
  wins ~
    rank_offense_cat +
    rank_defence_cat +
    made_playoffs_prev +
    avg_age +
    coach_tenure_year +
    offset(log(games_played))+
    avg_age:coach_tenure_year,
  data = Training_data,
  family = poisson,
  waves = season,
  id = team,
  corstr = "exchangeable",
  scale.fix = FALSE
)

### NB
NB_model <- geem(
  wins ~
    rank_offense_cat +
    rank_defence_cat +
    made_playoffs_prev +
    avg_age +
    coach_tenure_year +
    offset(log(games_played))+
    rank_offense_cat:made_playoffs_prev +
    avg_age:coach_tenure_year,
  data = Training_data,
  family = MASS::negative.binomial(2),
  id = team,
  waves = season,
  corstr = "exchangeable",
  scale.fix = TRUE
)


########### RMSE using Training Data (2003-2023 season)
fitted_poisson <- fitted(Poisson_model)
fitted_quasi   <- fitted(Quasi_model)
fitted_nb      <- fitted(NB_model)
rmse_poisson <- sqrt(
  mean((Training_data$wins - fitted_poisson)^2, na.rm = TRUE))
rmse_quasi <- sqrt(
  mean((Training_data$wins - fitted_quasi)^2, na.rm = TRUE))
rmse_nb <- sqrt(
  mean((Training_data$wins - fitted_nb)^2, na.rm = TRUE))
rmse_results <- data.frame(
  Model = c("Poisson GEE", "Quasi-Poisson GEE", "Negative Binomial GEE"),
  RMSE  = c(rmse_poisson, rmse_quasi, rmse_nb)
)
rmse_results


########### RMSE between Poisson and Quasi Poisson on Test Data (2024 season)
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}
Test_data <- Test_data %>%
  mutate(
    team = factor(team, levels = levels(Training_data$team)),
    rank_offense_cat = factor(
      rank_offense_cat,
      levels = levels(Training_data$rank_offense_cat)
    ),
    rank_defence_cat = factor(
      rank_defence_cat,
      levels = levels(Training_data$rank_defence_cat)
    ),
    made_playoffs_prev = factor(
      made_playoffs_prev,
      levels = levels(Training_data$made_playoffs_prev)
    )
  )
# Poisson
pred_poisson <- predict(
  Poisson_model,
  newdata = Test_data,
  type = "response"
)
# Quasi-Poisson 
pred_quasi <- predict(
  Quasi_model,
  newdata = Test_data,
  type = "response"
)
rmse_results <- data.frame(
  Model = c("Poisson GEE", "Quasi-Poisson GEE"),
  RMSE  = c(
    rmse(Test_data$wins, pred_poisson),
    rmse(Test_data$wins, pred_quasi)
  )
)
rmse_results

# Predictions 2024
Test_data <- Test_data %>%
  mutate(
    pred_poisson = predict(
      Poisson_model,
      newdata = Test_data,
      type = "response"
    ),
    pred_quasi = predict(
      Quasi_model,
      newdata = Test_data,
      type = "response"
    )
  )
Test_data %>%
  dplyr::select(
    team,
    wins,
    pred_poisson,
    pred_quasi
  ) %>%
  arrange(team)
summary(Quasi_model)

#### Final model check 
# Outliers
pearson_resid <- as.numeric(
  residuals(Quasi_model, type = "pearson")
)

fitted_mu <- as.numeric(
  fitted(Quasi_model)
)
plot(
  fitted_mu,
  pearson_resid,
  xlab = "Fitted mean wins",
  ylab = "Pearson residuals",
  pch = 16
)
abline(h = c(-3, 0, 3),
       col = c("red", "black", "red"),
       lty = c(2, 1, 2))











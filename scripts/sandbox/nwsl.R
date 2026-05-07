
current_year <- 2026
current_gameweek <- 8

fit_marcel_projection_1 <- function(data, ...) {
  fit <- data |>
    with(
      nls(
#        x ~ (d^2 * n_3 * x_3 + d * n_2 * x_2 + n_1 * x_1 + c * m) /
#          (d^2 * n_3 + d * n_2 + n_1 + c),
        x ~ (n_1 * x_1 + c * m) / (n_1 + c),
        weights = n,
#        start = list(c = coef(initial_fit)['c'], d = 1, m = coef(initial_fit)['m']),
#        start = list(c = 1, d = 1, m = 0),
        start = list(c = 1, m = 0),
#        lower = list(c = 1, d = 0, m = -Inf),
#        upper = list(c = Inf, d = 1, m = Inf),
#        algorithm = "port",
        ...
      )
    )
}

fit_marcel_projection_3 <- function(data, ...) {
#  initial_fit <- data |>
#    with(
#      nls(x ~ (n_1 * x_1 + c * m) / (n_1 + c),
#        weights = n,
#        start = list(c = 1, m = 0),
#        lower = list(c = 1, m = -Inf),
#        upper = list(c = Inf, m = Inf),
#        algorithm = "port"
#      )
#    )

  fit <- data |>
    with(
      nls(
        x ~ (d^2 * n_3 * x_3 + d * n_2 * x_2 + n_1 * x_1 + c * m) /
          (d^2 * n_3 + d * n_2 + n_1 + c),
        weights = n,
#        start = list(c = coef(initial_fit)['c'], d = 1, m = coef(initial_fit)['m']),
        start = list(c = 1, d = 1, m = 0),
#        lower = list(c = 1, d = 0, m = -Inf),
#        upper = list(c = Inf, d = 1, m = Inf),
#        algorithm = "port",
        ...
      )
    )
}

schedule <- jsonlite::fromJSON("https://app.americansocceranalysis.com/api/v1/nwsl/games?status=PreMatch,FullTime") |>
  dplyr::mutate(year = as.integer(season_name)) |>
  dplyr::mutate(
    date_time_pt = date_time_utc |>
      lubridate::ymd_hms(tz = "UTC") |>
      lubridate::with_tz(tzone = "America/Los_Angeles"),
    gameweek = dplyr::case_when(
      date_time_pt < "2026-01-01" ~ matchday,
      date_time_pt < "2026-03-18" ~ 1,
      date_time_pt < "2026-03-24" ~ 2,
      date_time_pt < "2026-03-28" ~ 3,
      date_time_pt < "2026-04-01" ~ 4,
      date_time_pt < "2026-04-15" ~ 5,
      date_time_pt < "2026-04-28" ~ 6,
      date_time_pt < "2026-05-05" ~ 7,
      date_time_pt < "2026-05-14" ~ 8,
      date_time_pt < "2026-05-19" ~ 9,
    )
  )



asa_client <- itscalledsoccer::AmericanSoccerAnalysis$new()

player <- asa_client$get_players(leagues = "nwsl")

team <- asa_client$get_teams(leagues = "nwsl")

game <- asa_client$get_games(leagues = "nwsl") |>
  dplyr::mutate(year = as.integer(season_name))

team_xgoals <- asa_client$get_team_xgoals(leagues = "nwsl", split_by_games = TRUE)

player_xgoals <- asa_client$get_player_xgoals(leagues = "nwsl", split_by_games = TRUE)

team_game <- game |>
  tidyr::pivot_longer(cols = c(home_team_id, away_team_id), values_to = "team_id") |>
  dplyr::select(team_id, year, matchday, game_id)

player_position <- player_xgoals |>
  dplyr::left_join(game, by = "game_id") |>
  dplyr::mutate(
    position = dplyr::case_when(
      general_position %in% c("GK") ~ "Goalkeeper",
      general_position %in% c("CB", "FB") ~ "Defender",
      general_position %in% c("DM", "CM", "AM") ~ "Midfielder",
      general_position %in% c("ST", "W") ~ "Forward",
    ),
    year = as.integer(season_name)
  ) |>
  dplyr::count(player_id, year, position) |>
  dplyr::group_by(player_id, year) |>
  dplyr::arrange(-n) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::select(player_id, year, position)

player_stint <- player_xgoals |>
  dplyr::left_join(game, by = "game_id") |>
  dplyr::group_by(player_id, year) |>
  dplyr::arrange(matchday) |>
  dplyr::mutate(stint = cumsum(dplyr::coalesce(team_id != dplyr::lag(team_id, 1), 0))) |>
  dplyr::ungroup() |>
  dplyr::group_by(player_id, year, stint, team_id) |>
  dplyr::summarize(matchday_first = min(matchday), .groups = "drop") |>
  dplyr::group_by(player_id, year) |>
  dplyr::mutate(
    matchday_last = dplyr::coalesce(dplyr::lead(matchday_first, 1) - 1, Inf),
    matchday_first = ifelse(stint == 0, 0, matchday_first)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(player_id, team_id, year, matchday_first, matchday_last)

player_team <- player_xgoals |>
  dplyr::left_join(game, by = "game_id") |>
  dplyr::group_by(player_id, year = as.integer(season_name)) |>
  dplyr::arrange(date_time_utc) |>
  dplyr::slice(dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::select(player_id, year, team_id)

data <- team_xgoals |>
  dplyr::left_join(game, by = "game_id") |>
  dplyr::mutate(
    year = as.integer(season_name),
    off_id = paste("off", season_name, team_id, sep = "_"),
    is_home = team_id == home_team_id,
    def_id = paste("def", season_name, ifelse(is_home, away_team_id, home_team_id), sep = "_")
  ) |>
  dplyr::select(year, off_id, def_id, is_home, xgoals = xgoals_for)

team_games_per_year <- team_xgoals |>
  dplyr::left_join(game, by = "game_id") |>
  dplyr::count(team_id, year = as.integer(season_name))

unique_column <- with(data, sort(unique(c(off_id, def_id, "is_home"))))

x <- Matrix::sparseMatrix(
  i = c(which(data$is_home), 1:nrow(data), 1:nrow(data)),
  j = match(c(rep("is_home", sum(data$is_home)), data$off_id, data$def_id), unique_column)
)

y <- data$xgoals

fit_init <- glmnet::cv.glmnet(x = x, y = y, alpha = 0, scale = FALSE)

coef_init <- tibble::tibble(
  name = c("intercept", unique_column),
  value = coef(fit_init, s = "lambda.min")[, 1]
)

coef_int_init <- coef_init |>
  dplyr::filter(name == "intercept") |>
  dplyr::select(coef_int = value)

coef_off_init <- coef_init |>
  dplyr::rename(off_id = name, coef_off = value)

coef_def_init <- coef_init |>
  dplyr::rename(def_id = name, coef_def = value)

coef_hfa_init <- coef_init |>
  dplyr::filter(name == "is_home") |>
  dplyr::transmute(is_home = TRUE, coef_hfa = value)



data_with_coef_init <- data |>
  dplyr::cross_join(coef_int_init) |>
  dplyr::left_join(coef_hfa_init, by = "is_home") |>
  dplyr::left_join(coef_off_init, by = "off_id") |>
  dplyr::left_join(coef_def_init, by = "def_id") |>
  tidyr::replace_na(list(coef_hfa = 0))

team_year_off <- data_with_coef_init |>
  dplyr::group_by(team_id = substring(off_id, 10), year, role = "off") |>
  dplyr::summarize(
    n = dplyr::n(),
    x = mean(xgoals - coef_int - coef_hfa - coef_def),
    .groups = "drop"
  )

team_year_def <- data_with_coef_init |>
  dplyr::group_by(team_id = substring(def_id, 10), year, role = "def") |>
  dplyr::summarize(
    n = dplyr::n(),
    x = mean(xgoals - coef_int - coef_hfa - coef_def),
    .groups = "drop"
  )

team_year <- dplyr::bind_rows(team_year_off, team_year_def)



projection_data <- team_year |>
  dplyr::full_join(
    y = dplyr::mutate(team_year, year = year + 1),
    by = c("role", "year", "team_id"),
    suffix = c("", "_1")
  ) |>
  dplyr::left_join(
    y = dplyr::mutate(team_year, year = year + 2),
    by = c("role", "year", "team_id"),
    suffix = c("", "_2")
  ) |>
  dplyr::left_join(
    y = dplyr::mutate(team_year, year = year + 3),
    by = c("role", "year", "team_id"),
    suffix = c("", "_3")
  ) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), \(x) tidyr::replace_na(x, 0)))

fit_marcel <- list()
prior <- projection_data |>
  dplyr::mutate(
    name = paste(role, year, team_id, sep = "_"),
    prior = NA
  ) |>
  dplyr::select(name, prior)

for (role in unique(projection_data$role)) {

  fit_marcel[[role]] <- projection_data |>
    dplyr::filter(role == !!role) |>
    fit_marcel_projection_1()

  prior$prior[substring(prior$name, 1, 3) == role] <- predict(
    object = fit_marcel[[role]],
    newdata = dplyr::filter(projection_data, role == !!role)
  )
}

data_with_prior <- data |>
  dplyr::left_join(
    y = prior |>
      dplyr::filter(substring(name, 1, 3) == "off") |>
      dplyr::rename(prior_off = prior),
    by = c("off_id" = "name")
  ) |>
  dplyr::left_join(
    y = prior |>
      dplyr::filter(substring(name, 1, 3) == "def") |>
      dplyr::rename(prior_def = prior),
    by = c("def_id" = "name")
  ) |>
  dplyr::mutate(y_res = y - prior_off - prior_def)

fit <- glmnet::cv.glmnet(x = x, y = data_with_prior$y_res, alpha = 0, scale = FALSE)

coef <- tibble::tibble(
  name = c("intercept", unique_column),
  value = coef(fit, s = "lambda.min")[, 1]
) |>
  dplyr::left_join(prior, by = "name") |>
  dplyr::mutate(
    role = substring(name, 1, 3),
    year = substring(name, 5, 8),
    team_id = substring(name, 10),
    value = value + dplyr::coalesce(prior, 0)
  )

coef_int <- coef |>
  dplyr::filter(name == "intercept") |>
  dplyr::select(coef_int = value)

coef_hfa <- coef |>
  dplyr::filter(name == "is_home") |>
  dplyr::transmute(coef_hfa = value, is_home = TRUE)

coef_off <- coef |>
  dplyr::filter(role == "off") |>
  dplyr::transmute(role, year = as.integer(year), off_id = team_id, coef_off = value)

coef_def <- coef |>
  dplyr::filter(role == "def") |>
  dplyr::transmute(role, year = as.integer(year), def_id = team_id, coef_def = value)

#coef |>
#  dplyr::left_join(coef_refit, by = "name", suffix = c("", "_refit")) |>
#  dplyr::mutate(
#    role = substring(name, 1, 3),
#    year = substring(name, 5, 8),
#    team_id = substring(name, 10)
#  ) |>
#  dplyr::left_join(team, by = "team_id") |>
#  dplyr::select(role, year, team_short_name, value, value_refit, prior) |>
#  dplyr::arrange(value_refit) |>
#  dplyr::filter(year == 2025, role == "def")

schedule_long <- dplyr::bind_rows(
  schedule |>
    dplyr::mutate(is_home = TRUE) |>
    dplyr::rename(off_id = home_team_id, def_id = away_team_id, goals = home_score) |>
    dplyr::select(year, game_id, gameweek, off_id, def_id, is_home, goals),
  schedule |>
    dplyr::mutate(is_home = FALSE) |>
    dplyr::rename(off_id = away_team_id, def_id = home_team_id, goals = away_score) |>
    dplyr::select(year, game_id, gameweek, off_id, def_id, is_home, goals),
)

pred_xgoals <- schedule_long |>
  dplyr::cross_join(coef_int) |>
  dplyr::left_join(coef_hfa, by = "is_home") |>
  dplyr::left_join(coef_off, by = c("off_id", "year")) |>
  dplyr::left_join(coef_def, by = c("def_id", "year")) |>
  tidyr::replace_na(list(coef_hfa = 0)) |>
  dplyr::mutate(pred_xgoals = coef_int + coef_hfa + coef_off + coef_def) |>
  dplyr::select(year, game_id, gameweek, off_id, def_id, is_home, pred_xgoals, goals)

fit_ordinal <- ordinal::clm(
  formula = goals_factor ~ pred_xgoals,
  data = pred_xgoals |>
    dplyr::mutate(goals_factor = as.factor(goals))
)

prob_goals <- predict(fit_ordinal, newdata = pred_xgoals)$fit

# Goalkeepers and defenders get +4 for shutout, -1 for 2-3 goals, -2 for 4+ goals
# Midfielders get +1 for shutout
team_game_goal_prob <- pred_xgoals |>
  dplyr::mutate(
    prob_goals_0 = prob_goals[, "0"],
    prob_goals_23 = rowSums(prob_goals[, c("2", "3")]),
    prob_goals_456 = rowSums(prob_goals[, c("4", "5", "6")]),
  ) |>
  dplyr::select(game_id, team_id = off_id, dplyr::starts_with("prob_goals"))

# ----

game_log <- player_stint |>
  dplyr::left_join(team_game, by = c("year", "team_id"), relationship = "many-to-many") |>
  dplyr::filter(matchday >= matchday_first, matchday <= matchday_last) |>
  dplyr::select(player_id, game_id, team_id) |>
  dplyr::left_join(player_xgoals, by = c("player_id", "game_id", "team_id")) |>
  dplyr::left_join(game, by = "game_id") |>
  tidyr::replace_na(list(minutes_played = 0, xgoals = 0, xassists = 0)) |>
  dplyr::mutate(
    def_id = ifelse(home_team_id == team_id, away_team_id, home_team_id),
    minutes_ratio = minutes_played / expanded_minutes,
    minutes_per_game = minutes_played,
    xgoals_per_minute = xgoals / minutes_played,
    xassists_per_minute = xassists / minutes_played
  ) |>
  dplyr::left_join(coef_def, by = c("def_id", "year")) |>
  dplyr::select(
    year, player_id, game_id, matchday, minutes = minutes_played, minutes_ratio, coef_def, dplyr::contains("_per_")
  ) |>
  tidyr::pivot_longer(cols = dplyr::contains("_per_"), names_to = "metric", values_to = "x") |>
  dplyr::mutate(
    denominator = sapply(stringr::str_split(metric, pattern = "_"), \(x) x[3]),
    n = dplyr::case_when(
      denominator == "game" ~ 1,
      denominator == "minute" ~ minutes
    ),
    # Undo the effect of SoS adjustment for metrics other than xG and xA
    coef_def = dplyr::case_when(
      metric %in% c("xgoals", "xassists") ~ coef_def,
      TRUE ~ 0
    )
  ) |>
  dplyr::filter(n > 0)


player_season_summary <- game_log |>
  dplyr::group_by(year, player_id, metric) |>
  dplyr::summarize(
    games = dplyr::n(),
    coef_def = weighted.mean(coef_def, w = minutes),
    x = weighted.mean(x, w = n) / exp(coef_def),   # SoS adjustment
    n = sum(n),
    .groups = "drop"
  ) |>
  dplyr::select(year, player_id, games, metric, x, n)

data_projection <- player_season_summary |>
  dplyr::full_join(
    y = dplyr::mutate(player_season_summary, year = year + 1),
    by = c("player_id", "year", "metric"),
    suffix = c("", "_1")
  ) |>
  dplyr::left_join(
    y = dplyr::mutate(player_season_summary, year = year + 2),
    by = c("player_id", "year", "metric"),
    suffix = c("", "_2")
  ) |>
  dplyr::left_join(
    y = dplyr::mutate(player_season_summary, year = year + 3),
    by = c("player_id", "year", "metric"),
    suffix = c("", "_3")
  ) |>
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = \(x) tidyr::replace_na(x, 0)))


fit_marcel_3 <- list()
prior_3 <- data_projection |>
  dplyr::select(player_id, year, metric) |>
  dplyr::mutate(prior = NA)

for (metric in unique(data_projection$metric)) {

  fit_marcel_3[[metric]] <- data_projection |>
    dplyr::filter(metric == !!metric, n > 0, n_1 + n_2 + n_3 > 0) |>
    fit_marcel_projection_3()

  prior_3$prior[data_projection$metric == metric] <- predict(
    object = fit_marcel_3[[metric]],
    newdata = dplyr::filter(data_projection, metric == !!metric)
  )
}

data_glmnet <- game_log |>
  dplyr::left_join(prior_3, by = c("player_id", "year", "metric")) |>
  dplyr::mutate(
    player_year = paste(player_id, year, sep = "_"),
    offset = log(n) + log(prior)
  ) |>
  dplyr::filter(n > 0, year > 2020)

fit_glmnet <- list()
coef_glmnet <- list()

for (metric in unique(data_projection$metric)) {

  data_glmnet_metric <- data_glmnet |>
    dplyr::filter(metric == !!metric)

  unique_player_year <- unique(data_glmnet_metric$player_year)

  fit_glmnet[[metric]] <- data_glmnet_metric |>
    with(
      glmnet::cv.glmnet(
        x = Matrix::sparseMatrix(
          i = 1:nrow(data_glmnet_metric),
          j = match(player_year, unique_player_year)
        ),
        y = n * x,    # for poisson model, we want cumulative, not rate
        family = "poisson",
        offset = with(data_glmnet_metric, log(n) + log(prior)),
        lambda = exp(seq(from = 4, to = -20, length = 100)),
        alpha = 0,
        standardize = FALSE
      )
    )

  coef_glmnet[[metric]] <- tibble::tibble(player_year = unique_player_year) |>
    dplyr::mutate(
      player_id = substring(player_year, 1, 10),
      year = as.integer(substring(player_year, 12)),
      intercept = coef(fit_glmnet[[metric]], s = "lambda.min")[1],
      coef = coef(fit_glmnet[[metric]], s = "lambda.min")[-1],
      metric = metric
    )
}

projection <- do.call(dplyr::bind_rows, args = coef_glmnet) |>
  dplyr::left_join(prior_3, by = c("player_id", "year", "metric")) |>
  dplyr::mutate(proj = exp(intercept + log(prior) + coef)) |>
  dplyr::select(player_id, year, metric, proj)

projection_wide <- projection |>
  tidyr::pivot_wider(names_from = metric, values_from = proj)

# Project minutes ----

player_minutes_ratio_last <- game_log |>
  dplyr::filter(metric == "minutes_per_game") |>
  dplyr::group_by(player_id, year) |>
  dplyr::summarize(minutes_ratio_last = mean(minutes_ratio), .groups = "drop") |>
  dplyr::mutate(year = year + 1)

data_minutes <- game_log |>
  dplyr::filter(metric == "minutes_per_game") |>
  dplyr::left_join(player_minutes_ratio_last, by = c("player_id", "year")) |>
  tidyr::replace_na(list(minutes_ratio_last = 0)) |>
  dplyr::arrange(year, matchday) |>
  dplyr::group_by(player_id) |>
  dplyr::mutate(
    minutes_ratio_1 = dplyr::coalesce(dplyr::lag(minutes_ratio, 1), 0),
    minutes_ratio_2 = dplyr::coalesce(dplyr::lag(minutes_ratio, 2), 0),
    minutes_ratio_3 = dplyr::coalesce(dplyr::lag(minutes_ratio, 3), 0),
    minutes_ratio_4 = dplyr::coalesce(dplyr::lag(minutes_ratio, 4), 0),
    minutes_ratio_5 = dplyr::coalesce(dplyr::lag(minutes_ratio, 5), 0),
  ) |>
  dplyr::ungroup() |>
  dplyr::group_by(player_id, year) |>
  dplyr::mutate(
    games_rest = dplyr::n():1,
    minutes_ratio_rest = (sum(minutes_ratio) - cumsum(minutes_ratio)) / games_rest
  ) |>
  dplyr::ungroup()

fit_minutes_next <- nls(
  formula = minutes_ratio ~ (
    w_rookie * (minutes_ratio_last == 0) * minutes_ratio_rookie +
    w_last * (minutes_ratio_last > 0) * minutes_ratio_last +
    w_recent * (
      minutes_ratio_1 +
      minutes_ratio_2 * d +
      minutes_ratio_3 * d^2 +
      minutes_ratio_4 * d^3 +
      minutes_ratio_5 * d^4
    ) +
    minutes_ratio_anchor
  ) / (
    w_rookie * (minutes_ratio_last == 0) + w_last * (minutes_ratio_last > 0) + w_recent * (1 + d + d^2 + d^3 + d^4) + 1
  ),
  data = data_minutes,
  start = list(w_rookie = 1, w_last = 1, w_recent = 1, d = 1, minutes_ratio_rookie = 0, minutes_ratio_anchor = 0)
)

fit_minutes_rest <- nls(
  formula = minutes_ratio_rest ~ (
    w_rookie * (minutes_ratio_last == 0) * minutes_ratio_rookie +
    w_last * (minutes_ratio_last > 0) * minutes_ratio_last +
    w_recent * (
      minutes_ratio_1 +
      minutes_ratio_2 * d +
      minutes_ratio_3 * d^2 +
      minutes_ratio_4 * d^3 +
      minutes_ratio_5 * d^4
    ) +
    minutes_ratio_anchor
  ) / (
    w_rookie * (minutes_ratio_last == 0) + w_last * (minutes_ratio_last > 0) + w_recent * (1 + d + d^2 + d^3 + d^4) + 1
  ),
  data = data_minutes,
  start = list(w_rookie = 1, w_last = 1, w_recent = 1, d = 1, minutes_ratio_rookie = 0, minutes_ratio_anchor = 0),
  weights = games_rest
)

mean_minutes <- mean(game$expanded_minutes[is.na(game$extra_time)])

data_minutes <- data_minutes |>
  dplyr::mutate(
    pred_minutes_ratio_next = predict(fit_minutes_next, newdata = data_minutes),
    pred_minutes_ratio_rest = predict(fit_minutes_rest, newdata = data_minutes),
    pred_minutes_next = mean_minutes * pred_minutes_ratio_next,
    pred_minutes_rest = mean_minutes * pred_minutes_ratio_rest
  )

fit_minutes_1_next <- glm(
  minutes >= 1 ~ pred_minutes_ratio_next,
  family = binomial(),
  data = data_minutes
)

fit_minutes_60_next <- glm(
  minutes >= 60 ~ pred_minutes_ratio_next,
  family = binomial(),
  data = data_minutes
)

fit_minutes_1_rest <- glm(
  minutes >= 1 ~ pred_minutes_ratio_rest,
  family = binomial(),
  data = data_minutes
)

fit_minutes_60_rest <- glm(
  minutes >= 60 ~ pred_minutes_ratio_rest,
  family = binomial(),
  data = data_minutes
)


proj_minutes <- data_minutes |>
  dplyr::mutate(
    minutes_ratio_5 = minutes_ratio_4,
    minutes_ratio_4 = minutes_ratio_3,
    minutes_ratio_3 = minutes_ratio_2,
    minutes_ratio_2 = minutes_ratio_1,
    minutes_ratio_1 = minutes_ratio
  )

proj_minutes <- proj_minutes |>
  dplyr::mutate(
    pred_minutes_ratio_next = predict(fit_minutes_next, newdata = proj_minutes),
    pred_minutes_ratio_rest = predict(fit_minutes_rest, newdata = proj_minutes),
    pred_minutes_next = mean_minutes * pred_minutes_ratio_next,
    pred_minutes_rest = mean_minutes * pred_minutes_ratio_rest
  )

proj_minutes <- proj_minutes |>
  dplyr::mutate(
    prob_1_minute_next = predict(fit_minutes_1_next, newdata = proj_minutes, type = "response"),
    prob_60_minutes_next = predict(fit_minutes_60_next, newdata = proj_minutes, type = "response"),
    prob_1_minute_rest = predict(fit_minutes_1_rest, newdata = proj_minutes, type = "response"),
    prob_60_minutes_rest = predict(fit_minutes_60_rest, newdata = proj_minutes, type = "response")
  ) |>
  dplyr::group_by(year, player_id) |>
  dplyr::arrange(matchday) |>
  dplyr::slice(dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::select(
    year, player_id,
    pred_minutes_next, prob_1_minute_next, prob_60_minutes_next,
    pred_minutes_rest, prob_1_minute_rest, prob_60_minutes_rest
  )

# ----
 
fantasy <- data.table::fread('data/fantasy_2026.csv', na.strings = "", fill = TRUE) |>
  dplyr::mutate(is_visionary = !is.na(is_visionary)) |>
  dplyr::select(player_id, position, price, is_visionary, my_team)
 
fantasy_points <- projection_wide |>
  dplyr::left_join(proj_minutes, by = c("player_id", "year")) |>
  dplyr::left_join(player_position, by = c("player_id", "year")) |>
  dplyr::left_join(player_team, by = c("player_id", "year")) |>
  dplyr::left_join(coef_def, by = c("team_id" = "def_id", "year")) |>
  dplyr::cross_join(coef_int) |>
  dplyr::cross_join(coef_hfa) |>
  dplyr::left_join(fantasy, by = "player_id", suffix = c("_asa", "_fantasy")) |>
  dplyr::mutate(
    position = dplyr::coalesce(position_fantasy, position_asa),
    pred_xgoals = coef_int + coef_hfa / 2 + coef_def,
    goal0s_per_game = predict(fit_ordinal, newdata = data.frame(pred_xgoals))$fit[, "0"],
    goal23s_per_game = rowSums(predict(fit_ordinal, newdata = data.frame(pred_xgoals))$fit[, c("2", "3")]),
    goal456s_per_game = rowSums(predict(fit_ordinal, newdata = data.frame(pred_xgoals))$fit[, c("4", "5", "6")]),
    points_per_xgoal = dplyr::case_when(
      position %in% c("Goalkeeper", "Defender") ~ 6,
      position %in% c("Midfielder") ~ 5,
      position %in% c("Forward") ~ 4
    ),
    points_per_xassist = 3,
    points_per_goal0 = dplyr::case_when(
      position %in% c("Goalkeeper", "Defender") ~ 4,
      position %in% c("Midfielder") ~ 1,
      position %in% c("Forward") ~ 0
    ),
    points_per_goal23 = ifelse(position %in% c("Goalkeeper", "Defender"), -1, 0),
    points_per_goal456 = ifelse(position %in% c("Goalkeeper", "Defender"), -2, 0),
    xgoals_per_game_rest = pred_minutes_rest * xgoals_per_minute,
    xassists_per_game_rest = pred_minutes_rest * xassists_per_minute,
    # Exclude visionary points from rest-of-season projection. Visionary status too unpredictable.
    fantasy_per_game_rest = 1 * prob_1_minute_rest + 1 * prob_60_minutes_rest +
      points_per_xgoal * xgoals_per_game_rest + points_per_xassist * xassists_per_game_rest +
      points_per_goal0 * goal0s_per_game * prob_60_minutes_rest +
      points_per_goal23 * goal23s_per_game + points_per_goal456 * goal456s_per_game
  )

ranking <- fantasy_points |>
  dplyr::left_join(player, by = "player_id") |>
  dplyr::filter(year == current_year) |>
  dplyr::select(
    player_name,
    position,
    price,
    fantasy = fantasy_per_game_rest,
    vision = is_visionary,
    minutes = pred_minutes_rest,
    xgoals = xgoals_per_game_rest,
    xassists = xassists_per_game_rest,
    shutouts = goal0s_per_game,
    dplyr::everything()
  ) |>
  dplyr::arrange(-fantasy)

upcoming_gameweek <- pred_xgoals |>
  dplyr::filter(year == current_year, gameweek == current_gameweek) |>
  dplyr::select(year, game_id, off_id, def_id)

daily_projection <- fantasy_points |>
  dplyr::select(-coef_def) |>
  dplyr::inner_join(upcoming_gameweek, by = c("year", "team_id" = "off_id"), relationship = "many-to-many") |>
  dplyr::left_join(coef_def, by = c("year", "def_id")) |>
  dplyr::left_join(pred_xgoals, by = c("year", "game_id", "def_id" = "off_id")) |>
  dplyr::left_join(team_game_goal_prob, by = c("game_id", "def_id" = "team_id")) |>
  dplyr::mutate(
    xgoals_per_game_next = pred_minutes_next * xgoals_per_minute * exp(coef_def),
    xassists_per_game_next = pred_minutes_next * xassists_per_minute * exp(coef_def),
    goal0s_per_game = prob_goals_0,
    goal23s_per_game = prob_goals_23,
    goal456s_per_game = prob_goals_456,
    fantasy_per_game = 1 * prob_1_minute_next + 1 * prob_60_minutes_next +
      points_per_xgoal * xgoals_per_game_next + points_per_xassist * xassists_per_game_next +
      points_per_goal0 * goal0s_per_game * prob_60_minutes_next +
      points_per_goal23 * goal23s_per_game + points_per_goal456 * goal456s_per_game,
    prob_points = (points_per_xgoal > 0) * (1 - ppois(lambda = xgoals_per_game_next, q = 0)) +
      (points_per_xassist > 0) * (1 - ppois(lambda = xassists_per_game_next, q = 0)) +
      (points_per_goal0 > 0) * goal0s_per_game * prob_60_minutes_next
  ) |>
  dplyr::left_join(player, by = "player_id") |>
  dplyr::left_join(team, by = c("def_id" = "team_id")) |>
  dplyr::left_join(team, by = "team_id", suffix = c("_def", "_off")) |>
  dplyr::group_by(
    player_name, position, price, team = team_abbreviation_off, my_team, vision = is_visionary
  ) |>
  dplyr::summarize(
    opp = paste(team_abbreviation_def, collapse = "/"),
    minutes = sum(pred_minutes_next),
    fantasy = sum(fantasy_per_game) + (1 - prod(1 - prob_points)) * dplyr::coalesce(mean(is_visionary), 0) * 3,
    .groups = "drop"
  ) |>
  dplyr::arrange(-fantasy)

# ----

ranking |>
  dplyr::filter(my_team) |>
  dplyr::arrange(position)

ranking |>
  dplyr::filter(position == "Defender")

daily_projection |>
  dplyr::filter(my_team)

daily_projection |>
  dplyr::filter(is.na(my_team))


schedule_long |>
  dplyr::filter(year == current_year) |>
  dplyr::count(off_id, gameweek) |>
  dplyr::left_join(team, by = c("off_id" = "team_id")) |>
  dplyr::arrange(gameweek, team_name) |>
  dplyr::select(team_name, gameweek, n)

team |>
  dplyr::left_join(coef_off, by = c("team_id" = "off_id")) |>
  dplyr::left_join(coef_def, by = c("team_id" = "def_id", "year")) |>
  dplyr::mutate(coef = coef_off - coef_def) |>
  dplyr::arrange(-coef) |>
  dplyr::select(year, team_short_name, coef_off, coef_def, coef) |>
  dplyr::filter(year == current_year)
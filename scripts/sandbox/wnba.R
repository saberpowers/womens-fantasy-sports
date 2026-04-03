
fit_marcel_projection <- function(data, ...) {
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

header_list <- list("players" = list("limit" = 1500))
endpoint <- "https://lm-api-reads.fantasy.espn.com/apis/v3/games/wfba/seasons/2025/players"
params <- list(scoringPeriodId = 0, view = "players_wl")
athlete_current_team <- httr2::request(endpoint) |>
  httr2::req_url_query(!!!params) |>
  httr2::req_headers(`x-fantasy-filter` = jsonlite::toJSON(header_list)) |>
  httr2::req_perform() |>
  httr2::resp_body_string() |>
  jsonlite::fromJSON() |>
  dplyr::select(athlete_id = id, current_team_id = proTeamId)

#swid <- ini::read.ini("credentials.ini")$espn$swid
#espn_s2 <- ini::read.ini("credentials.ini")$espn$espn_s2
#endpoint <- "https://lm-api-reads.fantasy.espn.com/apis/v3/games/wfba/seasons/2025/segments/0/leagues/1520392489"
#temp <- httr2::request(endpoint) |>
#  httr2::req_perform()  # 401 unauthorized
#  httr2::req_cookies_set(SWID = swid, espn_s2 = espn_s2) |>

box_score <- wehoop::load_wnba_player_box(seasons = 2003:2025)

athlete <- box_score |>
  dplyr::mutate(
    athlete_id = as.integer(athlete_id),
    athlete_position_abbreviation = ifelse(
      test = athlete_position_abbreviation == "NA",
      yes = NA,
      no = athlete_position_abbreviation
    ),
    athlete_position_name = ifelse(
      test = athlete_position_name == "Not Available",
      yes = NA,
      no = athlete_position_name
    )
  ) |>
  dplyr::distinct(athlete_id, athlete_display_name, athlete_position_abbreviation, athlete_position_name) |>
  dplyr::group_by(athlete_id) |>
  dplyr::slice(dplyr::n()) |>
  dplyr::left_join(athlete_current_team, by = "athlete_id")

game_log <- box_score |>
  dplyr::filter(!is.na(minutes)) |>
  dplyr::mutate(
    athlete_id = as.integer(athlete_id),
    year = lubridate::year(game_date),
    shots = field_goals_attempted,
    shot1s = free_throws_attempted,
    shot2s = field_goals_attempted - three_point_field_goals_attempted,
    shot3s = three_point_field_goals_attempted,
    minutes_per_game = minutes,
    rebounds_per_minute = rebounds / minutes,
    assists_per_minute = assists / minutes,
    steals_per_minute = steals / minutes,
    blocks_per_minute = blocks / minutes,
    shots_per_minute = shots / minutes,
    shot1s_per_shot = shot1s / shots,
    shot2s_per_shot = shot2s / shots,
    shot3s_per_shot = shot3s / shots,
    made1s_per_shot1 = free_throws_made / shot1s,
    made2s_per_shot2 = (field_goals_made - three_point_field_goals_made) / shot2s,
    made3s_per_shot3 = three_point_field_goals_made / shot3s
  ) |>
  dplyr::select(
    year, game_date, athlete_id, opponent_team_id, minutes, shots, shot1s, shot2s, shot3s,
    dplyr::contains("_per_")
  ) |>
  tidyr::pivot_longer(cols = dplyr::contains("_per_"), names_to = "metric", values_to = "x") |>
  dplyr::mutate(
    denominator = sapply(stringr::str_split(metric, pattern = "_"), \(x) x[3]),
    n = dplyr::case_when(
      denominator == "game" ~ 1,
      denominator == "minute" ~ minutes,
      denominator == "shot" ~ shots,
      denominator == "shot1" ~ shot1s,
      denominator == "shot2" ~ shot2s,
      denominator == "shot3" ~ shot3s
    )
  ) |>
  dplyr::filter(n > 0)

#data <- game_log |>
#  dplyr::filter(year == 2024, name == "made3s_per_shot3") |>
#  dplyr::select(year, athlete_id, opponent_team_id, name, value, weight) |>
#  dplyr::mutate(athlete_id = as.factor(athlete_id), opponent_team_id = as.factor(opponent_team_id))
#
#matrix_athlete <- Matrix::sparseMatrix(1:nrow(data), as.numeric(data$athlete_id))
#matrix_team <- Matrix::sparseMatrix(1:nrow(data), as.numeric(data$opponent_team_id))
#matrix <- cbind(matrix_athlete, matrix_team)
#
#fit <- glmnet::cv.glmnet(
#  x = matrix,
#  y = data$value,
#  weights = data$weight,
#  alpha = 0,
#  standardize = FALSE,
#  lambda = exp(seq(from = -20, to = 0, length = 100))
#)
#
#coef <- tibble::tibble(
#  name = c(
#    "intercept_0",
#    paste0("athlete_", levels(data$athlete_id)),
#    paste0("team_", levels(data$opponent_team_id))
#  ),
#  coef = coef(fit, s = "lambda.min")[, 1]
#) |>
#  dplyr::mutate(
#    type = sapply(stringr::str_split(name, pattern = "_"), \(x) x[[1]]),
#    entity_id = as.integer(sapply(stringr::str_split(name, pattern = "_"), \(x) x[[2]]))
#  ) |>
#  dplyr::select(type, entity_id, coef)





athlete_season_summary <- game_log |>
  dplyr::group_by(year, athlete_id, metric) |>
  dplyr::summarize(
    games = dplyr::n(),
    x = weighted.mean(x, w = n),
    n = sum(n),
    .groups = "drop"
  )

projection_data <- athlete_season_summary |>
  dplyr::full_join(
    y = dplyr::mutate(athlete_season_summary, year = year + 1),
    by = c("athlete_id", "year", "metric"),
    suffix = c("", "_1")
  ) |>
  dplyr::left_join(
    y = dplyr::mutate(athlete_season_summary, year = year + 2),
    by = c("athlete_id", "year", "metric"),
    suffix = c("", "_2")
  ) |>
  dplyr::left_join(
    y = dplyr::mutate(athlete_season_summary, year = year + 3),
    by = c("athlete_id", "year", "metric"),
    suffix = c("", "_3")
  ) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), \(x) replace(x, is.na(x), 0)))

fit_marcel <- list()
prior <- projection_data |>
  dplyr::select(athlete_id, year, metric) |>
  dplyr::mutate(prior = NA)

for (metric in unique(projection_data$metric)) {

  fit_marcel[[metric]] <- projection_data |>
    dplyr::filter(metric == !!metric, n > 0, n_1 + n_2 + n_3 > 0) |>
    fit_marcel_projection()

  prior$prior[projection_data$metric == metric] <- predict(
    object = fit_marcel[[metric]],
    newdata = dplyr::filter(projection_data, metric == !!metric)
  )
}

data_lmm <- game_log |>
  dplyr::left_join(prior, by = c("athlete_id", "year", "metric")) |>
  dplyr::mutate(
    athlete_year = paste(athlete_id, year, sep = "_"),
    x = x - prior
  ) |>
  dplyr::filter(n > 0, year > 2020)

fit_lmm <- list()

for (metric in unique(projection_data$metric)) {

  fit_lmm[[metric]] <- lme4::lmer(
    formula = x ~ (1 | athlete_year),
    data = data_lmm |>
      dplyr::filter(metric == !!metric),
    weights = n
  )
}

var_decomposition <- fit_lmm |>
  sapply(\(x) as.data.frame(lme4::VarCorr(x))$vcov) |>
  t() |>
  as.data.frame() |>
  tibble::rownames_to_column("metric") |>
  tibble::as_tibble() |>
  dplyr::rename(
    var_signal = V1,
    var_noise = V2
  )

projection <- projection_data |>
  dplyr::left_join(prior, by = c("athlete_id", "year", "metric")) |>
  dplyr::left_join(var_decomposition, by = "metric") |>
  dplyr::mutate(
    proj = (prior / var_signal + n * x / var_noise) / (1 / var_signal + n / var_noise)
  ) |>
  dplyr::select(athlete_id, year, metric, proj)

projection_wide <- projection |>
  tidyr::pivot_wider(names_from = metric, values_from = proj) |>
  dplyr::mutate(
    shots_per_game = shots_per_minute * minutes_per_game,
    made1s_per_game = made1s_per_shot1 * shot1s_per_shot * shots_per_game,
    made2s_per_game = made2s_per_shot2 * shot2s_per_shot * shots_per_game,
    made3s_per_game = dplyr::coalesce(made3s_per_shot3 * shot3s_per_shot * shots_per_game, 0),
    rebounds_per_game = minutes_per_game * rebounds_per_minute,
    assists_per_game = minutes_per_game * assists_per_minute,
    steals_per_game = minutes_per_game * steals_per_minute,
    blocks_per_game = minutes_per_game * blocks_per_minute,
    fantasy_per_game = made1s_per_game + 2 * made2s_per_game + 4 * made3s_per_game +
      rebounds_per_game + assists_per_game + 2 * steals_per_game + 2 * blocks_per_game
  )

manual_adjustment <- tibble::tribble(
  ~athlete_id, ~fpg_manual,
  4433730, 30.7,  # Paige Bueckers
  4898384, 25.0,  # Kiki Iriafen
  585, 0, # Diana Taurasi
  4280892, 0, # Chennedy Carter
  3142250, 0, # Jordin Canada
  2529458, 0, # Cheyenne Parker-Tyus
)

ranking <- projection_wide |>
  dplyr::filter(year == 2025) |>
  dplyr::arrange(-fantasy_per_game) |>
  dplyr::left_join(athlete, by = "athlete_id") |>
  dplyr::mutate(rank = 1:dplyr::n()) |>
  dplyr::select(rank, athlete_display_name, fantasy_per_game, dplyr::everything())

ranking |>
  print(n = 99)

ranking |>
  ggplot2::ggplot(ggplot2::aes(x = fantasy_per_game)) +
  ggplot2::geom_density(color = sputil::color("blue"), adjust = 0.3) +
  sputil::theme_sleek() +
  ggplot2::geom_vline(color = sputil::color("blue"), xintercept = c(18.2, 22.4, 26.3))

# Checks the stay-in-the-pen WAR baseline (linear, flexible, and matched controls) behind the conversion gains
# Run after value_analysis.R
# Output: results/validation_value.txt

library(dplyr)
library(readr)
library(splines)

role_seasons <- read_csv("data/fg_role_seasons.csv", guess_max = 20000, show_col_types = FALSE) |>
  mutate(IP = floor(IP) + (IP %% 1) * 10 / 3)

role_pace <- role_seasons |> group_by(Season, role) |> summarize(war_per_ip = sum(WAR, na.rm = TRUE) / sum(IP,
    na.rm = TRUE), .groups = "drop")
league <- role_seasons |> filter(!is.na(SIERA)) |> group_by(Season) |> summarize(lg_SIERA = weighted.mean(SIERA,
    TBF), .groups = "drop") |>
  left_join(role_seasons |> group_by(Season) |> summarize(runs_per_win = sum(RAR, na.rm = TRUE) / sum(WAR,
      na.rm = TRUE)), by = "Season")
league_rp <- role_seasons |> filter(role == "rel") |> group_by(Season) |>
    summarize(lg_rp_siera = weighted.mean(SIERA, TBF, na.rm = TRUE))
league_sp <- role_seasons |> filter(role == "sta") |> group_by(Season) |>
    summarize(lg_sp_siera = weighted.mean(SIERA, TBF, na.rm = TRUE))

fg_season <- role_seasons |>
  left_join(role_pace, by = c("Season", "role")) |>
  mutate(WAA_fg_row = WAR - war_per_ip * IP) |>
  group_by(playerid, Season) |>
  summarize(SIERA = weighted.mean(SIERA, TBF, na.rm = TRUE), xMLBAMID = first(xMLBAMID), Age = first(Age),
      G = sum(G), GS = sum(GS),
            IP = sum(IP), TBF = sum(TBF), WAR = sum(WAR), WAA_fg = sum(WAA_fg_row), .groups = "drop") |>
  left_join(league, by = "Season") |>
  mutate(skill_wins = (lg_SIERA - SIERA) * IP / 9 / runs_per_win)
relief_info <- role_seasons |>
  filter(role == "rel") |>
  left_join(league_rp, by = "Season") |>
  transmute(playerid, Season, rp_SIERA_vs_lg = SIERA - lg_rp_siera, rp_gmLI = gmLI)

# Stayers, same definition as waa_analysis.R
next_year <- fg_season |> transmute(playerid, Season, next_G = G, next_GS = GS, next_IP = IP, next_WAR = WAR,
    next_WAA_fg = WAA_fg)
stayers <- fg_season |>
  filter(GS / G <= 0.2, TBF >= 100) |>
  left_join(relief_info, by = c("playerid", "Season")) |>
  mutate(next_season = Season + 1) |>
  inner_join(next_year, by = c("playerid", "next_season" = "Season")) |>
  filter(next_GS / next_G <= 0.2, next_IP >= 20) |>
  left_join(league_sp, by = "Season") |>
  mutate(cushion = lg_sp_siera - SIERA, rp_age = Age) |>
  filter(!is.na(rp_SIERA_vs_lg), !is.na(rp_gmLI))

attempts <- read_csv("data/value_attempts.csv", show_col_types = FALSE) |>
  select(playerid, PlayerName, rp_season, sp_season, attempt, rp_StuffPlus, Cushion) |>
  inner_join(fg_season |> left_join(relief_info, by = c("playerid", "Season")) |>
               transmute(playerid, rp_season = Season, rp_SIERA_vs_lg, rp_gmLI, rp_age = Age, N_IP = IP,
                 N_WAR = WAR, N_WAA_fg = WAA_fg),
             by = c("playerid", "rp_season")) |>
  inner_join(fg_season |> transmute(playerid, sp_season = Season, sp_WAR = WAR, sp_WAA_fg = WAA_fg),
      by = c("playerid", "sp_season")) |>
  filter(!is.na(rp_SIERA_vs_lg), !is.na(rp_gmLI)) |>
  mutate(CushionGroup = case_when(Cushion < 0.5 ~ "cushion under 0.5", Cushion < 1 ~ "cushion 0.5 to 1",
      TRUE ~ "cushion 1+"),
         StuffGroup = case_when(is.na(rp_StuffPlus) ~ NA_character_, rp_StuffPlus >= 100 ~ "Stuff+ 100+",
           TRUE ~ "Stuff+ under 100"))

sink("results/validation_value.txt")
cat("Stayers:", nrow(stayers), "reliever seasons followed by another relief season of 20+ IP. Attempts:",
    nrow(attempts), "\n\n")

# 1. Calibration of the pen baseline
cat("1. Calibration of the stay-in-the-pen baseline for next season WAR. Cross-fitted by pitcher (10 folds) so every stayer is predicted by a model that never saw him.\n")
set.seed(0)
pitcher_ids <- unique(stayers$playerid)
fold_of <- sample(rep(1:10, length.out = length(pitcher_ids)))
stayers$fold <- fold_of[match(stayers$playerid, pitcher_ids)]
stayers$pred_linear <- NA
stayers$pred_flex <- NA
for (k in 1:10) {
  train <- stayers[stayers$fold != k, ]
  test <- stayers[stayers$fold == k, ]
  fit_lin <- lm(next_WAR ~ WAR + rp_SIERA_vs_lg + IP + rp_age, data = train)
  fit_flex <- lm(next_WAR ~ ns(WAR, 3) + ns(rp_SIERA_vs_lg, 3) + ns(IP, 3) + ns(rp_age, 2) + ns(rp_gmLI, 2),
      data = train)
  stayers$pred_linear[stayers$fold == k] <- predict(fit_lin, newdata = test)
  stayers$pred_flex[stayers$fold == k] <- predict(fit_flex, newdata = test)
}
cat("Cross-fitted RMSE of next season WAR: linear", round(sqrt(mean((stayers$next_WAR - stayers$pred_linear)^2)),
    3), " flexible", round(sqrt(mean((stayers$next_WAR - stayers$pred_flex)^2)), 3),
    " no model", round(sd(stayers$next_WAR), 3), "\n")
cat("Cross-fitted R^2: linear", round(1 - sum((stayers$next_WAR - stayers$pred_linear)^2) / sum((stayers$next_WAR - mean(stayers$next_WAR))^2), 3),
    " flexible", round(1 - sum((stayers$next_WAR - stayers$pred_flex)^2) / sum((stayers$next_WAR - mean(stayers$next_WAR))^2), 3), "\n\n")
cat("Actual minus predicted next season WAR by tenth of the linear prediction (a calibrated baseline is near 0 in every tenth, and the top tenth is where the elite relievers sit)\n")
calib <- stayers |> mutate(bin = ntile(pred_linear, 10)) |>
  group_by(bin) |>
  summarize(n = n(), mean_predicted = round(mean(pred_linear), 2), mean_actual = round(mean(next_WAR), 2),
            bias_linear = round(mean(next_WAR - pred_linear), 3), bias_flex = round(mean(next_WAR - pred_flex), 3),
            se_bias = round(sd(next_WAR - pred_linear) / sqrt(n()), 3), .groups = "drop")
print(as.data.frame(calib), row.names = FALSE)
cat("\nBias by cushion group among stayers (these are the groups the archetype results use)\n")
print(as.data.frame(stayers |>
                      mutate(group = case_when(cushion < 0.5 ~ "cushion under 0.5",
                        cushion < 1 ~ "cushion 0.5 to 1", TRUE ~ "cushion 1+")) |>
                      group_by(group) |>
                      summarize(n = n(), bias_linear = round(mean(next_WAR - pred_linear), 3),
                        bias_flex = round(mean(next_WAR - pred_flex), 3),
                                se = round(sd(next_WAR - pred_linear) / sqrt(n()), 3), .groups = "drop")),
                                  row.names = FALSE)

# Matched control baseline: for each attempt, the average next season WAR of the 25 closest stayers (other
# pitchers) on this season WAR, SIERA, IP, age, leverage
match_gain <- function(data, outcome_next) {
  scale_cols <- c("WAR", "rp_SIERA_vs_lg", "IP", "rp_age", "rp_gmLI")
  mu <- sapply(stayers[, scale_cols], mean)
  sdv <- sapply(stayers[, scale_cols], sd)
  z_stay <- sweep(sweep(as.matrix(stayers[, scale_cols]), 2, mu), 2, sdv, "/")
  attempts_for_match <- data |> transmute(WAR = N_WAR, rp_SIERA_vs_lg, IP = N_IP, rp_age, rp_gmLI)
  z_att <- sweep(sweep(as.matrix(attempts_for_match[, scale_cols]), 2, mu), 2, sdv, "/")
  control <- rep(NA, nrow(data))
  for (i in 1:nrow(data)) {
    d <- sqrt(rowSums((z_stay - matrix(z_att[i, ], nrow(z_stay), length(scale_cols), byrow = TRUE))^2))
    d[stayers$playerid == data$playerid[i]] <- Inf
    control[i] <- mean(stayers[[outcome_next]][order(d)[1:25]])
  }
  control
}
attempts$control_WAR <- match_gain(attempts, "next_WAR")
fit_lin_all <- lm(next_WAR ~ WAR + rp_SIERA_vs_lg + IP + rp_age, data = stayers)
fit_flex_all <- lm(next_WAR ~ ns(WAR, 3) + ns(rp_SIERA_vs_lg, 3) + ns(IP, 3) + ns(rp_age, 2) + ns(rp_gmLI, 2),
    data = stayers)
new_att <- attempts |> mutate(WAR = N_WAR, IP = N_IP)
attempts$pen_linear <- predict(fit_lin_all, newdata = new_att)
attempts$pen_flex <- predict(fit_flex_all, newdata = new_att)
attempts <- attempts |> mutate(gain_linear = sp_WAR - pen_linear, gain_flex = sp_WAR - pen_flex,
    gain_matched = sp_WAR - control_WAR)

cat("\nConversion WAR gain under three baselines (mean, standard error). Linear is the one in value_analysis.R.\n")
gain_summary <- function(data, label) {
  data.frame(group = label, n = nrow(data),
             linear = sprintf("%.2f (%.2f)", mean(data$gain_linear), sd(data$gain_linear) / sqrt(nrow(data))),
             flexible = sprintf("%.2f (%.2f)", mean(data$gain_flex), sd(data$gain_flex) / sqrt(nrow(data))),
             matched_controls = sprintf("%.2f (%.2f)", mean(data$gain_matched),
               sd(data$gain_matched) / sqrt(nrow(data))))
}
gains <- bind_rows(gain_summary(attempts, "all 114 attempts"), gain_summary(attempts |>
    filter(attempt == "5+ starts"), "5+ starts"),
                   gain_summary(attempts |> filter(attempt == "1 to 4 starts"), "1 to 4 starts"),
                   gain_summary(attempts |> filter(CushionGroup == "cushion under 0.5"), "cushion under 0.5"),
                   gain_summary(attempts |> filter(CushionGroup == "cushion 0.5 to 1"), "cushion 0.5 to 1"),
                   gain_summary(attempts |> filter(CushionGroup == "cushion 1+"), "cushion 1+"),
                   gain_summary(attempts |> filter(StuffGroup == "Stuff+ 100+"), "Stuff+ 100+"),
                   gain_summary(attempts |> filter(StuffGroup == "Stuff+ under 100"), "Stuff+ under 100"))
print(gains, row.names = FALSE)
cat("Correlation of the attempt level gains: linear vs flexible", round(cor(attempts$gain_linear,
    attempts$gain_flex), 3), " linear vs matched", round(cor(attempts$gain_linear, attempts$gain_matched), 3),
      "\n")

# The matched control method should give a gain of about 0 for relievers who stayed
stayer_z <- scale(stayers[, c("WAR", "rp_SIERA_vs_lg", "IP", "rp_age", "rp_gmLI")])
placebo_gain <- rep(NA, nrow(stayers))
for (i in 1:nrow(stayers)) {
  distance <- sqrt(colSums((t(stayer_z) - stayer_z[i, ])^2))
  distance[stayers$playerid == stayers$playerid[i]] <- Inf
  placebo_gain[i] <- stayers$next_WAR[i] - mean(stayers$next_WAR[order(distance)[1:25]])
}
cat("Placebo (all", nrow(stayers), "stayers against their matched controls): mean gain",
    round(mean(placebo_gain), 3), " standard error", round(sd(placebo_gain) / sqrt(nrow(stayers)), 3), "\n")
sink()
cat(readLines("results/validation_value.txt"), sep = "\n")

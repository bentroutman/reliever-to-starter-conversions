# Projects relievers as starters using swingmen (both roles in one season) who are not in the conversion sample
# Run after build_sample.R
# Outputs: results/swingman_proxy.txt, results/swingman_projection_2026.csv, and charts/proxy_vs_actual.png

library(dplyr)
library(readr)
library(ggplot2)

role_seasons <- read_csv("data/fg_role_seasons.csv", guess_max = 20000, show_col_types = FALSE) |>
  mutate(IP = floor(IP) + (IP %% 1) * 10 / 3)
conversions <- read_csv("data/conversions.csv", show_col_types = FALSE)

# Pitch groups, same as conversion_changes.R
for (column in c("pfxFA%", "pfxSI%", "pfxFC%", "pfxSL%", "pfxCU%", "pfxKC%", "pfxCV%", "pfxCH%", "pfxFS%",
                 "pfxFO%", "pfxSC%", "pfxST%")) {
  role_seasons[[column]][is.na(role_seasons[[column]])] <- 0
}
role_seasons <- role_seasons |>
  mutate(FourSeam = `pfxFA%`, Sinker = `pfxSI%`, Cutter = `pfxFC%`, Slider = `pfxSL%` + `pfxST%`,
         Curve = `pfxCU%` + `pfxKC%` + `pfxCV%`, Changeup = `pfxCH%` + `pfxFS%` + `pfxFO%` + `pfxSC%`,
         NumPitches10 = (FourSeam >= 0.1) + (Sinker >= 0.1) + (Cutter >= 0.1) + (Slider >= 0.1) +
           (Curve >= 0.1) + (Changeup >= 0.1),
         FBVelo = ifelse(Sinker > FourSeam, pfxvSI, pfxvFA),
         FB_Stuff = ifelse(Sinker > FourSeam, sp_s_SI, sp_s_FF))

league_sp <- role_seasons |>
  filter(role == "sta") |>
  group_by(Season) |>
  summarize(lg_sp_siera = weighted.mean(SIERA, TBF, na.rm = TRUE))
league_rp <- role_seasons |>
  filter(role == "rel") |>
  group_by(Season) |>
  summarize(lg_rp_siera = weighted.mean(SIERA, TBF, na.rm = TRUE))

rows <- role_seasons |>
  left_join(league_sp, by = "Season") |>
  left_join(league_rp, by = "Season") |>
  transmute(playerid, PlayerName, Season, role, Age, ThrowsL = Throws == "L", G, GS, IP, TBF, SIERA,
            SIERA_vs_lg = ifelse(role == "rel", SIERA - lg_rp_siera, SIERA - lg_sp_siera),
            KBB = `K-BB%`, SwStr = `SwStr%`, FStrike = `F-Strike%`, GB = `GB%`, gmLI,
            FBVelo, FB_Stuff, StuffPlus = sp_stuff, LocPlus = sp_location, NumPitches10)

# Relief rows get the prefix rp_ and starter rows get sp_
relief_rows <- rows |> filter(role == "rel") |> select(-role)
names(relief_rows)[-(1:3)] <- paste0("rp_", names(relief_rows)[-(1:3)])
starter_rows <- rows |> filter(role == "sta") |> select(-role)
names(starter_rows)[-(1:3)] <- paste0("sp_", names(starter_rows)[-(1:3)])

# Swingmen: both roles in the same season, real starts (not openers) and real relief work
swingmen <- inner_join(relief_rows |> select(-PlayerName), starter_rows, by = c("playerid", "Season")) |>
  filter(rp_TBF >= 50, sp_TBF >= 50, sp_GS >= 3, sp_IP / sp_GS >= 3)

# Conversions: relief season row and starter season row, from the same files
converts <- conversions |>
  select(playerid, rp_season, sp_season) |>
  inner_join(relief_rows |> select(-PlayerName) |> rename(rp_season = Season), by = c("playerid", "rp_season")) |>
  inner_join(starter_rows |> rename(sp_season = Season), by = c("playerid", "sp_season"))
convert_ids <- unique(conversions$playerid)

# Keep every pitcher who is in the conversion sample out of the fit so the test is clean
swing_fit <- swingmen |> filter(!playerid %in% convert_ids)

change_measures <- c("SIERA", "KBB", "SwStr", "FStrike", "GB", "FBVelo", "StuffPlus", "LocPlus")

auc <- function(score, outcome) {
  wins <- 0
  for (s in score[outcome == 1]) {
    wins <- wins + sum(s > score[outcome == 0]) + 0.5 * sum(s == score[outcome == 0])
  }
  wins / (sum(outcome == 1) * sum(outcome == 0))
}

sink("results/swingman_proxy.txt")
cat("Swingmen (both roles in one season, 3+ starts at 3+ IP per start, 50+ batters faced each way):",
    nrow(swingmen), "pitcher seasons\n")
cat("Pitchers:", n_distinct(swingmen$playerid),
    " Seasons 2014 to 2026.  Used for fitting (not in the conversion sample):",
    nrow(swing_fit), "pitcher seasons from", n_distinct(swing_fit$playerid), "pitchers\n")
cat("Average swingman: starter IP", round(mean(swingmen$sp_IP), 1), " relief IP", round(mean(swingmen$rp_IP), 1),
    "\n\n")

cat("1. Role effect (starter line minus relief line), swingmen vs conversions\n")
effect_table <- NULL
for (m in change_measures) {
  swing_change <- swingmen[[paste0("sp_", m)]] - swingmen[[paste0("rp_", m)]]
  convert_change <- converts[[paste0("sp_", m)]] - converts[[paste0("rp_", m)]]
  effect_table <- bind_rows(effect_table, data.frame(
    measure = m, swingmen_n = sum(!is.na(swing_change)), swingmen_change = mean(swing_change, na.rm = TRUE),
    converts_n = sum(!is.na(convert_change)), converts_change = mean(convert_change, na.rm = TRUE)))
}
print(effect_table, digits = 3)

# Role effect on SIERA by relief traits, swingmen only
cat("\nSwingman SIERA change (starter minus relief) by relief traits\n")
swingmen |>
  mutate(Velo = ifelse(rp_FBVelo >= 95, "95+ mph", "under 95"),
         Relief = ifelse(rp_SIERA_vs_lg < 0, "better than avg reliever", "worse than avg reliever"),
         Hand = ifelse(rp_ThrowsL, "lefty", "righty")) |>
  tidyr::pivot_longer(c(Velo, Relief, Hand), names_to = "trait", values_to = "group") |>
  filter(!is.na(group)) |>
  group_by(trait, group) |>
  summarize(n = n(), SIERA_change = mean(sp_SIERA - rp_SIERA), FBVelo_change = mean(sp_FBVelo - rp_FBVelo,
      na.rm = TRUE),
            sp_vs_lg = mean(sp_SIERA_vs_lg), .groups = "drop") |>
  as.data.frame() |> print(digits = 3)

# Fit the proxy on swingmen: starter SIERA vs league from the relief line
fit_simple <- lm(sp_SIERA_vs_lg ~ rp_SIERA_vs_lg, data = swing_fit)
fit_full <- lm(sp_SIERA_vs_lg ~ rp_SIERA_vs_lg + rp_KBB + rp_SwStr + rp_FBVelo + rp_Age + rp_NumPitches10 + rp_ThrowsL,
               data = swing_fit)
swing_stuff <- swing_fit |> filter(!is.na(rp_StuffPlus), !is.na(rp_FB_Stuff))
fit_stuff <- lm(sp_SIERA_vs_lg ~ rp_SIERA_vs_lg + rp_StuffPlus + rp_FB_Stuff + rp_FBVelo + rp_Age + rp_ThrowsL,
                data = swing_stuff)
cat("\n2. Proxy models fit on swingmen. Starter SIERA vs league =\n")
cat("simple: ", round(coef(fit_simple)[1], 2), " + ", round(coef(fit_simple)[2], 2),
    " x relief SIERA vs league  (n ",
    nrow(swing_fit), ", R2 ", round(summary(fit_simple)$r.squared, 3), ")\n", sep = "")
cat("full model R2:", round(summary(fit_full)$r.squared, 3), " n =", nrow(swing_fit), "\n")
print(round(summary(fit_full)$coefficients, 3))
cat("Stuff+ model R2:", round(summary(fit_stuff)$r.squared, 3), " n =", nrow(swing_stuff), "\n")
print(round(summary(fit_stuff)$coefficients, 3))

# Test on the conversions using only the relief season
test <- converts |>
  filter(!is.na(rp_FBVelo), !is.na(rp_KBB), !is.na(rp_SwStr), !is.na(rp_NumPitches10), !is.na(sp_SIERA_vs_lg))
test$pred_simple <- predict(fit_simple, newdata = test)
test$pred_full <- predict(fit_full, newdata = test)
test$Success <- as.numeric(test$sp_SIERA_vs_lg <= 0)

cat("\n3. Test on the", nrow(test), "conversions (relief season data only, swingmen fit)\n")
scores <- function(label, pred, actual, success) {
  data.frame(model = label, n = length(pred), RMSE = sqrt(mean((actual - pred)^2)),
             bias_actual_minus_pred = mean(actual - pred), spearman = cor(pred, actual, method = "spearman"),
             AUC_success = auc(-pred, success))
}
score_table <- bind_rows(
  scores("no model: average swingman starter", rep(mean(swing_fit$sp_SIERA_vs_lg), nrow(test)),
      test$sp_SIERA_vs_lg, test$Success),
  scores("best relievers: relief SIERA only, swingman fit", test$pred_simple, test$sp_SIERA_vs_lg, test$Success),
  scores("full swingman proxy", test$pred_full, test$sp_SIERA_vs_lg, test$Success))
cat("RMSE if we had known the conversions' own mean (best possible constant):",
    round(sd(test$sp_SIERA_vs_lg) * sqrt((nrow(test) - 1) / nrow(test)), 3), "\n")
print(score_table, digits = 3)

# Stuff+ version on the 2020 and later conversions
test_stuff <- converts |>
  filter(!is.na(rp_StuffPlus), !is.na(rp_FB_Stuff), !is.na(rp_FBVelo), !is.na(sp_SIERA_vs_lg))
test_stuff$pred_simple <- predict(fit_simple, newdata = test_stuff)
test_stuff$pred_stuff <- predict(fit_stuff, newdata = test_stuff)
test_stuff$Success <- as.numeric(test_stuff$sp_SIERA_vs_lg <= 0)
cat("\nStuff+ version, conversions from 2020 on (n =", nrow(test_stuff), ")\n")
print(bind_rows(
  scores("no model: average swingman starter", rep(mean(swing_stuff$sp_SIERA_vs_lg), nrow(test_stuff)),
      test_stuff$sp_SIERA_vs_lg, test_stuff$Success),
  scores("best relievers: relief SIERA only", test_stuff$pred_simple, test_stuff$sp_SIERA_vs_lg,
      test_stuff$Success),
  scores("swingman proxy with Stuff+", test_stuff$pred_stuff, test_stuff$sp_SIERA_vs_lg, test_stuff$Success)),
      digits = 3)

# Top third by each prediction among the conversions
cat("\nTop third of the conversions by predicted starter SIERA, actual results\n")
for (name in c("pred_simple", "pred_full")) {
  test |>
    group_by(top_third = .data[[name]] <= quantile(.data[[name]], 1 / 3)) |>
    summarize(n = n(), share_avg_or_better = mean(Success), sp_SIERA_vs_lg = mean(sp_SIERA_vs_lg)) |>
    mutate(model = name) |>
    as.data.frame() |> print(digits = 3)
}

# Project 2026 relievers
season_totals <- rows |>
  group_by(playerid, Season) |>
  summarize(G_total = sum(G), GS_total = sum(GS), .groups = "drop")
current <- relief_rows |>
  filter(Season == 2026, rp_TBF >= 100) |>
  left_join(season_totals, by = c("playerid", "Season")) |>
  filter(GS_total / G_total <= 0.2)
current$pred_simple <- predict(fit_simple, newdata = current)
current$pred_full <- predict(fit_full, newdata = current)
current$pred_stuff <- predict(fit_stuff, newdata = current)
lg_sp_2026 <- league_sp$lg_sp_siera[league_sp$Season == 2026]
projection <- current |>
  transmute(PlayerName, Age = rp_Age, IP = round(rp_IP, 1), relief_SIERA = round(rp_SIERA, 2),
            relief_SIERA_vs_lg = round(rp_SIERA_vs_lg, 2), StuffPlus = round(rp_StuffPlus),
            proj_starter_SIERA_simple = round(pred_simple + lg_sp_2026, 2),
            proj_starter_SIERA_full = round(pred_full + lg_sp_2026, 2),
            proj_starter_SIERA_stuff = round(pred_stuff + lg_sp_2026, 2),
            proj_vs_league_stuff = round(pred_stuff, 2)) |>
  arrange(proj_starter_SIERA_stuff)
write_csv(projection, "results/swingman_projection_2026.csv")
cat("\n4. 2026 relievers projected as starters. League starter SIERA in 2026:", round(lg_sp_2026, 2), "\n")
cat("Qualifying relievers:", nrow(projection), " projected better than a league average starter (Stuff+ model):",
    sum(projection$proj_vs_league_stuff <= 0, na.rm = TRUE), "\n")
print(as.data.frame(head(projection, 15)), digits = 3)
cat("\nFree agent names from the earlier check\n")
print(as.data.frame(projection |> filter(PlayerName %in% c("Adrian Morejon", "David Bednar", "Jonathan Loáisiga",
    "Gregory Soto",
                                                         "Alex Vesia", "JoJo Romero", "Pete Fairbanks",
                                                           "Evan Phillips"))), digits = 3)
sink()
cat(readLines("results/swingman_proxy.txt"), sep = "\n")

# proxy prediction vs actual starter SIERA vs league for the conversions
labels <- c("Garrett Crochet", "Reynaldo López", "Jordan Hicks", "Michael King", "Seth Lugo", "Clay Holmes",
            "Zack Littell", "Jeffrey Springs")
plot_data <- test
ggplot(plot_data, aes(pred_full, sp_SIERA_vs_lg)) +
  geom_abline(slope = 1, intercept = 0, color = "gray", linetype = "dashed") +
  geom_point(color = "steelblue", alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.6) +
  geom_text(data = plot_data |> filter(PlayerName %in% labels), aes(label = PlayerName), size = 3, vjust = -1) +
  labs(title = "Swingman proxy vs how the conversions actually did",
       subtitle = "Proxy fit on swingmen who are not in the sample, using only the relief season. Dashed line is a perfect match",
       x = "Proxy: predicted starter SIERA vs league", y = "Actual starter SIERA vs league") +
  theme_classic()
ggsave("charts/proxy_vs_actual.png", width = 8, height = 6, dpi = 150)

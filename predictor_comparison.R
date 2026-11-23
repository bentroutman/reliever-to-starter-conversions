# Checks whether any predictor beats picking the best relievers by relief SIERA
# Run after value_analysis.R and statcast_changes.R
# Output: results/model_vs_best_relievers.txt

library(dplyr)
library(readr)

conversions <- read_csv("data/conversions.csv", show_col_types = FALSE)
statcast <- read_csv("data/statcast_changes.csv", show_col_types = FALSE) |>
  select(xMLBAMID, sp_season, rp_Opp_KBB, rp_Platoon_KBB_gap)
expected <- read_csv("data/value_attempts.csv", show_col_types = FALSE) |>
  select(playerid, sp_season, WPA_gain, WAR_gain, Cushion)
conversions <- conversions |>
  left_join(statcast, by = c("xMLBAMID", "sp_season")) |>
  left_join(expected, by = c("playerid", "sp_season")) |>
  mutate(Success = as.numeric(sp_SIERA_vs_lg <= 0),
         Screen = (Cushion >= 1) + (rp_StuffPlus >= 100) + (rp_NumPlusPitches >= 2) + (rp_FB_Stuff >= 100))

# AUC: chance that a random success had a higher score than a random non-success
auc <- function(score, outcome) {
  keep <- !is.na(score) & !is.na(outcome)
  score <- score[keep]
  outcome <- outcome[keep]
  wins <- 0
  for (s in score[outcome == 1]) {
    wins <- wins + sum(s > score[outcome == 0]) + 0.5 * sum(s == score[outcome == 0])
  }
  wins / (sum(outcome == 1) * sum(outcome == 0))
}

# Leave one out RMSE for a linear model
loo_rmse <- function(formula, data, outcome) {
  errors <- rep(NA, nrow(data))
  for (i in 1:nrow(data)) {
    fit <- lm(formula, data = data[-i, ])
    errors[i] <- data[[outcome]][i] - predict(fit, newdata = data[i, ])
  }
  sqrt(mean(errors^2))
}

sink("results/model_vs_best_relievers.txt")

cat("PART 1. All 79 conversions (5+ starts). Outcome: starter SIERA vs league (lower is better)\n")
cat("Higher AUC is better. No model means guessing the average.\n\n")
full <- conversions |> filter(!is.na(rp_Opp_KBB), !is.na(rp_SIERA_vs_lg))
cat("n =", nrow(full), " no model RMSE =", round(sd(full$sp_SIERA_vs_lg) * sqrt((nrow(full) - 1) / nrow(full)), 3),
    "\n")
formulas_full <- c("sp_SIERA_vs_lg ~ rp_SIERA_vs_lg",
                   "sp_SIERA_vs_lg ~ rp_KBB",
                   "sp_SIERA_vs_lg ~ rp_SIERA_vs_lg + rp_age",
                   "sp_SIERA_vs_lg ~ rp_SIERA_vs_lg + rp_Opp_KBB",
                   "sp_SIERA_vs_lg ~ rp_SIERA_vs_lg + rp_SwStr + rp_age + rp_FBVelo",
                   "sp_SIERA_vs_lg ~ rp_SIERA_vs_lg + rp_age + rp_Opp_KBB + rp_gmLI + MiLB_GS")
table_full <- NULL
for (f in formulas_full) {
  fit <- lm(as.formula(f), data = full)
  table_full <- bind_rows(table_full, data.frame(
    model = sub("sp_SIERA_vs_lg ~ ", "", f), R2 = round(summary(fit)$r.squared, 3),
    LOO_RMSE = round(loo_rmse(as.formula(f), full, "sp_SIERA_vs_lg"), 3),
    AUC_success = round(auc(-predict(fit), full$Success), 3)))
}
print(table_full)

cat("\nBest relievers rule (top third by relief SIERA vs league) vs the rest\n")
cut_value <- quantile(full$rp_SIERA_vs_lg, 1 / 3)
full |>
  group_by(top_third_by_relief_SIERA = rp_SIERA_vs_lg <= cut_value) |>
  summarize(n = n(), share_avg_or_better = mean(Success), sp_SIERA_vs_lg = mean(sp_SIERA_vs_lg),
            stuck = mean(stuck, na.rm = TRUE), WAR_gain = mean(WAR_gain, na.rm = TRUE),
            WPA_gain = mean(WPA_gain, na.rm = TRUE)) |>
  as.data.frame() |> print(digits = 3)

cat("\nSame, using the stacked model (relief SIERA, age, Opp K-BB%, gmLI, MiLB starts) top third by prediction\n")
stack_fit <- lm(sp_SIERA_vs_lg ~ rp_SIERA_vs_lg + rp_age + rp_Opp_KBB + rp_gmLI + MiLB_GS, data = full)
# leave one out predictions so the ranking is not fit on its own answers
loo_pred <- rep(NA, nrow(full))
for (i in 1:nrow(full)) {
  loo_fit <- lm(formula(stack_fit), data = full[-i, ])
  loo_pred[i] <- predict(loo_fit, newdata = full[i, ])
}
full$stack_pred <- loo_pred
full |>
  group_by(top_third_by_model = stack_pred <= quantile(stack_pred, 1 / 3)) |>
  summarize(n = n(), share_avg_or_better = mean(Success), sp_SIERA_vs_lg = mean(sp_SIERA_vs_lg),
            stuck = mean(stuck, na.rm = TRUE), WAR_gain = mean(WAR_gain, na.rm = TRUE)) |>
  as.data.frame() |> print(digits = 3)

cat("\n\nPART 2. The 38 conversions from 2020 on (Stuff+ exists). Outcome: starter SIERA vs league\n")
stuff <- conversions |>
  filter(!is.na(rp_StuffPlus), !is.na(rp_NumPlusPitches), !is.na(rp_FB_Stuff), !is.na(Screen))
cat("n =", nrow(stuff), " no model RMSE =", round(sd(stuff$sp_SIERA_vs_lg) * sqrt((nrow(stuff) - 1) / nrow(stuff)),
    3), "\n")
formulas_stuff <- c("sp_SIERA_vs_lg ~ rp_SIERA_vs_lg",
                    "sp_SIERA_vs_lg ~ rp_KBB",
                    "sp_SIERA_vs_lg ~ rp_StuffPlus",
                    "sp_SIERA_vs_lg ~ rp_NumPlusPitches",
                    "sp_SIERA_vs_lg ~ Screen",
                    "sp_SIERA_vs_lg ~ rp_SIERA_vs_lg + rp_StuffPlus",
                    "sp_SIERA_vs_lg ~ rp_SIERA_vs_lg + Screen")
table_stuff <- NULL
for (f in formulas_stuff) {
  fit <- lm(as.formula(f), data = stuff)
  table_stuff <- bind_rows(table_stuff, data.frame(
    model = sub("sp_SIERA_vs_lg ~ ", "", f), R2 = round(summary(fit)$r.squared, 3),
    LOO_RMSE = round(loo_rmse(as.formula(f), stuff, "sp_SIERA_vs_lg"), 3),
    AUC_success = round(auc(-predict(fit), stuff$Success), 3)))
}
print(table_stuff)

cat("\nThe screen (count of cushion 1+, Stuff+ 100+, 2+ plus pitches, FB Stuff+ 100+) vs best relievers by SIERA\n")
cat("Screen of 3 or 4 vs the rest, and top third by relief SIERA (same 38)\n")
stuff |>
  group_by(Screen_3plus = Screen >= 3) |>
  summarize(n = n(), rp_SIERA_vs_lg = mean(rp_SIERA_vs_lg), share_avg_or_better = mean(Success),
            sp_SIERA_vs_lg = mean(sp_SIERA_vs_lg), stuck = mean(stuck, na.rm = TRUE),
            WAR_gain = mean(WAR_gain, na.rm = TRUE), WPA_gain = mean(WPA_gain, na.rm = TRUE)) |>
  as.data.frame() |> print(digits = 3)
stuff |>
  group_by(top_third_by_relief_SIERA = rp_SIERA_vs_lg <= quantile(rp_SIERA_vs_lg, 1 / 3)) |>
  summarize(n = n(), share_avg_or_better = mean(Success), sp_SIERA_vs_lg = mean(sp_SIERA_vs_lg),
            stuck = mean(stuck, na.rm = TRUE), WAR_gain = mean(WAR_gain, na.rm = TRUE),
            WPA_gain = mean(WPA_gain, na.rm = TRUE)) |>
  as.data.frame() |> print(digits = 3)
cat("Screen of 3+ is the best-SIERA group too: correlation of Screen with relief SIERA vs league =",
    round(cor(stuff$Screen, stuff$rp_SIERA_vs_lg, method = "spearman"), 2), "\n")

cat("\n\nPART 3. Same comparison for value gained (all attempts with a gain, WAR and WPA)\n")
attempts <- read_csv("data/value_attempts.csv", show_col_types = FALSE) |>
  select(playerid, sp_season, rp_StuffPlus, Cushion, WPA_gain, WAR_gain, sp_IP, rp_SIERA_vs_lg, rp_age)
for (outcome in c("WAR_gain", "WPA_gain")) {
  data_one <- attempts |> filter(!is.na(.data[[outcome]]), !is.na(rp_SIERA_vs_lg))
  cat("\nOutcome:", outcome, " n =", nrow(data_one), " no model RMSE =",
      round(sd(data_one[[outcome]]) * sqrt((nrow(data_one) - 1) / nrow(data_one)), 3), "\n")
  for (predictors in c("rp_SIERA_vs_lg", "rp_SIERA_vs_lg + rp_age")) {
    f <- as.formula(paste(outcome, "~", predictors))
    cat("  ", predictors, ": LOO RMSE", round(loo_rmse(f, data_one, outcome), 3), "\n")
  }
  stuff_one <- data_one |> filter(!is.na(rp_StuffPlus))
  cat("   (2020 on, n =", nrow(stuff_one), ") no model",
      round(sd(stuff_one[[outcome]]) * sqrt((nrow(stuff_one) - 1) / nrow(stuff_one)), 3),
      " SIERA", round(loo_rmse(as.formula(paste(outcome, "~ rp_SIERA_vs_lg")), stuff_one, outcome), 3),
      " Stuff+", round(loo_rmse(as.formula(paste(outcome, "~ rp_StuffPlus")), stuff_one, outcome), 3),
      " both", round(loo_rmse(as.formula(paste(outcome, "~ rp_SIERA_vs_lg + rp_StuffPlus")), stuff_one, outcome),
        3), "\n")
}
sink()
cat(readLines("results/model_vs_best_relievers.txt"), sep = "\n")

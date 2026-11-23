# Tests the success predictions out of time: fit on conversions through 2021, test on 2022 to 2026
# Run after build_sample.R
# Output: results/validation_success.txt

library(dplyr)
library(readr)

role_seasons <- read_csv("data/fg_role_seasons.csv", guess_max = 20000, show_col_types = FALSE) |>
  mutate(IP = floor(IP) + (IP %% 1) * 10 / 3)
conversions <- read_csv("data/conversions.csv", show_col_types = FALSE) |>
  mutate(lg_sp_siera = sp_SIERA - sp_SIERA_vs_lg, Cushion = lg_sp_siera - rp_SIERA,
      Success = as.numeric(sp_SIERA_vs_lg <= 0))

# Swingmen features, same as swingman_proxy.R
for (column in c("pfxFA%", "pfxSI%", "pfxFC%", "pfxSL%", "pfxCU%", "pfxKC%", "pfxCV%", "pfxCH%", "pfxFS%",
    "pfxFO%", "pfxSC%", "pfxST%")) {
  role_seasons[[column]][is.na(role_seasons[[column]])] <- 0
}
role_seasons <- role_seasons |>
  mutate(FourSeam = `pfxFA%`, Sinker = `pfxSI%`, Cutter = `pfxFC%`, Slider = `pfxSL%` + `pfxST%`,
      Curve = `pfxCU%` + `pfxKC%` + `pfxCV%`,
         Changeup = `pfxCH%` + `pfxFS%` + `pfxFO%` + `pfxSC%`,
         NumPitches10 = (FourSeam >= 0.1) + (Sinker >= 0.1) + (Cutter >= 0.1) + (Slider >= 0.1) + (Curve >= 0.1) + (Changeup >= 0.1),
         FBVelo = ifelse(Sinker > FourSeam, pfxvSI, pfxvFA))
league_sp <- role_seasons |> filter(role == "sta") |> group_by(Season) |>
    summarize(lg_sp_siera = weighted.mean(SIERA, TBF, na.rm = TRUE))
league_rp <- role_seasons |> filter(role == "rel") |> group_by(Season) |>
    summarize(lg_rp_siera = weighted.mean(SIERA, TBF, na.rm = TRUE))
rows <- role_seasons |>
  left_join(league_sp, by = "Season") |> left_join(league_rp, by = "Season") |>
  transmute(playerid, Season, role, Age, ThrowsL = as.numeric(Throws == "L"), IP, TBF, GS,
            SIERA_vs_lg = ifelse(role == "rel", SIERA - lg_rp_siera, SIERA - lg_sp_siera), KBB = `K-BB%`,
              SwStr = `SwStr%`, FBVelo, NumPitches10)
relief_rows <- rows |> filter(role == "rel") |> select(-role)
names(relief_rows)[-(1:2)] <- paste0("rp_", names(relief_rows)[-(1:2)])
starter_rows <- rows |> filter(role == "sta") |> select(-role)
names(starter_rows)[-(1:2)] <- paste0("sp_", names(starter_rows)[-(1:2)])
swingmen <- inner_join(relief_rows, starter_rows, by = c("playerid", "Season")) |>
  filter(rp_TBF >= 50, sp_TBF >= 50, sp_GS >= 3, sp_IP / sp_GS >= 3, !playerid %in% conversions$playerid)

auc_rank <- function(x, y) {
  ranks <- rank(x)
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  (sum(ranks[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
score_pred <- function(label, pred, actual, success) {
  data.frame(model = label, n = length(actual), RMSE = round(sqrt(mean((actual - pred)^2)), 3),
      bias_actual_minus_pred = round(mean(actual - pred), 3),
             spearman = round(ifelse(sd(pred) > 0, cor(pred, actual, method = "spearman"), NA), 2),
             AUC_success = round(ifelse(sd(pred) > 0, auc_rank(-pred, success), 0.5), 3))
}

sink("results/validation_success.txt")
train <- conversions |> filter(sp_season <= 2021)
test <- conversions |> filter(sp_season >= 2022)
cat("Train on conversions with a starter season through 2021 (n =", nrow(train), "), test on 2022 to 2026 (n =",
    nrow(test), ")\n\n")

# 1. Temporal validation
cat("1. Out of time test of relief SIERA vs league predicting starter SIERA vs league\n")
fit_train <- lm(sp_SIERA_vs_lg ~ rp_SIERA_vs_lg, data = train)
swing_train <- swingmen |> filter(Season <= 2021)
proxy_simple <- lm(sp_SIERA_vs_lg ~ rp_SIERA_vs_lg, data = swing_train)
proxy_full <- lm(sp_SIERA_vs_lg ~ rp_SIERA_vs_lg + rp_KBB + rp_SwStr + rp_FBVelo + rp_Age + rp_NumPitches10 + rp_ThrowsL, data = swing_train)
test_proxy <- test |> inner_join(relief_rows |> rename(rp_season = Season), by = c("playerid", "rp_season"),
    suffix = c("", ".r")) |>
  filter(!is.na(rp_FBVelo), !is.na(rp_KBB), !is.na(rp_SwStr))
cat("Test conversions with the features the proxy needs:", nrow(test_proxy), "\n")
pred_const <- rep(mean(train$sp_SIERA_vs_lg), nrow(test_proxy))
pred_linear <- predict(fit_train, newdata = test_proxy)
pred_proxy <- predict(proxy_simple, newdata = test_proxy)
shift <- mean(train$sp_SIERA_vs_lg - predict(proxy_simple, newdata = train))
pred_proxy_shift <- pred_proxy + shift
pred_proxy_full <- predict(proxy_full, newdata = test_proxy)
shift_full <- mean(train$sp_SIERA_vs_lg - predict(proxy_full, newdata = train |> inner_join(relief_rows |>
    rename(rp_season = Season), by = c("playerid", "rp_season"), suffix = c("", ".r")) |> filter(!is.na(rp_FBVelo),
      !is.na(rp_KBB), !is.na(rp_SwStr))), na.rm = TRUE)
results <- bind_rows(
  score_pred("train mean of the conversions (no model)", pred_const, test_proxy$sp_SIERA_vs_lg,
      test_proxy$Success),
  score_pred("linear on relief SIERA, fit on earlier conversions", pred_linear, test_proxy$sp_SIERA_vs_lg,
      test_proxy$Success),
  score_pred("swingman proxy (relief SIERA only)", pred_proxy, test_proxy$sp_SIERA_vs_lg, test_proxy$Success),
  score_pred("swingman proxy plus intercept shift from earlier conversions", pred_proxy_shift,
      test_proxy$sp_SIERA_vs_lg, test_proxy$Success),
  score_pred("swingman full proxy", pred_proxy_full, test_proxy$sp_SIERA_vs_lg, test_proxy$Success),
  score_pred("swingman full proxy plus intercept shift", pred_proxy_full + shift_full, test_proxy$sp_SIERA_vs_lg,
      test_proxy$Success))
print(results, row.names = FALSE)
cat("Intercept shift estimated on the earlier conversions (the swingman proxy runs this much too pessimistic):",
    round(shift, 3), "\n")
cat("Slope of starter SIERA on relief SIERA: earlier conversions", round(coef(fit_train)[2], 2),
    " later conversions", round(coef(lm(sp_SIERA_vs_lg ~ rp_SIERA_vs_lg, data = test))[2], 2),
    " swingmen through 2021", round(coef(proxy_simple)[2], 2), "\n")
cat("Mean starter SIERA vs league: conversions through 2021", round(mean(train$sp_SIERA_vs_lg), 2),
    " 2022 to 2026", round(mean(test$sp_SIERA_vs_lg), 2), "\n\n")

# Leave one out inside all 79
loo_linear <- rep(NA, nrow(conversions))
for (i in 1:nrow(conversions)) {
  loo_model <- lm(sp_SIERA_vs_lg ~ rp_SIERA_vs_lg, data = conversions[-i, ])
  loo_linear[i] <- predict(loo_model, newdata = conversions[i, ])
}
cat("Leave one out over all", nrow(conversions), "conversions: no model RMSE",
    round(sqrt(mean((conversions$sp_SIERA_vs_lg - mean(conversions$sp_SIERA_vs_lg))^2)), 3),
    "  linear on relief SIERA RMSE", round(sqrt(mean((conversions$sp_SIERA_vs_lg - loo_linear)^2)), 3), "\n\n")

# 2. Calibration of the probability of success
cat("2. Calibration of the predicted probability of success (starter SIERA at or below league average)\n")
loo_p <- rep(NA, nrow(conversions))
for (i in 1:nrow(conversions)) {
  loo_model <- glm(Success ~ rp_SIERA_vs_lg, data = conversions[-i, ], family = binomial)
  loo_p[i] <- predict(loo_model, newdata = conversions[i, ], type = "response")
}
conversions$loo_p <- loo_p
base_rate <- mean(conversions$Success)
cat("Leave one out: AUC", round(auc_rank(loo_p, conversions$Success), 3), " Brier",
    round(mean((loo_p - conversions$Success)^2), 4),
    " Brier of the base rate", round(mean((base_rate - conversions$Success)^2), 4),
    " log loss", round(-mean(conversions$Success * log(loo_p) + (1 - conversions$Success) * log(1 - loo_p)), 4),
    " base rate log loss", round(-mean(conversions$Success * log(base_rate) + (1 - conversions$Success) * log(1 - base_rate)), 4), "\n")
cal_fit <- glm(Success ~ I(log(loo_p / (1 - loo_p))), data = conversions, family = binomial)
cat("Calibration slope of the leave one out probabilities:", round(coef(cal_fit)[2], 2), " (95% interval",
    round(coef(cal_fit)[2] - 1.96 * summary(cal_fit)$coefficients[2, 2], 2), "to",
    round(coef(cal_fit)[2] + 1.96 * summary(cal_fit)$coefficients[2, 2], 2), ")\n")
print(as.data.frame(conversions |> mutate(bin = ntile(loo_p, 4)) |> group_by(quarter = bin) |>
                      summarize(n = n(), predicted_pct = round(mean(loo_p) * 100),
                        observed_pct = round(mean(Success) * 100), .groups = "drop")), row.names = FALSE)
train_p <- glm(Success ~ rp_SIERA_vs_lg, data = train, family = binomial)
test$p_out <- predict(train_p, newdata = test, type = "response")
cat("Out of time (fit through 2021, test 2022 to 2026): AUC", round(auc_rank(test$p_out, test$Success), 3),
    " Brier", round(mean((test$p_out - test$Success)^2), 4),
    " Brier of the earlier base rate", round(mean((mean(train$Success) - test$Success)^2), 4),
      " expected successes", round(sum(test$p_out), 1), "vs actual", sum(test$Success), "\n\n")

# 3. Thresholds chosen after looking
cat("3. Were the cushion and Stuff+ thresholds chosen after looking at all the data? Pick the cutoff on the earlier conversions, test on the later ones\n")
grid <- c(0.25, 0.5, 0.75, 1, 1.25, 1.5)
train_diffs <- rep(NA, length(grid))
for (k in 1:length(grid)) {
  above <- train$Cushion >= grid[k]
  if (sum(above) >= 5) train_diffs[k] <- mean(train$Success[above]) - mean(train$Success[!above])
}
cat("Success rate gap (at or above the cutoff minus below), earlier conversions:", paste(grid, round(train_diffs,
    2), sep = "=", collapse = "  "), "\n")
best <- grid[which.max(train_diffs)]
cat("Cutoff that looks best on the earlier conversions:", best, "\n")
cat("Test on 2022 to 2026: success rate at or above", best, ":", round(mean(test$Success[test$Cushion >= best]),
    2), "(n", sum(test$Cushion >= best), ") vs below",
    round(mean(test$Success[test$Cushion < best]), 2), "(n", sum(test$Cushion < best), ")\n")
cat("Cutoff of 1: earlier conversions", round(mean(train$Success[train$Cushion >= 1]), 2), "(n",
    sum(train$Cushion >= 1), ") vs", round(mean(train$Success[train$Cushion < 1]), 2),
    " later conversions", round(mean(test$Success[test$Cushion >= 1]), 2), "(n", sum(test$Cushion >= 1), ") vs",
      round(mean(test$Success[test$Cushion < 1]), 2), "\n")
with_stuff <- conversions |> filter(!is.na(rp_StuffPlus))
cat("Stuff+ 100 or more (relief seasons 2020 on): through 2021 starter seasons n =",
    sum(with_stuff$sp_season <= 2021),
    "(too few to evaluate), 2022 to 2026:", round(mean(with_stuff$Success[with_stuff$sp_season >= 2022 & with_stuff$rp_StuffPlus >= 100]), 2), "(n",
    sum(with_stuff$sp_season >= 2022 & with_stuff$rp_StuffPlus >= 100), ") vs",
      round(mean(with_stuff$Success[with_stuff$sp_season >= 2022 & with_stuff$rp_StuffPlus < 100]), 2), "(n",
    sum(with_stuff$sp_season >= 2022 & with_stuff$rp_StuffPlus < 100), ")\n")
# Bootstrap interval on the gap in the later period
set.seed(0)
boot_gap <- rep(NA, 2000)
for (b in 1:2000) {
  boot_test <- test[sample(nrow(test), replace = TRUE), ]
  boot_gap[b] <- mean(boot_test$Success[boot_test$Cushion >= 1]) - mean(boot_test$Success[boot_test$Cushion < 1])
}
cat("Success rate gap for cushion 1 or more vs less in 2022 to 2026:",
    round(mean(test$Success[test$Cushion >= 1]) - mean(test$Success[test$Cushion < 1]), 2),
    " bootstrap 95% interval", round(quantile(boot_gap, 0.025, na.rm = TRUE), 2), "to", round(quantile(boot_gap,
      0.975, na.rm = TRUE), 2), "\n")
sink()
cat(readLines("results/validation_success.txt"), sep = "\n")

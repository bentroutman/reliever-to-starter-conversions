# Predicts which relievers teams convert, tests the models by rolling origin, and scores the 2026 relievers
# Run after build_sample.R
# Outputs: results/selection_model.txt and results/conversion_likelihood_2026.csv

library(dplyr)
library(readr)
library(glmnet)

role_seasons <- read_csv("data/fg_role_seasons.csv", guess_max = 20000, show_col_types = FALSE)
conversions <- read_csv("data/conversions.csv", show_col_types = FALSE)

# Usage columns are NA when a pitch is not thrown
for (column in c("pfxFA%", "pfxSI%", "pfxFC%", "pfxSL%", "pfxCU%", "pfxKC%", "pfxCV%", "pfxCH%", "pfxFS%",
                 "pfxFO%", "pfxSC%", "pfxST%")) {
  role_seasons[[column]][is.na(role_seasons[[column]])] <- 0
}
role_seasons <- role_seasons |>
  mutate(FourSeam = `pfxFA%`, Sinker = `pfxSI%`, Cutter = `pfxFC%`, Slider = `pfxSL%` + `pfxST%`,
         Curve = `pfxCU%` + `pfxKC%` + `pfxCV%`, Changeup = `pfxCH%` + `pfxFS%` + `pfxFO%` + `pfxSC%`,
         NumPitches10 = (FourSeam >= 0.1) + (Sinker >= 0.1) + (Cutter >= 0.1) + (Slider >= 0.1) +
           (Curve >= 0.1) + (Changeup >= 0.1),
         FBVelo = ifelse(Sinker > FourSeam, pfxvSI, pfxvFA))

league_rp <- role_seasons |>
  filter(role == "rel") |>
  group_by(Season) |>
  summarize(lg_rp_siera = weighted.mean(SIERA, TBF, na.rm = TRUE))
season_totals <- role_seasons |>
  group_by(playerid, Season) |>
  summarize(G_total = sum(G), GS_total = sum(GS), .groups = "drop")

# Every qualifying relief season from 2016 on
relievers <- role_seasons |>
  filter(role == "rel", Season >= 2016, TBF >= 100) |>
  left_join(season_totals, by = c("playerid", "Season")) |>
  filter(GS_total / G_total <= 0.2) |>
  left_join(league_rp, by = "Season") |>
  transmute(playerid, PlayerName, Team = TeamNameAbb, Season, Age, ThrowsL = as.numeric(Throws == "L"),
            TBF_per_G = TBF / G, SIERA_vs_lg = SIERA - lg_rp_siera, KBB = `K-BB%` * 100,
            SwStr = `SwStr%` * 100, GB = `GB%` * 100, gmLI, FBVelo, NumPitches10,
            FourSeam = FourSeam * 100, Sinker = Sinker * 100, Changeup = Changeup * 100)

# Starts before the season (MLB starts only count from 2014, minor league starts from 2006)
prior_mlb <- relievers |>
  select(playerid, Season) |>
  inner_join(season_totals |> rename(StartSeason = Season), by = "playerid",
             relationship = "many-to-many") |>
  filter(StartSeason < Season) |>
  group_by(playerid, Season) |>
  summarize(prior_MLB_GS = sum(GS_total), .groups = "drop")
minors <- read_csv("data/fg_minors.csv", show_col_types = FALSE) |>
  mutate(playerid = suppressWarnings(as.numeric(playerid))) |>  # IDs with letters never reached MLB
  filter(!is.na(playerid))
prior_minors <- relievers |>
  select(playerid, Season) |>
  inner_join(minors |> select(playerid, StartSeason = Season, GS), by = "playerid",
             relationship = "many-to-many") |>
  filter(StartSeason <= Season) |>
  group_by(playerid, Season) |>
  summarize(MiLB_GS = sum(GS), .groups = "drop")

relievers <- relievers |>
  left_join(prior_mlb, by = c("playerid", "Season")) |>
  left_join(prior_minors, by = c("playerid", "Season")) |>
  left_join(conversions |> select(playerid, Season = rp_season, sp_season), by = c("playerid", "Season")) |>
  mutate(prior_MLB_GS = coalesce(prior_MLB_GS, 0), MiLB_GS = coalesce(MiLB_GS, 0),
         log_prior_MLB_GS = log1p(prior_MLB_GS), log_MiLB_GS = log1p(MiLB_GS),
         Converted = as.numeric(!is.na(sp_season))) |>
  filter(!is.na(FBVelo))
labeled <- relievers |> filter(Season <= 2025)
current <- relievers |> filter(Season == 2026)

all_traits <- c("SIERA_vs_lg", "KBB", "SwStr", "FBVelo", "GB", "NumPitches10", "Changeup", "Sinker", "FourSeam",
                "gmLI", "TBF_per_G", "Age", "ThrowsL", "log_prior_MLB_GS", "log_MiLB_GS")
formula_logistic <- as.formula(paste("Known ~", paste(all_traits, collapse = " + ")))
formula_four <- Known ~ Age + TBF_per_G + log_prior_MLB_GS + log_MiLB_GS

# Each test year only sees earlier relief seasons and conversions that had happened by then
set.seed(0)
test_rows <- NULL
for (year in 2021:2025) {
  train <- labeled |>
    filter(Season < year) |>
    mutate(Known = as.numeric(Converted == 1 & sp_season <= year))
  test <- labeled |> filter(Season == year)

  # Keep all of a pitcher's seasons in the same fold when choosing the penalty
  pitcher_ids <- unique(train$playerid)
  pitcher_folds <- sample(rep(1:5, length.out = length(pitcher_ids)))
  folds <- pitcher_folds[match(train$playerid, pitcher_ids)]
  x_train <- as.matrix(train[, all_traits])
  x_test <- as.matrix(test[, all_traits])

  logistic_model <- glm(formula_logistic, data = train, family = binomial)
  four_model <- glm(formula_four, data = train, family = binomial)
  ridge_model <- cv.glmnet(x_train, train$Known, family = "binomial", alpha = 0, foldid = folds)
  lasso_model <- cv.glmnet(x_train, train$Known, family = "binomial", alpha = 1, foldid = folds)

  test$logistic <- predict(logistic_model, newdata = test, type = "response")
  test$four_traits <- predict(four_model, newdata = test, type = "response")
  test$ridge <- as.numeric(predict(ridge_model, newx = x_test, s = "lambda.min", type = "response"))
  test$lasso <- as.numeric(predict(lasso_model, newx = x_test, s = "lambda.min", type = "response"))
  test$base_rate <- mean(train$Known)

  # Unfitted score with equal weight on each standardized trait
  test$simple_score <- as.numeric(scale(test$TBF_per_G)) + as.numeric(scale(test$log_prior_MLB_GS)) +
    as.numeric(scale(test$log_MiLB_GS)) - as.numeric(scale(test$Age))
  test_rows <- bind_rows(test_rows, test)
}

# Chance that a random convert scored higher than a random non-convert
auc_rank <- function(score, outcome) {
  n1 <- sum(outcome == 1)
  n0 <- sum(outcome == 0)
  (sum(rank(score)[outcome == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

sink("results/selection_model.txt")
cat("Test seasons 2021 to 2025:", nrow(test_rows), " converts:", sum(test_rows$Converted), "\n\n")

# Calibration slope is the coefficient on the predicted log odds (1 is perfect, below 1 is overconfident)
calibration <- NULL
for (model in c("base_rate", "logistic", "ridge", "lasso", "four_traits")) {
  test_rows$prob <- pmin(pmax(test_rows[[model]], 1e-6), 1 - 1e-6)
  test_rows$log_odds <- log(test_rows$prob / (1 - test_rows$prob))
  slope <- NA
  if (model != "base_rate") {
    slope <- coef(glm(Converted ~ log_odds, data = test_rows, family = binomial))[2]
  }
  calibration <- bind_rows(calibration, data.frame(
    model = model, expected = round(sum(test_rows$prob), 1), actual = sum(test_rows$Converted),
    slope = round(slope, 2),
    log_loss = round(-mean(ifelse(test_rows$Converted == 1, log(test_rows$prob), log(1 - test_rows$prob))), 4),
    brier = round(mean((test_rows$prob - test_rows$Converted)^2), 5)))
}
cat("Calibration\n")
print(calibration, row.names = FALSE)

# AUC averaged over the test years, and converts found in each year's top 20
scores <- c("logistic", "ridge", "lasso", "four_traits", "simple_score", "TBF_per_G")
ranking <- NULL
for (score in scores) {
  auc_by_year <- rep(NA, 5)
  top_20 <- 0
  for (i in 1:5) {
    one_year <- test_rows |> filter(Season == 2020 + i)
    auc_by_year[i] <- auc_rank(one_year[[score]], one_year$Converted)
    top_20 <- top_20 + sum(one_year$Converted[rank(-one_year[[score]]) <= 20])
  }
  ranking <- bind_rows(ranking, data.frame(score = score, mean_AUC = round(mean(auc_by_year), 3),
                                           worst_year_AUC = round(min(auc_by_year), 3), top_20 = top_20))
}

# Resample pitchers instead of rows since pitchers repeat across seasons
set.seed(0)
pitcher_ids <- unique(test_rows$playerid)
rows_by_pitcher <- split(1:nrow(test_rows), test_rows$playerid)
boot_auc <- matrix(NA, 1000, length(scores), dimnames = list(NULL, scores))
for (b in 1:1000) {
  sampled <- sample(as.character(pitcher_ids), replace = TRUE)
  boot_rows <- test_rows[unlist(rows_by_pitcher[sampled]), ]
  for (score in scores) {
    auc_by_year <- rep(NA, 5)
    for (i in 1:5) {
      in_year <- boot_rows$Season == 2020 + i
      auc_by_year[i] <- auc_rank(boot_rows[[score]][in_year], boot_rows$Converted[in_year])
    }
    boot_auc[b, score] <- mean(auc_by_year, na.rm = TRUE)
  }
}
ranking$lower <- round(apply(boot_auc, 2, quantile, 0.025), 3)
ranking$upper <- round(apply(boot_auc, 2, quantile, 0.975), 3)
cat("\nRanking, with 95% pitcher bootstrap intervals on AUC\n")
print(ranking, row.names = FALSE)
cat("Random picks would find about", round(20 * 5 * mean(test_rows$Converted), 1), "converts in the top 20s\n")
cat("Ridge minus the simple score in AUC:", round(ranking$mean_AUC[2] - ranking$mean_AUC[5], 3),
    " 95% interval", round(quantile(boot_auc[, "ridge"] - boot_auc[, "simple_score"], c(0.025, 0.975)), 3), "\n")

# Final ridge model on every relief season through 2025
set.seed(0)
pitcher_ids <- unique(labeled$playerid)
pitcher_folds <- sample(rep(1:5, length.out = length(pitcher_ids)))
folds <- pitcher_folds[match(labeled$playerid, pitcher_ids)]
ridge_model <- cv.glmnet(as.matrix(labeled[, all_traits]), labeled$Converted, family = "binomial", alpha = 0,
                         foldid = folds)
current$Prob <- as.numeric(predict(ridge_model, newx = as.matrix(current[, all_traits]), s = "lambda.min",
                                   type = "response"))
likelihood <- current |>
  arrange(desc(Prob)) |>
  transmute(PlayerName, Team, Age, TBF_per_G = round(TBF_per_G, 1), SIERA_vs_lg = round(SIERA_vs_lg, 2),
            prior_MLB_GS, MiLB_GS, ProbPct = round(Prob * 100, 1))
write_csv(likelihood, "results/conversion_likelihood_2026.csv")
cat("\n2026 relievers scored:", nrow(current), " expected conversions:", round(sum(current$Prob), 1), "\n")
print(as.data.frame(head(likelihood, 20)), row.names = FALSE)
sink()
cat(readLines("results/selection_model.txt"), sep = "\n")

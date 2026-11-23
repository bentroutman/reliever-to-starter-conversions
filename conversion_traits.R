# Compares the relief season of each conversion to every other qualifying relief season, 2016 to 2025
# Run after build_sample.R
# Outputs: results/why_teams_convert.txt, results/convert_vs_population.csv, and charts/convert_profile.png

library(dplyr)
library(readr)
library(ggplot2)

role_seasons <- read_csv("data/fg_role_seasons.csv", guess_max = 20000, show_col_types = FALSE) |>
  mutate(IP = floor(IP) + (IP %% 1) * 10 / 3)
conversions <- read_csv("data/conversions.csv", show_col_types = FALSE)

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

league_rp <- role_seasons |>
  filter(role == "rel") |>
  group_by(Season) |>
  summarize(lg_rp_siera = weighted.mean(SIERA, TBF, na.rm = TRUE))
season_totals <- role_seasons |>
  group_by(playerid, Season) |>
  summarize(G_total = sum(G), GS_total = sum(GS), .groups = "drop")

population <- role_seasons |>
  filter(role == "rel", Season >= 2016, Season <= 2025, TBF >= 100) |>
  left_join(season_totals, by = c("playerid", "Season")) |>
  filter(GS_total / G_total <= 0.2) |>
  left_join(league_rp, by = "Season") |>
  transmute(playerid, PlayerName, Season, Team = TeamNameAbb, Age, ThrowsL = as.numeric(Throws == "L"), G, IP, TBF,
            TBF_per_G = TBF / G, GS_same_season = GS_total,
            SIERA_vs_lg = SIERA - lg_rp_siera, KBB = `K-BB%` * 100, K = `K%` * 100, BB = `BB%` * 100,
            SwStr = `SwStr%` * 100, Zone = `Zone%` * 100, FStrike = `F-Strike%` * 100, GB = `GB%` * 100,
            HardHit = `HardHit%` * 100, gmLI, FBVelo, NumPitches10, FourSeam = FourSeam * 100,
              Sinker = Sinker * 100,
            Cutter = Cutter * 100, Slider = Slider * 100, Curve = Curve * 100, Changeup = Changeup * 100,
            StuffPlus = sp_stuff, FB_Stuff, LocPlus = sp_location)

# Label: this relief season is the one right before a conversion
convert_keys <- conversions |> select(playerid, Season = rp_season) |> mutate(converted = 1)
population <- population |>
  left_join(convert_keys, by = c("playerid", "Season")) |>
  mutate(converted = coalesce(converted, 0))
# Other relief seasons of pitchers who did convert are dropped, since teams did not choose them then
other_convert_seasons <- population$converted == 0 & population$playerid %in% conversions$playerid
population <- population[!other_convert_seasons, ]

# Starting history before the season
prior_mlb <- population |>
  select(playerid, Season) |>
  inner_join(season_totals |> select(playerid, mSeason = Season, GS_total), by = "playerid",
      relationship = "many-to-many") |>
  filter(mSeason < Season) |>
  group_by(playerid, Season) |>
  summarize(prior_MLB_GS = sum(GS_total), .groups = "drop")
minors <- read_csv("data/fg_minors.csv", show_col_types = FALSE) |>
  mutate(playerid = suppressWarnings(as.numeric(playerid))) |>
  filter(!is.na(playerid)) |>
  group_by(playerid, mSeason = Season) |>
  summarize(GS = sum(GS), G = sum(G), .groups = "drop")
prior_milb <- population |>
  select(playerid, Season) |>
  inner_join(minors, by = "playerid", relationship = "many-to-many") |>
  filter(mSeason <= Season) |>
  group_by(playerid, Season) |>
  summarize(MiLB_GS = sum(GS), MiLB_G = sum(G), .groups = "drop")
population <- population |>
  left_join(prior_mlb, by = c("playerid", "Season")) |>
  left_join(prior_milb, by = c("playerid", "Season")) |>
  mutate(prior_MLB_GS = coalesce(prior_MLB_GS, 0), MiLB_GS = coalesce(MiLB_GS, 0), MiLB_G = coalesce(MiLB_G, 0),
         MiLB_GS_share = ifelse(MiLB_G > 0, MiLB_GS / MiLB_G * 100, NA),
         log_prior_MLB_GS = log1p(prior_MLB_GS), log_MiLB_GS = log1p(MiLB_GS))

features <- c("Age", "ThrowsL", "TBF", "TBF_per_G", "GS_same_season", "gmLI", "SIERA_vs_lg", "KBB", "K", "BB",
    "SwStr",
              "Zone", "FStrike", "GB", "HardHit", "FBVelo", "NumPitches10", "FourSeam", "Sinker", "Cutter",
                "Slider",
              "Curve", "Changeup", "StuffPlus", "FB_Stuff", "LocPlus", "prior_MLB_GS", "MiLB_GS", "MiLB_GS_share")

# AUC from ranks: chance a random convert has a higher value than a random non-convert
auc_rank <- function(x, y) {
  keep <- !is.na(x)
  x <- x[keep]
  y <- y[keep]
  ranks <- rank(x)
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  (sum(ranks[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

sink("results/why_teams_convert.txt")
cat("Qualifying relief seasons 2016 to 2025 (100+ TBF, starts in 20% or less of games):", nrow(population), "\n")
cat("Converted the next season:", sum(population$converted), " (", round(mean(population$converted) * 100, 1),
    "%), pitchers dropped as other seasons of converts excluded:", sum(other_convert_seasons), "\n\n")

cat("1. Converts vs every other reliever season, one trait at a time\n")
cat("diff_in_SD = (convert mean - other mean) / SD of others. AUC is the chance a random convert is higher than a random non-convert (0.5 = no difference)\n")
univariate <- NULL
for (f in features) {
  x <- population[[f]]
  convert_x <- x[population$converted == 1]
  other_x <- x[population$converted == 0]
  univariate <- bind_rows(univariate, data.frame(
    trait = f, n_converts = sum(!is.na(convert_x)), convert_mean = mean(convert_x, na.rm = TRUE),
    other_mean = mean(other_x, na.rm = TRUE),
    diff_in_SD = (mean(convert_x, na.rm = TRUE) - mean(other_x, na.rm = TRUE)) / sd(other_x, na.rm = TRUE),
    convert_percentile_of_others = mean(other_x <= median(convert_x, na.rm = TRUE), na.rm = TRUE) * 100,
    AUC = auc_rank(x, population$converted),
    p_value = ifelse(sum(!is.na(convert_x)) > 5, wilcox.test(convert_x, other_x)$p.value, NA)))
}
univariate <- univariate |> arrange(desc(abs(AUC - 0.5)))
write_csv(univariate, "results/convert_vs_population.csv")
print(univariate, digits = 3)

# Multivariate: which traits matter once the others are held fixed
cat("\n2. Logistic regression of being converted (traits in SD units, odds ratio per SD)\n")
model_data <- population |>
  filter(!is.na(FBVelo), !is.na(HardHit), !is.na(GB)) |>
  mutate(across(c(Age, TBF_per_G, gmLI, SIERA_vs_lg, KBB, SwStr, FBVelo, GB, NumPitches10, log_prior_MLB_GS,
      log_MiLB_GS,
                  Changeup, Sinker, FourSeam), ~ as.numeric(scale(.x))))
fit_all <- glm(converted ~ SIERA_vs_lg + Age + ThrowsL + gmLI + TBF_per_G + FBVelo + GB + NumPitches10 + Changeup +
                 log_prior_MLB_GS + log_MiLB_GS, data = model_data, family = binomial)
coefs <- summary(fit_all)$coefficients
print(data.frame(trait = rownames(coefs), odds_ratio_per_SD = round(exp(coefs[, 1]), 2), p = round(coefs[, 4], 3)),
    row.names = FALSE)
cat("n =", nrow(model_data), " converts =", sum(model_data$converted), "\n")

# Cross validated AUC by type of trait
cat("\n3. How well each group of traits picks out the converts (5 fold cross validated AUC)\n")
set.seed(0)
fold <- rep(NA, nrow(model_data))
for (outcome in c(0, 1)) {
  rows <- which(model_data$converted == outcome)
  fold[rows] <- sample(rep(1:5, length.out = length(rows)))
}
formulas <- c(
  "skills only (SIERA, K-BB%, SwStr%, velo, GB%)" = "converted ~ SIERA_vs_lg + KBB + SwStr + FBVelo + GB",
  "arsenal (pitches, changeup, sinker, four-seam usage)" = "converted ~ NumPitches10 + Changeup + Sinker + FourSeam",
  "role and context (leverage, batters per game, age, handedness)" = "converted ~ gmLI + TBF_per_G + Age + ThrowsL",
  "starting history (MLB and minor league starts)" = "converted ~ log_prior_MLB_GS + log_MiLB_GS",
  "skills + arsenal" = "converted ~ SIERA_vs_lg + KBB + SwStr + FBVelo + GB + NumPitches10 + Changeup + Sinker + FourSeam",
  "skills + role + history" = "converted ~ SIERA_vs_lg + KBB + SwStr + FBVelo + GB + gmLI + TBF_per_G + Age + ThrowsL + log_prior_MLB_GS + log_MiLB_GS",
  "everything" = "converted ~ SIERA_vs_lg + KBB + SwStr + FBVelo + GB + NumPitches10 + Changeup + Sinker + FourSeam + gmLI + TBF_per_G + Age + ThrowsL + log_prior_MLB_GS + log_MiLB_GS")
cv_table <- NULL
for (name in names(formulas)) {
  predictions <- rep(NA, nrow(model_data))
  for (k in 1:5) {
    fit <- glm(as.formula(formulas[[name]]), data = model_data[fold != k, ], family = binomial)
    predictions[fold == k] <- predict(fit, newdata = model_data[fold == k, ])
  }
  cv_table <- bind_rows(cv_table, data.frame(traits = name, cv_AUC = round(auc_rank(predictions,
      model_data$converted), 3)))
}
print(cv_table, row.names = FALSE)

# The 2020 and later seasons add Stuff+
cat("\n4. 2020 and later, with Stuff+ (relief seasons 2020 to 2025)\n")
stuff_data <- population |>
  filter(Season >= 2020, !is.na(StuffPlus), !is.na(FB_Stuff), !is.na(FBVelo)) |>
  mutate(across(c(Age, gmLI, TBF_per_G, SIERA_vs_lg, StuffPlus, FB_Stuff, FBVelo, LocPlus, log_MiLB_GS,
      log_prior_MLB_GS),
                ~ as.numeric(scale(.x))))
fit_stuff <- glm(converted ~ SIERA_vs_lg + StuffPlus + FB_Stuff + LocPlus + FBVelo + Age + gmLI + TBF_per_G +
                   log_prior_MLB_GS + log_MiLB_GS, data = stuff_data, family = binomial)
coefs_stuff <- summary(fit_stuff)$coefficients
print(data.frame(trait = rownames(coefs_stuff), odds_ratio_per_SD = round(exp(coefs_stuff[, 1]), 2),
    p = round(coefs_stuff[, 4], 3)), row.names = FALSE)
cat("n =", nrow(stuff_data), " converts =", sum(stuff_data$converted), "\n")

# Teams and years
cat("\n5. By team (relief season team), conversions per 100 qualifying relief seasons, teams with 8+ converts or 40+ seasons\n")
population |>
  group_by(Team) |>
  summarize(seasons = n(), converts = sum(converted), per_100 = round(sum(converted) / n() * 100, 1)) |>
  filter(Team != "2 Tms", Team != "3 Tms") |>
  arrange(desc(per_100)) |>
  head(10) |>
  as.data.frame() |> print()
cat("\nBy relief season year\n")
population |>
  group_by(Season) |>
  summarize(seasons = n(), converts = sum(converted), per_100 = round(sum(converted) / n() * 100, 1)) |>
  as.data.frame() |> print()

cat("\nShares\n")
cat("Lefty: converts", round(mean(population$ThrowsL[population$converted == 1]) * 100), "% vs others",
    round(mean(population$ThrowsL[population$converted == 0]) * 100), "%\n")
cat("Prior MLB start (any): converts", round(mean(population$prior_MLB_GS[population$converted == 1] > 0) * 100),
    "% vs others",
    round(mean(population$prior_MLB_GS[population$converted == 0] > 0) * 100), "%\n")
cat("20+ minor league starts: converts", round(mean(population$MiLB_GS[population$converted == 1] >= 20) * 100),
    "% vs others",
    round(mean(population$MiLB_GS[population$converted == 0] >= 20) * 100), "%\n")
cat("Made a start in the relief season: converts",
    round(mean(population$GS_same_season[population$converted == 1] > 0) * 100), "% vs others",
    round(mean(population$GS_same_season[population$converted == 0] > 0) * 100), "%\n")
sink()
cat(readLines("results/why_teams_convert.txt"), sep = "\n")

# how the converts differ from other relievers, in SD units
plot_data <- univariate |>
  filter(n_converts >= 30) |>
  mutate(trait = factor(trait, levels = trait[order(diff_in_SD)]))
ggplot(plot_data, aes(diff_in_SD, trait)) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_col(fill = "steelblue") +
  labs(title = "How the relief seasons of converted pitchers differ from other relievers",
       subtitle = "Converts minus all other qualifying relief seasons 2016 to 2025, in SDs of the other relievers",
       x = "Difference in SD", y = NULL) +
  theme_classic()
ggsave("charts/convert_profile.png", width = 8, height = 8, dpi = 150)

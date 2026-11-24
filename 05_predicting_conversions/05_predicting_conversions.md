Step 5: Who Gets Converted
================

``` r
library(dplyr)
library(readr)
library(ggplot2)
library(glmnet)
```

## Traits of converts

The relief season of each conversion compared to every other qualifying
relief season from 2016 to 2025.

``` r
role_seasons <- read_csv("../data/fg_role_seasons.csv", guess_max = 20000, show_col_types = FALSE) |>
  mutate(IP = floor(IP) + (IP %% 1) * 10 / 3)
conversions <- read_csv("../data/conversions.csv", show_col_types = FALSE)

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
```

``` r
# Label: this relief season is the one right before a conversion
convert_keys <- conversions |> select(playerid, Season = rp_season) |> mutate(converted = 1)
population <- population |>
  left_join(convert_keys, by = c("playerid", "Season")) |>
  mutate(converted = coalesce(converted, 0))
# Other relief seasons of pitchers who did convert are dropped, since teams did not choose them then
other_convert_seasons <- population$converted == 0 & population$playerid %in% conversions$playerid
population <- population[!other_convert_seasons, ]
```

``` r
# Starting history before the season
prior_mlb <- population |>
  select(playerid, Season) |>
  inner_join(season_totals |> select(playerid, mSeason = Season, GS_total), by = "playerid",
      relationship = "many-to-many") |>
  filter(mSeason < Season) |>
  group_by(playerid, Season) |>
  summarize(prior_MLB_GS = sum(GS_total), .groups = "drop")
minors <- read_csv("../data/fg_minors.csv", show_col_types = FALSE) |>
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
```

``` r
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

cat("Qualifying relief seasons 2016 to 2025 (100+ TBF, starts in 20% or less of games):", nrow(population), "\n")
```

    ## Qualifying relief seasons 2016 to 2025 (100+ TBF, starts in 20% or less of games): 2415

``` r
cat("Converted the next season:", sum(population$converted), " (", round(mean(population$converted) * 100, 1),
    "%), pitchers dropped as other seasons of converts excluded:", sum(other_convert_seasons), "\n\n")
```

    ## Converted the next season: 125  ( 5.2 %), pitchers dropped as other seasons of converts excluded: 121

``` r
cat("1. Converts vs every other reliever season, one trait at a time\n")
```

    ## 1. Converts vs every other reliever season, one trait at a time

``` r
cat("diff_in_SD = (convert mean - other mean) / SD of others. AUC is the chance a random convert is higher than a random non-convert (0.5 = no difference)\n")
```

    ## diff_in_SD = (convert mean - other mean) / SD of others. AUC is the chance a random convert is higher than a random non-convert (0.5 = no difference)

``` r
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
```

    ##             trait n_converts convert_mean other_mean diff_in_SD
    ## 1       TBF_per_G        125       6.2933      4.556    1.42369
    ## 2   MiLB_GS_share        124      75.4898     37.453    1.11159
    ## 3    prior_MLB_GS        125      22.7200      5.714    0.98128
    ## 4         MiLB_GS        125      75.3120     45.970    0.67459
    ## 5  GS_same_season        125       1.5600      0.352    1.15401
    ## 6             Age        125      26.9920     29.100   -0.60955
    ## 7           Curve        125      11.0676      7.459    0.29464
    ## 8    NumPitches10        125       3.2240      2.822    0.51037
    ## 9            gmLI        125       0.9715      1.134   -0.44617
    ## 10        LocPlus         67     100.8774     98.310    0.40702
    ## 11          SwStr        125      10.9463     11.868   -0.33991
    ## 12       Changeup        125      11.3200     10.035    0.10337
    ## 13              K        125      22.6322     24.195   -0.25934
    ## 14         Cutter        125       7.5440      6.481    0.07311
    ## 15      StuffPlus         67     102.0204    103.934   -0.20668
    ## 16             BB        125       8.7185      9.377   -0.21783
    ## 17         Slider        125      22.4868     26.699   -0.18648
    ## 18            KBB        125      13.9136     14.818   -0.13721
    ## 19             GB        125      44.8563     43.408    0.15476
    ## 20         FBVelo        125      93.7708     94.008   -0.08849
    ## 21    SIERA_vs_lg        125      -0.0454     -0.106    0.08235
    ## 22           Zone        125      43.0464     42.720    0.08619
    ## 23            TBF        125     215.7120    207.159    0.13415
    ## 24        ThrowsL        125       0.3040      0.257    0.10704
    ## 25         Sinker        125      17.1307     17.991   -0.03965
    ## 26       FB_Stuff         67      98.6234     99.636   -0.08192
    ## 27        FStrike        125      60.5001     60.199    0.05994
    ## 28       FourSeam        125      34.0644     34.792   -0.03242
    ## 29        HardHit        125      36.2541     36.281   -0.00452
    ##    convert_percentile_of_others   AUC  p_value
    ## 1                          89.3 0.809 0.00e+00
    ## 2                          87.0 0.807 0.00e+00
    ## 3                          85.7 0.736 0.00e+00
    ## 4                          70.4 0.704 1.35e-14
    ## 5                          92.2 0.690 0.00e+00
    ## 6                          24.4 0.318 5.61e-12
    ## 7                          69.8 0.641 8.38e-09
    ## 8                          82.2 0.637 2.78e-08
    ## 9                          36.0 0.380 5.68e-06
    ## 10                         69.0 0.611 2.12e-03
    ## 11                         40.2 0.402 2.04e-04
    ## 12                         61.1 0.597 2.42e-04
    ## 13                         40.9 0.425 4.48e-03
    ## 14                         69.6 0.570 1.23e-03
    ## 15                         43.4 0.439 9.04e-02
    ## 16                         39.1 0.439 2.18e-02
    ## 17                         43.6 0.443 3.17e-02
    ## 18                         41.0 0.455 8.73e-02
    ## 19                         55.5 0.544 9.41e-02
    ## 20                         48.0 0.464 1.76e-01
    ## 21                         56.0 0.528 2.89e-01
    ## 22                         56.1 0.527 3.11e-01
    ## 23                         47.0 0.527 3.12e-01
    ## 24                         74.3 0.523 2.45e-01
    ## 25                         54.5 0.518 4.86e-01
    ## 26                         48.2 0.482 6.26e-01
    ## 27                         53.3 0.513 6.26e-01
    ## 28                         48.7 0.491 7.20e-01
    ## 29                         48.3 0.491 7.27e-01

``` r
# Multivariate: which traits matter once the others are held fixed
cat("\n2. Logistic regression of being converted (traits in SD units, odds ratio per SD)\n")
```

    ## 
    ## 2. Logistic regression of being converted (traits in SD units, odds ratio per SD)

``` r
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
```

    ##             trait odds_ratio_per_SD     p
    ##       (Intercept)              0.02 0.000
    ##       SIERA_vs_lg              0.78 0.041
    ##               Age              0.36 0.000
    ##           ThrowsL              0.86 0.546
    ##              gmLI              0.89 0.367
    ##         TBF_per_G              1.47 0.000
    ##            FBVelo              0.84 0.192
    ##                GB              1.18 0.151
    ##      NumPitches10              1.26 0.032
    ##          Changeup              0.86 0.243
    ##  log_prior_MLB_GS              2.19 0.000
    ##       log_MiLB_GS              1.62 0.010

``` r
cat("n =", nrow(model_data), " converts =", sum(model_data$converted), "\n")
```

    ## n = 2409  converts = 125

``` r
# Cross validated AUC by type of trait
cat("\n3. How well each group of traits picks out the converts (5 fold cross validated AUC)\n")
```

    ## 
    ## 3. How well each group of traits picks out the converts (5 fold cross validated AUC)

``` r
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
```

    ##                                                          traits cv_AUC
    ##                   skills only (SIERA, K-BB%, SwStr%, velo, GB%)  0.592
    ##            arsenal (pitches, changeup, sinker, four-seam usage)  0.627
    ##  role and context (leverage, batters per game, age, handedness)  0.794
    ##                  starting history (MLB and minor league starts)  0.790
    ##                                                skills + arsenal  0.657
    ##                                         skills + role + history  0.878
    ##                                                      everything  0.874

``` r
# The 2020 and later seasons add Stuff+
cat("\n4. 2020 and later, with Stuff+ (relief seasons 2020 to 2025)\n")
```

    ## 
    ## 4. 2020 and later, with Stuff+ (relief seasons 2020 to 2025)

``` r
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
```

    ##             trait odds_ratio_per_SD     p
    ##       (Intercept)              0.02 0.000
    ##       SIERA_vs_lg              0.74 0.114
    ##         StuffPlus              0.94 0.833
    ##          FB_Stuff              1.21 0.407
    ##           LocPlus              1.35 0.056
    ##            FBVelo              0.83 0.340
    ##               Age              0.31 0.000
    ##              gmLI              0.87 0.443
    ##         TBF_per_G              1.37 0.002
    ##  log_prior_MLB_GS              2.10 0.000
    ##       log_MiLB_GS              1.74 0.030

``` r
cat("n =", nrow(stuff_data), " converts =", sum(stuff_data$converted), "\n")
```

    ## n = 1377  converts = 67

``` r
# Teams and years
cat("\n5. By team (relief season team), conversions per 100 qualifying relief seasons, teams with 8+ converts or 40+ seasons\n")
```

    ## 
    ## 5. By team (relief season team), conversions per 100 qualifying relief seasons, teams with 8+ converts or 40+ seasons

``` r
population |>
  group_by(Team) |>
  summarize(seasons = n(), converts = sum(converted), per_100 = round(sum(converted) / n() * 100, 1)) |>
  filter(Team != "2 Tms", Team != "3 Tms") |>
  arrange(desc(per_100)) |>
  head(10) |>
  as.data.frame() |> print()
```

    ##    Team seasons converts per_100
    ## 1   STL      67        9    13.4
    ## 2   NYY      59        6    10.2
    ## 3   TBR      66        6     9.1
    ## 4   COL      82        7     8.5
    ## 5   DET      71        6     8.5
    ## 6   CHC      60        5     8.3
    ## 7   SDP      65        5     7.7
    ## 8   LAA      67        5     7.5
    ## 9   KCR      74        5     6.8
    ## 10  BOS      75        5     6.7

``` r
cat("\nBy relief season year\n")
```

    ## 
    ## By relief season year

``` r
population |>
  group_by(Season) |>
  summarize(seasons = n(), converts = sum(converted), per_100 = round(sum(converted) / n() * 100, 1)) |>
  as.data.frame() |> print()
```

    ##    Season seasons converts per_100
    ## 1    2016     249       16     6.4
    ## 2    2017     252       11     4.4
    ## 3    2018     267       14     5.2
    ## 4    2019     262       17     6.5
    ## 5    2020      60        3     5.0
    ## 6    2021     281       15     5.3
    ## 7    2022     267       16     6.0
    ## 8    2023     259       14     5.4
    ## 9    2024     254       10     3.9
    ## 10   2025     264        9     3.4

``` r
cat("\nShares\n")
```

    ## 
    ## Shares

``` r
cat("Lefty: converts", round(mean(population$ThrowsL[population$converted == 1]) * 100), "% vs others",
    round(mean(population$ThrowsL[population$converted == 0]) * 100), "%\n")
```

    ## Lefty: converts 30 % vs others 26 %

``` r
cat("Prior MLB start (any): converts", round(mean(population$prior_MLB_GS[population$converted == 1] > 0) * 100),
    "% vs others",
    round(mean(population$prior_MLB_GS[population$converted == 0] > 0) * 100), "%\n")
```

    ## Prior MLB start (any): converts 72 % vs others 34 %

``` r
cat("20+ minor league starts: converts", round(mean(population$MiLB_GS[population$converted == 1] >= 20) * 100),
    "% vs others",
    round(mean(population$MiLB_GS[population$converted == 0] >= 20) * 100), "%\n")
```

    ## 20+ minor league starts: converts 98 % vs others 59 %

``` r
cat("Made a start in the relief season: converts",
    round(mean(population$GS_same_season[population$converted == 1] > 0) * 100), "% vs others",
    round(mean(population$GS_same_season[population$converted == 0] > 0) * 100), "%\n")
```

    ## Made a start in the relief season: converts 53 % vs others 17 %

``` r
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
```

![](05_predicting_conversions_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
ggsave("charts/convert_profile.png", width = 8, height = 8, dpi = 150)
```

## Predicting conversions

Four models tested by rolling origin, then the 2026 relievers scored
with ridge.

``` r
role_seasons <- read_csv("../data/fg_role_seasons.csv", guess_max = 20000, show_col_types = FALSE)
conversions <- read_csv("../data/conversions.csv", show_col_types = FALSE)

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
```

``` r
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
```

``` r
# Starts before the season (MLB starts only count from 2014, minor league starts from 2006)
prior_mlb <- relievers |>
  select(playerid, Season) |>
  inner_join(season_totals |> rename(StartSeason = Season), by = "playerid",
             relationship = "many-to-many") |>
  filter(StartSeason < Season) |>
  group_by(playerid, Season) |>
  summarize(prior_MLB_GS = sum(GS_total), .groups = "drop")
minors <- read_csv("../data/fg_minors.csv", show_col_types = FALSE) |>
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
```

``` r
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
```

``` r
# Chance that a random convert scored higher than a random non-convert
auc_rank <- function(score, outcome) {
  n1 <- sum(outcome == 1)
  n0 <- sum(outcome == 0)
  (sum(rank(score)[outcome == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

cat("Test seasons 2021 to 2025:", nrow(test_rows), " converts:", sum(test_rows$Converted), "\n\n")
```

    ## Test seasons 2021 to 2025: 1398  converts: 64

``` r
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
```

    ## Calibration

``` r
print(calibration, row.names = FALSE)
```

    ##        model expected actual slope log_loss   brier
    ##    base_rate     72.3     64    NA   0.1862 0.04372
    ##     logistic     72.2     64  0.82   0.1521 0.04049
    ##        ridge     75.6     64  0.93   0.1531 0.04069
    ##        lasso     77.0     64  0.93   0.1536 0.04107
    ##  four_traits     73.5     64  0.83   0.1532 0.04153

``` r
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
```

``` r
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
```

    ## 
    ## Ranking, with 95% pitcher bootstrap intervals on AUC

``` r
print(ranking, row.names = FALSE)
```

    ##         score mean_AUC worst_year_AUC top_20 lower upper
    ##      logistic    0.841          0.727     23 0.794 0.886
    ##         ridge    0.840          0.725     23 0.794 0.884
    ##         lasso    0.845          0.755     23 0.802 0.887
    ##   four_traits    0.844          0.740     23 0.801 0.883
    ##  simple_score    0.850          0.760     22 0.809 0.887
    ##     TBF_per_G    0.790          0.753     18 0.739 0.845

``` r
cat("Random picks would find about", round(20 * 5 * mean(test_rows$Converted), 1), "converts in the top 20s\n")
```

    ## Random picks would find about 4.6 converts in the top 20s

``` r
cat("Ridge minus the simple score in AUC:", round(ranking$mean_AUC[2] - ranking$mean_AUC[5], 3),
    " 95% interval", round(quantile(boot_auc[, "ridge"] - boot_auc[, "simple_score"], c(0.025, 0.975)), 3), "\n")
```

    ## Ridge minus the simple score in AUC: -0.01  95% interval -0.03 0.007

``` r
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
```

    ## 
    ## 2026 relievers scored: 274  expected conversions: 14

``` r
print(as.data.frame(head(likelihood, 20)), row.names = FALSE)
```

    ##         PlayerName  Team Age TBF_per_G SIERA_vs_lg prior_MLB_GS MiLB_GS ProbPct
    ##       José Urquidy 3 Tms  31      11.9       -0.52           70      94    52.5
    ##       Bryse Wilson 3 Tms  28       9.4       -0.68           57     111    51.6
    ##    Jesse Scholtens   TBR  32      16.2        0.34           11     140    47.0
    ##          Brad Lord   WSN  26       8.7       -0.37           19      42    38.2
    ##  Jordan Montgomery   TEX  33       8.0       -0.52          161      70    38.1
    ##    Mitchell Parker   WSN  26       8.1        0.12           59      75    37.3
    ##    Trevor Williams 2 Tms  34       8.4       -0.81          178      95    32.5
    ##       Tobias Myers   NYM  27       7.1        0.09           31     140    27.9
    ##          Ty Madden   DET  26       9.2       -0.55            1      84    27.8
    ##       Mitch Farris   LAA  25       7.8       -0.09            5      46    26.0
    ##      Jake Woodford 3 Tms  29       7.1       -0.12           25     163    25.1
    ##       Beau Brieske   DET  28       5.7       -0.54           29      33    22.8
    ##       Chad Patrick   MIL  27       6.3        0.05           23      68    20.1
    ##       Wilber Dotel   PIT  23       8.1       -0.27            0      86    19.3
    ##        Luis Medina   ATH  27       5.3       -0.45           25      93    19.1
    ##         Dax Fulton   MIA  24       7.6       -0.20            0      81    18.6
    ##     Didier Fuentes   ATL  21       4.9       -1.21            4      51    18.5
    ##        José Suarez 3 Tms  28       5.9        0.11           62      95    18.4
    ##        Mason Black   KCR  26       8.4        1.85            8     102    17.7
    ##     Paul Blackburn   NYY  32       5.4       -0.09           86     158    16.8

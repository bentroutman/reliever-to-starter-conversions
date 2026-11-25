Step 2: How Conversions Went
================

``` r
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(splines)
library(ggrepel)

# Enables table to print on one line
options(width = 200)
```

## WAR and WPA compared to staying in the bullpen

The comparison is each pitcher’s next season to what a similar reliever
who stayed in the bullpen did the next season.

``` r
pitcher_seasons <- read_csv("../data/pitcher_seasons.csv", guess_max = 20000, show_col_types = FALSE)
lg_sp <- read_csv("../data/league_siera.csv", show_col_types = FALSE) |>
  select(Season, lg_sp_siera)

# Average WAR per inning for each role and season
role_pace <- pitcher_seasons |>
  group_by(Season, role) |>
  summarize(war_per_ip = sum(WAR, na.rm = TRUE) / sum(IP, na.rm = TRUE), .groups = "drop")

# FanGraphs season totals (starter and reliever rows added together)
# WAA_fg is WAR minus what an average pitcher in the same role would earn over those innings
season_values <- pitcher_seasons |>
  left_join(role_pace, by = c("Season", "role")) |>
  mutate(WAA_fg_row = WAR - war_per_ip * IP) |>
  group_by(playerid, Season) |>
  summarize(xMLBAMID = first(xMLBAMID), G = sum(G), GS = sum(GS), IP = sum(IP), WAR = sum(WAR),
            WAA_fg = sum(WAA_fg_row), .groups = "drop")
```

``` r
# Baseball-Reference win probability (WPA and average leverage)
bref_wp <- read_csv("../data/bref_win_probability.csv", show_col_types = FALSE) |>
  group_by(bref_id, Season) |>
  filter(n() == 1 | team_ID == "TOT") |>
  filter(IP == max(IP)) |>
  slice(1) |>
  ungroup() |>
  select(bref_id, Season, WPA = wpa_def, aLI = leverage_index_avg)
id_map <- read_csv("../data/id_map.csv", show_col_types = FALSE) |>
  distinct(bref_id, .keep_all = TRUE)

bref <- bref_wp |>
  inner_join(id_map |> select(xMLBAMID, bref_id), by = "bref_id") |>
  select(xMLBAMID, Season, WPA, aLI)

season_values <- season_values |>
  left_join(bref, by = c("xMLBAMID", "Season"))
cat("FanGraphs pitcher seasons matched to Baseball-Reference win probability:",
    sum(!is.na(season_values$WPA)), "of", nrow(season_values), "\n")
```

    ## FanGraphs pitcher seasons matched to Baseball-Reference win probability: 10528 of 10528

``` r
season_N <- season_values |>
  select(playerid, rp_season = Season, N_G = G, N_GS = GS, N_IP = IP, N_WAR = WAR, N_WAA_fg = WAA_fg,
         N_WPA = WPA, N_aLI = aLI)
season_next <- season_values |>
  select(playerid, next_season = Season, next_G = G, next_GS = GS, next_IP = IP, next_WAR = WAR,
         next_WAA_fg = WAA_fg, next_WPA = WPA, next_aLI = aLI)
```

``` r
relief_seasons <- pitcher_seasons |>
  filter(QualifyingRelief) |>
  transmute(playerid, rp_season = Season, rp_age = Age, rp_gmLI = gmLI, rp_SIERA = SIERA,
            rp_SIERA_vs_lg = SIERA_vs_lg) |>
  inner_join(season_N, by = c("playerid", "rp_season"))

# Baseline group: relievers who stayed relievers the next season and threw 20+ IP (not counting conversion attempts)
stayers <- relief_seasons |>
  mutate(next_season = rp_season + 1) |>
  anti_join(read_csv("../data/conversions.csv", show_col_types = FALSE),
            by = c("playerid", "next_season" = "sp_season")) |>
  inner_join(season_next, by = c("playerid", "next_season")) |>
  filter(next_GS / next_G <= 0.2, next_IP >= 20) |>
  left_join(lg_sp, by = c("rp_season" = "Season")) |>
  mutate(Cushion = lg_sp_siera - rp_SIERA,
         CushionGroup = case_when(Cushion < 0.5 ~ "cushion under 0.5", Cushion < 1 ~ "cushion 0.5 to 1",
                                  TRUE ~ "cushion 1+"))
cat("Stay-in-pen baseline seasons:", nrow(stayers), "\n")
```

    ## Stay-in-pen baseline seasons: 1775

### Choosing the bullpen baseline

The gains depend on the baseline, so I first compared a linear model of
next season WAR to a flexible model that allows curves.

``` r
cat("Calibration of the stay-in-the-pen baseline for next season WAR. Cross-fitted by pitcher (10 folds) so every stayer is predicted by a model that never saw him.\n")
```

    ## Calibration of the stay-in-the-pen baseline for next season WAR. Cross-fitted by pitcher (10 folds) so every stayer is predicted by a model that never saw him.

``` r
set.seed(0)
pitcher_ids <- unique(stayers$playerid)
fold_of <- sample(rep(1:10, length.out = length(pitcher_ids)))
stayers$fold <- fold_of[match(stayers$playerid, pitcher_ids)]
stayers$pred_linear <- NA
stayers$pred_flex <- NA
for (k in 1:10) {
  train <- stayers[stayers$fold != k, ]
  test <- stayers[stayers$fold == k, ]
  fit_lin <- lm(next_WAR ~ N_WAR + rp_SIERA_vs_lg + N_IP + rp_age, data = train)
  fit_flex <- lm(next_WAR ~ ns(N_WAR, 3) + ns(rp_SIERA_vs_lg, 3) + ns(N_IP, 3) + ns(rp_age, 2) + ns(rp_gmLI, 2),
                 data = train)
  stayers$pred_linear[stayers$fold == k] <- predict(fit_lin, newdata = test)
  stayers$pred_flex[stayers$fold == k] <- predict(fit_flex, newdata = test)
}
total_variation <- sum((stayers$next_WAR - mean(stayers$next_WAR))^2)
baseline_fit <- data.frame(
  model = c("no model (guess the average)", "linear", "flexible"),
  RMSE = round(c(sd(stayers$next_WAR), sqrt(mean((stayers$next_WAR - stayers$pred_linear)^2)),
                 sqrt(mean((stayers$next_WAR - stayers$pred_flex)^2))), 3),
  R2 = round(c(0, 1 - sum((stayers$next_WAR - stayers$pred_linear)^2) / total_variation,
               1 - sum((stayers$next_WAR - stayers$pred_flex)^2) / total_variation), 3))
cat("\nCross-fitted error of the next season WAR predictions\n")
```

    ## 
    ## Cross-fitted error of the next season WAR predictions

``` r
print(baseline_fit, row.names = FALSE)
```

    ##                         model  RMSE    R2
    ##  no model (guess the average) 0.742 0.000
    ##                        linear 0.692 0.128
    ##                      flexible 0.688 0.138

``` r
cat("\n")
```

``` r
cat("Actual minus predicted next season WAR by tenth of the linear prediction (a calibrated baseline is near 0 in every tenth, and the top tenth is where the elite relievers sit)\n")
```

    ## Actual minus predicted next season WAR by tenth of the linear prediction (a calibrated baseline is near 0 in every tenth, and the top tenth is where the elite relievers sit)

``` r
calib <- stayers |> mutate(bin = ntile(pred_linear, 10)) |>
  group_by(bin) |>
  summarize(n = n(), mean_predicted = round(mean(pred_linear), 2), mean_actual = round(mean(next_WAR), 2),
            bias_linear = round(mean(next_WAR - pred_linear), 3), bias_flex = round(mean(next_WAR - pred_flex), 3),
            se_bias = round(sd(next_WAR - pred_linear) / sqrt(n()), 3), .groups = "drop")
print(as.data.frame(calib), row.names = FALSE)
```

    ##  bin   n mean_predicted mean_actual bias_linear bias_flex se_bias
    ##    1 178           0.04        0.18       0.141     0.020   0.045
    ##    2 178           0.21        0.28       0.073     0.047   0.049
    ##    3 178           0.29        0.29       0.001     0.012   0.051
    ##    4 178           0.36        0.35      -0.009     0.016   0.048
    ##    5 178           0.42        0.36      -0.055    -0.009   0.048
    ##    6 177           0.48        0.41      -0.066    -0.009   0.049
    ##    7 177           0.55        0.36      -0.181    -0.123   0.047
    ##    8 177           0.63        0.57      -0.063    -0.019   0.048
    ##    9 177           0.75        0.74      -0.016    -0.005   0.062
    ##   10 177           1.00        1.18       0.183     0.084   0.066

``` r
cat("\nBias by cushion group among stayers (these are the groups the archetype results use)\n")
```

    ## 
    ## Bias by cushion group among stayers (these are the groups the archetype results use)

``` r
print(as.data.frame(stayers |>
                      group_by(group = CushionGroup) |>
                      summarize(n = n(), bias_linear = round(mean(next_WAR - pred_linear), 3),
                                bias_flex = round(mean(next_WAR - pred_flex), 3),
                                se = round(sd(next_WAR - pred_linear) / sqrt(n()), 3), .groups = "drop")),
      row.names = FALSE)
```

    ##              group   n bias_linear bias_flex    se
    ##   cushion 0.5 to 1 515      -0.074    -0.024 0.030
    ##         cushion 1+ 553       0.045     0.024 0.033
    ##  cushion under 0.5 707       0.021     0.003 0.024

The flexible model predicts only slightly better than the linear one
(cross-fitted R^2 of 0.14 vs. 0.13), and both explain little because
most of a reliever’s next season WAR is luck and playing time.

### Conversion gains and archetypes

``` r
# Expected next season value if the pitcher had stayed in the pen
war_model <- lm(next_WAR ~ N_WAR + rp_SIERA_vs_lg + N_IP + rp_age, data = stayers)
wpa_model <- lm(next_WPA ~ N_WPA + rp_SIERA_vs_lg + N_IP + rp_age, data = stayers)

archetype_cols <- c("playerid", "PlayerName", "rp_season", "sp_season", "sp_GS", "sp_IP",
                    "rp_FBVelo", "rp_NumPitches10", "rp_TBFPerG", "rp_StuffPlus",
                    "rp_NumPlusPitches", "prev_MLB_GS", "MiLB_GS")

# Split by how many starts the attempt lasted
conversions <- read_csv("../data/conversions.csv", show_col_types = FALSE) |>
  select(all_of(archetype_cols)) |>
  mutate(attempt = ifelse(sp_GS >= 5, "5+ starts", "2 to 4 starts"))

attempts <- conversions |>
  inner_join(relief_seasons, by = c("playerid", "rp_season")) |>
  inner_join(season_next, by = c("playerid", "sp_season" = "next_season")) |>
  left_join(lg_sp, by = c("sp_season" = "Season")) |>
  mutate(Cushion = lg_sp_siera - rp_SIERA,
         CushionGroup = case_when(Cushion < 0.5 ~ "cushion under 0.5", Cushion < 1 ~ "cushion 0.5 to 1",
                                  TRUE ~ "cushion 1+"))

attempts$pen_WAR <- predict(war_model, newdata = attempts)
attempts$pen_WPA <- predict(wpa_model, newdata = attempts)
attempts <- attempts |>
  mutate(WAR_gain = next_WAR - pen_WAR,
         WPA_gain = next_WPA - pen_WPA)

attempts |>
  select(playerid, PlayerName, rp_season, sp_season, sp_GS, sp_IP, rp_FBVelo, rp_NumPitches10, rp_gmLI,
         rp_TBFPerG, rp_StuffPlus, rp_NumPlusPitches, attempt, rp_age, rp_SIERA, rp_SIERA_vs_lg, N_WAR, N_WPA,
         next_IP, next_WAR, next_WPA, lg_sp_siera, Cushion, pen_WAR, pen_WPA, WAR_gain, WPA_gain,
         prev_MLB_GS, MiLB_GS) |>
  write_csv("../data/value_attempts.csv")

cat("Stay-in-pen models fit on", nrow(stayers), "reliever seasons followed by another relief season (20+ IP)\n")
```

    ## Stay-in-pen models fit on 1775 reliever seasons followed by another relief season (20+ IP)

``` r
print(data.frame(model = c("WAR", "WPA"),
                 R2 = round(c(summary(war_model)$r.squared, summary(wpa_model)$r.squared), 3)), row.names = FALSE)
```

    ##  model    R2
    ##    WAR 0.136
    ##    WPA 0.080

``` r
cat("\n")
```

``` r
cat("Stayers, average change from season N to N+1\n")
```

    ## Stayers, average change from season N to N+1

``` r
stayers |>
  summarize(n = n(), N_IP = mean(N_IP), next_IP = mean(next_IP), N_WAR = mean(N_WAR), next_WAR = mean(next_WAR),
            N_WPA = mean(N_WPA, na.rm = TRUE), next_WPA = mean(next_WPA, na.rm = TRUE)) |>
  as.data.frame() |> print(digits = 3)
```

    ##      n N_IP next_IP N_WAR next_WAR N_WPA next_WPA
    ## 1 1775 53.9    50.9 0.596    0.472 0.533    0.403

``` r
cat("\nAttempts, actual next season vs. expected if they had stayed in the pen\n")
```

    ## 
    ## Attempts, actual next season vs. expected if they had stayed in the pen

``` r
bind_rows(attempts, attempts |> mutate(attempt = "all attempts")) |>
  mutate(attempt = factor(attempt, levels = c("all attempts", "5+ starts", "2 to 4 starts"))) |>
  group_by(attempt) |>
  summarize(n = n(), N_IP = mean(N_IP), next_IP = mean(next_IP), next_WAR = mean(next_WAR), pen_WAR = mean(pen_WAR),
            share_WAR_gain = mean(WAR_gain > 0), WAR_gain_se = sd(WAR_gain) / sqrt(n()), WAR_gain = mean(WAR_gain),
            share_WPA_gain = mean(WPA_gain > 0, na.rm = TRUE),
            WPA_gain_se = sd(WPA_gain, na.rm = TRUE) / sqrt(sum(!is.na(WPA_gain))),
            WPA_gain = mean(WPA_gain, na.rm = TRUE)) |>
  relocate(WAR_gain, .before = WAR_gain_se) |>
  relocate(WPA_gain, .before = WPA_gain_se) |>
  as.data.frame() |> print(digits = 3)
```

    ##         attempt   n N_IP next_IP next_WAR pen_WAR share_WAR_gain WAR_gain WAR_gain_se share_WPA_gain WPA_gain WPA_gain_se
    ## 1  all attempts 145 56.2    89.8   0.8945   0.417          0.614    0.478      0.0905          0.407   -0.233       0.090
    ## 2     5+ starts 122 57.2    99.6   1.0764   0.423          0.705    0.654      0.0978          0.467   -0.134       0.102
    ## 3 2 to 4 starts  23 50.4    37.9  -0.0702   0.386          0.130   -0.456      0.1072          0.087   -0.761       0.134

Relievers who stayed in the pen lost value from one year to the next
(0.60 to 0.47 WAR), so the baseline expects a drop. The attempts threw
about 34 more innings as starters (89.8 vs. 56.2), which is why their
WAR rises. The 23 attempts with 2 to 4 starts threw only 38 innings and
lost 0.46 WAR against the baseline.

``` r
# Archetypes, measured in the relief season
# Age and minor league starts split at the median of the attempts
age_cut <- median(attempts$rp_age)
milb_cut <- median(attempts$MiLB_GS)
attempts <- attempts |>
  mutate(Velo = ifelse(rp_FBVelo >= 95, "95+ mph", "under 95 mph"),
         Arsenal = ifelse(rp_NumPitches10 >= 3, "3+ pitches at 10%", "2 pitches"),
         Leverage = ifelse(rp_gmLI >= 1, "above average leverage (gmLI 1+)", "below average leverage"),
         Length = ifelse(rp_TBFPerG >= 6, "long relief (6+ TBF per G)", "short relief"),
         Age = ifelse(rp_age <= age_cut, paste0(age_cut, " and under"), paste0(age_cut + 1, " and older")),
         ReliefQuality = ifelse(rp_SIERA_vs_lg < 0, "SIERA better than league RP", "SIERA worse than league RP"),
         StartingHistory = case_when(prev_MLB_GS >= 20 ~ "former MLB starter (20+ GS)",
                                     MiLB_GS >= milb_cut ~ paste0("MiLB starter (", milb_cut, "+ GS)"),
                                     TRUE ~ "little starting history"),
         StuffPlus = case_when(is.na(rp_StuffPlus) ~ NA,
                               rp_StuffPlus >= 100 ~ "Stuff+ 100+",
                               TRUE ~ "Stuff+ under 100"),
         PlusPitches = case_when(is.na(rp_NumPlusPitches) ~ NA,
                                 rp_NumPlusPitches >= 2 ~ "2+ pitches with Stuff+ 100+",
                                 TRUE ~ "0 or 1 pitches with Stuff+ 100+"))

archetypes <- c("Velo", "Arsenal", "Leverage", "Length", "Age", "ReliefQuality", "StartingHistory",
                "StuffPlus", "PlusPitches")
archetype_table <- NULL
for (archetype in archetypes) {
  group_summary <- attempts |>
    filter(!is.na(.data[[archetype]])) |>
    group_by(group = .data[[archetype]]) |>
    summarize(n = n(), share_5plus_GS = mean(attempt == "5+ starts"), N_IP = mean(N_IP), next_IP = mean(next_IP),
              next_WAR = mean(next_WAR), pen_WAR = mean(pen_WAR), share_WAR_gain = mean(WAR_gain > 0), WAR_gain_se = sd(WAR_gain) / sqrt(n()),
              WAR_gain = mean(WAR_gain), share_WPA_gain = mean(WPA_gain > 0, na.rm = TRUE),
              WPA_gain_se = sd(WPA_gain, na.rm = TRUE) / sqrt(sum(!is.na(WPA_gain))),
              WPA_gain = mean(WPA_gain, na.rm = TRUE)) |>
    relocate(WAR_gain, .before = WAR_gain_se) |>
    relocate(WPA_gain, .before = WPA_gain_se) |>
    mutate(archetype = archetype, .before = group)
  archetype_table <- bind_rows(archetype_table, group_summary)
}
archetype_table <- archetype_table |> mutate(across(where(is.numeric), ~ round(.x, 3)))
write_csv(archetype_table, "results/value_by_archetype.csv")
print(as.data.frame(archetype_table))
```

    ##          archetype                            group   n share_5plus_GS   N_IP next_IP next_WAR pen_WAR share_WAR_gain WAR_gain WAR_gain_se share_WPA_gain WPA_gain WPA_gain_se
    ## 1             Velo                          95+ mph  40          0.825 54.983  94.842    1.232   0.577          0.700    0.655       0.191          0.450   -0.128       0.180
    ## 2             Velo                     under 95 mph 105          0.848 56.610  87.940    0.766   0.356          0.581    0.410       0.101          0.390   -0.273       0.104
    ## 3          Arsenal                        2 pitches  30          0.767 53.967  90.444    0.978   0.444          0.600    0.534       0.231          0.300   -0.384       0.245
    ## 4          Arsenal                3+ pitches at 10% 115          0.861 56.733  89.687    0.873   0.410          0.617    0.463       0.097          0.435   -0.194       0.094
    ## 5         Leverage above average leverage (gmLI 1+)  70          0.886 61.043 102.000    1.326   0.551          0.686    0.775       0.153          0.514   -0.005       0.140
    ## 6         Leverage           below average leverage  75          0.800 51.604  78.498    0.492   0.292          0.547    0.200       0.091          0.307   -0.446       0.111
    ## 7           Length       long relief (6+ TBF per G)  63          0.873 53.862  87.159    0.762   0.351          0.635    0.411       0.117          0.349   -0.363       0.131
    ## 8           Length                     short relief  82          0.817 57.927  91.907    0.996   0.467          0.598    0.529       0.133          0.451   -0.134       0.123
    ## 9              Age                     26 and under  77          0.844 53.069  91.991    0.947   0.449          0.662    0.498       0.129          0.377   -0.248       0.117
    ## 10             Age                     27 and older  68          0.838 59.662  87.412    0.835   0.380          0.559    0.454       0.127          0.441   -0.216       0.140
    ## 11   ReliefQuality      SIERA better than league RP  69          0.812 59.329  93.918    1.271   0.631          0.609    0.640       0.157          0.420   -0.186       0.135
    ## 12   ReliefQuality       SIERA worse than league RP  76          0.868 53.285  86.145    0.553   0.223          0.618    0.331       0.095          0.395   -0.276       0.121
    ## 13 StartingHistory            MiLB starter (75+ GS)  47          0.809 54.440  82.376    0.547   0.387          0.511    0.160       0.135          0.340   -0.344       0.138
    ## 14 StartingHistory      former MLB starter (20+ GS)  47          0.894 63.177  88.709    0.875   0.383          0.553    0.491       0.178          0.426   -0.300       0.185
    ## 15 StartingHistory          little starting history  51          0.824 51.281  97.771    1.233   0.475          0.765    0.758       0.146          0.451   -0.070       0.142
    ## 16       StuffPlus                      Stuff+ 100+  37          0.946 58.667 112.550    1.794   0.573          0.865    1.222       0.195          0.622    0.262       0.163
    ## 17       StuffPlus                 Stuff+ under 100  30          0.833 49.856  81.778    0.567   0.373          0.567    0.194       0.174          0.333   -0.413       0.185
    ## 18     PlusPitches  0 or 1 pitches with Stuff+ 100+  26          0.808 46.231  76.115    0.701   0.367          0.538    0.334       0.218          0.346   -0.356       0.220
    ## 19     PlusPitches      2+ pitches with Stuff+ 100+  41          0.951 60.106 113.138    1.589   0.557          0.854    1.032       0.184          0.585    0.160       0.150

Stuff+ and leverage separate the groups most. Attempts with a Stuff+ of
100 or more gained 1.22 WAR on average vs. 0.19 for the rest, and
relievers who entered at above average leverage gained 0.78 vs. 0.20.
The Stuff+ 100+ attempts also stuck as starters more often (95% had 5 or
more starts vs. 83%), so part of that gap is durability. Each group has
25 to 115 attempts and standard errors of 0.1 to 0.2 WAR, so smaller
differences are noise, and the starting history groups move a lot when
the cutoffs change.

``` r
# Actual next season WAR vs. expected WAR in the pen
# Labels the biggest gains, biggest failures, and high expectation conversions
outliers <- attempts |>
  filter(abs(WAR_gain - mean(WAR_gain)) > 1.75 * sd(WAR_gain) | (pen_WAR >= 0.8))

ggplot(attempts, aes(pen_WAR, next_WAR, color = attempt)) +
  geom_abline(slope = 1, intercept = 0, color = "gray", linetype = "dashed") +
  geom_point(alpha = 0.7, size = 2) +
  geom_text_repel(data = outliers, aes(label = paste0(sub(".* ", "", PlayerName), " '", substr(sp_season, 3, 4))),
                  size = 2.6, seed = 1, min.segment.length = 0.2, show.legend = FALSE) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.1))) +
  scale_color_manual(values = c("5+ starts" = "steelblue", "2 to 4 starts" = "red")) +
  labs(title = "Next season WAR after a move to the rotation vs. expected WAR in the pen",
       subtitle = "Expected WAR comes from relievers who stayed relievers. Points above the line gained value",
       x = "Expected WAR if he stayed in the pen", y = "Actual WAR the next season (all roles)",
       color = "Attempt") +
  theme_classic()
```

![](02_conversion_value_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ggsave("charts/war_vs_pen_baseline.png", width = 8, height = 6, dpi = 150)
```

``` r
# Average WAR and WPA gain by archetype
archetype_table <- archetype_table |>
  mutate(label = paste0(group, " (n=", n, ")"), label = factor(label, levels = rev(label)))
war_plot <- ggplot(archetype_table, aes(WAR_gain, label)) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_col(fill = "steelblue") +
  labs(title = "WAR gain by archetype", x = "Average gain (wins)", y = NULL) +
  theme_classic()
wpa_plot <- ggplot(archetype_table, aes(WPA_gain, label)) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_col(fill = "steelblue") +
  labs(title = "WPA gain by archetype", x = "Average gain (wins)", y = NULL) +
  theme_classic()
ggsave("charts/value_gain_by_archetype.png", grid.arrange(war_plot, wpa_plot, ncol = 2),
       width = 11, height = 6, dpi = 150)
```

![](02_conversion_value_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Every archetype gains WAR, but only the two Stuff+ groups (Stuff+ 100+
and 2+ pitches with Stuff+ 100+) gain WPA. Leverage shows up in WPA too,
as relievers who entered at above average leverage roughly break even
(-0.01), while those at below average leverage lose 0.45.

## Value against an average pitcher (WAA)

WAR compares to replacement level, so I also checked measures that
compare to an average pitcher. WAA_fg is FanGraphs WAR minus the average
pace for the same role and season.

``` r
measures <- c("WAR", "WAA_fg", "WPA")
cat("How well each measure holds year to year for", nrow(stayers), "relievers who stayed relievers\n")
```

    ## How well each measure holds year to year for 1775 relievers who stayed relievers

``` r
cat("Correlation of this season with next season. Totals include innings pitched, rates are per 65 IP. Standard error is about",
    round(1 / sqrt(nrow(stayers) - 3), 3), "\n")
```

    ## Correlation of this season with next season. Totals include innings pitched, rates are per 65 IP. Standard error is about 0.024

``` r
stability <- NULL
for (m in measures) {
  this_total <- stayers[[paste0("N_", m)]]
  next_total <- stayers[[paste0("next_", m)]]
  this_rate <- this_total / stayers$N_IP * 65
  next_rate <- next_total / stayers$next_IP * 65
  stability <- bind_rows(stability, data.frame(
    measure = m, r_total = round(cor(this_total, next_total), 3),
    r_rate_per_65_IP = round(cor(this_rate, next_rate), 3)))
}
print(stability, row.names = FALSE)
```

    ##  measure r_total r_rate_per_65_IP
    ##      WAR   0.319            0.287
    ##   WAA_fg   0.315            0.290
    ##      WPA   0.164            0.120

``` r
cat("\n")
```

WAR and WAA repeat about equally well from one season to the next
(0.32), while WPA repeats at only 0.16, so the WPA gains are noisier
than the WAR gains.

``` r
# Conversion gains: actual next season minus the stay-in-the-pen projection
cat("Conversion gain by measure\n")
```

    ## Conversion gain by measure

``` r
gain_measures <- c("WAR", "WAA_fg", "WPA")
# The WAR and WPA gains are already in the table above
waa_model <- lm(next_WAA_fg ~ N_WAA_fg + rp_SIERA_vs_lg + N_IP + rp_age, data = stayers)
attempts$pen_WAA_fg <- predict(waa_model, newdata = attempts)
attempts$WAA_fg_gain <- attempts$next_WAA_fg - attempts$pen_WAA_fg

# Which attempts are in each group
in_group <- list("all attempts" = rep(TRUE, nrow(attempts)),
                 "5+ starts" = attempts$attempt == "5+ starts",
                 "2 to 4 starts" = attempts$attempt == "2 to 4 starts",
                 "cushion under 0.5" = attempts$CushionGroup == "cushion under 0.5",
                 "cushion 0.5 to 1" = attempts$CushionGroup == "cushion 0.5 to 1",
                 "cushion 1+" = attempts$CushionGroup == "cushion 1+",
                 "Stuff+ 100+" = attempts$StuffPlus %in% "Stuff+ 100+",
                 "Stuff+ under 100" = attempts$StuffPlus %in% "Stuff+ under 100")

gain_table <- NULL
for (g in names(in_group)) {
  for (m in gain_measures) {
    gain <- attempts[[paste0(m, "_gain")]][in_group[[g]]]
    gain_table <- bind_rows(gain_table, data.frame(
      group = g, measure = m, n = length(gain), mean_gain = round(mean(gain), 3),
      se = round(sd(gain) / sqrt(length(gain)), 3), share_gaining = round(mean(gain > 0), 2)))
  }
}
write_csv(gain_table, "results/waa_gains.csv")
gain_table |>
  rename(gain = mean_gain, share = share_gaining) |>
  tidyr::pivot_wider(names_from = measure, values_from = c(gain, se, share), names_glue = "{measure}_{.value}") |>
  select(group, n, starts_with("WAR_"), starts_with("WAA_fg_"), starts_with("WPA_")) |>
  as.data.frame() |> print(row.names = FALSE)
```

    ##              group   n WAR_gain WAR_se WAR_share WAA_fg_gain WAA_fg_se WAA_fg_share WPA_gain WPA_se WPA_share
    ##       all attempts 145    0.478  0.090      0.61      -0.218     0.067         0.34   -0.233  0.090      0.41
    ##          5+ starts 122    0.654  0.098      0.70      -0.172     0.076         0.39   -0.134  0.102      0.47
    ##      2 to 4 starts  23   -0.456  0.107      0.13      -0.461     0.104         0.09   -0.761  0.134      0.09
    ##  cushion under 0.5  76    0.381  0.112      0.59      -0.258     0.085         0.30   -0.212  0.125      0.42
    ##   cushion 0.5 to 1  36    0.339  0.195      0.53      -0.354     0.145         0.33   -0.404  0.196      0.31
    ##         cushion 1+  33    0.852  0.208      0.76       0.024     0.145         0.42   -0.094  0.169      0.48
    ##        Stuff+ 100+  37    1.222  0.195      0.86       0.232     0.153         0.51    0.262  0.163      0.62
    ##   Stuff+ under 100  30    0.194  0.174      0.57      -0.408     0.127         0.30   -0.413  0.185      0.33

``` r
# Spearman compares ranks, so a few huge gains do not dominate the correlation
cat("\nSpearman correlation of conversion gains\n")
```

    ## 
    ## Spearman correlation of conversion gains

``` r
gain_matrix <- attempts[, paste0(gain_measures, "_gain")]
print(round(cor(gain_matrix, method = "spearman"), 2))
```

    ##             WAR_gain WAA_fg_gain WPA_gain
    ## WAR_gain        1.00        0.83     0.64
    ## WAA_fg_gain     0.83        1.00     0.62
    ## WPA_gain        0.64        0.62     1.00

WAR gains are positive, but gains measured against an average pitcher
are negative (WAA -0.22 and WPA -0.23). The extra WAR comes from
innings, not from pitching better than average. Only the Stuff+ 100+
group is above zero on both WAA (+0.23) and WPA (+0.26). WAR and WAA
agree most on who gained (0.83) because WAA is built from WAR, and WPA
agrees less (0.62 to 0.64).

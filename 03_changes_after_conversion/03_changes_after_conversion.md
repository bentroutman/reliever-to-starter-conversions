Step 3: What Changed After the Move
================

``` r
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
```

## Results and pitch mix

The relief side is the FanGraphs reliever line and the starter side is
the starter line.

``` r
pitcher_seasons <- read_csv("../data/pitcher_seasons.csv", guess_max = 20000, show_col_types = FALSE)
conversions <- read_csv("../data/conversions.csv", show_col_types = FALSE) |>
  select(playerid, xMLBAMID, PlayerName, Throws, rp_season, sp_season, sp_GS, sp_IP, rp_SIERA_vs_lg, sp_SIERA_vs_lg,
         stuck) |>
  mutate(Success = ifelse(sp_SIERA_vs_lg <= 0, "league average or better", "worse than average"))
expected <- read_csv("../data/value_attempts.csv", show_col_types = FALSE) |>
  select(playerid, sp_season, WAR_gain)

# Sweepers are already in the slider column, knuckle curves go with curves, splitters with changeups
pitch_groups <- c("FourSeam", "Sinker", "Cutter", "Slider", "Curve", "Changeup")
process_measures <- c(pitch_groups, "NumPitches10", "FBVelo", "pfxvSL", "pfxvCH", "sp_stuff", "FB_Stuff",
                      "sp_s_SL", "sp_s_CH", "sp_s_CU", "sp_location", "sp_pitching", "Zone%", "F-Strike%")
result_measures <- c("K%", "BB%", "K-BB%", "SwStr%", "O-Swing%", "Z-Contact%", "GB%", "HardHit%", "SIERA")
all_measures <- c(process_measures, result_measures)

relief_side <- pitcher_seasons |>
  filter(role == "rel") |>
  select(playerid, rp_season = Season, all_of(all_measures))
starter_side <- pitcher_seasons |>
  filter(role == "sta") |>
  select(playerid, sp_season = Season, all_of(all_measures))
```

``` r
# Long format with the relief value, the starter value, and the change for every measure
changes <- conversions |>
  left_join(expected, by = c("playerid", "sp_season")) |>
  inner_join(relief_side |> pivot_longer(-c(playerid, rp_season), names_to = "measure", values_to = "relief"),
             by = c("playerid", "rp_season"), relationship = "many-to-many") |>
  inner_join(starter_side |> pivot_longer(-c(playerid, sp_season), names_to = "measure", values_to = "starter"),
             by = c("playerid", "sp_season", "measure")) |>
  mutate(change = starter - relief,
         type = ifelse(measure %in% result_measures, "result", "process"))
write_csv(changes, "results/conversion_changes.csv")
```

``` r
# A pitch is added when it goes from under 5% (too rare to get a Stuff+ rating) to 10% or more (a regular pitch,
# as in NumPitches10), and dropped in the other direction
mix <- changes |>
  filter(measure %in% pitch_groups) |>
  mutate(added = relief < 0.05 & starter >= 0.10,
         dropped = relief >= 0.10 & starter < 0.05)
per_pitcher <- mix |>
  group_by(playerid, sp_season, PlayerName, sp_SIERA_vs_lg, WAR_gain, stuck, rp_SIERA_vs_lg) |>
  summarize(added_any = any(added), .groups = "drop")

cat("Relief vs starter lines for", n_distinct(paste(changes$playerid, changes$sp_season)), "conversions\n\n")
```

    ## Relief vs starter lines for 145 conversions

``` r
cat("Average change from relief to starter (usage in percentage points, velo in mph)\n")
```

    ## Average change from relief to starter (usage in percentage points, velo in mph)

``` r
change_summary <- changes |>
  group_by(type, measure) |>
  summarize(n = sum(!is.na(change)), relief = mean(relief, na.rm = TRUE), starter = mean(starter, na.rm = TRUE),
            share_up = mean(change > 0, na.rm = TRUE), change = mean(change, na.rm = TRUE), .groups = "drop")
print(as.data.frame(change_summary), digits = 3)
```

    ##       type      measure   n   relief  starter share_up    change
    ## 1  process     Changeup 145   0.1071   0.1373    0.669  0.030196
    ## 2  process        Curve 145   0.1126   0.1123    0.414 -0.000327
    ## 3  process       Cutter 145   0.0685   0.0791    0.317  0.010529
    ## 4  process    F-Strike% 145   0.6000   0.6148    0.566  0.014813
    ## 5  process       FBVelo 145  93.6635  92.5472    0.138 -1.116358
    ## 6  process     FB_Stuff  67  98.6234  94.1932    0.313 -4.750180
    ## 7  process     FourSeam 145   0.3506   0.2936    0.372 -0.057050
    ## 8  process NumPitches10 145   3.1793   3.6138    0.462  0.434483
    ## 9  process       Sinker 145   0.1805   0.2085    0.497  0.027919
    ## 10 process       Slider 145   0.1815   0.1713    0.386 -0.010209
    ## 11 process        Zone% 145   0.4323   0.4304    0.538 -0.001882
    ## 12 process       pfxvCH 125  85.6735  85.0176    0.280 -0.749286
    ## 13 process       pfxvSL 105  84.9065  83.8697    0.286 -0.863470
    ## 14 process  sp_location  67 100.8774 101.8423    0.612  2.085121
    ## 15 process  sp_pitching  67 102.2121  98.9661    0.388 -2.649900
    ## 16 process      sp_s_CH  53  93.7860  90.5615    0.453 -2.736727
    ## 17 process      sp_s_CU  37 103.5908  98.8722    0.297 -5.088145
    ## 18 process      sp_s_SL  54 108.0694 103.7763    0.278 -5.753687
    ## 19 process     sp_stuff  67 102.0204  97.2332    0.269 -5.128215
    ## 20  result          BB% 145   0.0883   0.0824    0.379 -0.005870
    ## 21  result          GB% 145   0.4561   0.4278    0.331 -0.028279
    ## 22  result     HardHit% 136   0.3592   0.3875    0.706  0.030789
    ## 23  result           K% 145   0.2206   0.2050    0.366 -0.015578
    ## 24  result        K-BB% 145   0.1323   0.1226    0.490 -0.009707
    ## 25  result     O-Swing% 145   0.3087   0.3048    0.428 -0.003896
    ## 26  result        SIERA 145   3.8907   4.5006    0.766  0.609879
    ## 27  result       SwStr% 145   0.1068   0.0983    0.317 -0.008452
    ## 28  result   Z-Contact% 145   0.8587   0.8685    0.634  0.009753

``` r
cat("\nPitches added (under 5% to 10%+) and dropped (10%+ to under 5%), number of conversions\n")
```

    ## 
    ## Pitches added (under 5% to 10%+) and dropped (10%+ to under 5%), number of conversions

``` r
print(mix |> group_by(measure) |> summarize(added = sum(added), dropped = sum(dropped)) |> as.data.frame())
```

    ##    measure added dropped
    ## 1 Changeup    16       2
    ## 2    Curve     9       4
    ## 3   Cutter    10       7
    ## 4 FourSeam     5       6
    ## 5   Sinker    16       2
    ## 6   Slider    14       4

``` r
cat("\nAdded a pitch or not\n")
```

    ## 
    ## Added a pitch or not

``` r
per_pitcher |>
  group_by(added_any) |>
  summarize(n = n(), rp_SIERA_vs_lg = mean(rp_SIERA_vs_lg), share_avg_or_better = mean(sp_SIERA_vs_lg <= 0),
            sp_SIERA_vs_lg = mean(sp_SIERA_vs_lg), WAR_gain = mean(WAR_gain, na.rm = TRUE),
            stuck = mean(stuck, na.rm = TRUE)) |>
  as.data.frame() |> print(digits = 3)
```

    ##   added_any  n rp_SIERA_vs_lg share_avg_or_better sp_SIERA_vs_lg WAR_gain stuck
    ## 1     FALSE 87        -0.0455               0.414          0.225    0.384 0.304
    ## 2      TRUE 58         0.0897               0.414          0.139    0.618 0.375

Moving to the rotation costs about 1.1 mph of fastball velocity (93.7 to
92.5 mph, and 86% of the 145 pitchers lost velocity). Pitchers throw
fewer four-seamers (down 5.7 percentage points) and more changeups (up
3.0), and their results get worse (SIERA rises 0.61, and 77% got worse).
Stuff+ falls 5.1 points while Location+ rises 2.1, which is consistent
with starters giving up some stuff to throw more strikes. The 58
pitchers who added a pitch did only slightly better as starters (SIERA
of 0.14 versus 0.23 above league, and 38% versus 30% stuck in the
rotation).

``` r
# Does each change relate to the starter result once relief SIERA is known
cat("\nChange vs starter SIERA vs league (Spearman), and the change's effect after relief SIERA (SD units)\n")
```

    ## 
    ## Change vs starter SIERA vs league (Spearman), and the change's effect after relief SIERA (SD units)

``` r
effect_table <- NULL
for (m in all_measures) {
  one <- changes |> filter(measure == m, !is.na(change))
  fit <- lm(sp_SIERA_vs_lg ~ rp_SIERA_vs_lg + scale(change), data = one)
  effect_table <- bind_rows(effect_table, data.frame(
    measure = m, type = ifelse(m %in% result_measures, "result", "process"), n = nrow(one),
    spearman_SIERA = round(cor(one$change, one$sp_SIERA_vs_lg, method = "spearman"), 3),
    beta_after_rp_SIERA = round(coef(summary(fit))[3, 1], 3),
    p = coef(summary(fit))[3, 4]))
}
# Adjust for testing many measures (Benjamini-Hochberg)
effect_table$q <- round(p.adjust(effect_table$p, method = "BH"), 3)
effect_table$p <- round(effect_table$p, 3)
print(effect_table)
```

    ##         measure    type   n spearman_SIERA beta_after_rp_SIERA     p     q
    ## 1      FourSeam process 145          0.032               0.042 0.527 0.689
    ## 2        Sinker process 145         -0.048               0.005 0.935 0.970
    ## 3        Cutter process 145         -0.084              -0.098 0.136 0.201
    ## 4        Slider process 145          0.049               0.007 0.910 0.970
    ## 5         Curve process 145         -0.007              -0.002 0.970 0.970
    ## 6      Changeup process 145          0.004               0.015 0.817 0.954
    ## 7  NumPitches10 process 145         -0.084              -0.097 0.137 0.201
    ## 8        FBVelo process 145         -0.081              -0.188 0.005 0.014
    ## 9        pfxvSL process 105          0.000              -0.006 0.937 0.970
    ## 10       pfxvCH process 125          0.101               0.034 0.628 0.764
    ## 11     sp_stuff process  67         -0.072              -0.293 0.002 0.005
    ## 12     FB_Stuff process  67          0.019              -0.189 0.048 0.089
    ## 13      sp_s_SL process  54         -0.158              -0.209 0.043 0.087
    ## 14      sp_s_CH process  53         -0.223              -0.124 0.135 0.201
    ## 15      sp_s_CU process  37          0.054              -0.061 0.542 0.689
    ## 16  sp_location process  67         -0.264              -0.234 0.010 0.024
    ## 17  sp_pitching process  67         -0.198              -0.392 0.000 0.000
    ## 18        Zone% process 145         -0.083              -0.176 0.007 0.019
    ## 19    F-Strike% process 145         -0.264              -0.262 0.000 0.000
    ## 20           K%  result 145         -0.360              -0.654 0.000 0.000
    ## 21          BB%  result 145          0.325               0.589 0.000 0.000
    ## 22        K-BB%  result 145         -0.427              -0.875 0.000 0.000
    ## 23       SwStr%  result 145         -0.215              -0.428 0.000 0.000
    ## 24     O-Swing%  result 145         -0.314              -0.362 0.000 0.000
    ## 25   Z-Contact%  result 145          0.054               0.169 0.012 0.026
    ## 26          GB%  result 145         -0.184              -0.119 0.069 0.120
    ## 27     HardHit%  result 136          0.020              -0.043 0.528 0.689
    ## 28        SIERA  result 145          0.493               0.829 0.000 0.000

Changes in pitch mix do not predict how a pitcher did as a starter once
relief SIERA is known (every usage change has p above 0.1). Holding up
velocity, Stuff+, and command does. Pitchers who held or gained
Pitching+ (beta -0.39), Stuff+ (-0.29), first pitch strike rate (-0.26),
Location+ (-0.23), and fastball velocity (-0.19) had better starter
SIERA, and all of them stay significant (q of 0.03 or lower) after
adjusting for the 28 measures tested (67 pitchers for the Stuff+ and
Location+ measures). The result measures such as K-BB% are strongly
related, but SIERA is built from them, so that relationship is partly
mechanical.

``` r
# usage change by pitch group, successes vs the rest
usage_plot <- changes |>
  filter(measure %in% pitch_groups) |>
  group_by(measure, Success) |>
  summarize(n = n(), change = mean(change, na.rm = TRUE) * 100, .groups = "drop")
usage_plot$measure <- factor(usage_plot$measure, levels = pitch_groups)
ggplot(usage_plot, aes(measure, change, fill = Success)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("league average or better" = "steelblue", "worse than average" = "red")) +
  labs(title = "Pitch mix change from the relief season to the starter season",
       subtitle = "Conversion attempts, split by starter SIERA vs league. Sliders include sweepers, changeups include splitters",
       x = NULL, y = "Usage change (percentage points)", fill = "As a starter") +
  theme_classic()
```

![](03_changes_after_conversion_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
ggsave("charts/usage_change.png", width = 9, height = 5.5, dpi = 150)
```

## Statcast changes

Platoon results, velocity within starts, and times through the order.
The relief season platoon K-BB% and whiff rate are saved to
`statcast_changes.csv` for notebook 04.

``` r
relief_pitches <- read_csv("../data/savant/relief_season_pitches.csv", guess_max = 50000, show_col_types = FALSE)
starter_pitches <- read_csv("../data/savant/starter_season_pitches.csv", guess_max = 50000, show_col_types = FALSE)

swing_types <- c("swinging_strike", "swinging_strike_blocked", "foul", "foul_tip", "hit_into_play",
                 "foul_bunt", "missed_bunt")
whiff_types <- c("swinging_strike", "swinging_strike_blocked")
```

``` r
# Starter season pitches include any relief outings, so keep only games where he pitched in the 1st inning (his
# starts)
starts <- starter_pitches |>
  filter(inning == 1) |>
  distinct(pitcher, game_pk)
starter_pitches <- starter_pitches |> semi_join(starts, by = c("pitcher", "game_pk"))

relief_pitches$role <- "relief"
starter_pitches$role <- "starter"
pitches <- bind_rows(relief_pitches, starter_pitches) |>
  filter(!is.na(pitch_type)) |>
  mutate(OppHand = stand != p_throws,
         Swing = description %in% swing_types,
         Whiff = description %in% whiff_types,
         PA = !is.na(events) & events != "truncated_pa",
         K = PA & events %in% c("strikeout", "strikeout_double_play"),
         BB = PA & events %in% c("walk", "intent_walk"),
         Offspeed = pitch_type %in% c("CH", "FS", "FO", "SC"),
         Breaking = pitch_type %in% c("SL", "ST", "SV", "CU", "KC", "CS"),
         Fastball = pitch_type %in% c("FF", "SI"))

pitches <- pitches |>
  arrange(pitcher, game_year, game_pk, at_bat_number, pitch_number) |>
  group_by(pitcher, game_year, game_pk) |>
  mutate(GamePitchNum = row_number()) |>
  ungroup()
```

``` r
# Primary fastball each season and role
primary_fb <- pitches |>
  filter(Fastball) |>
  count(pitcher, game_year, role, pitch_type) |>
  group_by(pitcher, game_year, role) |>
  filter(n == max(n)) |>
  slice(1) |>
  select(pitcher, game_year, role, primary_fb = pitch_type)

season_measures <- pitches |>
  left_join(primary_fb, by = c("pitcher", "game_year", "role")) |>
  group_by(pitcher, game_year, role) |>
  summarize(PitchesPerOuting = n() / n_distinct(game_pk),
            OppShare_PA = sum(PA & OppHand) / sum(PA),
            Opp_KBB = (sum(K & OppHand) - sum(BB & OppHand)) / sum(PA & OppHand),
            Same_KBB = (sum(K & !OppHand) - sum(BB & !OppHand)) / sum(PA & !OppHand),
            Opp_Whiff = sum(Whiff & OppHand) / sum(Swing & OppHand),
            Opp_Offspeed = mean(Offspeed[OppHand]),
            Opp_Breaking = mean(Breaking[OppHand]),
            Opp_Fastball = mean(Fastball[OppHand]),
            # Primary fastball velocity lost per 10 pitches thrown in a game (the slope of velocity on pitch number)
            Velo_per10 = cov(GamePitchNum[pitch_type == primary_fb], release_speed[pitch_type == primary_fb],
                             use = "complete.obs") / var(GamePitchNum[pitch_type == primary_fb]) * 10,
            .groups = "drop") |>
  mutate(Platoon_KBB_gap = Same_KBB - Opp_KBB)
```

``` r
# Wide: relief and starter side by side for each conversion
relief_statcast <- season_measures |> filter(role == "relief") |> select(-role)
starter_statcast <- season_measures |> filter(role == "starter") |> select(-role)
names(relief_statcast)[-(1:2)] <- paste0("rp_", names(relief_statcast)[-(1:2)])
names(starter_statcast)[-(1:2)] <- paste0("sp_", names(starter_statcast)[-(1:2)])

statcast_changes <- conversions |>
  inner_join(relief_statcast, by = c("xMLBAMID" = "pitcher", "rp_season" = "game_year")) |>
  inner_join(starter_statcast, by = c("xMLBAMID" = "pitcher", "sp_season" = "game_year")) |>
  mutate(Opp_KBB_change = sp_Opp_KBB - rp_Opp_KBB,
         Same_KBB_change = sp_Same_KBB - rp_Same_KBB,
         Opp_Whiff_change = sp_Opp_Whiff - rp_Opp_Whiff,
         Opp_Offspeed_change = sp_Opp_Offspeed - rp_Opp_Offspeed,
         Opp_Breaking_change = sp_Opp_Breaking - rp_Opp_Breaking,
         Opp_Fastball_change = sp_Opp_Fastball - rp_Opp_Fastball,
         OppShare_change = sp_OppShare_PA - rp_OppShare_PA)
write_csv(statcast_changes, "../data/statcast_changes.csv")

cat("Conversions with Statcast in both seasons:", nrow(statcast_changes), "\n\n")
```

    ## Conversions with Statcast in both seasons: 136

``` r
cat("Averages, relief season vs starter season (starts only)\n")
```

    ## Averages, relief season vs starter season (starts only)

``` r
statcast_changes |>
  summarize(rp_PitchesPerOuting = mean(rp_PitchesPerOuting), sp_PitchesPerOuting = mean(sp_PitchesPerOuting),
            rp_OppShare = mean(rp_OppShare_PA), sp_OppShare = mean(sp_OppShare_PA),
            rp_Opp_KBB = mean(rp_Opp_KBB), sp_Opp_KBB = mean(sp_Opp_KBB),
            rp_Same_KBB = mean(rp_Same_KBB), sp_Same_KBB = mean(sp_Same_KBB),
            rp_Opp_Offspeed = mean(rp_Opp_Offspeed), sp_Opp_Offspeed = mean(sp_Opp_Offspeed),
            rp_Opp_Breaking = mean(rp_Opp_Breaking), sp_Opp_Breaking = mean(sp_Opp_Breaking),
            sp_Velo_per10 = mean(sp_Velo_per10, na.rm = TRUE)) |>
  pivot_longer(everything()) |>
  as.data.frame() |> print(digits = 3)
```

    ##                   name   value
    ## 1  rp_PitchesPerOuting 26.1312
    ## 2  sp_PitchesPerOuting 81.4413
    ## 3          rp_OppShare  0.5012
    ## 4          sp_OppShare  0.5852
    ## 5           rp_Opp_KBB  0.1132
    ## 6           sp_Opp_KBB  0.1112
    ## 7          rp_Same_KBB  0.1620
    ## 8          sp_Same_KBB  0.1484
    ## 9      rp_Opp_Offspeed  0.1661
    ## 10     sp_Opp_Offspeed  0.1878
    ## 11     rp_Opp_Breaking  0.2438
    ## 12     sp_Opp_Breaking  0.2455
    ## 13       sp_Velo_per10 -0.0822

``` r
change_measures <- c("Opp_KBB_change", "Same_KBB_change", "Opp_Whiff_change", "Opp_Offspeed_change",
                     "Opp_Breaking_change", "Opp_Fastball_change", "OppShare_change", "sp_Velo_per10",
                     "rp_Platoon_KBB_gap", "sp_Platoon_KBB_gap")

cat("\nEach measure vs starter SIERA vs league (Spearman), and its effect after relief SIERA (SD units)\n")
```

    ## 
    ## Each measure vs starter SIERA vs league (Spearman), and its effect after relief SIERA (SD units)

``` r
effect_table <- NULL
for (m in change_measures) {
  one <- statcast_changes |> filter(!is.na(.data[[m]]))
  fit <- lm(sp_SIERA_vs_lg ~ rp_SIERA_vs_lg + scale(one[[m]]), data = one)
  effect_table <- bind_rows(effect_table, data.frame(
    measure = m, n = nrow(one),
    spearman_SIERA = round(cor(one[[m]], one$sp_SIERA_vs_lg, method = "spearman"), 3),
    beta_after_rp_SIERA = round(coef(summary(fit))[3, 1], 3),
    p = coef(summary(fit))[3, 4]))
}
effect_table$q <- round(p.adjust(effect_table$p, method = "BH"), 3)
effect_table$p <- round(effect_table$p, 3)
print(effect_table)
```

    ##                measure   n spearman_SIERA beta_after_rp_SIERA     p     q
    ## 1       Opp_KBB_change 136         -0.383              -0.616 0.000 0.000
    ## 2      Same_KBB_change 136         -0.170              -0.405 0.000 0.000
    ## 3     Opp_Whiff_change 136         -0.152              -0.158 0.022 0.074
    ## 4  Opp_Offspeed_change 136         -0.010               0.052 0.447 0.639
    ## 5  Opp_Breaking_change 136         -0.009              -0.010 0.880 0.880
    ## 6  Opp_Fastball_change 136          0.101               0.083 0.220 0.410
    ## 7      OppShare_change 136         -0.028              -0.043 0.531 0.664
    ## 8        sp_Velo_per10 136          0.035              -0.022 0.747 0.831
    ## 9   rp_Platoon_KBB_gap 136         -0.098              -0.091 0.183 0.410
    ## 10  sp_Platoon_KBB_gap 136          0.108               0.079 0.246 0.410

``` r
# wOBA by time through the order as a starter, pooling every plate appearance
tto <- pitches |>
  filter(role == "starter", n_thruorder_pitcher %in% 1:3) |>
  inner_join(statcast_changes |> select(xMLBAMID, sp_season, Success),
             by = c("pitcher" = "xMLBAMID", "game_year" = "sp_season")) |>
  group_by(Success, time = n_thruorder_pitcher) |>
  summarize(PA = sum(woba_denom, na.rm = TRUE), wOBA = sum(woba_value, na.rm = TRUE) / sum(woba_denom, na.rm = TRUE),
            .groups = "drop")
cat("\nwOBA allowed by time through the order\n")
```

    ## 
    ## wOBA allowed by time through the order

``` r
print(as.data.frame(tto), digits = 3)
```

    ##                    Success time   PA  wOBA
    ## 1 league average or better    1 7813 0.298
    ## 2 league average or better    2 7258 0.332
    ## 3 league average or better    3 3598 0.317
    ## 4       worse than average    1 8819 0.335
    ## 5       worse than average    2 8331 0.355
    ## 6       worse than average    3 3878 0.362

Starters face more opposite-handed batters (58.5% of plate appearances
versus 50.1% in relief) and throw 81 pitches per start versus 26 per
relief outing. Fastball velocity falls only 0.08 mph per 10 pitches
within a start, and that slope does not predict the starter result once
relief SIERA is known (p = 0.75). The change in K-BB% against
opposite-handed hitters is the strongest signal (beta -0.62), though
like the other result measures it is partly mechanical. wOBA allowed
rises from the first to the second time through the order for both
groups, but the converts who did well fall back the third time (.298,
.332, and .317) while the others keep rising (.335, .355, and .362).

``` r
# wOBA by time through the order as a starter, by outcome
tto_plot <- tto |> mutate(time = c("1st", "2nd", "3rd")[time])
ggplot(tto_plot, aes(time, wOBA, color = Success, group = Success)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  scale_color_manual(values = c("league average or better" = "steelblue", "worse than average" = "red")) +
  labs(title = "wOBA allowed by time through the order in the starter season",
       subtitle = "Converted relievers, starts only, split by starter SIERA vs league",
       x = "Time through the order", y = "wOBA allowed", color = "As a starter") +
  theme_classic()
```

![](03_changes_after_conversion_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
ggsave("charts/tto_by_outcome.png", width = 8, height = 5.5, dpi = 150)
```

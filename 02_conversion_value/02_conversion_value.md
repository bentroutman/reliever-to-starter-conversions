Step 2: How Conversions Went
================

``` r
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(splines)
```

## WAR and WPA compared to staying in the bullpen

The comparison is each pitcher’s next season to what a similar reliever
who stayed in the bullpen did the next season.

``` r
role_seasons <- read_csv("../data/fg_role_seasons.csv", guess_max = 20000, show_col_types = FALSE) |>
  mutate(IP = floor(IP) + (IP %% 1) * 10 / 3)

# FanGraphs season totals (starter and reliever rows added together)
season_values <- role_seasons |>
  group_by(playerid, Season) |>
  summarize(xMLBAMID = first(xMLBAMID), G = sum(G), GS = sum(GS), IP = sum(IP), WAR = sum(WAR),
            .groups = "drop")
```

``` r
# Traded pitchers have TOT rows plus one row per team. Pitchers traded across leagues also get
# AL and NL subtotals labeled TOT, so the full season is the TOT row with the most innings
bref <- read_csv("../data/bref_win_probability.csv", show_col_types = FALSE) |>
  group_by(bref_id, Season) |>
  filter(n() == 1 | team_ID == "TOT") |>
  filter(IP == max(IP)) |>
  ungroup()
id_map <- read_csv("../data/id_map.csv", show_col_types = FALSE) |>
  distinct(bref_id, .keep_all = TRUE)
bref <- bref |>
  inner_join(id_map, by = "bref_id") |>
  select(xMLBAMID, Season, WPA = wpa_def, aLI = leverage_index_avg)

season_values <- season_values |>
  left_join(bref, by = c("xMLBAMID", "Season"))
cat("FanGraphs pitcher seasons matched to Baseball-Reference:", sum(!is.na(season_values$WPA)), "of",
    nrow(season_values), "\n")
```

    ## FanGraphs pitcher seasons matched to Baseball-Reference: 10529 of 10529

``` r
lg_rp <- role_seasons |>
  filter(role == "rel") |>
  group_by(Season) |>
  summarize(lg_rp_siera = weighted.mean(SIERA, TBF, na.rm = TRUE))
lg_sp <- role_seasons |>
  filter(role == "sta") |>
  group_by(Season) |>
  summarize(lg_sp_siera = weighted.mean(SIERA, TBF, na.rm = TRUE))

season_N <- season_values |>
  select(playerid, rp_season = Season, N_G = G, N_GS = GS, N_IP = IP, N_WAR = WAR, N_WPA = WPA,
         N_aLI = aLI)
season_next <- season_values |>
  select(playerid, next_season = Season, next_G = G, next_GS = GS, next_IP = IP, next_WAR = WAR,
         next_WPA = WPA, next_aLI = aLI)
```

``` r
# Same relief season definition as 01_data_and_sample.Rmd
relief_seasons <- role_seasons |>
  filter(role == "rel", TBF >= 100) |>
  left_join(lg_rp, by = "Season") |>
  transmute(playerid, rp_season = Season, rp_age = Age, rp_SIERA = SIERA, rp_SIERA_vs_lg = SIERA - lg_rp_siera) |>
  inner_join(season_N, by = c("playerid", "rp_season")) |>
  filter(N_GS / N_G <= 0.2)

# Baseline group: relievers who stayed relievers the next season and threw 20+ IP (not counting attempts)
stayers <- relief_seasons |>
  mutate(next_season = rp_season + 1) |>
  anti_join(read_csv("../data/conversions.csv", show_col_types = FALSE),
            by = c("playerid", "next_season" = "sp_season")) |>
  inner_join(season_next, by = c("playerid", "next_season")) |>
  filter(next_GS / next_G <= 0.2, next_IP >= 20)
cat("Stay-in-pen baseline seasons:", nrow(stayers), "\n")
```

    ## Stay-in-pen baseline seasons: 1775

``` r
# Expected next season value if the pitcher had stayed in the pen
war_model <- lm(next_WAR ~ N_WAR + rp_SIERA_vs_lg + N_IP + rp_age, data = stayers)
wpa_model <- lm(next_WPA ~ N_WPA + rp_SIERA_vs_lg + N_IP + rp_age, data = stayers)

# Split by how many starts the attempt lasted
archetype_cols <- c("playerid", "PlayerName", "rp_season", "sp_season", "sp_GS", "sp_IP",
                    "rp_FBVelo", "rp_NumPitches10", "rp_gmLI", "rp_TBFPerG", "rp_StuffPlus",
                    "rp_NumPlusPitches", "rp_KBB")
conversions <- read_csv("../data/conversions.csv", show_col_types = FALSE) |>
  select(all_of(archetype_cols)) |>
  mutate(attempt = ifelse(sp_GS >= 5, "5+ starts", "2 to 4 starts"))

attempts <- conversions |>
  inner_join(relief_seasons, by = c("playerid", "rp_season")) |>
  inner_join(season_next, by = c("playerid", "sp_season" = "next_season")) |>
  left_join(lg_sp, by = c("sp_season" = "Season")) |>
  mutate(Cushion = lg_sp_siera - rp_SIERA)

attempts$pen_WAR <- predict(war_model, newdata = attempts)
attempts$pen_WPA <- predict(wpa_model, newdata = attempts)
attempts <- attempts |>
  mutate(WAR_gain = next_WAR - pen_WAR,
         WPA_gain = next_WPA - pen_WPA)
```

``` r
# Starting history before the relief season (MLB from 2014, minors from 2006)
minors <- read_csv("../data/fg_minors.csv", show_col_types = FALSE)
attempts$prev_MLB_GS <- 0
attempts$MiLB_GS <- 0
for (i in 1:nrow(attempts)) {
  before <- season_values |> filter(playerid == attempts$playerid[i], Season < attempts$rp_season[i])
  player_minors <- minors |> filter(playerid == as.character(attempts$playerid[i]),
      Season <= attempts$rp_season[i])
  attempts$prev_MLB_GS[i] <- sum(before$GS)
  attempts$MiLB_GS[i] <- sum(player_minors$GS)
}

write_csv(attempts, "../data/value_attempts.csv")

cat("Stay-in-pen models fit on", nrow(stayers), "reliever seasons followed by another relief season (20+ IP)\n")
```

    ## Stay-in-pen models fit on 1775 reliever seasons followed by another relief season (20+ IP)

``` r
cat("WAR model R^2:", round(summary(war_model)$r.squared, 3),
    " WPA model R^2:", round(summary(wpa_model)$r.squared, 3), "\n\n")
```

    ## WAR model R^2: 0.136  WPA model R^2: 0.08

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
cat("\nAttempts, actual next season vs expected if they had stayed in the pen\n")
```

    ## 
    ## Attempts, actual next season vs expected if they had stayed in the pen

``` r
attempts |>
  group_by(attempt) |>
  summarize(n = n(), N_IP = mean(N_IP), next_IP = mean(next_IP), N_aLI = mean(N_aLI, na.rm = TRUE),
            next_aLI = mean(next_aLI, na.rm = TRUE), N_WAR = mean(N_WAR), next_WAR = mean(next_WAR),
            pen_WAR = mean(pen_WAR), share_WAR_gain = mean(WAR_gain > 0), WAR_gain_se = sd(WAR_gain) / sqrt(n()),
            WAR_gain = mean(WAR_gain), WPA_gain = mean(WPA_gain, na.rm = TRUE)) |>
  as.data.frame() |> print(digits = 3)
```

    ##         attempt   n N_IP next_IP N_aLI next_aLI N_WAR next_WAR pen_WAR
    ## 1 2 to 4 starts  23 50.4    37.9 0.854    0.840 0.169  -0.0702   0.386
    ## 2     5+ starts 122 57.2    99.6 0.914    0.917 0.378   1.0764   0.423
    ##   share_WAR_gain WAR_gain_se WAR_gain WPA_gain
    ## 1          0.130      0.1072   -0.456   -0.761
    ## 2          0.705      0.0978    0.654   -0.134

``` r
cat("\nAll attempts together\n")
```

    ## 
    ## All attempts together

``` r
attempts |>
  summarize(n = n(), next_WAR = mean(next_WAR), pen_WAR = mean(pen_WAR),
            share_WAR_gain = mean(WAR_gain > 0), share_WPA_gain = mean(WPA_gain > 0, na.rm = TRUE),
            WAR_gain_se = sd(WAR_gain) / sqrt(n()),
            WPA_gain_se = sd(WPA_gain, na.rm = TRUE) / sqrt(sum(!is.na(WPA_gain))),
            WAR_gain = mean(WAR_gain), WPA_gain = mean(WPA_gain, na.rm = TRUE)) |>
  as.data.frame() |> print(digits = 3)
```

    ##     n next_WAR pen_WAR share_WAR_gain share_WPA_gain WAR_gain_se WPA_gain_se
    ## 1 145    0.895   0.417          0.614          0.407      0.0905        0.09
    ##   WAR_gain WPA_gain
    ## 1    0.478   -0.233

``` r
# Archetypes, measured in the relief season
attempts <- attempts |>
  mutate(Velo = ifelse(rp_FBVelo >= 95, "95+ mph", "under 95 mph"),
         Arsenal = ifelse(rp_NumPitches10 >= 3, "3+ pitches at 10%", "2 pitches"),
         Leverage = ifelse(rp_gmLI >= 1.3, "high leverage (gmLI 1.3+)", "lower leverage"),
         Length = ifelse(rp_TBFPerG >= 6, "long relief (6+ TBF per G)", "short relief"),
         Age = ifelse(rp_age <= 25, "25 and under", "26 and older"),
         ReliefQuality = ifelse(rp_SIERA_vs_lg < 0, "SIERA better than league RP", "SIERA worse than league RP"),
         StartingHistory = case_when(prev_MLB_GS >= 20 ~ "former MLB starter (20+ GS)",
                                     MiLB_GS >= 50 ~ "MiLB starter (50+ GS)",
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
    summarize(n = n(), share_5plus_GS = mean(attempt == "5+ starts"), next_IP = mean(next_IP),
              next_WAR = mean(next_WAR), pen_WAR = mean(pen_WAR), share_WAR_gain = mean(WAR_gain > 0),
              WAR_gain_se = sd(WAR_gain) / sqrt(n()), WPA_gain_se = sd(WPA_gain, na.rm = TRUE) / sqrt(n()),
              WAR_gain = mean(WAR_gain), WPA_gain = mean(WPA_gain, na.rm = TRUE)) |>
    mutate(archetype = archetype, .before = group)
  archetype_table <- bind_rows(archetype_table, group_summary)
}
archetype_table <- archetype_table |> mutate(across(where(is.numeric), ~ round(.x, 3)))
write_csv(archetype_table, "results/value_by_archetype.csv")
print(as.data.frame(archetype_table))
```

    ##          archetype                           group   n share_5plus_GS next_IP
    ## 1             Velo                         95+ mph  40          0.825  94.842
    ## 2             Velo                    under 95 mph 105          0.848  87.940
    ## 3          Arsenal                       2 pitches  27          0.778  86.370
    ## 4          Arsenal               3+ pitches at 10% 118          0.856  90.638
    ## 5         Leverage       high leverage (gmLI 1.3+)  30          0.900 109.489
    ## 6         Leverage                  lower leverage 115          0.826  84.719
    ## 7           Length      long relief (6+ TBF per G)  63          0.873  87.159
    ## 8           Length                    short relief  82          0.817  91.907
    ## 9              Age                    25 and under  46          0.826  90.123
    ## 10             Age                    26 and older  99          0.848  89.714
    ## 11   ReliefQuality     SIERA better than league RP  69          0.812  93.918
    ## 12   ReliefQuality      SIERA worse than league RP  76          0.868  86.145
    ## 13 StartingHistory           MiLB starter (50+ GS)  73          0.849  90.589
    ## 14 StartingHistory     former MLB starter (20+ GS)  47          0.894  88.709
    ## 15 StartingHistory         little starting history  25          0.720  89.800
    ## 16       StuffPlus                     Stuff+ 100+  37          0.946 112.550
    ## 17       StuffPlus                Stuff+ under 100  30          0.833  81.778
    ## 18     PlusPitches 0 or 1 pitches with Stuff+ 100+  26          0.808  76.115
    ## 19     PlusPitches     2+ pitches with Stuff+ 100+  41          0.951 113.138
    ##    next_WAR pen_WAR share_WAR_gain WAR_gain_se WPA_gain_se WAR_gain WPA_gain
    ## 1     1.232   0.577          0.700       0.191       0.180    0.655   -0.128
    ## 2     0.766   0.356          0.581       0.101       0.104    0.410   -0.273
    ## 3     0.805   0.402          0.556       0.237       0.261    0.402   -0.483
    ## 4     0.915   0.420          0.627       0.098       0.093    0.495   -0.176
    ## 5     1.535   0.621          0.700       0.230       0.220    0.914    0.075
    ## 6     0.728   0.364          0.591       0.095       0.097    0.364   -0.314
    ## 7     0.762   0.351          0.635       0.117       0.131    0.411   -0.363
    ## 8     0.996   0.467          0.598       0.133       0.123    0.529   -0.134
    ## 9     0.973   0.475          0.609       0.190       0.151    0.498   -0.127
    ## 10    0.858   0.390          0.616       0.100       0.112    0.468   -0.282
    ## 11    1.271   0.631          0.609       0.157       0.135    0.640   -0.186
    ## 12    0.553   0.223          0.618       0.095       0.121    0.331   -0.276
    ## 13    0.880   0.416          0.630       0.119       0.118    0.464   -0.235
    ## 14    0.875   0.383          0.553       0.178       0.185    0.491   -0.300
    ## 15    0.973   0.481          0.680       0.214       0.188    0.492   -0.102
    ## 16    1.794   0.573          0.865       0.195       0.163    1.222    0.262
    ## 17    0.567   0.373          0.567       0.174       0.185    0.194   -0.413
    ## 18    0.701   0.367          0.538       0.218       0.220    0.334   -0.356
    ## 19    1.589   0.557          0.854       0.184       0.150    1.032    0.160

``` r
# actual next season WAR vs expected WAR in the pen
known <- c("Garrett Crochet", "Reynaldo López", "Jordan Hicks", "Michael King", "Seth Lugo",
           "Clay Holmes", "A.J. Puk", "Zack Littell", "Jeffrey Springs")
ggplot(attempts, aes(pen_WAR, next_WAR, color = attempt)) +
  geom_abline(slope = 1, intercept = 0, color = "gray", linetype = "dashed") +
  geom_point(alpha = 0.7, size = 2) +
  geom_text(data = attempts |> filter(PlayerName %in% known), aes(label = PlayerName),
            size = 3, vjust = -1, show.legend = FALSE) +
  scale_color_manual(values = c("5+ starts" = "steelblue", "2 to 4 starts" = "red")) +
  labs(title = "Next season WAR after a move to the rotation vs expected WAR in the pen",
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
# Average gain by archetype, WAR and WPA side by side
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

## WAA

WAR compares to replacement level, so I also checked measures that
compare to an average pitcher. `WAA_fg` is FanGraphs WAR minus the
average pace for the same role and season. `WAA_br` is from
Baseball-Reference.

``` r
role_seasons <- read_csv("../data/fg_role_seasons.csv", guess_max = 20000, show_col_types = FALSE) |>
  mutate(IP = floor(IP) + (IP %% 1) * 10 / 3)

# FanGraphs WAA by role: WAR minus what an average pitcher in the same role and season earns per inning
role_pace <- role_seasons |>
  group_by(Season, role) |>
  summarize(war_per_ip = sum(WAR, na.rm = TRUE) / sum(IP, na.rm = TRUE), .groups = "drop")
league <- role_seasons |>
  filter(!is.na(SIERA)) |>
  group_by(Season) |>
  summarize(lg_SIERA = weighted.mean(SIERA, TBF), .groups = "drop") |>
  left_join(role_seasons |> group_by(Season) |> summarize(runs_per_win = sum(RAR, na.rm = TRUE) / sum(WAR,
      na.rm = TRUE)),
            by = "Season")
league_rp <- role_seasons |>
  filter(role == "rel") |>
  group_by(Season) |>
  summarize(lg_rp_siera = weighted.mean(SIERA, TBF, na.rm = TRUE))

fg_season <- role_seasons |>
  left_join(role_pace, by = c("Season", "role")) |>
  mutate(WAA_fg_row = WAR - war_per_ip * IP) |>
  group_by(playerid, Season) |>
  summarize(SIERA = weighted.mean(SIERA, TBF, na.rm = TRUE), xMLBAMID = first(xMLBAMID), Age = first(Age),
            G = sum(G), GS = sum(GS), IP = sum(IP), TBF = sum(TBF), WAR = sum(WAR), WAA_fg = sum(WAA_fg_row),
            .groups = "drop") |>
  left_join(league, by = "Season") |>
  mutate(skill_wins = (lg_SIERA - SIERA) * IP / 9 / runs_per_win)
```

``` r
# Baseball-Reference value (WAA, leverage adjustment) and win probability (WPA, aLI)
bref_value <- read_csv("../data/bref_value_pitching.csv", show_col_types = FALSE) |>
  mutate(IP_br = floor(p_ip) + (p_ip %% 1) * 10 / 3) |>
  group_by(bref_id, Season) |>
  filter(n() == 1 | team_ID == "TOT" | grepl("^[0-9]TM$", team_ID)) |>
  filter(IP_br == max(IP_br)) |>
  slice(1) |>
  ungroup() |>
  transmute(bref_id, Season, br_G = p_g, br_GS = p_gs, br_IP = IP_br, WAA_br = p_waa,
      WAA_adj_br = coalesce(p_waa_adj, 0),
            RAA_br = p_raa, WAR_br = p_war, RA9role_br = p_ra9_role)
bref_wp <- read_csv("../data/bref_win_probability.csv", show_col_types = FALSE) |>
  group_by(bref_id, Season) |>
  filter(n() == 1 | team_ID == "TOT") |>
  filter(IP == max(IP)) |>
  slice(1) |>
  ungroup() |>
  select(bref_id, Season, WPA = wpa_def, aLI = leverage_index_avg)
id_map <- read_csv("../data/id_map.csv", show_col_types = FALSE) |> distinct(bref_id, .keep_all = TRUE)

seasons <- fg_season |>
  inner_join(id_map |> select(xMLBAMID, bref_id), by = "xMLBAMID") |>
  inner_join(bref_value, by = c("bref_id", "Season")) |>
  inner_join(bref_wp, by = c("bref_id", "Season")) |>
  mutate(WAA_br_adj = WAA_br + WAA_adj_br)

cat("FanGraphs pitcher seasons matched to Baseball-Reference value and win probability:", nrow(seasons), "of",
    nrow(fg_season), "\n\n")
```

    ## FanGraphs pitcher seasons matched to Baseball-Reference value and win probability: 10528 of 10528

``` r
cat("1. Is the Baseball-Reference WAA baseline adjusted for role? IP weighted mean WAA per 100 IP\n")
```

    ## 1. Is the Baseball-Reference WAA baseline adjusted for role? IP weighted mean WAA per 100 IP

``` r
pure <- bref_value |> filter(br_IP >= 20)
cat("Pure relievers (no starts):", round(sum(pure$WAA_br[pure$br_GS == 0]) / sum(pure$br_IP[pure$br_GS == 0]) * 100, 2),
    " pure starters (all games starts):", round(sum(pure$WAA_br[pure$br_GS == pure$br_G]) / sum(pure$br_IP[pure$br_GS == pure$br_G]) * 100, 2),
    " mean role adjustment (RA9 role) relievers:", round(mean(pure$RA9role_br[pure$br_GS == 0]), 2), " starters:",
    round(mean(pure$RA9role_br[pure$br_GS == pure$br_G]), 2), "\n")
```

    ## Pure relievers (no starts): 0.25  pure starters (all games starts): 0.54  mean role adjustment (RA9 role) relievers: -0.31  starters: 0.2

``` r
cat("FanGraphs WAR per 100 IP for comparison, relievers:",
    round(sum(seasons$WAR[seasons$GS == 0 & seasons$IP >= 20]) / sum(seasons$IP[seasons$GS == 0 & seasons$IP >= 20]) * 100, 2),
    " starters:", round(sum(seasons$WAR[seasons$GS == seasons$G & seasons$IP >= 20]) / sum(seasons$IP[seasons$GS == seasons$G & seasons$IP >= 20]) * 100, 2), "\n\n")
```

    ## FanGraphs WAR per 100 IP for comparison, relievers: 0.86  starters: 1.52

``` r
# Relievers who stayed relievers (same definition as 06)
relievers <- seasons |> filter(GS / G <= 0.2, TBF >= 100)
next_year <- seasons |>
  transmute(playerid, Season, next_G = G, next_GS = GS, next_IP = IP, next_WAR = WAR, next_WAA_fg = WAA_fg,
            next_WAA_br = WAA_br, next_WAA_br_adj = WAA_br_adj, next_WPA = WPA)
stayers <- relievers |>
  mutate(next_season = Season + 1) |>
  anti_join(read_csv("../data/conversions.csv", show_col_types = FALSE),
            by = c("playerid", "next_season" = "sp_season")) |>
  inner_join(next_year, by = c("playerid", "next_season" = "Season")) |>
  filter(next_GS / next_G <= 0.2, next_IP >= 20)

measures <- c("WAR", "WAA_fg", "WAA_br", "WAA_br_adj", "WPA")
cat("2. How well each measure repeats for", nrow(stayers), "relievers who stayed relievers\n")
```

    ## 2. How well each measure repeats for 1790 relievers who stayed relievers

``` r
cat("Correlation of this season with next season. Totals include innings pitched, rates are per 65 IP. Standard error is about",
    round(1 / sqrt(nrow(stayers) - 3), 3), "\n")
```

    ## Correlation of this season with next season. Totals include innings pitched, rates are per 65 IP. Standard error is about 0.024

``` r
stability <- NULL
for (m in measures) {
  this_total <- stayers[[m]]
  next_total <- stayers[[paste0("next_", m)]]
  this_rate <- this_total / stayers$IP * 65
  next_rate <- next_total / stayers$next_IP * 65
  stability <- bind_rows(stability, data.frame(
    measure = m, r_total = round(cor(this_total, next_total), 3), r_rate_per_65_IP = round(cor(this_rate,
      next_rate), 3),
    r_with_skill_wins_same_season = round(cor(this_total, stayers$skill_wins), 3)))
}
print(stability, row.names = FALSE)
```

    ##     measure r_total r_rate_per_65_IP r_with_skill_wins_same_season
    ##         WAR   0.320            0.287                         0.780
    ##      WAA_fg   0.316            0.290                         0.766
    ##      WAA_br   0.113            0.076                         0.521
    ##  WAA_br_adj   0.127            0.092                         0.537
    ##         WPA   0.165            0.119                         0.545

``` r
cat("(r_with_skill_wins_same_season shows how much of each measure is the SIERA based skill signal)\n\n")
```

    ## (r_with_skill_wins_same_season shows how much of each measure is the SIERA based skill signal)

``` r
# Conversion gains: actual next season minus the stay-in-the-pen projection, same method as 02_conversion_value.Rmd
cat("3. Conversion gain by measure (actual starter season minus the stay-in-the-pen projection)\n")
```

    ## 3. Conversion gain by measure (actual starter season minus the stay-in-the-pen projection)

``` r
lg_rp <- league_rp
relief_info <- role_seasons |>
  filter(role == "rel") |>
  left_join(lg_rp, by = "Season") |>
  transmute(playerid, Season, rp_SIERA_vs_lg = SIERA - lg_rp_siera, rp_age = Age)
stay_fit <- stayers |>
  left_join(relief_info, by = c("playerid", "Season"))
attempts <- read_csv("../data/value_attempts.csv", show_col_types = FALSE) |>
  select(playerid, PlayerName, rp_season, sp_season, attempt, rp_StuffPlus, Cushion, WAR_gain_06 = WAR_gain)
N_values <- seasons |>
  left_join(relief_info, by = c("playerid", "Season")) |>
  transmute(playerid, rp_season = Season, rp_SIERA_vs_lg, rp_age, N_IP = IP, N_WAR = WAR, N_WAA_fg = WAA_fg,
      N_WAA_br = WAA_br,
            N_WAA_br_adj = WAA_br_adj, N_WPA = WPA)
sp_values <- seasons |>
  transmute(playerid, sp_season = Season, sp_WAR = WAR, sp_WAA_fg = WAA_fg, sp_WAA_br = WAA_br,
      sp_WAA_br_adj = WAA_br_adj,
            sp_WPA = WPA)
attempts <- attempts |>
  inner_join(N_values, by = c("playerid", "rp_season")) |>
  inner_join(sp_values, by = c("playerid", "sp_season"))

gain_measures <- c("WAR", "WAA_fg", "WAA_br", "WAA_br_adj", "WPA")
for (m in gain_measures) {
  fit <- lm(as.formula(paste0("next_", m, " ~ ", m, " + rp_SIERA_vs_lg + IP + rp_age")), data = stay_fit)
  newdata <- attempts
  newdata[[m]] <- attempts[[paste0("N_", m)]]
  newdata$IP <- attempts$N_IP
  attempts[[paste0("pen_", m)]] <- predict(fit, newdata = newdata)
  attempts[[paste0(m, "_gain")]] <- attempts[[paste0("sp_", m)]] - attempts[[paste0("pen_", m)]]
}
cat("Check against 02_conversion_value.Rmd: mean WAR gain here", round(mean(attempts$WAR_gain), 3), " there",
    round(mean(attempts$WAR_gain_06), 3),
    " (n =", nrow(attempts), ")\n\n")
```

    ## Check against 02_conversion_value.Rmd: mean WAR gain here 0.479  there 0.478  (n = 145 )

``` r
# Summary of gains for a group of attempts
gain_summary <- function(data, label) {
  rows <- NULL
  for (m in gain_measures) {
    gain <- data[[paste0(m, "_gain")]]
    rows <- bind_rows(rows, data.frame(group = label, measure = m, n = length(gain), mean_gain = round(mean(gain),
      3),
                                       se = round(sd(gain) / sqrt(length(gain)), 3),
                                         share_gaining = round(mean(gain > 0), 2)))
  }
  rows
}
attempts <- attempts |>
  mutate(CushionGroup = case_when(Cushion < 0.5 ~ "cushion under 0.5", Cushion < 1 ~ "cushion 0.5 to 1",
      TRUE ~ "cushion 1+"),
         StuffGroup = case_when(is.na(rp_StuffPlus) ~ NA_character_, rp_StuffPlus >= 100 ~ "Stuff+ 100+",
           TRUE ~ "Stuff+ under 100"))
gain_table <- gain_summary(attempts, "all attempts")
gain_table <- bind_rows(gain_table, gain_summary(attempts |> filter(attempt == "5+ starts"), "5+ starts"))
gain_table <- bind_rows(gain_table, gain_summary(attempts |> filter(attempt == "2 to 4 starts"), "2 to 4 starts"))
for (g in c("cushion under 0.5", "cushion 0.5 to 1", "cushion 1+")) {
  gain_table <- bind_rows(gain_table, gain_summary(attempts |> filter(CushionGroup == g), g))
}
for (g in c("Stuff+ 100+", "Stuff+ under 100")) {
  gain_table <- bind_rows(gain_table, gain_summary(attempts |> filter(StuffGroup == g), g))
}
write_csv(gain_table, "results/waa_gains.csv")
print(as.data.frame(gain_table |> tidyr::pivot_wider(names_from = measure, values_from = c(mean_gain, se,
    share_gaining)) |>
                      select(group, n, starts_with("mean_gain"))), row.names = FALSE)
```

    ##              group   n mean_gain_WAR mean_gain_WAA_fg mean_gain_WAA_br
    ##       all attempts 145         0.479           -0.217            0.044
    ##          5+ starts 122         0.655           -0.171            0.170
    ##      2 to 4 starts  23        -0.454           -0.459           -0.621
    ##  cushion under 0.5  76         0.383           -0.257           -0.112
    ##   cushion 0.5 to 1  36         0.339           -0.353            0.023
    ##         cushion 1+  33         0.853            0.026            0.428
    ##        Stuff+ 100+  37         1.223            0.233            0.769
    ##   Stuff+ under 100  30         0.196           -0.407           -0.227
    ##  mean_gain_WAA_br_adj mean_gain_WPA
    ##                 0.018        -0.231
    ##                 0.137        -0.132
    ##                -0.611        -0.755
    ##                -0.108        -0.212
    ##                -0.022        -0.401
    ##                 0.353        -0.088
    ##                 0.704         0.266
    ##                -0.227        -0.408

``` r
cat("\nStandard errors and shares gaining\n")
```

    ## 
    ## Standard errors and shares gaining

``` r
print(as.data.frame(gain_table |> filter(measure %in% c("WAR", "WAA_fg", "WAA_br", "WAA_br_adj")) |>
                      select(group, n, measure, se, share_gaining) |>
                      tidyr::pivot_wider(id_cols = c(group, n), names_from = measure, values_from = c(se,
                        share_gaining))), row.names = FALSE)
```

    ##              group   n se_WAR se_WAA_fg se_WAA_br se_WAA_br_adj
    ##       all attempts 145  0.090     0.067     0.098         0.096
    ##          5+ starts 122  0.098     0.076     0.109         0.107
    ##      2 to 4 starts  23  0.107     0.103     0.154         0.157
    ##  cushion under 0.5  76  0.112     0.085     0.123         0.119
    ##   cushion 0.5 to 1  36  0.195     0.144     0.222         0.219
    ##         cushion 1+  33  0.208     0.145     0.204         0.207
    ##        Stuff+ 100+  37  0.195     0.153     0.181         0.178
    ##   Stuff+ under 100  30  0.174     0.127     0.186         0.180
    ##  share_gaining_WAR share_gaining_WAA_fg share_gaining_WAA_br
    ##               0.61                 0.34                 0.43
    ##               0.70                 0.39                 0.48
    ##               0.13                 0.09                 0.17
    ##               0.59                 0.30                 0.36
    ##               0.53                 0.33                 0.44
    ##               0.76                 0.42                 0.58
    ##               0.86                 0.51                 0.76
    ##               0.57                 0.30                 0.40
    ##  share_gaining_WAA_br_adj
    ##                      0.41
    ##                      0.47
    ##                      0.13
    ##                      0.34
    ##                      0.42
    ##                      0.58
    ##                      0.76
    ##                      0.37

``` r
cat("\n4. Do the measures agree on who gained? Spearman correlation of conversion gains\n")
```

    ## 
    ## 4. Do the measures agree on who gained? Spearman correlation of conversion gains

``` r
gain_matrix <- attempts[, paste0(gain_measures, "_gain")]
print(round(cor(gain_matrix, method = "spearman"), 2))
```

    ##                 WAR_gain WAA_fg_gain WAA_br_gain WAA_br_adj_gain WPA_gain
    ## WAR_gain            1.00        0.83        0.72            0.71     0.64
    ## WAA_fg_gain         0.83        1.00        0.63            0.64     0.62
    ## WAA_br_gain         0.72        0.63        1.00            0.99     0.79
    ## WAA_br_adj_gain     0.71        0.64        0.99            1.00     0.80
    ## WPA_gain            0.64        0.62        0.79            0.80     1.00

## Checking the bullpen baseline

The gains depend on the baseline, so I compared the linear model to a
flexible model and to matched controls.

``` r
role_seasons <- read_csv("../data/fg_role_seasons.csv", guess_max = 20000, show_col_types = FALSE) |>
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
```

``` r
# Stayers, same definition as 02_conversion_value.Rmd
next_year <- fg_season |> transmute(playerid, Season, next_G = G, next_GS = GS, next_IP = IP, next_WAR = WAR,
    next_WAA_fg = WAA_fg)
stayers <- fg_season |>
  filter(GS / G <= 0.2, TBF >= 100) |>
  left_join(relief_info, by = c("playerid", "Season")) |>
  mutate(next_season = Season + 1) |>
  anti_join(read_csv("../data/conversions.csv", show_col_types = FALSE),
            by = c("playerid", "next_season" = "sp_season")) |>
  inner_join(next_year, by = c("playerid", "next_season" = "Season")) |>
  filter(next_GS / next_G <= 0.2, next_IP >= 20) |>
  left_join(league_sp, by = "Season") |>
  mutate(cushion = lg_sp_siera - SIERA, rp_age = Age) |>
  filter(!is.na(rp_SIERA_vs_lg), !is.na(rp_gmLI))

attempts <- read_csv("../data/value_attempts.csv", show_col_types = FALSE) |>
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

cat("Stayers:", nrow(stayers), "reliever seasons followed by another relief season of 20+ IP. Attempts:",
    nrow(attempts), "\n\n")
```

    ## Stayers: 1790 reliever seasons followed by another relief season of 20+ IP. Attempts: 145

``` r
# 1. Calibration of the pen baseline
cat("1. Calibration of the stay-in-the-pen baseline for next season WAR. Cross-fitted by pitcher (10 folds) so every stayer is predicted by a model that never saw him.\n")
```

    ## 1. Calibration of the stay-in-the-pen baseline for next season WAR. Cross-fitted by pitcher (10 folds) so every stayer is predicted by a model that never saw him.

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
  fit_lin <- lm(next_WAR ~ WAR + rp_SIERA_vs_lg + IP + rp_age, data = train)
  fit_flex <- lm(next_WAR ~ ns(WAR, 3) + ns(rp_SIERA_vs_lg, 3) + ns(IP, 3) + ns(rp_age, 2) + ns(rp_gmLI, 2),
      data = train)
  stayers$pred_linear[stayers$fold == k] <- predict(fit_lin, newdata = test)
  stayers$pred_flex[stayers$fold == k] <- predict(fit_flex, newdata = test)
}
cat("Cross-fitted RMSE of next season WAR: linear", round(sqrt(mean((stayers$next_WAR - stayers$pred_linear)^2)),
    3), " flexible", round(sqrt(mean((stayers$next_WAR - stayers$pred_flex)^2)), 3),
    " no model", round(sd(stayers$next_WAR), 3), "\n")
```

    ## Cross-fitted RMSE of next season WAR: linear 0.69  flexible 0.686  no model 0.74

``` r
cat("Cross-fitted R^2: linear", round(1 - sum((stayers$next_WAR - stayers$pred_linear)^2) / sum((stayers$next_WAR - mean(stayers$next_WAR))^2), 3),
    " flexible", round(1 - sum((stayers$next_WAR - stayers$pred_flex)^2) / sum((stayers$next_WAR - mean(stayers$next_WAR))^2), 3), "\n\n")
```

    ## Cross-fitted R^2: linear 0.13  flexible 0.14

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
    ##    1 179           0.03        0.18       0.149     0.024   0.045
    ##    2 179           0.20        0.27       0.068     0.047   0.049
    ##    3 179           0.29        0.30       0.010     0.026   0.049
    ##    4 179           0.35        0.35      -0.007     0.009   0.047
    ##    5 179           0.41        0.34      -0.073    -0.024   0.047
    ##    6 179           0.48        0.43      -0.046     0.010   0.050
    ##    7 179           0.54        0.39      -0.158    -0.105   0.046
    ##    8 179           0.63        0.55      -0.079    -0.029   0.049
    ##    9 179           0.75        0.75      -0.001     0.006   0.063
    ##   10 179           1.00        1.15       0.155     0.050   0.064

``` r
cat("\nBias by cushion group among stayers (these are the groups the archetype results use)\n")
```

    ## 
    ## Bias by cushion group among stayers (these are the groups the archetype results use)

``` r
print(as.data.frame(stayers |>
                      mutate(group = case_when(cushion < 0.5 ~ "cushion under 0.5",
                        cushion < 1 ~ "cushion 0.5 to 1", TRUE ~ "cushion 1+")) |>
                      group_by(group) |>
                      summarize(n = n(), bias_linear = round(mean(next_WAR - pred_linear), 3),
                        bias_flex = round(mean(next_WAR - pred_flex), 3),
                                se = round(sd(next_WAR - pred_linear) / sqrt(n()), 3), .groups = "drop")),
                                  row.names = FALSE)
```

    ##              group   n bias_linear bias_flex    se
    ##   cushion 0.5 to 1 525      -0.076    -0.028 0.029
    ##         cushion 1+ 543       0.048     0.023 0.033
    ##  cushion under 0.5 722       0.024     0.006 0.023

``` r
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

cat("\nConversion WAR gain under three baselines (mean, standard error). Linear is the one in 02_conversion_value.Rmd.\n")
```

    ## 
    ## Conversion WAR gain under three baselines (mean, standard error). Linear is the one in 02_conversion_value.Rmd.

``` r
gain_summary <- function(data, label) {
  data.frame(group = label, n = nrow(data),
             linear = sprintf("%.2f (%.2f)", mean(data$gain_linear), sd(data$gain_linear) / sqrt(nrow(data))),
             flexible = sprintf("%.2f (%.2f)", mean(data$gain_flex), sd(data$gain_flex) / sqrt(nrow(data))),
             matched_controls = sprintf("%.2f (%.2f)", mean(data$gain_matched),
               sd(data$gain_matched) / sqrt(nrow(data))))
}
gains <- bind_rows(gain_summary(attempts, "all attempts"), gain_summary(attempts |>
    filter(attempt == "5+ starts"), "5+ starts"),
                   gain_summary(attempts |> filter(attempt == "2 to 4 starts"), "2 to 4 starts"),
                   gain_summary(attempts |> filter(CushionGroup == "cushion under 0.5"), "cushion under 0.5"),
                   gain_summary(attempts |> filter(CushionGroup == "cushion 0.5 to 1"), "cushion 0.5 to 1"),
                   gain_summary(attempts |> filter(CushionGroup == "cushion 1+"), "cushion 1+"),
                   gain_summary(attempts |> filter(StuffGroup == "Stuff+ 100+"), "Stuff+ 100+"),
                   gain_summary(attempts |> filter(StuffGroup == "Stuff+ under 100"), "Stuff+ under 100"))
print(gains, row.names = FALSE)
```

    ##              group   n       linear     flexible matched_controls
    ##       all attempts 145  0.48 (0.09)  0.45 (0.09)      0.51 (0.09)
    ##          5+ starts 122  0.65 (0.10)  0.62 (0.10)      0.69 (0.10)
    ##      2 to 4 starts  23 -0.45 (0.11) -0.47 (0.10)     -0.44 (0.09)
    ##  cushion under 0.5  76  0.38 (0.11)  0.33 (0.11)      0.36 (0.12)
    ##   cushion 0.5 to 1  36  0.34 (0.19)  0.33 (0.17)      0.44 (0.19)
    ##         cushion 1+  33  0.85 (0.21)  0.84 (0.20)      0.94 (0.21)
    ##        Stuff+ 100+  37  1.22 (0.19)  1.18 (0.19)      1.30 (0.19)
    ##   Stuff+ under 100  30  0.20 (0.17)  0.23 (0.17)      0.27 (0.17)

``` r
cat("Correlation of the attempt level gains: linear vs flexible", round(cor(attempts$gain_linear,
    attempts$gain_flex), 3), " linear vs matched", round(cor(attempts$gain_linear, attempts$gain_matched), 3),
      "\n")
```

    ## Correlation of the attempt level gains: linear vs flexible 0.988  linear vs matched 0.989

``` r
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
```

    ## Placebo (all 1790 stayers against their matched controls): mean gain 0.015  standard error 0.016

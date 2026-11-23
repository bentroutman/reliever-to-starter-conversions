# Checks whether moving to the rotation added value, using WAR (FanGraphs) and WPA (Baseball-Reference)
# The comparison is each pitcher's next season vs what a similar reliever who stayed in the pen did the next season
# Run after build_sample.R and pull_win_probability.R
# Outputs: results/value_overall.txt, results/value_by_archetype.csv, data/value_attempts.csv, and two charts

library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)

role_seasons <- read_csv("data/fg_role_seasons.csv", guess_max = 20000, show_col_types = FALSE) |>
  mutate(IP = floor(IP) + (IP %% 1) * 10 / 3)

# FanGraphs season totals (starter and reliever rows added together)
season_values <- role_seasons |>
  group_by(playerid, Season) |>
  summarize(xMLBAMID = first(xMLBAMID), G = sum(G), GS = sum(GS), IP = sum(IP), WAR = sum(WAR),
            .groups = "drop")

# Traded pitchers have TOT rows plus one row per team. Pitchers traded across leagues also get
# AL and NL subtotals labeled TOT, so the full season is the TOT row with the most innings
bref <- read_csv("data/bref_win_probability.csv", show_col_types = FALSE) |>
  group_by(bref_id, Season) |>
  filter(n() == 1 | team_ID == "TOT") |>
  filter(IP == max(IP)) |>
  ungroup()
id_map <- read_csv("data/id_map.csv", show_col_types = FALSE) |>
  distinct(bref_id, .keep_all = TRUE)
bref <- bref |>
  inner_join(id_map, by = "bref_id") |>
  select(xMLBAMID, Season, WPA = wpa_def, aLI = leverage_index_avg)

season_values <- season_values |>
  left_join(bref, by = c("xMLBAMID", "Season"))
cat("FanGraphs pitcher seasons matched to Baseball-Reference:", sum(!is.na(season_values$WPA)), "of",
    nrow(season_values), "\n")

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

# Same relief season definition as build_sample.R
relief_seasons <- role_seasons |>
  filter(role == "rel", TBF >= 100) |>
  left_join(lg_rp, by = "Season") |>
  transmute(playerid, rp_season = Season, rp_age = Age, rp_SIERA = SIERA, rp_SIERA_vs_lg = SIERA - lg_rp_siera) |>
  inner_join(season_N, by = c("playerid", "rp_season")) |>
  filter(N_GS / N_G <= 0.2)

# Baseline group: relievers who stayed relievers the next season and threw 20+ IP
stayers <- relief_seasons |>
  mutate(next_season = rp_season + 1) |>
  inner_join(season_next, by = c("playerid", "next_season")) |>
  filter(next_GS / next_G <= 0.2, next_IP >= 20)
cat("Stay-in-pen baseline seasons:", nrow(stayers), "\n")

# Expected next season value if the pitcher had stayed in the pen
war_model <- lm(next_WAR ~ N_WAR + rp_SIERA_vs_lg + N_IP + rp_age, data = stayers)
wpa_model <- lm(next_WPA ~ N_WPA + rp_SIERA_vs_lg + N_IP + rp_age, data = stayers)

# All attempts: 5+ starts (the main sample) and 1 to 4 starts (short attempts)
archetype_cols <- c("playerid", "PlayerName", "rp_season", "sp_season", "sp_GS", "sp_IP",
                    "rp_FBVelo", "rp_NumPitches10", "rp_gmLI", "rp_TBFPerG", "rp_StuffPlus",
                    "rp_NumPlusPitches", "rp_KBB")
conversions <- read_csv("data/conversions.csv", show_col_types = FALSE) |>
  select(all_of(archetype_cols)) |>
  mutate(attempt = "5+ starts")
short_attempts <- read_csv("data/short_attempts.csv", show_col_types = FALSE) |>
  select(all_of(archetype_cols)) |>
  mutate(attempt = "1 to 4 starts")

attempts <- bind_rows(conversions, short_attempts) |>
  inner_join(relief_seasons, by = c("playerid", "rp_season")) |>
  inner_join(season_next, by = c("playerid", "sp_season" = "next_season")) |>
  left_join(lg_sp, by = c("sp_season" = "Season")) |>
  mutate(Cushion = lg_sp_siera - rp_SIERA)

attempts$pen_WAR <- predict(war_model, newdata = attempts)
attempts$pen_WPA <- predict(wpa_model, newdata = attempts)
attempts <- attempts |>
  mutate(WAR_gain = next_WAR - pen_WAR,
         WPA_gain = next_WPA - pen_WPA)

# Starting history before the relief season (MLB from 2014, minors from 2006)
minors <- read_csv("data/fg_minors.csv", show_col_types = FALSE)
attempts$prev_MLB_GS <- 0
attempts$MiLB_GS <- 0
for (i in 1:nrow(attempts)) {
  before <- season_values |> filter(playerid == attempts$playerid[i], Season < attempts$rp_season[i])
  player_minors <- minors |> filter(playerid == as.character(attempts$playerid[i]),
      Season <= attempts$rp_season[i])
  attempts$prev_MLB_GS[i] <- sum(before$GS)
  attempts$MiLB_GS[i] <- sum(player_minors$GS)
}

write_csv(attempts, "data/value_attempts.csv")

sink("results/value_overall.txt")
cat("Stay-in-pen models fit on", nrow(stayers), "reliever seasons followed by another relief season (20+ IP)\n")
cat("WAR model R^2:", round(summary(war_model)$r.squared, 3),
    " WPA model R^2:", round(summary(wpa_model)$r.squared, 3), "\n\n")
cat("Stayers, average change from season N to N+1\n")
stayers |>
  summarize(n = n(), N_IP = mean(N_IP), next_IP = mean(next_IP), N_WAR = mean(N_WAR), next_WAR = mean(next_WAR),
            N_WPA = mean(N_WPA, na.rm = TRUE), next_WPA = mean(next_WPA, na.rm = TRUE)) |>
  as.data.frame() |> print(digits = 3)
cat("\nAttempts, actual next season vs expected if they had stayed in the pen\n")
attempts |>
  group_by(attempt) |>
  summarize(n = n(), N_IP = mean(N_IP), next_IP = mean(next_IP), N_aLI = mean(N_aLI, na.rm = TRUE),
            next_aLI = mean(next_aLI, na.rm = TRUE), N_WAR = mean(N_WAR), next_WAR = mean(next_WAR),
            pen_WAR = mean(pen_WAR), share_WAR_gain = mean(WAR_gain > 0), WAR_gain_se = sd(WAR_gain) / sqrt(n()),
            WAR_gain = mean(WAR_gain), WPA_gain = mean(WPA_gain, na.rm = TRUE)) |>
  as.data.frame() |> print(digits = 3)
cat("\nAll attempts together\n")
attempts |>
  summarize(n = n(), next_WAR = mean(next_WAR), pen_WAR = mean(pen_WAR),
            share_WAR_gain = mean(WAR_gain > 0), share_WPA_gain = mean(WPA_gain > 0, na.rm = TRUE),
            WAR_gain_se = sd(WAR_gain) / sqrt(n()),
            WPA_gain_se = sd(WPA_gain, na.rm = TRUE) / sqrt(sum(!is.na(WPA_gain))),
            WAR_gain = mean(WAR_gain), WPA_gain = mean(WPA_gain, na.rm = TRUE)) |>
  as.data.frame() |> print(digits = 3)
sink()
cat(readLines("results/value_overall.txt"), sep = "\n")

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

# actual next season WAR vs expected WAR in the pen
known <- c("Garrett Crochet", "Reynaldo López", "Jordan Hicks", "Michael King", "Seth Lugo",
           "Clay Holmes", "A.J. Puk", "Zack Littell", "Jeffrey Springs")
ggplot(attempts, aes(pen_WAR, next_WAR, color = attempt)) +
  geom_abline(slope = 1, intercept = 0, color = "gray", linetype = "dashed") +
  geom_point(alpha = 0.7, size = 2) +
  geom_text(data = attempts |> filter(PlayerName %in% known), aes(label = PlayerName),
            size = 3, vjust = -1, show.legend = FALSE) +
  scale_color_manual(values = c("5+ starts" = "steelblue", "1 to 4 starts" = "red")) +
  labs(title = "Next season WAR after a move to the rotation vs expected WAR in the pen",
       subtitle = "Expected WAR comes from relievers who stayed relievers. Points above the line gained value",
       x = "Expected WAR if he stayed in the pen", y = "Actual WAR the next season (all roles)",
       color = "Attempt") +
  theme_classic()
ggsave("charts/war_vs_pen_baseline.png", width = 8, height = 6, dpi = 150)

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

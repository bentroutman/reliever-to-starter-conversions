# Statcast changes from the relief season to the starter season: platoon, velo within outings, and times through
# the order
# Run after pull_relief_pitches.py, pull_starter_pitches.py, and conversion_changes.R
# Outputs: data/statcast_changes.csv, results/statcast_changes.txt, charts/tto_by_outcome.png

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

conversions <- read_csv("data/conversions.csv", show_col_types = FALSE) |>
  select(xMLBAMID, PlayerName, rp_season, sp_season, rp_SIERA_vs_lg, sp_SIERA_vs_lg, stuck)
relief_pitches <- read_csv("data/savant/relief_season_pitches.csv", guess_max = 50000, show_col_types = FALSE)
starter_pitches <- read_csv("data/savant/starter_season_pitches.csv", guess_max = 50000, show_col_types = FALSE)

swing_types <- c("swinging_strike", "swinging_strike_blocked", "foul", "foul_tip", "hit_into_play",
                 "foul_bunt", "missed_bunt")
whiff_types <- c("swinging_strike", "swinging_strike_blocked")

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
         BB = PA & events %in% c("walk", "hit_by_pitch"),
         Offspeed = pitch_type %in% c("CH", "FS", "FO", "SC"),
         Breaking = pitch_type %in% c("SL", "ST", "SV", "CU", "KC", "CS"),
         Fastball = pitch_type %in% c("FF", "SI"))

pitches <- pitches |>
  arrange(pitcher, game_year, game_pk, at_bat_number, pitch_number) |>
  group_by(pitcher, game_year, game_pk) |>
  mutate(OutingPitch = row_number()) |>
  ungroup()

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
  summarize(Outings = n_distinct(game_pk),
            PitchesPerOuting = n() / n_distinct(game_pk),
            OppShare_PA = sum(PA & OppHand) / sum(PA),
            Opp_KBB = (sum(K & OppHand) - sum(BB & OppHand)) / sum(PA & OppHand),
            Same_KBB = (sum(K & !OppHand) - sum(BB & !OppHand)) / sum(PA & !OppHand),
            Opp_Whiff = sum(Whiff & OppHand) / sum(Swing & OppHand),
            Opp_Offspeed = mean(Offspeed[OppHand]),
            Opp_Breaking = mean(Breaking[OppHand]),
            Opp_Fastball = mean(Fastball[OppHand]),
            Same_Breaking = mean(Breaking[!OppHand]),
            Velo_first15 = mean(release_speed[pitch_type == primary_fb & OutingPitch <= 15], na.rm = TRUE),
            Velo_61plus = mean(release_speed[pitch_type == primary_fb & OutingPitch > 60], na.rm = TRUE),
            FB_61plus = sum(pitch_type == primary_fb & OutingPitch > 60, na.rm = TRUE),
            xwOBA_1st = sum(estimated_woba_using_speedangle[n_thruorder_pitcher == 1], na.rm = TRUE) /
              sum(woba_denom[n_thruorder_pitcher == 1], na.rm = TRUE),
            wOBA_1st = sum(woba_value[n_thruorder_pitcher == 1], na.rm = TRUE) /
              sum(woba_denom[n_thruorder_pitcher == 1], na.rm = TRUE),
            wOBA_2nd = sum(woba_value[n_thruorder_pitcher == 2], na.rm = TRUE) /
              sum(woba_denom[n_thruorder_pitcher == 2], na.rm = TRUE),
            wOBA_3rd = sum(woba_value[n_thruorder_pitcher == 3], na.rm = TRUE) /
              sum(woba_denom[n_thruorder_pitcher == 3], na.rm = TRUE),
            PA_3rd = sum(PA & n_thruorder_pitcher == 3),
            .groups = "drop") |>
  mutate(Platoon_KBB_gap = Same_KBB - Opp_KBB,
         Velo_fade_in_start = ifelse(FB_61plus >= 30, Velo_61plus - Velo_first15, NA),
         TTO_penalty = ifelse(PA_3rd >= 40, wOBA_3rd - wOBA_1st, NA))

# Wide: relief and starter side by side for each conversion
relief_side <- season_measures |> filter(role == "relief") |> select(-role)
starter_side <- season_measures |> filter(role == "starter") |> select(-role)
names(relief_side)[-(1:2)] <- paste0("rp_", names(relief_side)[-(1:2)])
names(starter_side)[-(1:2)] <- paste0("sp_", names(starter_side)[-(1:2)])

statcast_changes <- conversions |>
  inner_join(relief_side, by = c("xMLBAMID" = "pitcher", "rp_season" = "game_year")) |>
  inner_join(starter_side, by = c("xMLBAMID" = "pitcher", "sp_season" = "game_year")) |>
  mutate(Opp_KBB_change = sp_Opp_KBB - rp_Opp_KBB,
         Same_KBB_change = sp_Same_KBB - rp_Same_KBB,
         Opp_Whiff_change = sp_Opp_Whiff - rp_Opp_Whiff,
         Opp_Offspeed_change = sp_Opp_Offspeed - rp_Opp_Offspeed,
         Opp_Breaking_change = sp_Opp_Breaking - rp_Opp_Breaking,
         Opp_Fastball_change = sp_Opp_Fastball - rp_Opp_Fastball,
         OppShare_change = sp_OppShare_PA - rp_OppShare_PA,
         Velo_first15_change = sp_Velo_first15 - rp_Velo_first15,
         Success = ifelse(sp_SIERA_vs_lg <= 0, "league average or better", "worse than average"))
write_csv(statcast_changes, "data/statcast_changes.csv")

sink("results/statcast_changes.txt")
cat("Conversions with Statcast in both seasons:", nrow(statcast_changes), "\n\n")

cat("Averages, relief season vs starter season (starts only)\n")
statcast_changes |>
  summarize(n_fade = sum(!is.na(sp_Velo_fade_in_start)), n_tto = sum(!is.na(sp_TTO_penalty)),
            rp_PitchesPerOuting = mean(rp_PitchesPerOuting), sp_PitchesPerOuting = mean(sp_PitchesPerOuting),
            rp_OppShare = mean(rp_OppShare_PA), sp_OppShare = mean(sp_OppShare_PA),
            rp_Opp_KBB = mean(rp_Opp_KBB), sp_Opp_KBB = mean(sp_Opp_KBB),
            rp_Same_KBB = mean(rp_Same_KBB), sp_Same_KBB = mean(sp_Same_KBB),
            rp_Opp_Offspeed = mean(rp_Opp_Offspeed), sp_Opp_Offspeed = mean(sp_Opp_Offspeed),
            rp_Opp_Breaking = mean(rp_Opp_Breaking), sp_Opp_Breaking = mean(sp_Opp_Breaking),
            rp_Velo_first15 = mean(rp_Velo_first15, na.rm = TRUE), sp_Velo_first15 = mean(sp_Velo_first15,
              na.rm = TRUE),
            sp_Velo_fade_in_start = mean(sp_Velo_fade_in_start, na.rm = TRUE),
            sp_wOBA_1st = mean(sp_wOBA_1st, na.rm = TRUE), sp_wOBA_2nd = mean(sp_wOBA_2nd, na.rm = TRUE),
            sp_wOBA_3rd = mean(sp_wOBA_3rd, na.rm = TRUE), sp_TTO_penalty = mean(sp_TTO_penalty, na.rm = TRUE)) |>
  pivot_longer(everything()) |>
  as.data.frame() |> print(digits = 3)

change_measures <- c("Opp_KBB_change", "Same_KBB_change", "Opp_Whiff_change", "Opp_Offspeed_change",
                     "Opp_Breaking_change", "Opp_Fastball_change", "OppShare_change", "Velo_first15_change",
                     "sp_Velo_fade_in_start", "sp_TTO_penalty", "rp_Platoon_KBB_gap", "sp_Platoon_KBB_gap")

cat("\nAverage by outcome\n")
statcast_changes |>
  group_by(Success) |>
  summarize(n = n(), across(all_of(change_measures), ~ mean(.x, na.rm = TRUE))) |>
  pivot_longer(-c(Success, n)) |>
  pivot_wider(names_from = Success, values_from = c(n, value)) |>
  as.data.frame() |> print(digits = 3)

cat("\nEach measure vs starter SIERA vs league (Spearman), and its effect after relief SIERA (SD units)\n")
effect_table <- NULL
for (m in change_measures) {
  one <- statcast_changes |> filter(!is.na(.data[[m]]))
  fit <- lm(sp_SIERA_vs_lg ~ rp_SIERA_vs_lg + scale(one[[m]]), data = one)
  effect_table <- bind_rows(effect_table, data.frame(
    measure = m, n = nrow(one),
    spearman_SIERA = round(cor(one[[m]], one$sp_SIERA_vs_lg, method = "spearman"), 3),
    beta_after_rp_SIERA = round(coef(summary(fit))[3, 1], 3),
    p = round(coef(summary(fit))[3, 4], 3)))
}
print(effect_table)
sink()
cat(readLines("results/statcast_changes.txt"), sep = "\n")

# wOBA by time through the order as a starter, and velo fade within starts, by outcome
tto_plot <- statcast_changes |>
  select(Success, sp_wOBA_1st, sp_wOBA_2nd, sp_wOBA_3rd) |>
  pivot_longer(-Success, names_to = "time", values_to = "wOBA") |>
  mutate(time = case_when(time == "sp_wOBA_1st" ~ "1st", time == "sp_wOBA_2nd" ~ "2nd", TRUE ~ "3rd")) |>
  group_by(Success, time) |>
  summarize(wOBA = mean(wOBA, na.rm = TRUE), .groups = "drop")
ggplot(tto_plot, aes(time, wOBA, color = Success, group = Success)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  scale_color_manual(values = c("league average or better" = "steelblue", "worse than average" = "red")) +
  labs(title = "wOBA allowed by time through the order in the starter season",
       subtitle = "Converted relievers, starts only, split by starter SIERA vs league",
       x = "Time through the order", y = "wOBA allowed", color = "As a starter") +
  theme_classic()
ggsave("charts/tto_by_outcome.png", width = 8, height = 5.5, dpi = 150)

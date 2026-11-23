# Turns the relief season Statcast pitches into platoon, arsenal, and workload measures
# Run after pull_relief_pitches.R
# Output: data/savant_features.csv (one row per pitcher relief season)

library(dplyr)
library(readr)

pitches <- read_csv("data/savant/relief_season_pitches.csv", guess_max = 50000, show_col_types = FALSE)

swing_types <- c("swinging_strike", "swinging_strike_blocked", "foul", "foul_tip", "hit_into_play",
                 "foul_bunt", "missed_bunt")
whiff_types <- c("swinging_strike", "swinging_strike_blocked")

pitches <- pitches |>
  filter(!is.na(pitch_type)) |>
  mutate(OppHand = stand != p_throws,
         Swing = description %in% swing_types,
         Whiff = description %in% whiff_types,
         PA = !is.na(events) & events != "truncated_pa",
         K = PA & events %in% c("strikeout", "strikeout_double_play"),
         BB = PA & events %in% c("walk", "hit_by_pitch"))

# Pitch count within each outing
pitches <- pitches |>
  arrange(pitcher, game_year, game_pk, at_bat_number, pitch_number) |>
  group_by(pitcher, game_year, game_pk) |>
  mutate(OutingPitch = row_number()) |>
  ungroup()

# Results against same-handed and opposite-handed hitters
platoon <- pitches |>
  group_by(pitcher, game_year) |>
  summarize(OppShare = mean(OppHand),
            Opp_PA = sum(PA & OppHand),
            Opp_KBB = (sum(K & OppHand) - sum(BB & OppHand)) / sum(PA & OppHand),
            Same_KBB = (sum(K & !OppHand) - sum(BB & !OppHand)) / sum(PA & !OppHand),
            Opp_Whiff = sum(Whiff & OppHand) / sum(Swing & OppHand),
            Same_Whiff = sum(Whiff & !OppHand) / sum(Swing & !OppHand),
            Opp_wOBA = sum(woba_value[OppHand], na.rm = TRUE) / sum(woba_denom[OppHand], na.rm = TRUE),
            Same_wOBA = sum(woba_value[!OppHand], na.rm = TRUE) / sum(woba_denom[!OppHand], na.rm = TRUE),
            .groups = "drop") |>
  mutate(Platoon_KBB_gap = Same_KBB - Opp_KBB,
         Platoon_wOBA_gap = Opp_wOBA - Same_wOBA)

# Arsenal used against opposite-handed hitters
opp_usage <- pitches |>
  filter(OppHand) |>
  group_by(pitcher, game_year, pitch_type) |>
  summarize(n = n(), swings = sum(Swing), whiffs = sum(Whiff), .groups = "drop") |>
  group_by(pitcher, game_year) |>
  mutate(usage = n / sum(n))

opp_arsenal <- opp_usage |>
  summarize(Opp_NumPitches10 = sum(usage >= 0.10),
            Opp_OffspeedUsage = sum(usage[pitch_type %in% c("CH", "FS", "FO", "SC")]),
            Opp_CurveUsage = sum(usage[pitch_type %in% c("CU", "KC", "CS", "SV")]),
            .groups = "drop")

# Best whiff rate vs opposite-handed hitters on a non-fastball with 25+ swings
opp_weapon <- opp_usage |>
  filter(!pitch_type %in% c("FF", "SI"), swings >= 25) |>
  summarize(Opp_BestSecondaryWhiff = max(whiffs / swings), .groups = "drop")

# Outing length
outings <- pitches |>
  group_by(pitcher, game_year, game_pk) |>
  summarize(outing_pitches = n(), .groups = "drop") |>
  group_by(pitcher, game_year) |>
  summarize(Outings = n(),
            Share25Plus = mean(outing_pitches >= 25),
            Share40Plus = mean(outing_pitches >= 40),
            MaxOutingPitches = max(outing_pitches),
            .groups = "drop")

# Fastball velo on the first 15 pitches of an outing vs pitch 26 and later
primary_fb <- pitches |>
  filter(pitch_type %in% c("FF", "SI")) |>
  count(pitcher, game_year, pitch_type) |>
  group_by(pitcher, game_year) |>
  filter(n == max(n)) |>
  select(pitcher, game_year, primary_fb = pitch_type)

velo_fade <- pitches |>
  inner_join(primary_fb, by = c("pitcher", "game_year")) |>
  filter(pitch_type == primary_fb, !is.na(release_speed)) |>
  group_by(pitcher, game_year) |>
  summarize(Velo_first15 = mean(release_speed[OutingPitch <= 15]),
            Velo_after25 = mean(release_speed[OutingPitch > 25]),
            FB_after25 = sum(OutingPitch > 25),
            Velo_p95 = quantile(release_speed, 0.95),
            Velo_mean = mean(release_speed),
            .groups = "drop") |>
  mutate(Velo_fade = ifelse(FB_after25 >= 20, Velo_after25 - Velo_first15, NA),
         Velo_max_gap = Velo_p95 - Velo_mean)

savant_features <- platoon |>
  left_join(opp_arsenal, by = c("pitcher", "game_year")) |>
  left_join(opp_weapon, by = c("pitcher", "game_year")) |>
  left_join(outings, by = c("pitcher", "game_year")) |>
  left_join(velo_fade, by = c("pitcher", "game_year")) |>
  rename(xMLBAMID = pitcher, rp_season = game_year)

write_csv(savant_features, "data/savant_features.csv")
cat("Pitcher relief seasons with Statcast features:", nrow(savant_features), "\n")

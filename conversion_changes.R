# What changed from the relief season to the starter season, and which changes went with success
# Run after value_analysis.R
# Outputs: results/conversion_changes.csv, results/conversion_changes.txt, and charts/usage_change.png

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

role_seasons <- read_csv("data/fg_role_seasons.csv", guess_max = 20000, show_col_types = FALSE)
conversions <- read_csv("data/conversions.csv", show_col_types = FALSE) |>
  select(playerid, PlayerName, Throws, rp_season, sp_season, sp_GS, sp_IP, rp_SIERA_vs_lg, sp_SIERA_vs_lg, stuck)
expected <- read_csv("data/value_attempts.csv", show_col_types = FALSE) |>
  select(playerid, sp_season, WAR_gain)

# Pitch groups. Sweepers go with sliders, knuckle curves with curves, splitters with changeups
role_seasons[is.na(role_seasons$`pfxST%`), "pfxST%"] <- 0
for (column in c("pfxFA%", "pfxSI%", "pfxFC%", "pfxSL%", "pfxCU%", "pfxKC%", "pfxCV%", "pfxCH%", "pfxFS%",
                 "pfxFO%", "pfxSC%")) {
  role_seasons[[column]][is.na(role_seasons[[column]])] <- 0
}
role_seasons <- role_seasons |>
  mutate(FourSeam = `pfxFA%`,
         Sinker = `pfxSI%`,
         Cutter = `pfxFC%`,
         Slider = `pfxSL%` + `pfxST%`,
         Curve = `pfxCU%` + `pfxKC%` + `pfxCV%`,
         Changeup = `pfxCH%` + `pfxFS%` + `pfxFO%` + `pfxSC%`,
         NumPitches10 = (FourSeam >= 0.1) + (Sinker >= 0.1) + (Cutter >= 0.1) + (Slider >= 0.1) +
           (Curve >= 0.1) + (Changeup >= 0.1),
         FBVelo = ifelse(Sinker > FourSeam, pfxvSI, pfxvFA),
         FB_Stuff = ifelse(Sinker > FourSeam, sp_s_SI, sp_s_FF))

pitch_groups <- c("FourSeam", "Sinker", "Cutter", "Slider", "Curve", "Changeup")
process_measures <- c(pitch_groups, "NumPitches10", "FBVelo", "pfxvSL", "pfxvCH", "sp_stuff", "FB_Stuff",
                      "sp_s_SL", "sp_s_CH", "sp_s_CU", "sp_location", "sp_pitching", "Zone%", "F-Strike%")
result_measures <- c("K%", "BB%", "K-BB%", "SwStr%", "O-Swing%", "Z-Contact%", "GB%", "HardHit%", "SIERA")
all_measures <- c(process_measures, result_measures)

relief_side <- role_seasons |>
  filter(role == "rel") |>
  select(playerid, rp_season = Season, all_of(all_measures))
starter_side <- role_seasons |>
  filter(role == "sta") |>
  select(playerid, sp_season = Season, all_of(all_measures))

# Long format with the relief value, the starter value, and the change for every measure
changes <- conversions |>
  left_join(expected, by = c("playerid", "sp_season")) |>
  inner_join(relief_side |> pivot_longer(-c(playerid, rp_season), names_to = "measure", values_to = "relief"),
             by = c("playerid", "rp_season"), relationship = "many-to-many") |>
  inner_join(starter_side |> pivot_longer(-c(playerid, sp_season), names_to = "measure", values_to = "starter"),
             by = c("playerid", "sp_season", "measure")) |>
  mutate(change = starter - relief,
         type = ifelse(measure %in% result_measures, "result", "process"),
         Success = ifelse(sp_SIERA_vs_lg <= 0, "league average or better", "worse than average"))
write_csv(changes, "results/conversion_changes.csv")

# Pitches added and dropped: under 5% in relief to 8%+ as a starter, and the reverse
mix <- changes |>
  filter(measure %in% pitch_groups) |>
  mutate(added = relief < 0.05 & starter >= 0.08,
         dropped = relief >= 0.08 & starter < 0.05)
per_pitcher <- mix |>
  group_by(playerid, sp_season, PlayerName, sp_SIERA_vs_lg, WAR_gain, stuck, rp_SIERA_vs_lg) |>
  summarize(added_any = any(added), dropped_any = any(dropped),
            added_pitches = paste(measure[added], collapse = " "),
            dropped_pitches = paste(measure[dropped], collapse = " "),
            biggest_gain = measure[which.max(change)], biggest_gain_pp = max(change) * 100, .groups = "drop")

sink("results/conversion_changes.txt")
cat("Relief vs starter lines for", n_distinct(paste(changes$playerid, changes$sp_season)), "conversions\n\n")

cat("Average change from relief to starter (usage in percentage points, velo in mph)\n")
change_summary <- changes |>
  group_by(type, measure) |>
  summarize(n = sum(!is.na(change)), relief = mean(relief, na.rm = TRUE), starter = mean(starter, na.rm = TRUE),
            share_up = mean(change > 0, na.rm = TRUE), median_change = median(change, na.rm = TRUE),
            change = mean(change, na.rm = TRUE), .groups = "drop")
print(as.data.frame(change_summary), digits = 3)

cat("\nPitches added (under 5% to 8%+) and dropped (8%+ to under 5%), number of conversions\n")
print(mix |> group_by(measure) |> summarize(added = sum(added), dropped = sum(dropped)) |> as.data.frame())

cat("\nAdded a pitch or not\n")
per_pitcher |>
  group_by(added_any) |>
  summarize(n = n(), rp_SIERA_vs_lg = mean(rp_SIERA_vs_lg), share_avg_or_better = mean(sp_SIERA_vs_lg <= 0),
            sp_SIERA_vs_lg = mean(sp_SIERA_vs_lg), WAR_gain = mean(WAR_gain, na.rm = TRUE),
            stuck = mean(stuck, na.rm = TRUE)) |>
  as.data.frame() |> print(digits = 3)
cat("\nWho added a pitch\n")
print(as.data.frame(per_pitcher |> filter(added_any) |>
                      transmute(PlayerName, sp_season, added_pitches, dropped_pitches,
                                sp_SIERA_vs_lg = round(sp_SIERA_vs_lg, 2), WAR_gain = round(WAR_gain, 2))))

cat("\nAverage change by outcome (starter SIERA vs league at or below 0 = success)\n")
by_success <- changes |>
  group_by(type, measure, Success) |>
  summarize(n = sum(!is.na(change)), change = mean(change, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = Success, values_from = c(n, change))
print(as.data.frame(by_success), digits = 3)

# Does each change relate to the starter result once relief SIERA is known
cat("\nChange vs starter SIERA vs league (Spearman), and the change's effect after relief SIERA (SD units)\n")
effect_table <- NULL
for (m in all_measures) {
  one <- changes |> filter(measure == m, !is.na(change))
  fit <- lm(sp_SIERA_vs_lg ~ rp_SIERA_vs_lg + scale(change), data = one)
  effect_table <- bind_rows(effect_table, data.frame(
    measure = m, type = ifelse(m %in% result_measures, "result", "process"), n = nrow(one),
    spearman_SIERA = round(cor(one$change, one$sp_SIERA_vs_lg, method = "spearman"), 3),
    spearman_WAR_gain = round(cor(one$change, one$WAR_gain, method = "spearman", use = "complete.obs"), 3),
    beta_after_rp_SIERA = round(coef(summary(fit))[3, 1], 3),
    p = round(coef(summary(fit))[3, 4], 3)))
}
print(effect_table)
sink()
cat(readLines("results/conversion_changes.txt"), sep = "\n")

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
       subtitle = "79 conversions, split by starter SIERA vs league. Sliders include sweepers, changeups include splitters",
       x = NULL, y = "Usage change (percentage points)", fill = "As a starter") +
  theme_classic()
ggsave("charts/usage_change.png", width = 9, height = 5.5, dpi = 150)

# Checks which relief season indicators relate to how the conversion to starting went
# Run after pitch_features.R
# Outputs: results/indicator_correlations.csv, results/group_splits.txt, and charts

library(dplyr)
library(readr)
library(ggplot2)

conversions <- read_csv("data/conversions.csv", show_col_types = FALSE)
savant_features <- read_csv("data/savant_features.csv", show_col_types = FALSE)

conversions <- conversions |>
  left_join(savant_features, by = c("xMLBAMID", "rp_season"))

indicators <- c("rp_SIERA_vs_lg", "rp_KBB", "rp_K", "rp_BB",
                "rp_Zone", "rp_FStrike", "rp_LocationPlus", "rp_botCmd",
                "rp_StuffPlus", "rp_botStf", "rp_FB_Stuff", "rp_BestSecondary_Stuff",
                "rp_SecondSecondary_Stuff", "rp_Offspeed_Stuff", "rp_NumPlusPitches", "rp_FBVelo", "rp_FA_IVB",
                  "Velo_max_gap", "Velo_fade",
                "rp_SwStr", "rp_ZContact", "rp_OSwing", "rp_OContact", "rp_CStr",
                "rp_GB", "rp_HardHit", "rp_Barrel",
                "rp_NumPitches10", "rp_NumPitches5", "rp_ThirdPitchUsage", "rp_TopTwoUsage",
                "rp_OffspeedUsage", "rp_FastballUsage",
                "Opp_NumPitches10", "Opp_OffspeedUsage", "Opp_CurveUsage", "Opp_BestSecondaryWhiff",
                "Opp_KBB", "Opp_Whiff", "Opp_wOBA", "Platoon_KBB_gap", "Platoon_wOBA_gap",
                "rp_PitchesPerG", "rp_TBFPerG", "Share25Plus", "Share40Plus", "rp_gmLI",
                "rp_age", "prev_MLB_GS", "MiLB_GS", "MiLB_GS_share", "MiLB_max_IP", "MaxProIP")

# Spearman correlation of each indicator with each outcome
# SIERA vs league is the starter SIERA minus the league starter SIERA that year, so negative is better
correlations <- data.frame(indicator = indicators, n = NA, SIERA_vs_lg = NA, SIERA_vs_lg_60IP = NA,
                           KBB_change = NA, sp_IP = NA, sp_WAR_per_150 = NA,
                           beta_after_rp_SIERA = NA, p_after_rp_SIERA = NA)
long_enough <- conversions$sp_IP >= 60
for (i in 1:length(indicators)) {
  x <- conversions[[indicators[i]]]
  correlations$n[i] <- sum(!is.na(x))
  correlations$SIERA_vs_lg[i] <- cor(x, conversions$sp_SIERA_vs_lg, method = "spearman", use = "complete.obs")
  correlations$SIERA_vs_lg_60IP[i] <- cor(x[long_enough], conversions$sp_SIERA_vs_lg[long_enough],
                                          method = "spearman", use = "complete.obs")
  correlations$KBB_change[i] <- cor(x, conversions$KBB_change, method = "spearman", use = "complete.obs")
  correlations$sp_IP[i] <- cor(x, conversions$sp_IP, method = "spearman", use = "complete.obs")
  correlations$sp_WAR_per_150[i] <- cor(x, conversions$sp_WAR_per_150, method = "spearman", use = "complete.obs")

  # Does the indicator add anything once relief SIERA is known (indicator in SD units)
  if (indicators[i] != "rp_SIERA_vs_lg") {
    fit <- lm(sp_SIERA_vs_lg ~ rp_SIERA_vs_lg + scale(x), data = conversions)
    correlations$beta_after_rp_SIERA[i] <- coef(summary(fit))[3, 1]
    correlations$p_after_rp_SIERA[i] <- coef(summary(fit))[3, 4]
  }
}
correlations[, 3:9] <- round(correlations[, 3:9], 3)
write_csv(correlations, "results/indicator_correlations.csv")
print(correlations)

# Simple group splits
conversions <- conversions |>
  mutate(ArsenalGroup = case_when(rp_NumPitches10 <= 2 ~ "2 pitches",
                                  rp_NumPitches10 == 3 ~ "3 pitches",
                                  TRUE ~ "4+ pitches"),
         StarterHistory = case_when(prev_MLB_GS == 0 ~ "0 prior MLB starts",
                                    prev_MLB_GS < 20 ~ "1 to 19",
                                    TRUE ~ "20+"),
         VeloGroup = case_when(rp_FBVelo < 93 ~ "under 93",
                               rp_FBVelo < 95 ~ "93 to 95",
                               TRUE ~ "95+"),
         AgeGroup = case_when(rp_age <= 25 ~ "25 and under",
                              rp_age <= 28 ~ "26 to 28",
                              TRUE ~ "29+"),
         PlatoonGroup = case_when(is.na(Platoon_KBB_gap) ~ NA,
                                  Platoon_KBB_gap > 0.10 ~ "big gap (10+ pts)",
                                  Platoon_KBB_gap > 0 ~ "small gap",
                                  TRUE ~ "reverse or none"),
         MinorsGroup = case_when(MiLB_GS < 20 ~ "under 20 MiLB starts",
                                 MiLB_GS < 60 ~ "20 to 59",
                                 TRUE ~ "60+"),
         WorkloadGroup = case_when(MaxProIP < 100 ~ "max season under 100 IP",
                                   MaxProIP < 140 ~ "100 to 139",
                                   TRUE ~ "140+"),
         PlusPitchGroup = case_when(is.na(rp_NumPlusPitches) ~ NA,
                                    rp_NumPlusPitches <= 1 ~ "0 or 1 pitches Stuff+ 100+",
                                    rp_NumPlusPitches == 2 ~ "2",
                                    TRUE ~ "3+"),
         LengthGroup = case_when(rp_TBFPerG < 4.5 ~ "under 4.5 TBF per G",
                                 rp_TBFPerG < 6 ~ "4.5 to 6",
                                 TRUE ~ "6+"))

sink("results/group_splits.txt")
cat("Overall\n")
conversions |>
  summarize(n = n(), rp_SIERA = mean(rp_SIERA), sp_SIERA = mean(sp_SIERA),
            sp_SIERA_vs_lg = mean(sp_SIERA_vs_lg), rp_KBB = mean(rp_KBB), sp_KBB = mean(sp_KBB),
            KBB_change = mean(KBB_change), Velo_change = mean(Velo_change, na.rm = TRUE),
            sp_IP = mean(sp_IP), stuck = mean(stuck, na.rm = TRUE)) |>
  as.data.frame() |> print()
for (group_name in c("ArsenalGroup", "StarterHistory", "MinorsGroup", "WorkloadGroup", "PlusPitchGroup",
                     "VeloGroup", "AgeGroup", "PlatoonGroup", "LengthGroup")) {
  cat("\n", group_name, "\n")
  conversions |>
    filter(!is.na(.data[[group_name]])) |>
    group_by(.data[[group_name]]) |>
    summarize(n = n(), rp_SIERA_vs_lg = mean(rp_SIERA_vs_lg), sp_SIERA_vs_lg = mean(sp_SIERA_vs_lg),
              KBB_change = mean(KBB_change), Velo_change = mean(Velo_change, na.rm = TRUE),
              sp_IP = mean(sp_IP), sp_WAR_per_150 = mean(sp_WAR_per_150),
              stuck = mean(stuck, na.rm = TRUE)) |>
    as.data.frame() |> print(digits = 3)
}
sink()
cat(readLines("results/group_splits.txt"), sep = "\n")

known <- c("Garrett Crochet", "Reynaldo López", "Jordan Hicks", "Michael King", "Seth Lugo",
           "Clay Holmes", "Zack Littell", "Jeffrey Springs")
labeled <- conversions |> filter(PlayerName %in% known)

# relief K-BB% vs starter K-BB%
ggplot(conversions, aes(rp_KBB * 100, sp_KBB * 100)) +
  geom_abline(slope = 1, intercept = 0, color = "gray", linetype = "dashed") +
  geom_point(aes(size = sp_IP), color = "steelblue", alpha = 0.6) +
  geom_text(data = labeled, aes(label = PlayerName), size = 3, vjust = -1) +
  labs(title = "K-BB% as a reliever vs K-BB% as a starter the next season",
       subtitle = "79 conversion attempts, 2015 to 2026. Dashed line is no change",
       x = "Relief season K-BB%", y = "Starter season K-BB%", size = "Starter IP") +
  theme_classic()
ggsave("charts/kbb_relief_vs_starter.png", width = 8, height = 6, dpi = 150)

# arsenal depth
ggplot(conversions, aes(ArsenalGroup, sp_SIERA_vs_lg)) +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  geom_jitter(width = 0.12, color = "steelblue", alpha = 0.6, size = 2) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.4, color = "red") +
  labs(title = "Starter SIERA vs league by arsenal depth in the relief season",
       subtitle = "Pitches thrown at least 10% of the time. Red bar is the group mean, lower is better",
       x = "Arsenal in relief season", y = "Starter SIERA minus league starter SIERA") +
  theme_classic()
ggsave("charts/arsenal_depth.png", width = 7, height = 5, dpi = 150)

# correlation of every indicator with starter SIERA vs league
plot_correlations <- correlations |>
  filter(n >= 30) |>
  arrange(SIERA_vs_lg)
plot_correlations$indicator <- factor(plot_correlations$indicator, levels = plot_correlations$indicator)
ggplot(plot_correlations, aes(SIERA_vs_lg, indicator)) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0("n=", n), x = ifelse(SIERA_vs_lg > 0, SIERA_vs_lg + 0.03, SIERA_vs_lg - 0.03)),
      size = 2.5) +
  labs(title = "Relief season indicators vs starter SIERA (relative to league)",
       subtitle = "Spearman correlation. Negative means more of the indicator went with a better starter season",
       x = "Spearman correlation with starter SIERA vs league", y = NULL) +
  theme_classic()
ggsave("charts/indicator_correlations.png", width = 8, height = 9, dpi = 150)

# velocity lost when stretched out
ggplot(conversions, aes(rp_FBVelo, Velo_change)) +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  geom_point(color = "steelblue", alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.6) +
  geom_text(data = labeled, aes(label = PlayerName), size = 3, vjust = -1) +
  labs(title = "Fastball velocity change from the relief season to the starter season",
       x = "Relief season fastball velo (mph)", y = "Starter velo minus relief velo (mph)") +
  theme_classic()
ggsave("charts/velo_change.png", width = 8, height = 6, dpi = 150)

# results vs opposite-handed hitters
ggplot(conversions, aes(Opp_KBB * 100, sp_SIERA_vs_lg)) +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  geom_point(color = "steelblue", alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.6) +
  geom_text(data = labeled, aes(label = PlayerName), size = 3, vjust = -1) +
  labs(title = "K-BB% vs opposite-handed hitters in relief vs starter SIERA",
       subtitle = "Relief seasons from 2015 on (Statcast). Lower SIERA vs league is better",
       x = "Relief season K-BB% vs opposite-handed hitters", y = "Starter SIERA minus league starter SIERA") +
  theme_classic()
ggsave("charts/opposite_hand_kbb.png", width = 8, height = 6, dpi = 150)

# prior MLB starting experience
ggplot(conversions, aes(StarterHistory, sp_SIERA_vs_lg)) +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  geom_jitter(width = 0.12, color = "steelblue", alpha = 0.6, size = 2) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.4, color = "red") +
  labs(title = "Starter SIERA vs league by prior MLB starts (2014 on)",
       subtitle = "Red bar is the group mean, lower is better",
       x = "MLB starts before the relief season", y = "Starter SIERA minus league starter SIERA") +
  theme_classic()
ggsave("charts/prior_starts.png", width = 7, height = 5, dpi = 150)

# quality of the third pitch (second best secondary by Stuff+), 2020 on
ggplot(conversions |> filter(!is.na(rp_SecondSecondary_Stuff)), aes(rp_SecondSecondary_Stuff, sp_SIERA_vs_lg)) +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  geom_point(color = "steelblue", alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.6) +
  geom_text(data = labeled |> filter(!is.na(rp_SecondSecondary_Stuff)), aes(label = PlayerName), size = 3,
      vjust = -1) +
  labs(title = "Stuff+ of the third pitch in relief vs starter SIERA",
       subtitle = "Third pitch is the second best secondary thrown 5%+ of the time. Relief seasons 2020 on",
       x = "Relief season Stuff+ of the third pitch", y = "Starter SIERA minus league starter SIERA") +
  theme_classic()
ggsave("charts/third_pitch_stuff.png", width = 8, height = 6, dpi = 150)

# minor league starts before the relief season
ggplot(conversions, aes(MiLB_GS, sp_SIERA_vs_lg)) +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  geom_point(color = "steelblue", alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.6) +
  geom_text(data = labeled, aes(label = PlayerName), size = 3, vjust = -1) +
  labs(title = "Minor league starts before the relief season vs starter SIERA",
       subtitle = "Minors data from 2006 on, includes rehab starts",
       x = "Minor league starts through the relief season", y = "Starter SIERA minus league starter SIERA") +
  theme_classic()
ggsave("charts/minor_league_starts.png", width = 8, height = 6, dpi = 150)

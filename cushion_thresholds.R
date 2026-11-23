# Tests larger SIERA cushions and how SIERA, WAR, and WPA changed at each level
# Cushion = league average starter SIERA in the starter season minus the pitcher's relief SIERA
# SIERA results use the 79 conversions (5+ starts). WAR and WPA use all 114 attempts, so short failed tries count
# Run after value_analysis.R
# Outputs: results/cushion_thresholds.txt and charts/cushion_bins.png

library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)

role_seasons <- read_csv("data/fg_role_seasons.csv", guess_max = 20000, show_col_types = FALSE)

# League average starter SIERA each season, same as build_sample.R
league_sp <- role_seasons |>
  filter(role == "sta") |>
  group_by(Season) |>
  summarize(lg_sp_siera = weighted.mean(SIERA, TBF, na.rm = TRUE))

# value_attempts.csv from value_analysis.R already has the relief SIERA and the cushion for each attempt
attempts <- read_csv("data/value_attempts.csv", show_col_types = FALSE)

conversions <- read_csv("data/conversions.csv", show_col_types = FALSE) |>
  left_join(league_sp, by = c("sp_season" = "Season")) |>
  mutate(Cushion = lg_sp_siera - rp_SIERA,
         SIERA_vs_lg_change = sp_SIERA_vs_lg - (rp_SIERA - lg_sp_siera))

thresholds <- c(-0.5, 0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2)

sink("results/cushion_thresholds.txt")
cat("Average league starter SIERA in these starter seasons:", round(mean(conversions$lg_sp_siera), 2), "\n")
cat("Cushion range in the 79 conversions:", round(min(conversions$Cushion), 2), "to",
    round(max(conversions$Cushion), 2), "\n\n")

cat("Pitchers AT OR ABOVE each cushion\n")
cat("SIERA columns use the 79 conversions, WAR and WPA columns use all 114 attempts\n")
threshold_table <- NULL
for (cutoff in thresholds) {
  conv_above <- conversions |> filter(Cushion >= cutoff)
  att_above <- attempts |> filter(Cushion >= cutoff)
  threshold_table <- bind_rows(threshold_table, data.frame(
    cushion_at_least = cutoff,
    n_conv = nrow(conv_above),
    rp_SIERA = mean(conv_above$rp_SIERA),
    sp_SIERA = mean(conv_above$sp_SIERA),
    SIERA_change = mean(conv_above$SIERA_change),
    sp_SIERA_vs_lg = mean(conv_above$sp_SIERA_vs_lg),
    share_avg_or_better = mean(conv_above$sp_SIERA_vs_lg <= 0),
    stuck = mean(conv_above$stuck, na.rm = TRUE),
    n_att = nrow(att_above),
    share_5plus_GS = mean(att_above$attempt == "5+ starts"),
    next_WAR = mean(att_above$next_WAR),
    WAR_gain = mean(att_above$WAR_gain),
    WAR_gain_se = sd(att_above$WAR_gain) / sqrt(nrow(att_above)),
    next_WPA = mean(att_above$next_WPA, na.rm = TRUE),
    WPA_gain = mean(att_above$WPA_gain, na.rm = TRUE),
    WPA_gain_se = sd(att_above$WPA_gain, na.rm = TRUE) / sqrt(nrow(att_above))))
}
print(threshold_table, digits = 3)

# Bins so each group stands on its own
bin_breaks <- c(-Inf, 0, 0.5, 1, 1.5, Inf)
bin_labels <- c("under 0", "0 to 0.5", "0.5 to 1", "1 to 1.5", "1.5+")
conversions$CushionBin <- cut(conversions$Cushion, bin_breaks, bin_labels, right = FALSE)
attempts$CushionBin <- cut(attempts$Cushion, bin_breaks, bin_labels, right = FALSE)

cat("\nCushion bins, 79 conversions\n")
conv_bins <- conversions |>
  group_by(CushionBin) |>
  summarize(n = n(), rp_SIERA = mean(rp_SIERA), sp_SIERA = mean(sp_SIERA),
            SIERA_change_se = sd(SIERA_change) / sqrt(n()), SIERA_change = mean(SIERA_change),
            share_avg_or_better = mean(sp_SIERA_vs_lg <= 0), sp_SIERA_vs_lg = mean(sp_SIERA_vs_lg),
            stuck = mean(stuck, na.rm = TRUE), sp_IP = mean(sp_IP))
print(as.data.frame(conv_bins), digits = 3)

cat("\nCushion bins, all 114 attempts\n")
att_bins <- attempts |>
  group_by(CushionBin) |>
  summarize(n = n(), share_WAR_gain = mean(WAR_gain > 0), share_WPA_gain = mean(WPA_gain > 0, na.rm = TRUE),
            share_5plus_GS = mean(attempt == "5+ starts"), N_WAR = mean(N_WAR),
            next_WAR = mean(next_WAR), pen_WAR = mean(pen_WAR),
            WAR_gain_se = sd(WAR_gain) / sqrt(n()), WAR_gain = mean(WAR_gain),
            N_WPA = mean(N_WPA, na.rm = TRUE), next_WPA = mean(next_WPA, na.rm = TRUE),
            pen_WPA = mean(pen_WPA, na.rm = TRUE),
            WPA_gain_se = sd(WPA_gain, na.rm = TRUE) / sqrt(n()), WPA_gain = mean(WPA_gain, na.rm = TRUE))
print(as.data.frame(att_bins), digits = 3)

# Does the cushion still matter once you know the relief season value
cat("\nCushion as a predictor of the gain (all 114 attempts)\n")
cat("WAR gain on cushion: slope", round(coef(lm(WAR_gain ~ Cushion, data = attempts))[2], 3),
    " p =", round(summary(lm(WAR_gain ~ Cushion, data = attempts))$coefficients[2, 4], 3), "\n")
cat("WPA gain on cushion: slope", round(coef(lm(WPA_gain ~ Cushion, data = attempts))[2], 3),
    " p =", round(summary(lm(WPA_gain ~ Cushion, data = attempts))$coefficients[2, 4], 3), "\n")
sink()
cat(readLines("results/cushion_thresholds.txt"), sep = "\n")

# Bars are 1 standard error
siera_plot <- ggplot(conv_bins, aes(CushionBin, SIERA_change)) +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  geom_errorbar(aes(ymin = SIERA_change - SIERA_change_se, ymax = SIERA_change + SIERA_change_se), width = 0.15,
      color = "gray") +
  geom_point(color = "steelblue", size = 3) +
  geom_text(aes(label = paste0("n=", n)), vjust = -1.2, size = 3) +
  labs(title = "SIERA change by cushion (79 conversions)", x = "Relief SIERA cushion (runs)", y = "Runs") +
  theme_classic()
war_plot <- ggplot(att_bins, aes(CushionBin, WAR_gain)) +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  geom_errorbar(aes(ymin = WAR_gain - WAR_gain_se, ymax = WAR_gain + WAR_gain_se), width = 0.15, color = "gray") +
  geom_point(color = "steelblue", size = 3) +
  geom_text(aes(label = paste0("n=", n)), vjust = -1.2, size = 3) +
  labs(title = "WAR gain by cushion (114 attempts)", x = "Relief SIERA cushion (runs)", y = "Wins") +
  theme_classic()
wpa_plot <- ggplot(att_bins, aes(CushionBin, WPA_gain)) +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  geom_errorbar(aes(ymin = WPA_gain - WPA_gain_se, ymax = WPA_gain + WPA_gain_se), width = 0.15, color = "gray") +
  geom_point(color = "steelblue", size = 3) +
  geom_text(aes(label = paste0("n=", n)), vjust = -1.2, size = 3) +
  labs(title = "WPA gain by cushion (114 attempts)", x = "Relief SIERA cushion (runs)", y = "Wins") +
  theme_classic()
ggsave("charts/cushion_bins.png", grid.arrange(siera_plot, war_plot, wpa_plot, ncol = 1),
       width = 8, height = 9, dpi = 150)

# Checks whether the definition of a conversion changes the main results
# Run after indicator_analysis.R
# Outputs: results/definition_sensitivity.csv and results/definition_sensitivity.txt

library(dplyr)
library(readr)

role_seasons <- read_csv("data/fg_role_seasons.csv", guess_max = 20000, show_col_types = FALSE) |>
  mutate(IP = floor(IP) + (IP %% 1) * 10 / 3)

season_totals <- role_seasons |>
  group_by(playerid, Season) |>
  summarize(G_total = sum(G), GS_total = sum(GS), .groups = "drop") |>
  arrange(playerid, Season) |>
  group_by(playerid) |>
  mutate(prior_GS = cumsum(GS_total) - GS_total) |>
  ungroup()

league_sp <- role_seasons |>
  filter(role == "sta") |>
  group_by(Season) |>
  summarize(lg_sp_siera = weighted.mean(SIERA, TBF, na.rm = TRUE))
league_rp <- role_seasons |>
  filter(role == "rel") |>
  group_by(Season) |>
  summarize(lg_rp_siera = weighted.mean(SIERA, TBF, na.rm = TRUE))

relief_all <- role_seasons |>
  filter(role == "rel") |>
  left_join(season_totals, by = c("playerid", "Season")) |>
  left_join(league_rp, by = "Season") |>
  transmute(playerid, rp_season = Season, rp_TBF = TBF, rp_GS_share = GS_total / G_total, rp_SIERA = SIERA,
            rp_SIERA_vs_lg = SIERA - lg_rp_siera, prior_GS)
starter_all <- role_seasons |>
  filter(role == "sta") |>
  left_join(season_totals, by = c("playerid", "Season")) |>
  left_join(league_sp, by = "Season") |>
  transmute(playerid, PlayerName, sp_season = Season, sp_GS = GS, sp_IP = IP,
      sp_starter_share = GS_total / G_total,
            sp_SIERA = SIERA, sp_SIERA_vs_lg = SIERA - lg_sp_siera, lg_sp_siera)

# Same pairing rules as build_sample.R, with the cutoffs as inputs
build_variant <- function(min_gs, min_ip_per_gs, max_relief_gs_share, min_relief_tbf, max_gap) {
  starters <- starter_all |>
    filter(sp_GS >= min_gs, sp_starter_share >= 0.5, sp_IP / sp_GS >= min_ip_per_gs, sp_season >= 2015)
  relievers <- relief_all |>
    filter(rp_TBF >= min_relief_tbf, rp_GS_share <= max_relief_gs_share)
  pairs <- starters |>
    inner_join(relievers, by = "playerid", relationship = "many-to-many") |>
    mutate(gap = sp_season - rp_season) |>
    filter(gap >= 1, gap <= max_gap) |>
    group_by(playerid, sp_season) |>
    filter(gap == min(gap)) |>
    ungroup()
  # starts made in the years between the relief season and the starter season
  pairs$GS_between <- 0
  for (i in which(pairs$gap >= 2)) {
    between <- season_totals |>
      filter(playerid == pairs$playerid[i], Season > pairs$rp_season[i], Season < pairs$sp_season[i])
    pairs$GS_between[i] <- sum(between$GS_total)
  }
  pairs |> filter(GS_between <= 3)
}

# Headline numbers for one version of the sample
summarize_variant <- function(label, pairs) {
  pairs <- pairs |> mutate(cushion = lg_sp_siera - rp_SIERA, success = sp_SIERA_vs_lg <= 0)
  top_third <- pairs$rp_SIERA_vs_lg <= quantile(pairs$rp_SIERA_vs_lg, 1 / 3)
  slope_fit <- lm(sp_SIERA ~ rp_SIERA, data = pairs)
  data.frame(
    version = label, attempts = nrow(pairs), pitchers = n_distinct(pairs$playerid),
    mean_starter_SIERA_vs_lg = round(mean(pairs$sp_SIERA_vs_lg), 2),
    mean_SIERA_change = round(mean(pairs$sp_SIERA - pairs$rp_SIERA), 2),
    share_SIERA_worse = round(mean(pairs$sp_SIERA > pairs$rp_SIERA), 2),
    spearman_relief_vs_starter = round(cor(pairs$rp_SIERA_vs_lg, pairs$sp_SIERA_vs_lg, method = "spearman"), 2),
    slope = round(coef(slope_fit)[2], 2),
    success_top_third = round(mean(pairs$success[top_third]), 2),
    success_rest = round(mean(pairs$success[!top_third]), 2),
    n_cushion_1plus = sum(pairs$cushion >= 1),
    success_cushion_1plus = round(mean(pairs$success[pairs$cushion >= 1]), 2),
    success_cushion_under_1 = round(mean(pairs$success[pairs$cushion < 1]), 2))
}

baseline <- build_variant(5, 3.5, 0.2, 100, 3)
versions <- list(
  "baseline: 5+ starts, 3.5+ IP per start, relief TBF 100+, relief starts 20% or less, lookback 3" = baseline,
  "10+ starts" = build_variant(10, 3.5, 0.2, 100, 3),
  "15+ starts" = build_variant(15, 3.5, 0.2, 100, 3),
  "3+ IP per start (lets in shorter starts)" = build_variant(5, 3.0, 0.2, 100, 3),
  "4.5+ IP per start" = build_variant(5, 4.5, 0.2, 100, 3),
  "stricter relief season (150+ TBF, starts 10% or less)" = build_variant(5, 3.5, 0.1, 150, 3),
  "looser relief season (60+ TBF, starts 30% or less)" = build_variant(5, 3.5, 0.3, 60, 3),
  "lookback 1 year only" = build_variant(5, 3.5, 0.2, 100, 1))

# Versions that filter the baseline sample
baseline_prior <- baseline
versions[["true relievers only (under 20 prior MLB starts)"]] <- baseline_prior |> filter(prior_GS < 20)
versions[["former starters only (20+ prior MLB starts)"]] <- baseline_prior |> filter(prior_GS >= 20)
versions[["one attempt per pitcher (first one)"]] <- baseline_prior |> arrange(playerid, sp_season) |>
  group_by(playerid) |> slice(1) |> ungroup()
versions[["drop 2020 (short season) as either year"]] <- baseline_prior |> filter(sp_season != 2020,
    rp_season != 2020)
versions[["2015 to 2020 only"]] <- baseline_prior |> filter(sp_season <= 2020)
versions[["2021 to 2026 only"]] <- baseline_prior |> filter(sp_season >= 2021)

results <- NULL
for (label in names(versions)) {
  results <- bind_rows(results, summarize_variant(label, versions[[label]]))
}
write_csv(results, "results/definition_sensitivity.csv")

sink("results/definition_sensitivity.txt")
cat("1. Headline results under different definitions of a conversion\n")
cat("success = starter SIERA at or below the league average starter. top_third = best third of relievers by relief SIERA vs league in that version.\n\n")
print(as.data.frame(results |> select(version, attempts, pitchers, mean_SIERA_change, spearman_relief_vs_starter,
    slope,
                                      success_top_third, success_rest, success_cushion_1plus,
                                        success_cushion_under_1)), row.names = FALSE)
cat("\nOther columns\n")
print(as.data.frame(results |> select(version, mean_starter_SIERA_vs_lg, share_SIERA_worse, n_cushion_1plus)),
    row.names = FALSE)

# Duplicate pitchers: how many appear more than once
cat("\n2. Pitchers who appear more than once in the baseline sample:\n")
repeat_pitchers <- baseline |> count(playerid, PlayerName) |> filter(n > 1)
print(as.data.frame(repeat_pitchers), row.names = FALSE)

# How noisy is a reliever's SIERA? The year to year relationship for relievers who stayed relievers shows how much
# of
# the 0.44 slope from relief SIERA to starter SIERA is regression to the mean instead of a role effect
cat("\n4. Year to year reliability of SIERA (SIERA vs league, this season to next season)\n")
relief_next <- relief_all |> filter(rp_TBF >= 100, rp_GS_share <= 0.2) |>
  mutate(next_season = rp_season + 1) |>
  inner_join(relief_all |> filter(rp_TBF >= 100, rp_GS_share <= 0.2) |>
               select(playerid, next_season = rp_season, next_SIERA_vs_lg = rp_SIERA_vs_lg, next_SIERA = rp_SIERA),
             by = c("playerid", "next_season"))
cat("Relievers to relievers, both seasons 100+ TBF:", nrow(relief_next), " correlation",
    round(cor(relief_next$rp_SIERA_vs_lg, relief_next$next_SIERA_vs_lg), 2),
    " slope of next SIERA on this SIERA", round(coef(lm(next_SIERA ~ rp_SIERA, data = relief_next))[2], 2), "\n")
starter_pairs <- starter_all |> filter(sp_GS >= 10) |>
  mutate(next_season = sp_season + 1) |>
  inner_join(starter_all |> filter(sp_GS >= 10) |> select(playerid, next_season = sp_season, next_SIERA = sp_SIERA,
      next_SIERA_vs_lg = sp_SIERA_vs_lg),
             by = c("playerid", "next_season"))
cat("Starters to starters, both seasons 10+ starts:", nrow(starter_pairs), " correlation",
    round(cor(starter_pairs$sp_SIERA_vs_lg, starter_pairs$next_SIERA_vs_lg), 2),
    " slope", round(coef(lm(next_SIERA ~ sp_SIERA, data = starter_pairs))[2], 2), "\n")
cat("The conversion slope (relief SIERA to starter SIERA) is", round(coef(lm(sp_SIERA ~ rp_SIERA,
    data = baseline))[2], 2),
    "so most of the shrinkage toward the mean is what a one year reliever sample would show anyway\n")

# Other outcomes: the same relationship measured with K-BB%, xFIP, FIP, and WAR per 150 IP instead of SIERA
cat("\n5. Does the relief to starter relationship hold for other outcomes? Spearman correlations in the baseline sample\n")
alternate <- baseline |>
  left_join(role_seasons |> filter(role == "sta") |>
              transmute(playerid, sp_season = Season, sp_KBB = `K-BB%`, sp_xFIP = xFIP, sp_FIP = FIP,
                sp_WAR_per_150 = WAR / IP * 150),
            by = c("playerid", "sp_season")) |>
  left_join(role_seasons |> filter(role == "rel") |>
              transmute(playerid, rp_season = Season, rp_KBB = `K-BB%`, rp_xFIP = xFIP, rp_FIP = FIP),
            by = c("playerid", "rp_season"))
cat("relief K-BB% with starter K-BB%:", round(cor(alternate$rp_KBB, alternate$sp_KBB, method = "spearman"), 2),
    "\n")
cat("relief xFIP with starter xFIP:", round(cor(alternate$rp_xFIP, alternate$sp_xFIP, method = "spearman"), 2),
    "\n")
cat("relief FIP with starter FIP:", round(cor(alternate$rp_FIP, alternate$sp_FIP, method = "spearman"), 2), "\n")
cat("relief SIERA vs league with starter WAR per 150 IP:", round(cor(alternate$rp_SIERA_vs_lg,
    alternate$sp_WAR_per_150, method = "spearman"), 2), "\n")
cat("relief K-BB% with starter WAR per 150 IP:", round(cor(alternate$rp_KBB, alternate$sp_WAR_per_150,
    method = "spearman"), 2), "\n")

# Testing many indicators: adjust the Spearman p-values (Benjamini-Hochberg)
cat("\n3. Indicator correlations with starter SIERA vs league, adjusted for testing", "many indicators at once\n")
correlations <- read_csv("results/indicator_correlations.csv", show_col_types = FALSE) |>
  filter(!is.na(SIERA_vs_lg)) |>
  mutate(t_stat = SIERA_vs_lg * sqrt((n - 2) / (1 - SIERA_vs_lg^2)),
         p_raw = 2 * pt(-abs(t_stat), df = n - 2),
         q_BH = p.adjust(p_raw, method = "BH"))
cat("Indicators tested:", nrow(correlations), " raw p under 0.05:", sum(correlations$p_raw < 0.05),
    " expected by chance alone:", round(nrow(correlations) * 0.05, 1), " BH q under 0.10:",
      sum(correlations$q_BH < 0.10), "\n")
print(as.data.frame(correlations |> arrange(p_raw) |> head(12) |>
                      transmute(indicator, n, spearman = SIERA_vs_lg, p_raw = round(p_raw, 4), q_BH = round(q_BH,
                        3))),
      row.names = FALSE)
sink()
cat(readLines("results/definition_sensitivity.txt"), sep = "\n")

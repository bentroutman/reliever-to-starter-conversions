# Compares WAR, WAA, and WPA on how well they repeat for relievers, and on the conversion gain
# WAA_fg is FanGraphs WAR minus the average pace for the same role and season. WAA_br is from Baseball-Reference
# Run after value_analysis.R and pull_bref_value.py
# Outputs: results/waa_analysis.txt and results/waa_gains.csv

library(dplyr)
library(readr)

role_seasons <- read_csv("data/fg_role_seasons.csv", guess_max = 20000, show_col_types = FALSE) |>
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

# Baseball-Reference value (WAA, leverage adjustment) and win probability (WPA, aLI)
bref_value <- read_csv("data/bref_value_pitching.csv", show_col_types = FALSE) |>
  mutate(IP_br = floor(p_ip) + (p_ip %% 1) * 10 / 3) |>
  group_by(bref_id, Season) |>
  filter(n() == 1 | team_ID == "TOT" | grepl("^[0-9]TM$", team_ID)) |>
  filter(IP_br == max(IP_br)) |>
  slice(1) |>
  ungroup() |>
  transmute(bref_id, Season, br_G = p_g, br_GS = p_gs, br_IP = IP_br, WAA_br = p_waa,
      WAA_adj_br = coalesce(p_waa_adj, 0),
            RAA_br = p_raa, WAR_br = p_war, RA9role_br = p_ra9_role)
bref_wp <- read_csv("data/bref_win_probability.csv", show_col_types = FALSE) |>
  group_by(bref_id, Season) |>
  filter(n() == 1 | team_ID == "TOT") |>
  filter(IP == max(IP)) |>
  slice(1) |>
  ungroup() |>
  select(bref_id, Season, WPA = wpa_def, aLI = leverage_index_avg)
id_map <- read_csv("data/id_map.csv", show_col_types = FALSE) |> distinct(bref_id, .keep_all = TRUE)

seasons <- fg_season |>
  inner_join(id_map |> select(xMLBAMID, bref_id), by = "xMLBAMID") |>
  inner_join(bref_value, by = c("bref_id", "Season")) |>
  inner_join(bref_wp, by = c("bref_id", "Season")) |>
  mutate(WAA_br_adj = WAA_br + WAA_adj_br)

sink("results/waa_analysis.txt")
cat("FanGraphs pitcher seasons matched to Baseball-Reference value and win probability:", nrow(seasons), "of",
    nrow(fg_season), "\n\n")

cat("1. Is the Baseball-Reference WAA baseline adjusted for role? IP weighted mean WAA per 100 IP\n")
pure <- bref_value |> filter(br_IP >= 20)
cat("Pure relievers (no starts):", round(sum(pure$WAA_br[pure$br_GS == 0]) / sum(pure$br_IP[pure$br_GS == 0]) * 100, 2),
    " pure starters (all games starts):", round(sum(pure$WAA_br[pure$br_GS == pure$br_G]) / sum(pure$br_IP[pure$br_GS == pure$br_G]) * 100, 2),
    " mean role adjustment (RA9 role) relievers:", round(mean(pure$RA9role_br[pure$br_GS == 0]), 2), " starters:",
    round(mean(pure$RA9role_br[pure$br_GS == pure$br_G]), 2), "\n")
cat("FanGraphs WAR per 100 IP for comparison, relievers:",
    round(sum(seasons$WAR[seasons$GS == 0 & seasons$IP >= 20]) / sum(seasons$IP[seasons$GS == 0 & seasons$IP >= 20]) * 100, 2),
    " starters:", round(sum(seasons$WAR[seasons$GS == seasons$G & seasons$IP >= 20]) / sum(seasons$IP[seasons$GS == seasons$G & seasons$IP >= 20]) * 100, 2), "\n\n")

# Relievers who stayed relievers (same definition as 06)
relievers <- seasons |> filter(GS / G <= 0.2, TBF >= 100)
next_year <- seasons |>
  transmute(playerid, Season, next_G = G, next_GS = GS, next_IP = IP, next_WAR = WAR, next_WAA_fg = WAA_fg,
            next_WAA_br = WAA_br, next_WAA_br_adj = WAA_br_adj, next_WPA = WPA)
stayers <- relievers |>
  mutate(next_season = Season + 1) |>
  inner_join(next_year, by = c("playerid", "next_season" = "Season")) |>
  filter(next_GS / next_G <= 0.2, next_IP >= 20)

measures <- c("WAR", "WAA_fg", "WAA_br", "WAA_br_adj", "WPA")
cat("2. How well each measure repeats for", nrow(stayers), "relievers who stayed relievers\n")
cat("Correlation of this season with next season. Totals include innings pitched, rates are per 65 IP. Standard error is about",
    round(1 / sqrt(nrow(stayers) - 3), 3), "\n")
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
cat("(r_with_skill_wins_same_season shows how much of each measure is the SIERA based skill signal)\n\n")

# Conversion gains: actual next season minus the stay-in-the-pen projection, same method as value_analysis.R
cat("3. Conversion gain by measure (actual starter season minus the stay-in-the-pen projection), 114 attempts\n")
lg_rp <- league_rp
relief_info <- role_seasons |>
  filter(role == "rel") |>
  left_join(lg_rp, by = "Season") |>
  transmute(playerid, Season, rp_SIERA_vs_lg = SIERA - lg_rp_siera, rp_age = Age)
stay_fit <- stayers |>
  left_join(relief_info, by = c("playerid", "Season"))
attempts <- read_csv("data/value_attempts.csv", show_col_types = FALSE) |>
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
cat("Check against value_analysis.R: mean WAR gain here", round(mean(attempts$WAR_gain), 3), " there",
    round(mean(attempts$WAR_gain_06), 3),
    " (n =", nrow(attempts), ")\n\n")

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
gain_table <- gain_summary(attempts, "all 114 attempts")
gain_table <- bind_rows(gain_table, gain_summary(attempts |> filter(attempt == "5+ starts"), "5+ starts"))
gain_table <- bind_rows(gain_table, gain_summary(attempts |> filter(attempt == "1 to 4 starts"), "1 to 4 starts"))
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
cat("\nStandard errors and shares gaining\n")
print(as.data.frame(gain_table |> filter(measure %in% c("WAR", "WAA_fg", "WAA_br", "WAA_br_adj")) |>
                      select(group, n, measure, se, share_gaining) |>
                      tidyr::pivot_wider(id_cols = c(group, n), names_from = measure, values_from = c(se,
                        share_gaining))), row.names = FALSE)

cat("\n4. Do the measures agree on who gained? Spearman correlation of conversion gains, 114 attempts\n")
gain_matrix <- attempts[, paste0(gain_measures, "_gain")]
print(round(cor(gain_matrix, method = "spearman"), 2))
sink()
cat(readLines("results/waa_analysis.txt"), sep = "\n")

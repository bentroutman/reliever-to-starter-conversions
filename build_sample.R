# Builds the reliever to starter conversion sample from the FanGraphs role splits
# Run after json_to_csv.py
# Outputs: data/conversions.csv (one row per conversion) and data/short_attempts.csv

library(dplyr)
library(readr)

role_seasons <- read_csv("data/fg_role_seasons.csv", guess_max = 20000, show_col_types = FALSE)

# FanGraphs writes 64 1/3 innings as 64.1
role_seasons <- role_seasons |>
  mutate(IP = floor(IP) + (IP %% 1) * 10 / 3)

# Total games and starts per pitcher season (the sta row has G = GS, the rel row has relief games)
season_totals <- role_seasons |>
  group_by(playerid, Season) |>
  summarize(G_total = sum(G), GS_total = sum(GS), IP_total = sum(IP), .groups = "drop")

# Minor league lines (2006 on, no 2020 season). Rehab starts count here too
minors <- read_csv("data/fg_minors.csv", show_col_types = FALSE) |>
  mutate(IP = floor(IP) + (IP %% 1) * 10 / 3) |>
  group_by(playerid, Season) |>
  summarize(MiLB_G = sum(G), MiLB_GS = sum(GS), MiLB_IP = sum(IP), .groups = "drop")

# League average starter line each year, used to put SIERA on the same scale across seasons
league_sp <- role_seasons |>
  filter(role == "sta") |>
  group_by(Season) |>
  summarize(lg_sp_siera = weighted.mean(SIERA, TBF, na.rm = TRUE),
            lg_sp_kbb = weighted.mean(`K-BB%`, TBF, na.rm = TRUE))

league_rp <- role_seasons |>
  filter(role == "rel") |>
  group_by(Season) |>
  summarize(lg_rp_siera = weighted.mean(SIERA, TBF, na.rm = TRUE))

# Starter seasons: at least 5 starts, mostly starting, and 3.5+ IP per start to drop openers
starter_seasons <- role_seasons |>
  filter(role == "sta") |>
  left_join(season_totals, by = c("playerid", "Season")) |>
  filter(GS >= 5, GS_total / G_total >= 0.5, IP / GS >= 3.5)

# Relief seasons: 100+ batters faced in relief and no more than 20% of games as starts
relief_seasons <- role_seasons |>
  filter(role == "rel") |>
  left_join(season_totals, by = c("playerid", "Season")) |>
  filter(TBF >= 100, GS_total / G_total <= 0.2)

# Usage columns are NA when a pitch is not thrown
usage_cols <- c("pfxFA%", "pfxFC%", "pfxFS%", "pfxFO%", "pfxSI%", "pfxSL%", "pfxCU%", "pfxKC%",
                "pfxCH%", "pfxSC%", "pfxKN%", "pfxST%", "pfxCV%", "pfxEP%")
usage <- as.matrix(relief_seasons[, usage_cols])
usage[is.na(usage)] <- 0

# Arsenal measures from the relief season
relief_seasons$NumPitches10 <- rowSums(usage >= 0.10)
relief_seasons$NumPitches5 <- rowSums(usage >= 0.05)
relief_seasons$TopTwoUsage <- NA
relief_seasons$ThirdPitchUsage <- NA
for (i in 1:nrow(usage)) {
  sorted_usage <- sort(usage[i, ], decreasing = TRUE)
  relief_seasons$TopTwoUsage[i] <- sorted_usage[1] + sorted_usage[2]
  relief_seasons$ThirdPitchUsage[i] <- sorted_usage[3]
}
relief_seasons$OffspeedUsage <- usage[, "pfxCH%"] + usage[, "pfxFS%"] + usage[, "pfxFO%"] + usage[, "pfxSC%"]
relief_seasons$FastballUsage <- usage[, "pfxFA%"] + usage[, "pfxSI%"]

# Stuff+ by pitch type (2020 on), only counting pitches thrown at least 5% of the time
# FanGraphs Stuff+ files sweepers under SL, so SL usage includes ST
relief_seasons$FB_Stuff <- NA
relief_seasons$BestSecondary_Stuff <- NA
relief_seasons$SecondSecondary_Stuff <- NA
relief_seasons$Offspeed_Stuff <- NA
relief_seasons$NumPlusPitches <- NA
for (i in 1:nrow(relief_seasons)) {
  if (is.na(relief_seasons$sp_stuff[i])) next
  pitch_stuff <- c(FF = relief_seasons$sp_s_FF[i], SI = relief_seasons$sp_s_SI[i],
                   FC = relief_seasons$sp_s_FC[i], SL = relief_seasons$sp_s_SL[i],
                   CU = relief_seasons$sp_s_CU[i], KC = relief_seasons$sp_s_KC[i],
                   CH = relief_seasons$sp_s_CH[i], FS = relief_seasons$sp_s_FS[i],
                   FO = relief_seasons$sp_s_FO[i])
  pitch_usage <- c(FF = usage[i, "pfxFA%"], SI = usage[i, "pfxSI%"], FC = usage[i, "pfxFC%"],
                   SL = usage[i, "pfxSL%"] + usage[i, "pfxST%"], CU = usage[i, "pfxCU%"] + usage[i, "pfxCV%"],
                   KC = usage[i, "pfxKC%"], CH = usage[i, "pfxCH%"], FS = usage[i, "pfxFS%"],
                   FO = usage[i, "pfxFO%"])
  used <- !is.na(pitch_stuff) & pitch_usage >= 0.05
  fastballs <- used & names(pitch_stuff) %in% c("FF", "SI")
  secondaries <- sort(pitch_stuff[used & !names(pitch_stuff) %in% c("FF", "SI")], decreasing = TRUE)
  offspeed <- pitch_stuff[used & names(pitch_stuff) %in% c("CH", "FS", "FO")]

  if (any(fastballs)) {
    relief_seasons$FB_Stuff[i] <- pitch_stuff[fastballs][which.max(pitch_usage[fastballs])]
  }
  relief_seasons$BestSecondary_Stuff[i] <- secondaries[1]
  relief_seasons$SecondSecondary_Stuff[i] <- secondaries[2]  # NA when there is only one secondary
  if (length(offspeed) > 0) relief_seasons$Offspeed_Stuff[i] <- max(offspeed)
  relief_seasons$NumPlusPitches[i] <- sum(pitch_stuff[used] >= 100)
}

# Primary fastball velo is the four-seam unless the sinker is thrown more
relief_seasons <- relief_seasons |>
  mutate(FBVelo = ifelse(coalesce(`pfxSI%`, 0) > coalesce(`pfxFA%`, 0), pfxvSI, pfxvFA),
         PitchesPerG = Pitches / G,
         TBFPerG = TBF / G)
starter_seasons <- starter_seasons |>
  mutate(FBVelo = ifelse(coalesce(`pfxSI%`, 0) > coalesce(`pfxFA%`, 0), pfxvSI, pfxvFA))

relief_side <- relief_seasons |>
  left_join(league_rp, by = "Season") |>
  transmute(playerid, rp_season = Season, rp_age = Age, rp_G = G, rp_IP = IP, rp_TBF = TBF,
            rp_SIERA = SIERA, rp_SIERA_vs_lg = SIERA - lg_rp_siera,
            rp_K = `K%`, rp_BB = `BB%`, rp_KBB = `K-BB%`, rp_GB = `GB%`,
            rp_Zone = `Zone%`, rp_FStrike = `F-Strike%`, rp_SwStr = `SwStr%`, rp_CStr = `CStr%`,
            rp_OSwing = `O-Swing%`, rp_ZContact = `Z-Contact%`, rp_OContact = `O-Contact%`,
            rp_HardHit = `HardHit%`, rp_Barrel = `Barrel%`, rp_gmLI = gmLI,
            rp_FBVelo = FBVelo, rp_FA_IVB = `pfxFA-Z`,
            rp_NumPitches10 = NumPitches10, rp_NumPitches5 = NumPitches5,
            rp_TopTwoUsage = TopTwoUsage, rp_ThirdPitchUsage = ThirdPitchUsage,
            rp_OffspeedUsage = OffspeedUsage, rp_FastballUsage = FastballUsage,
            rp_PitchesPerG = PitchesPerG, rp_TBFPerG = TBFPerG,
            rp_StuffPlus = sp_stuff, rp_LocationPlus = sp_location, rp_PitchingPlus = sp_pitching,
            rp_botStf = pb_stuff, rp_botCmd = pb_command,
            rp_FB_Stuff = FB_Stuff, rp_BestSecondary_Stuff = BestSecondary_Stuff,
            rp_SecondSecondary_Stuff = SecondSecondary_Stuff, rp_Offspeed_Stuff = Offspeed_Stuff,
            rp_NumPlusPitches = NumPlusPitches)

starter_side <- starter_seasons |>
  left_join(league_sp, by = "Season") |>
  transmute(playerid, xMLBAMID, PlayerName, Throws, sp_season = Season, sp_team = TeamNameAbb,
            sp_GS = GS, sp_IP = IP, sp_TBF = TBF, sp_WAR = WAR,
            sp_SIERA = SIERA, sp_SIERA_vs_lg = SIERA - lg_sp_siera,
            sp_K = `K%`, sp_BB = `BB%`, sp_KBB = `K-BB%`, sp_KBB_vs_lg = `K-BB%` - lg_sp_kbb,
            sp_FBVelo = FBVelo, sp_StuffPlus = sp_stuff, sp_LocationPlus = sp_location)

# Pair each starter season with the most recent relief season in the 3 years before it
pairs <- starter_side |>
  inner_join(relief_side, by = "playerid", relationship = "many-to-many") |>
  mutate(gap = sp_season - rp_season) |>
  filter(gap >= 1, gap <= 3) |>
  group_by(playerid, sp_season) |>
  filter(gap == min(gap)) |>
  ungroup()

# Starts made between the relief season and the starter season (should be close to none)
pairs$GS_between <- 0
pairs$prev_MLB_GS <- 0
pairs$next_GS <- NA
for (i in 1:nrow(pairs)) {
  player_totals <- season_totals |> filter(playerid == pairs$playerid[i])
  between <- player_totals |> filter(Season > pairs$rp_season[i], Season < pairs$sp_season[i])
  before <- player_totals |> filter(Season < pairs$rp_season[i])
  after <- player_totals |> filter(Season == pairs$sp_season[i] + 1)
  pairs$GS_between[i] <- sum(between$GS_total)
  pairs$prev_MLB_GS[i] <- sum(before$GS_total)  # only counts 2014 on
  if (pairs$sp_season[i] < 2026) pairs$next_GS[i] <- sum(after$GS_total)
}

# Minor league starting history through the relief season
pairs$MiLB_G <- 0
pairs$MiLB_GS <- 0
pairs$MiLB_max_IP <- 0
pairs$MaxProIP <- 0
for (i in 1:nrow(pairs)) {
  player_minors <- minors |>
    filter(playerid == as.character(pairs$playerid[i]), Season <= pairs$rp_season[i])
  player_mlb <- season_totals |>
    filter(playerid == pairs$playerid[i], Season <= pairs$rp_season[i])
  pairs$MiLB_G[i] <- sum(player_minors$MiLB_G)
  pairs$MiLB_GS[i] <- sum(player_minors$MiLB_GS)
  pairs$MiLB_max_IP[i] <- max(c(0, player_minors$MiLB_IP))

  # Most innings in any one pro season, minors and majors combined (majors only counted from 2014)
  pro_ip <- full_join(player_minors, player_mlb, by = "Season") |>
    mutate(pro_IP = coalesce(MiLB_IP, 0) + coalesce(IP_total, 0))
  pairs$MaxProIP[i] <- max(c(0, pro_ip$pro_IP))
}
pairs$MiLB_GS_share <- ifelse(pairs$MiLB_G > 0, pairs$MiLB_GS / pairs$MiLB_G, NA)

conversions <- pairs |>
  filter(GS_between <= 3, sp_season >= 2015) |>
  mutate(KBB_change = sp_KBB - rp_KBB,
         SIERA_change = sp_SIERA - rp_SIERA,
         Velo_change = sp_FBVelo - rp_FBVelo,
         sp_WAR_per_150 = sp_WAR / sp_IP * 150,
         stuck = next_GS >= 10) |>
  arrange(sp_season, PlayerName)

write_csv(conversions, "data/conversions.csv")

cat("Conversion attempts:", nrow(conversions), "\n")
cat("Unique pitchers:", n_distinct(conversions$playerid), "\n")
cat("Attempts with 60+ starter IP:", sum(conversions$sp_IP >= 60), "\n")
print(table(conversions$sp_season))
print(table(gap = conversions$gap))

# Short attempts that miss the 5 start cutoff (1 to 4 real starts the year after a relief season)
short_attempts <- role_seasons |>
  filter(role == "sta", GS <= 4, IP / GS >= 3.5) |>
  transmute(playerid, PlayerName, sp_season = Season, sp_GS = GS, sp_IP = IP, sp_SIERA = SIERA,
            rp_season = Season - 1) |>
  inner_join(relief_side, by = c("playerid", "rp_season")) |>
  arrange(sp_season, PlayerName)
write_csv(short_attempts, "data/short_attempts.csv")
cat("Short attempts (1 to 4 starts):", nrow(short_attempts), "\n")
cat("Qualifying relief seasons, 2014 to 2025:", sum(relief_side$rp_season <= 2025), "\n")

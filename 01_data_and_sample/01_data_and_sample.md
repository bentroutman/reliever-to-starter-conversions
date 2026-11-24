Step 1: Data and Sample
================

``` r
library(knitr)
opts_chunk$set(message = FALSE)
```

``` r
library(chromote)
library(dplyr)
library(jsonlite)
library(processx)
library(readr)
library(rvest)
```

## Pull FanGraphs

Starter and reliever lines (2014 to 2026) and minor league lines (2006
to 2026), pulled from FanGraphs. This notebook has eval=FALSE, so
knitting does not download everything again. Run the code blocks by hand
the first time to download the data.

``` r
site <- "https://www.fangraphs.com"
columns <- c("playerid", "xMLBAMID", "PlayerName", "Throws", "Season", "Age", "TeamNameAbb", "G", "GS", "IP", "TBF",
             "Pitches", "WAR", "RAR", "SIERA", "xFIP", "FIP", "K%", "BB%", "K-BB%", "GB%", "Zone%", "F-Strike%",
             "SwStr%", "CStr%", "O-Swing%", "O-Contact%", "Z-Contact%", "gmLI", "HardHit%", "Barrel%",
             "pfxFA%", "pfxFC%", "pfxFS%", "pfxFO%", "pfxSI%", "pfxSL%", "pfxCU%", "pfxKC%", "pfxCH%", "pfxSC%",
             "pfxKN%", "pfxST%", "pfxCV%", "pfxEP%", "pfxvFA", "pfxvSI", "pfxvFC", "pfxvSL", "pfxvCH", "pfxFA-Z",
             "sp_stuff", "sp_location", "sp_pitching", "sp_s_FF", "sp_s_SI", "sp_s_FC", "sp_s_SL", "sp_s_CU",
             "sp_s_CH", "sp_s_FS", "sp_s_KC", "sp_s_FO", "pb_stuff", "pb_command")

# Open a Chrome window and connect to it
chrome <- process$new(find_chrome(), c("--remote-debugging-port=9222", "--no-first-run",
                                       paste0("--user-data-dir=", tempfile("chrome_profile"))))
Sys.sleep(2)
tab <- ChromoteSession$new(parent = Chromote$new(browser = ChromeRemote$new("127.0.0.1", 9222)))
tab$go_to(paste0(site, "/leaders/major-league"))
```

If a security check shows up in the Chrome window, complete it, then run
the next code block.

``` r
# Takes about 5 minutes
role_seasons <- NULL
for (season in 2014:2026) {
  for (role_name in c("sta", "rel")) {
    url <- paste0(site, "/api/leaders/major-league/data?pos=all&lg=all&qual=0&month=0&team=0&pageitems=2000",
                  "&pagenum=1&ind=1&rost=0&players=0&sortdir=desc&sortstat=WAR&type=8&stats=", role_name,
                  "&season=", season, "&season1=", season)
    tab$go_to(url)
    page_text <- tab$Runtime$evaluate("document.body.innerText")$result$value
    players <- fromJSON(page_text)$data |>
      select(all_of(columns)) |>
      mutate(role = role_name)
    cat(season, role_name, nrow(players), "pitchers\n")
    role_seasons <- bind_rows(role_seasons, players)
    Sys.sleep(4)  # FanGraphs blocks requests that come too fast
  }
}

# No minor league season in 2020
minors <- NULL
for (season in setdiff(2006:2026, 2020)) {
  url <- paste0(site, "/api/leaders/minor-league/data?pos=all&level=0&lg=2,4,5,6,7,8,9,10,11,14,12,13,15,16,17,18,30,32",
                "&stats=pit&qual=0&type=0&team=&season=", season, "&seasonEnd=", season,
                "&org=&ind=0&splitTeam=false")
  tab$go_to(url)
  page_text <- tab$Runtime$evaluate("document.body.innerText")$result$value
  page_data <- fromJSON(page_text)
  if (!is.data.frame(page_data)) page_data <- page_data$data
  players <- page_data |>
    transmute(playerid = as.character(playerids), xMLBAMID, PlayerName, Season = season, Age, G, GS, IP, TBF)
  cat(season, "minors", nrow(players), "pitchers\n")
  minors <- bind_rows(minors, players)
  Sys.sleep(8)
}

write_csv(role_seasons, "../data/fg_role_seasons.csv", na = "")
write_csv(minors, "../data/fg_minors.csv", na = "")
chrome$kill()
```

## Pull game logs

Pitches, batters faced, strikeouts, and walks for every appearance, from
the MLB Stats API. Includes each season since 2015 with at least one
start.

``` r
# Takes about 30 minutes
role_seasons <- read_csv("../data/fg_role_seasons.csv", guess_max = 20000)
log_seasons <- role_seasons |>
  group_by(xMLBAMID, Season) |>
  summarize(GS_total = sum(GS), .groups = "drop") |>
  filter(Season >= 2015, GS_total >= 1)

game_logs <- NULL
for (i in 1:nrow(log_seasons)) {
  url <- paste0("https://statsapi.mlb.com/api/v1/people/", log_seasons$xMLBAMID[i],
                "/stats?stats=gameLog&group=pitching&season=", log_seasons$Season[i], "&gameType=R")
  splits <- fromJSON(url, flatten = TRUE)$stats$splits[[1]]
  game_logs <- bind_rows(game_logs, splits |>
    transmute(xMLBAMID = log_seasons$xMLBAMID[i], Season = log_seasons$Season[i], date, game_pk = game.gamePk,
              GS = stat.gamesStarted, Pitches = stat.numberOfPitches, TBF = stat.battersFaced,
              SO = stat.strikeOuts, BB = stat.baseOnBalls))
  if (i %% 100 == 0) cat(i, "of", nrow(log_seasons), "seasons\n")
  Sys.sleep(0.2)
}
write_csv(game_logs, "../data/game_logs.csv")
cat("Appearances:", nrow(game_logs), "\n")
```

## Build the conversion sample

I counted a season as a conversion attempt when the pitcher made at
least 2 starts of 70+ pitches. I did not require a minimum share of
games started or innings per start, since those rules drop attempts that
went badly or ended early. Each attempt is paired with the most recent
relief season in the three years before it (100+ batters faced, with
starts in 20% or less of games). The starter results use all of his
starts that season, including the short ones.

``` r
role_seasons <- read_csv("../data/fg_role_seasons.csv", guess_max = 20000)

# Rewrite innings ending in .1 or .2 as 1/3 or 2/3
role_seasons <- role_seasons |>
  mutate(IP = floor(IP) + (IP %% 1) * 10 / 3)

# The sta row has G = GS and the rel row has the relief games
season_totals <- role_seasons |>
  group_by(playerid, Season) |>
  summarize(G_total = sum(G), GS_total = sum(GS), IP_total = sum(IP), .groups = "drop")
```

``` r
minors <- read_csv("../data/fg_minors.csv") |>
  mutate(IP = floor(IP) + (IP %% 1) * 10 / 3) |>
  group_by(playerid, Season) |>
  summarize(MiLB_G = sum(G), MiLB_GS = sum(GS), MiLB_IP = sum(IP), .groups = "drop")

# League average SIERA for starters and relievers each season
league_sp <- role_seasons |>
  filter(role == "sta") |>
  group_by(Season) |>
  summarize(lg_sp_siera = weighted.mean(SIERA, TBF, na.rm = TRUE))

league_rp <- role_seasons |>
  filter(role == "rel") |>
  group_by(Season) |>
  summarize(lg_rp_siera = weighted.mean(SIERA, TBF, na.rm = TRUE))
```

``` r
game_logs <- read_csv("../data/game_logs.csv")
long_starts <- game_logs |>
  filter(GS == 1, Pitches >= 70) |>
  group_by(xMLBAMID, Season) |>
  summarize(LongStarts = n(), .groups = "drop")

starter_seasons <- role_seasons |>
  filter(role == "sta") |>
  inner_join(long_starts, by = c("xMLBAMID", "Season")) |>
  filter(LongStarts >= 2)

relief_seasons <- role_seasons |>
  filter(role == "rel") |>
  left_join(season_totals, by = c("playerid", "Season")) |>
  filter(TBF >= 100, GS_total / G_total <= 0.2)
```

``` r
usage_cols <- c("pfxFA%", "pfxFC%", "pfxFS%", "pfxFO%", "pfxSI%", "pfxSL%", "pfxCU%", "pfxKC%",
                "pfxCH%", "pfxSC%", "pfxKN%", "pfxST%", "pfxCV%", "pfxEP%")
usage <- as.matrix(relief_seasons[, usage_cols])
usage[is.na(usage)] <- 0

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
```

``` r
# Stuff+ by pitch type (2020 on), only for pitches thrown at least 5% of the time
# FanGraphs files sweepers under SL
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
  relief_seasons$SecondSecondary_Stuff[i] <- secondaries[2]  # NA if there is only one secondary
  if (length(offspeed) > 0) relief_seasons$Offspeed_Stuff[i] <- max(offspeed)
  relief_seasons$NumPlusPitches[i] <- sum(pitch_stuff[used] >= 100)
}
```

``` r
# Primary fastball is the four-seam unless the sinker is thrown more (cutter if neither is thrown)
relief_seasons <- relief_seasons |>
  mutate(FBVelo = case_when(coalesce(`pfxSI%`, 0) > coalesce(`pfxFA%`, 0) ~ pfxvSI,
                            coalesce(`pfxFA%`, 0) > 0 ~ pfxvFA,
                            TRUE ~ pfxvFC),
         PitchesPerG = Pitches / G,
         TBFPerG = TBF / G)
starter_seasons <- starter_seasons |>
  mutate(FBVelo = case_when(coalesce(`pfxSI%`, 0) > coalesce(`pfxFA%`, 0) ~ pfxvSI,
                            coalesce(`pfxFA%`, 0) > 0 ~ pfxvFA,
                            TRUE ~ pfxvFC))

relief_side <- relief_seasons |>
  left_join(league_rp, by = "Season") |>
  transmute(playerid, rp_season = Season, rp_age = Age, rp_IP = IP, rp_TBF = TBF,
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
            rp_StuffPlus = sp_stuff, rp_LocationPlus = sp_location,
            rp_botStf = pb_stuff, rp_botCmd = pb_command,
            rp_FB_Stuff = FB_Stuff, rp_BestSecondary_Stuff = BestSecondary_Stuff,
            rp_SecondSecondary_Stuff = SecondSecondary_Stuff, rp_Offspeed_Stuff = Offspeed_Stuff,
            rp_NumPlusPitches = NumPlusPitches)

starter_side <- starter_seasons |>
  left_join(league_sp, by = "Season") |>
  transmute(playerid, xMLBAMID, PlayerName, Throws, sp_season = Season,
            sp_GS = GS, sp_LongStarts = LongStarts, sp_IP = IP, sp_TBF = TBF, sp_WAR = WAR,
            sp_SIERA = SIERA, sp_SIERA_vs_lg = SIERA - lg_sp_siera, sp_KBB = `K-BB%`, sp_FBVelo = FBVelo)
```

``` r
# Pair each starter season with the most recent relief season in the 3 years before it
pairs <- starter_side |>
  inner_join(relief_side, by = "playerid", relationship = "many-to-many") |>
  mutate(gap = sp_season - rp_season) |>
  filter(gap >= 1, gap <= 3) |>
  group_by(playerid, sp_season) |>
  filter(gap == min(gap)) |>
  ungroup()
```

``` r
pairs$GS_between <- 0
pairs$prev_MLB_GS <- 0
pairs$next_GS <- NA
for (i in 1:nrow(pairs)) {
  player_totals <- season_totals |> filter(playerid == pairs$playerid[i])
  between <- player_totals |> filter(Season > pairs$rp_season[i], Season < pairs$sp_season[i])
  before <- player_totals |> filter(Season < pairs$rp_season[i])
  after <- player_totals |> filter(Season == pairs$sp_season[i] + 1)
  pairs$GS_between[i] <- sum(between$GS_total)
  pairs$prev_MLB_GS[i] <- sum(before$GS_total)
  if (pairs$sp_season[i] < 2026) pairs$next_GS[i] <- sum(after$GS_total)
}
```

``` r
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

  # Most innings in one season, minors and majors combined
  pro_ip <- full_join(player_minors, player_mlb, by = "Season") |>
    mutate(pro_IP = coalesce(MiLB_IP, 0) + coalesce(IP_total, 0))
  pairs$MaxProIP[i] <- max(c(0, pro_ip$pro_IP))
}
pairs$MiLB_GS_share <- ifelse(pairs$MiLB_G > 0, pairs$MiLB_GS / pairs$MiLB_G, NA)

conversions <- pairs |>
  filter(GS_between <= 3) |>
  mutate(KBB_change = sp_KBB - rp_KBB,
         SIERA_change = sp_SIERA - rp_SIERA,
         Velo_change = sp_FBVelo - rp_FBVelo,
         sp_WAR_per_150 = sp_WAR / sp_IP * 150,
         stuck = next_GS >= 10) |>
  arrange(sp_season, PlayerName)

write_csv(conversions, "../data/conversions.csv")

cat("Conversion attempts:", nrow(conversions), "\n")
```

    ## Conversion attempts: 145

``` r
cat("Unique pitchers:", n_distinct(conversions$playerid), "\n")
```

    ## Unique pitchers: 129

``` r
cat("Attempts with 5+ starts:", sum(conversions$sp_GS >= 5), "\n")
```

    ## Attempts with 5+ starts: 122

``` r
print(table(conversions$sp_season))
```

    ## 
    ## 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 2025 2026 
    ##    7   12   16   10   16   10    9   13   15   15   12   10

``` r
print(table(season_gap = conversions$gap))
```

    ## season_gap
    ##   1   2   3 
    ## 126  13   6

This gives 145 conversion attempts from 129 pitchers, and 122 of them
have 5 or more starts. The relief season was the year before for 126
attempts, two years before for 13, and three years before for 6.

Some pitchers moved to the rotation in the middle of a season, so they
are not in the sample above. I counted a mid-season move when the
pitcher faced 50+ batters in relief with starts in 20% or less of his
games, and then made at least 2 starts of 70+ pitches. His relief
numbers are the games before the first long start and his starter
numbers are every start from then on.

``` r
appearances <- game_logs |>
  group_by(xMLBAMID, Season) |>
  mutate(FirstLongStart = ifelse(any(GS == 1 & Pitches >= 70), min(date[GS == 1 & Pitches >= 70]), NA),
         LongStarts = sum(GS == 1 & Pitches >= 70)) |>
  ungroup() |>
  filter(LongStarts >= 2)

before_move <- appearances |>
  filter(date < FirstLongStart) |>
  group_by(xMLBAMID, Season) |>
  summarize(start_share_before = mean(GS), before_TBF = sum(TBF[GS == 0]),
            before_KBB = sum((SO - BB)[GS == 0]) / before_TBF, .groups = "drop") |>
  filter(before_TBF >= 50, start_share_before <= 0.2)

after_move <- appearances |>
  filter(date >= FirstLongStart, GS == 1) |>
  group_by(xMLBAMID, Season) |>
  summarize(after_GS = n(), after_KBB = sum(SO - BB) / sum(TBF), .groups = "drop")

last_year <- season_totals |>
  transmute(playerid, Season = Season + 1, GS_last_year = GS_total)

midseason_attempts <- before_move |>
  inner_join(after_move, by = c("xMLBAMID", "Season")) |>
  inner_join(role_seasons |> distinct(xMLBAMID, playerid, Season, PlayerName), by = c("xMLBAMID", "Season")) |>
  left_join(last_year, by = c("playerid", "Season")) |>
  mutate(GS_last_year = coalesce(GS_last_year, 0)) |>
  anti_join(conversions, by = c("xMLBAMID", "Season" = "sp_season")) |>
  select(xMLBAMID, playerid, PlayerName, Season, before_KBB, after_GS, after_KBB, GS_last_year)

write_csv(midseason_attempts, "../data/midseason_attempts.csv")
cat("Mid-season attempts not already in the sample:", nrow(midseason_attempts), "\n")
```

    ## Mid-season attempts not already in the sample: 133

## Pull relief season pitches

Every Statcast pitch from the relief season of each pitcher in the
sample.

``` r
# Takes about 8 minutes
keep_columns <- c("pitcher", "game_year", "game_pk", "pitch_type", "release_speed", "stand", "p_throws",
                  "description", "events", "inning", "at_bat_number", "pitch_number", "n_thruorder_pitcher",
                  "woba_value", "woba_denom", "estimated_woba_using_speedangle")

# Statcast starts in 2015
pitcher_seasons <- read_csv("../data/conversions.csv") |>
  filter(rp_season >= 2015) |>
  distinct(xMLBAMID, rp_season, PlayerName)

all_pitches <- NULL
for (i in 1:nrow(pitcher_seasons)) {
  url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&type=details&player_type=pitcher",
                "&hfGT=R%7C&hfSea=", pitcher_seasons$rp_season[i],
                "%7C&pitchers_lookup%5B%5D=", pitcher_seasons$xMLBAMID[i])
  # Read as text so every pitcher season stacks
  pitches <- read_csv(url, col_types = cols(.default = "c"))
  cat(pitcher_seasons$PlayerName[i], pitcher_seasons$rp_season[i], nrow(pitches), "pitches\n")
  if (nrow(pitches) > 0) {
    all_pitches <- bind_rows(all_pitches, pitches |> select(all_of(keep_columns)))
  }
  Sys.sleep(1)
}

write_csv(all_pitches, "../data/savant/relief_season_pitches.csv", na = "")
cat("Saved", nrow(all_pitches), "pitches\n")
```

## Pull starter season pitches

``` r
# Takes about 11 minutes
keep_columns <- c("pitcher", "game_year", "game_pk", "pitch_type", "release_speed", "stand", "p_throws",
                  "description", "events", "inning", "at_bat_number", "pitch_number", "n_thruorder_pitcher",
                  "woba_value", "woba_denom", "estimated_woba_using_speedangle")

pitcher_seasons <- read_csv("../data/conversions.csv") |>
  distinct(xMLBAMID, sp_season, PlayerName)

all_pitches <- NULL
for (i in 1:nrow(pitcher_seasons)) {
  url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&type=details&player_type=pitcher",
                "&hfGT=R%7C&hfSea=", pitcher_seasons$sp_season[i],
                "%7C&pitchers_lookup%5B%5D=", pitcher_seasons$xMLBAMID[i])
  pitches <- read_csv(url, col_types = cols(.default = "c"))
  cat(pitcher_seasons$PlayerName[i], pitcher_seasons$sp_season[i], nrow(pitches), "pitches\n")
  if (nrow(pitches) > 0) {
    all_pitches <- bind_rows(all_pitches, pitches |> select(all_of(keep_columns)))
  }
  Sys.sleep(1)
}

write_csv(all_pitches, "../data/savant/starter_season_pitches.csv", na = "")
cat("Saved", nrow(all_pitches), "pitches\n")
```

## Pull win probability

WPA and average leverage from Baseball-Reference, plus the Chadwick
register to match player IDs.

``` r
# Takes about 2 minutes
stat_columns <- c("age", "team_ID", "IP", "wpa_def", "leverage_index_avg")

win_probability <- NULL
for (season in 2014:2026) {
  url <- paste0("https://www.baseball-reference.com/leagues/majors/", season, "-win_probability-pitching.shtml")
  # The table is inside an HTML comment
  page <- paste(read_lines(url), collapse = "\n")
  page <- gsub("<!--|-->", "", page)
  rows <- read_html(page) |>
    html_elements("#players_win_probability_pitching tr:has([data-append-csv])")

  season_table <- data.frame(bref_id = rows |> html_element("[data-append-csv]") |> html_attr("data-append-csv"),
                             Season = season)
  for (column in stat_columns) {
    season_table[[column]] <- rows |> html_element(paste0("[data-stat='", column, "']")) |> html_text()
    season_table[[column]] <- gsub("%", "", gsub(" ", " ", season_table[[column]]))
  }
  cat(season, nrow(season_table), "rows\n")
  win_probability <- bind_rows(win_probability, season_table)
  Sys.sleep(5)
}
write_csv(win_probability, "../data/bref_win_probability.csv", na = "")

# The register is split into 16 files by the first character of the person key
id_map <- NULL
for (letter in c(0:9, "a", "b", "c", "d", "e", "f")) {
  url <- paste0("https://raw.githubusercontent.com/chadwickbureau/register/master/data/people-", letter, ".csv")
  people <- read_csv(url, col_types = cols(.default = "c")) |>
    filter(!is.na(key_mlbam), !is.na(key_bbref)) |>
    select(xMLBAMID = key_mlbam, bref_id = key_bbref)
  id_map <- bind_rows(id_map, people)
}
write_csv(id_map, "../data/id_map.csv", na = "")
cat("ID map:", nrow(id_map), "people\n")
```

## Pull pitching value

Wins above average (WAA) and WAR from Baseball-Reference. The pull also
includes the role adjustment and the WAA adjustment, which adds credit
for relievers who pitched in high-leverage situations.

``` r
# Takes about 1 minute
stat_columns <- c("p_ip", "p_g", "p_gs", "p_ra9_role", "p_raa", "p_waa", "p_waa_adj", "p_war")

value_pitching <- NULL
for (season in 2014:2026) {
  url <- paste0("https://www.baseball-reference.com/leagues/majors/", season, "-value-pitching.shtml")
  page <- paste(read_lines(url), collapse = "\n")
  page <- gsub("<!--|-->", "", page)
  rows <- read_html(page) |>
    html_elements("#players_value_pitching tr:has([data-append-csv])")

  season_table <- data.frame(bref_id = rows |> html_element("[data-append-csv]") |> html_attr("data-append-csv"),
                             Season = season,
                             team_ID = rows |> html_element("[data-stat='team_name_abbr']") |> html_text())
  for (column in stat_columns) {
    season_table[[column]] <- rows |> html_element(paste0("[data-stat='", column, "']")) |> html_text()
  }
  cat(season, nrow(season_table), "rows\n")
  value_pitching <- bind_rows(value_pitching, season_table)
  Sys.sleep(5)
}
write_csv(value_pitching, "../data/bref_value_pitching.csv", na = "")
```

## Pitch features

Platoon splits, arsenal, and workload from the relief season pitches.

``` r
pitches <- read_csv("../data/savant/relief_season_pitches.csv", guess_max = 50000)

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
         BB = PA & events %in% c("walk", "intent_walk"))
```

``` r
pitches <- pitches |>
  arrange(pitcher, game_year, game_pk, at_bat_number, pitch_number) |>
  group_by(pitcher, game_year, game_pk) |>
  mutate(OutingPitch = row_number()) |>
  ungroup()

# Results against same-handed and opposite-handed hitters
platoon <- pitches |>
  group_by(pitcher, game_year) |>
  summarize(Opp_KBB = (sum(K & OppHand) - sum(BB & OppHand)) / sum(PA & OppHand),
            Same_KBB = (sum(K & !OppHand) - sum(BB & !OppHand)) / sum(PA & !OppHand),
            Opp_Whiff = sum(Whiff & OppHand) / sum(Swing & OppHand),
            Opp_wOBA = sum(woba_value[OppHand], na.rm = TRUE) / sum(woba_denom[OppHand], na.rm = TRUE),
            Same_wOBA = sum(woba_value[!OppHand], na.rm = TRUE) / sum(woba_denom[!OppHand], na.rm = TRUE),
            .groups = "drop") |>
  mutate(Platoon_KBB_gap = Same_KBB - Opp_KBB,
         Platoon_wOBA_gap = Opp_wOBA - Same_wOBA) |>
  select(-Same_wOBA)
```

``` r
# Pitches thrown to opposite-handed hitters
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
```

``` r
# Best whiff rate against opposite-handed hitters on a non-fastball, with 25+ swings
opp_weapon <- opp_usage |>
  filter(!pitch_type %in% c("FF", "SI"), swings >= 25) |>
  summarize(Opp_BestSecondaryWhiff = max(whiffs / swings), .groups = "drop")

outings <- pitches |>
  group_by(pitcher, game_year, game_pk) |>
  summarize(outing_pitches = n(), .groups = "drop") |>
  group_by(pitcher, game_year) |>
  summarize(Outings = n(),
            Share25Plus = mean(outing_pitches >= 25),
            Share40Plus = mean(outing_pitches >= 40),
            .groups = "drop")
```

``` r
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
         Velo_max_gap = Velo_p95 - Velo_mean) |>
  select(pitcher, game_year, Velo_first15, Velo_fade, Velo_max_gap)
```

    ## Warning in inner_join(pitches, primary_fb, by = c("pitcher", "game_year")): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 49342 of `x` matches multiple rows in `y`.
    ## ℹ Row 1 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
savant_features <- platoon |>
  left_join(opp_arsenal, by = c("pitcher", "game_year")) |>
  left_join(opp_weapon, by = c("pitcher", "game_year")) |>
  left_join(outings, by = c("pitcher", "game_year")) |>
  left_join(velo_fade, by = c("pitcher", "game_year")) |>
  rename(xMLBAMID = pitcher, rp_season = game_year)

write_csv(savant_features, "../data/savant_features.csv")
cat("Pitcher relief seasons with Statcast features:", nrow(savant_features), "\n")
```

    ## Pitcher relief seasons with Statcast features: 136

Statcast starts in 2015, so the 9 attempts with a 2014 relief season
have no pitch features. That leaves features for 136 relief seasons.

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

## Pull FanGraphs role and minor league lines

Pulls starter and reliever lines (2014 to 2026) and minor league lines
(2006 to 2026) from FanGraphs. Statcast starts in 2015, so the first
conversion attempts are in 2015 and the relief seasons start in 2014.
The pull code blocks have eval=FALSE, so knitting does not download
everything again. Run them by hand the first time to download the data.

``` r
site <- "https://www.fangraphs.com"
columns <- c("playerid", "xMLBAMID", "PlayerName", "Throws", "Season", "Age", "TeamNameAbb", "G", "GS", "IP", "TBF",
             "WAR", "RAR", "SIERA", "K%", "BB%", "K-BB%", "GB%", "Zone%", "F-Strike%",
             "SwStr%", "O-Swing%", "O-Contact%", "Z-Contact%", "gmLI", "HardHit%", "Barrel%",
             "pfxFA%", "pfxFC%", "pfxFS%", "pfxFO%", "pfxSI%", "pfxSL%", "pfxCU%", "pfxKC%", "pfxCH%", "pfxSC%",
             "pfxKN%", "pfxCV%", "pfxEP%", "pfxvFA", "pfxvSI", "pfxvSL", "pfxvCH",
             "sp_stuff", "sp_location", "sp_pitching", "sp_s_FF", "sp_s_SI", "sp_s_FC", "sp_s_SL", "sp_s_CU",
             "sp_s_CH", "sp_s_FS", "sp_s_KC", "sp_s_FO")

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
    Sys.sleep(4)
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

## Pull MLB game logs

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

## Pull win probability and player IDs

Win probability added (WPA) and average leverage index for every pitcher
season from Baseball-Reference. The Chadwick register converts
Baseball-Reference player IDs to MLBAM IDs so these match the rest of
the data.

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

## Prepare pitcher seasons

Every FanGraphs pitcher season with the measures the later notebooks use
(SIERA relative to the league, pitch mix, Stuff+, and starting history
before each season).

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

write_csv(left_join(league_rp, league_sp, by = "Season"), "../data/league_siera.csv")
```

``` r
# Usage columns are NA when a pitch is not thrown
for (column in c("pfxFA%", "pfxSI%", "pfxFC%", "pfxSL%", "pfxCU%", "pfxKC%", "pfxCV%", "pfxCH%", "pfxFS%",
                 "pfxFO%", "pfxSC%")) {
  role_seasons[[column]][is.na(role_seasons[[column]])] <- 0
}

# Every pitcher season, with the measures the later notebooks share
pitcher_seasons <- role_seasons |>
  left_join(season_totals |> select(playerid, Season, G_total, GS_total), by = c("playerid", "Season")) |>
  left_join(league_rp, by = "Season") |>
  left_join(league_sp, by = "Season") |>
  mutate(SIERA_vs_lg = ifelse(role == "rel", SIERA - lg_rp_siera, SIERA - lg_sp_siera),
         TBF_per_G = TBF / G,
         # Sweepers are already in the slider column, knuckle curves go with curves, splitters with changeups
         FourSeam = `pfxFA%`, Sinker = `pfxSI%`, Cutter = `pfxFC%`, Slider = `pfxSL%`,
         Curve = `pfxCU%` + `pfxKC%` + `pfxCV%`, Changeup = `pfxCH%` + `pfxFS%` + `pfxFO%` + `pfxSC%`,
         NumPitches10 = (FourSeam >= 0.1) + (Sinker >= 0.1) + (Cutter >= 0.1) + (Slider >= 0.1) +
           (Curve >= 0.1) + (Changeup >= 0.1),
         FastballUsage = FourSeam + Sinker,
         # Primary fastball is the four-seam unless the sinker is thrown more
         # Cutters are left out since they behave more like sliders
         FBVelo = ifelse(Sinker > FourSeam, pfxvSI, pfxvFA),
         QualifyingRelief = role == "rel" & TBF >= 100 & GS_total / G_total <= 0.2)
```

``` r
# Stuff+ by pitch type (2020 on), only for pitches thrown at least 5% of the time
usage_cols <- c("pfxFA%", "pfxFC%", "pfxFS%", "pfxFO%", "pfxSI%", "pfxSL%", "pfxCU%", "pfxKC%",
                "pfxCH%", "pfxSC%", "pfxKN%", "pfxCV%", "pfxEP%")
usage <- as.matrix(pitcher_seasons[, usage_cols])
usage[is.na(usage)] <- 0

pitcher_seasons$FB_Stuff <- NA
pitcher_seasons$BestSecondary_Stuff <- NA
pitcher_seasons$SecondSecondary_Stuff <- NA
pitcher_seasons$NumPlusPitches <- NA
for (i in 1:nrow(pitcher_seasons)) {
  if (is.na(pitcher_seasons$sp_stuff[i])) next
  pitch_stuff <- c(FF = pitcher_seasons$sp_s_FF[i], SI = pitcher_seasons$sp_s_SI[i],
                   FC = pitcher_seasons$sp_s_FC[i], SL = pitcher_seasons$sp_s_SL[i],
                   CU = pitcher_seasons$sp_s_CU[i], KC = pitcher_seasons$sp_s_KC[i],
                   CH = pitcher_seasons$sp_s_CH[i], FS = pitcher_seasons$sp_s_FS[i],
                   FO = pitcher_seasons$sp_s_FO[i])
  pitch_usage <- c(FF = usage[i, "pfxFA%"], SI = usage[i, "pfxSI%"], FC = usage[i, "pfxFC%"],
                   SL = usage[i, "pfxSL%"], CU = usage[i, "pfxCU%"] + usage[i, "pfxCV%"],
                   KC = usage[i, "pfxKC%"], CH = usage[i, "pfxCH%"], FS = usage[i, "pfxFS%"],
                   FO = usage[i, "pfxFO%"])
  used <- !is.na(pitch_stuff) & pitch_usage >= 0.05
  fastballs <- used & names(pitch_stuff) %in% c("FF", "SI")
  secondaries <- sort(pitch_stuff[used & !names(pitch_stuff) %in% c("FF", "SI")], decreasing = TRUE)

  if (any(fastballs)) {
    pitcher_seasons$FB_Stuff[i] <- pitch_stuff[fastballs][which.max(pitch_usage[fastballs])]
  }
  pitcher_seasons$BestSecondary_Stuff[i] <- secondaries[1]
  pitcher_seasons$SecondSecondary_Stuff[i] <- secondaries[2]  # NA if there is only one secondary
  pitcher_seasons$NumPlusPitches[i] <- sum(pitch_stuff[used] >= 100)
}
```

``` r
# Starting history before each season (MLB starts from 2014, minor league starts from 2006)
season_keys <- pitcher_seasons |> distinct(playerid, Season)
prior_mlb <- season_keys |>
  inner_join(season_totals |> select(playerid, mSeason = Season, GS_total), by = "playerid",
             relationship = "many-to-many") |>
  filter(mSeason < Season) |>
  group_by(playerid, Season) |>
  summarize(prior_MLB_GS = sum(GS_total), .groups = "drop")
prior_milb <- season_keys |>
  inner_join(minors |> transmute(playerid = suppressWarnings(as.numeric(playerid)), mSeason = Season,
                                 MiLB_G, MiLB_GS) |> filter(!is.na(playerid)),
             by = "playerid", relationship = "many-to-many") |>
  filter(mSeason <= Season) |>
  group_by(playerid, Season) |>
  summarize(MiLB_G = sum(MiLB_G), MiLB_GS = sum(MiLB_GS), .groups = "drop")
pitcher_seasons <- pitcher_seasons |>
  left_join(prior_mlb, by = c("playerid", "Season")) |>
  left_join(prior_milb, by = c("playerid", "Season")) |>
  mutate(prior_MLB_GS = coalesce(prior_MLB_GS, 0), MiLB_G = coalesce(MiLB_G, 0), MiLB_GS = coalesce(MiLB_GS, 0))

write_csv(pitcher_seasons, "../data/pitcher_seasons.csv", na = "")
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
game_logs <- read_csv("../data/game_logs.csv")
long_starts <- game_logs |>
  filter(GS == 1, Pitches >= 70) |>
  group_by(xMLBAMID, Season) |>
  summarize(LongStarts = n(), .groups = "drop")

starter_seasons <- pitcher_seasons |>
  filter(role == "sta") |>
  inner_join(long_starts, by = c("xMLBAMID", "Season")) |>
  filter(LongStarts >= 2)
relief_seasons <- pitcher_seasons |> filter(QualifyingRelief)
```

``` r
relief_side <- relief_seasons |>
  transmute(playerid, rp_season = Season, rp_age = Age,
            rp_SIERA = SIERA, rp_SIERA_vs_lg = SIERA_vs_lg,
            rp_K = `K%`, rp_BB = `BB%`, rp_KBB = `K-BB%`, rp_GB = `GB%`,
            rp_Zone = `Zone%`, rp_FStrike = `F-Strike%`, rp_SwStr = `SwStr%`,
            rp_OSwing = `O-Swing%`, rp_ZContact = `Z-Contact%`, rp_OContact = `O-Contact%`,
            rp_HardHit = `HardHit%`, rp_Barrel = `Barrel%`, rp_gmLI = gmLI,
            rp_FBVelo = FBVelo, rp_NumPitches10 = NumPitches10,
            rp_FastballUsage = FastballUsage, rp_TBFPerG = TBF_per_G,
            rp_StuffPlus = sp_stuff, rp_LocationPlus = sp_location,
            rp_FB_Stuff = FB_Stuff, rp_BestSecondary_Stuff = BestSecondary_Stuff,
            rp_SecondSecondary_Stuff = SecondSecondary_Stuff, rp_NumPlusPitches = NumPlusPitches,
            prev_MLB_GS = prior_MLB_GS, MiLB_G, MiLB_GS)

starter_side <- starter_seasons |>
  transmute(playerid, xMLBAMID, PlayerName, Throws, sp_season = Season,
            sp_GS = GS, sp_LongStarts = LongStarts, sp_IP = IP, sp_TBF = TBF, sp_WAR = WAR,
            sp_SIERA = SIERA, sp_SIERA_vs_lg = SIERA_vs_lg, sp_KBB = `K-BB%`, sp_FBVelo = FBVelo)
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
pairs$next_GS <- NA
for (i in 1:nrow(pairs)) {
  player_totals <- season_totals |> filter(playerid == pairs$playerid[i])
  between <- player_totals |> filter(Season > pairs$rp_season[i], Season < pairs$sp_season[i])
  after <- player_totals |> filter(Season == pairs$sp_season[i] + 1)
  pairs$GS_between[i] <- sum(between$GS_total)
  if (pairs$sp_season[i] < 2026) pairs$next_GS[i] <- sum(after$GS_total)
}
```

``` r
pairs$MaxProIP <- 0
for (i in 1:nrow(pairs)) {
  player_minors <- minors |>
    filter(playerid == as.character(pairs$playerid[i]), Season <= pairs$rp_season[i])
  player_mlb <- season_totals |>
    filter(playerid == pairs$playerid[i], Season <= pairs$rp_season[i])

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

write_csv(conversions |> select(-GS_between, -gap, -MiLB_G, -sp_FBVelo, -sp_WAR), "../data/conversions.csv")

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
cat("\nConversion attempts by starter season\n")
```

    ## 
    ## Conversion attempts by starter season

``` r
print(table(conversions$sp_season))
```

    ## 
    ## 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 2025 2026 
    ##    7   12   16   10   16   10    9   13   15   15   12   10

``` r
cat("\nConversion attempts by years between the relief season and the starter season\n")
```

    ## 
    ## Conversion attempts by years between the relief season and the starter season

``` r
print(table(conversions$gap))
```

    ## 
    ##   1   2   3 
    ## 126  13   6

This gives 145 conversion attempts from 129 pitchers, and 122 of them
have 5 or more starts. The relief season was the year before for 126
attempts, two years before for 13, and three years before for 6.

## Find mid-season attempts

Some pitchers moved to the rotation in the middle of a season, so they
are not in the sample above. I counted a mid-season move when the
pitcher faced 100+ batters in relief with starts in 20% or less of his
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
  filter(before_TBF >= 100, start_share_before <= 0.2)

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

    ## Mid-season attempts not already in the sample: 51

That is about a third as many as the 145 attempts in the main sample,
which is enough for a separate look in notebook 04.

## Pull Statcast pitches for relief seasons

Every Statcast pitch from the relief season of each pitcher in the
sample.

``` r
# Takes about 8 minutes
keep_columns <- c("pitcher", "game_year", "game_pk", "pitch_type", "release_speed", "stand", "p_throws",
                  "description", "events", "inning", "at_bat_number", "pitch_number", "n_thruorder_pitcher",
                  "woba_value", "woba_denom")

# Statcast starts in 2015
pull_seasons <- read_csv("../data/conversions.csv") |>
  filter(rp_season >= 2015) |>
  distinct(xMLBAMID, rp_season, PlayerName)

all_pitches <- NULL
for (i in 1:nrow(pull_seasons)) {
  url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&type=details&player_type=pitcher",
                "&hfGT=R%7C&hfSea=", pull_seasons$rp_season[i],
                "%7C&pitchers_lookup%5B%5D=", pull_seasons$xMLBAMID[i])
  # Read as text so every pitcher season stacks
  pitches <- read_csv(url, col_types = cols(.default = "c"))
  cat(pull_seasons$PlayerName[i], pull_seasons$rp_season[i], nrow(pitches), "pitches\n")
  if (nrow(pitches) > 0) {
    all_pitches <- bind_rows(all_pitches, pitches |> select(all_of(keep_columns)))
  }
  Sys.sleep(1)
}

write_csv(all_pitches, "../data/savant/relief_season_pitches.csv", na = "")
cat("Saved", nrow(all_pitches), "pitches\n")
```

## Pull Statcast pitches for starter seasons

Every Statcast pitch from the starter season of each pitcher in the
sample.

``` r
# Takes about 11 minutes
keep_columns <- c("pitcher", "game_year", "game_pk", "pitch_type", "release_speed", "stand", "p_throws",
                  "description", "events", "inning", "at_bat_number", "pitch_number", "n_thruorder_pitcher",
                  "woba_value", "woba_denom")

pull_seasons <- read_csv("../data/conversions.csv") |>
  distinct(xMLBAMID, sp_season, PlayerName)

all_pitches <- NULL
for (i in 1:nrow(pull_seasons)) {
  url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&type=details&player_type=pitcher",
                "&hfGT=R%7C&hfSea=", pull_seasons$sp_season[i],
                "%7C&pitchers_lookup%5B%5D=", pull_seasons$xMLBAMID[i])
  pitches <- read_csv(url, col_types = cols(.default = "c"))
  cat(pull_seasons$PlayerName[i], pull_seasons$sp_season[i], nrow(pitches), "pitches\n")
  if (nrow(pitches) > 0) {
    all_pitches <- bind_rows(all_pitches, pitches |> select(all_of(keep_columns)))
  }
  Sys.sleep(1)
}

write_csv(all_pitches, "../data/savant/starter_season_pitches.csv", na = "")
cat("Saved", nrow(all_pitches), "pitches\n")
```

## Build relief season Statcast features

Platoon wOBA, K-BB%, and whiff rate, arsenal against opposite-handed
hitters, and velocity fade from the relief season pitches.

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
  mutate(GamePitchNum = row_number()) |>
  ungroup()

# wOBA against same-handed and opposite-handed hitters
platoon <- pitches |>
  group_by(pitcher, game_year) |>
  summarize(Opp_wOBA = sum(woba_value[OppHand], na.rm = TRUE) / sum(woba_denom[OppHand], na.rm = TRUE),
            Same_wOBA = sum(woba_value[!OppHand], na.rm = TRUE) / sum(woba_denom[!OppHand], na.rm = TRUE),
            .groups = "drop") |>
  mutate(Platoon_wOBA_gap = Opp_wOBA - Same_wOBA) |>
  select(-Same_wOBA)

# K-BB% and whiff rate against same-handed and opposite-handed hitters
handedness <- pitches |>
  group_by(pitcher, game_year) |>
  summarize(Opp_KBB = (sum(K & OppHand) - sum(BB & OppHand)) / sum(PA & OppHand),
            Same_KBB = (sum(K & !OppHand) - sum(BB & !OppHand)) / sum(PA & !OppHand),
            Opp_Whiff = sum(Whiff & OppHand) / sum(Swing & OppHand),
            .groups = "drop") |>
  mutate(Platoon_KBB_gap = Same_KBB - Opp_KBB) |>
  select(-Same_KBB)
```

``` r
# Pitches thrown to opposite-handed hitters
opp_usage <- pitches |>
  filter(OppHand) |>
  group_by(pitcher, game_year, pitch_type) |>
  summarize(n = n(), .groups = "drop") |>
  group_by(pitcher, game_year) |>
  mutate(usage = n / sum(n))

opp_arsenal <- opp_usage |>
  summarize(Opp_NumPitches10 = sum(usage >= 0.10),
            .groups = "drop")
```

``` r
# Fastball velo on the first 15 pitches of an outing vs. pitch 26 and later
primary_fb <- pitches |>
  filter(pitch_type %in% c("FF", "SI")) |>
  count(pitcher, game_year, pitch_type) |>
  group_by(pitcher, game_year) |>
  filter(n == max(n)) |>
  slice(1) |>
  select(pitcher, game_year, primary_fb = pitch_type)

velo_fade <- pitches |>
  inner_join(primary_fb, by = c("pitcher", "game_year")) |>
  filter(pitch_type == primary_fb, !is.na(release_speed)) |>
  group_by(pitcher, game_year) |>
  summarize(Velo_first15 = mean(release_speed[GamePitchNum <= 15]),
            Velo_after25 = mean(release_speed[GamePitchNum > 25]),
            FB_after25 = sum(GamePitchNum > 25),
            Velo_p95 = quantile(release_speed, 0.95),
            Velo_mean = mean(release_speed),
            .groups = "drop") |>
  mutate(Velo_fade = ifelse(FB_after25 >= 20, Velo_after25 - Velo_first15, NA),
         Velo_max_gap = Velo_p95 - Velo_mean) |>
  select(pitcher, game_year, Velo_fade, Velo_max_gap)

savant_features <- platoon |>
  left_join(handedness, by = c("pitcher", "game_year")) |>
  left_join(opp_arsenal, by = c("pitcher", "game_year")) |>
  left_join(velo_fade, by = c("pitcher", "game_year")) |>
  rename(xMLBAMID = pitcher, rp_season = game_year)

write_csv(savant_features, "../data/savant_features.csv")
cat("Pitcher relief seasons with Statcast features:", nrow(savant_features), "\n")
```

    ## Pitcher relief seasons with Statcast features: 136

Statcast starts in 2015, so the 9 attempts with a 2014 relief season
have no pitch features. That leaves features for 136 relief seasons.

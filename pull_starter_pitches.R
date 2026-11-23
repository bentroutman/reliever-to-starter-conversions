# Pulls every Statcast pitch from the starter season of each pitcher in the conversion sample
# Run after build_sample.R (needs internet, about 8 seconds per pitcher season)
# Output: data/savant/starter_season_pitches.csv

library(dplyr)
library(readr)

keep_columns <- c("pitcher", "player_name", "game_year", "game_date", "game_pk", "pitch_type", "release_speed",
                  "stand", "p_throws", "description", "events", "zone", "balls", "strikes", "inning",
                  "at_bat_number", "pitch_number", "n_thruorder_pitcher", "woba_value", "woba_denom",
                  "estimated_woba_using_speedangle", "delta_run_exp", "pfx_x", "pfx_z")

pitcher_seasons <- read_csv("data/conversions.csv", show_col_types = FALSE) |>
  distinct(xMLBAMID, sp_season, PlayerName)

all_pitches <- NULL
for (i in 1:nrow(pitcher_seasons)) {
  url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&type=details&player_type=pitcher",
                "&hfGT=R%7C&hfSea=", pitcher_seasons$sp_season[i],
                "%7C&pitchers_lookup%5B%5D=", pitcher_seasons$xMLBAMID[i])
  # Columns are read as text so every pitcher season stacks the same way
  pitches <- read_csv(url, col_types = cols(.default = "c"))
  cat(pitcher_seasons$PlayerName[i], pitcher_seasons$sp_season[i], nrow(pitches), "pitches\n")
  if (nrow(pitches) > 0) {
    all_pitches <- bind_rows(all_pitches, pitches |> select(all_of(keep_columns)))
  }
  Sys.sleep(1)
}

write_csv(all_pitches, "data/savant/starter_season_pitches.csv", na = "")
cat("Saved", nrow(all_pitches), "pitches\n")

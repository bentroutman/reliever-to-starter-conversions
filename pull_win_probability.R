# Pulls season WPA and average leverage for every pitcher from Baseball-Reference,
# plus the Chadwick register to join Baseball-Reference IDs to MLBAM IDs
# Outputs: data/bref_win_probability.csv and data/id_map.csv (needs internet)

library(dplyr)
library(readr)
library(rvest)

stat_columns <- c("player", "age", "team_ID", "IP", "apWins", "wpa_def", "leverage_index_avg", "wpa_li_def",
                  "re24_def", "rew_def")

win_probability <- NULL
for (season in 2014:2026) {
  url <- paste0("https://www.baseball-reference.com/leagues/majors/", season, "-win_probability-pitching.shtml")
  # Some Baseball-Reference tables are inside HTML comments
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
  Sys.sleep(5)  # Baseball-Reference allows about 20 requests a minute
}
write_csv(win_probability, "data/bref_win_probability.csv", na = "")

# The register is split into 16 files by the first character of the person key
id_map <- NULL
for (letter in c(0:9, "a", "b", "c", "d", "e", "f")) {
  url <- paste0("https://raw.githubusercontent.com/chadwickbureau/register/master/data/people-", letter, ".csv")
  people <- read_csv(url, col_types = cols(.default = "c")) |>
    filter(!is.na(key_mlbam), !is.na(key_bbref)) |>
    select(xMLBAMID = key_mlbam, bref_id = key_bbref, fg_id = key_fangraphs)
  id_map <- bind_rows(id_map, people)
}
write_csv(id_map, "data/id_map.csv", na = "")
cat("ID map:", nrow(id_map), "people\n")

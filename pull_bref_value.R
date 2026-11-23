# Pulls season pitching value (WAA, WAR, and the role and leverage adjustments) from Baseball-Reference
# Output: data/bref_value_pitching.csv (needs internet)

library(dplyr)
library(readr)
library(rvest)

stat_columns <- c("p_ip", "p_g", "p_gs", "p_ra9", "p_ra9_role", "p_ra9_avg_pitcher", "p_raa", "p_waa",
                  "p_leverage_index_avg_rp", "p_waa_adj", "p_war", "p_rar")

value_pitching <- NULL
for (season in 2014:2026) {
  url <- paste0("https://www.baseball-reference.com/leagues/majors/", season, "-value-pitching.shtml")
  # Some Baseball-Reference tables are inside HTML comments
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
  Sys.sleep(5)  # Baseball-Reference allows about 20 requests a minute
}
write_csv(value_pitching, "data/bref_value_pitching.csv", na = "")

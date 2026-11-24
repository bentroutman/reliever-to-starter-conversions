# Pulls starter and reliever lines (2014 to 2026) and minor league lines (2006 to 2026) from FanGraphs, with permission
# Opens a Chrome window. If a security check shows up, complete it, then press Enter in R
# Run with source("pull_fangraphs.R") from the project folder (about 5 minutes)
# Outputs: data/fg_role_seasons.csv and data/fg_minors.csv

library(chromote)
library(dplyr)
library(jsonlite)
library(processx)
library(readr)

site <- "https://www.fangraphs.com"
columns <- c("playerid", "xMLBAMID", "PlayerName", "Throws", "Season", "Age", "TeamNameAbb", "G", "GS", "IP", "TBF",
             "Pitches", "WAR", "RAR", "SIERA", "xFIP", "FIP", "K%", "BB%", "K-BB%", "GB%", "Zone%", "F-Strike%",
             "SwStr%", "CStr%", "O-Swing%", "O-Contact%", "Z-Contact%", "gmLI", "HardHit%", "Barrel%",
             "pfxFA%", "pfxFC%", "pfxFS%", "pfxFO%", "pfxSI%", "pfxSL%", "pfxCU%", "pfxKC%", "pfxCH%", "pfxSC%",
             "pfxKN%", "pfxST%", "pfxCV%", "pfxEP%", "pfxvFA", "pfxvSI", "pfxvSL", "pfxvCH", "pfxFA-Z",
             "sp_stuff", "sp_location", "sp_pitching", "sp_s_FF", "sp_s_SI", "sp_s_FC", "sp_s_SL", "sp_s_CU",
             "sp_s_CH", "sp_s_FS", "sp_s_KC", "sp_s_FO", "pb_stuff", "pb_command")

# Start a visible Chrome with its own profile and connect to it
chrome <- process$new(find_chrome(), c("--remote-debugging-port=9222", "--no-first-run",
                                       paste0("--user-data-dir=", tempfile("chrome_profile"))))
Sys.sleep(5)
tab <- ChromoteSession$new(parent = Chromote$new(browser = ChromeRemote$new("127.0.0.1", 9222)))
tab$go_to(paste0(site, "/leaders/major-league"))
readline("Press Enter once the FanGraphs page has loaded: ")

# Opens a page of JSON in the window and returns it as a data frame
read_page_json <- function(url) {
  tab$go_to(url)
  page_text <- tab$Runtime$evaluate("document.body.innerText")$result$value
  if (!grepl("^\\s*[\\[{]", page_text)) stop("Did not get data from ", url)
  page_data <- fromJSON(page_text)
  if (is.data.frame(page_data)) page_data else page_data$data
}

role_seasons <- NULL
for (season in 2014:2026) {
  for (role_name in c("sta", "rel")) {
    url <- paste0(site, "/api/leaders/major-league/data?pos=all&lg=all&qual=0&month=0&team=0&pageitems=2000",
                  "&pagenum=1&ind=1&rost=0&players=0&sortdir=desc&sortstat=WAR&type=8&stats=", role_name,
                  "&season=", season, "&season1=", season)
    players <- read_page_json(url) |>
      select(any_of(columns)) |>
      mutate(role = role_name)
    cat(season, role_name, nrow(players), "pitchers\n")
    role_seasons <- bind_rows(role_seasons, players)
    Sys.sleep(4)
  }
}
missing_columns <- setdiff(columns, names(role_seasons))
if (length(missing_columns) > 0) stop("Missing columns: ", paste(missing_columns, collapse = ", "))

minors <- NULL
for (season in setdiff(2006:2026, 2020)) {
  url <- paste0(site, "/api/leaders/minor-league/data?pos=all&level=0&lg=2,4,5,6,7,8,9,10,11,14,12,13,15,16,17,18,30,32",
                "&stats=pit&qual=0&type=0&team=&season=", season, "&seasonEnd=", season,
                "&org=&ind=0&splitTeam=false")
  players <- read_page_json(url) |>
    transmute(playerid = as.character(playerids), xMLBAMID, PlayerName, Season = season, Age, G, GS, IP, TBF)
  cat(season, "minors", nrow(players), "pitchers\n")
  minors <- bind_rows(minors, players)
  Sys.sleep(8)
}

# Only save once everything has been pulled
write_csv(role_seasons |> select(all_of(columns), role), "data/fg_role_seasons.csv", na = "")
write_csv(minors, "data/fg_minors.csv", na = "")
chrome$kill()

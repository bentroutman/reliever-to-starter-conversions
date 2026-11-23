# Pulls season win probability lines (WPA and average leverage) for every pitcher from Baseball-Reference,
# plus the Chadwick register to join Baseball-Reference IDs to MLBAM IDs
# Outputs: data/bref_win_probability.csv and data/id_map.csv
# Needs internet, independent of the other scripts (about 5 seconds per season)
import csv
import io
import re
import time
import urllib.request

SEASONS = range(2014, 2027)
COLUMNS = ["player", "age", "team_ID", "IP", "apWins", "wpa_def", "leverage_index_avg", "wpa_li_def",
           "re24_def", "rew_def"]


# Downloads a page and returns its text
def get_text(url):
    request = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    return urllib.request.urlopen(request, timeout=60).read().decode("utf-8")


# Pulls one season of the players_win_probability_pitching table into a list of rows
def pull_season(season):
    html = get_text(f"https://www.baseball-reference.com/leagues/majors/{season}-win_probability-pitching.shtml")
    html = html.replace("<!--", "").replace("-->", "")
    start = html.find('id="players_win_probability_pitching"')
    table = html[start: html.find("</table>", start)]
    season_rows = []
    for row in re.findall(r"<tr[^>]*>(.*?)</tr>", table, re.S):
        bref_id = re.findall(r'data-append-csv="([^"]+)"', row)
        if not bref_id:
            continue
        cells = dict(re.findall(r'data-stat="([^"]+)"[^>]*>(.*?)</t[dh]>', row, re.S))
        values = [re.sub("<[^>]+>", "", cells.get(column, "")).replace("&nbsp;", " ").replace("%", "")
                  for column in COLUMNS]
        season_rows.append([bref_id[0], season] + values)
    return season_rows


def main():
    all_rows = []
    for season in SEASONS:
        season_rows = pull_season(season)
        print(f"{season}: {len(season_rows)} rows", flush=True)
        all_rows = all_rows + season_rows
        time.sleep(5)  # Baseball-Reference allows about 20 requests a minute

    with open("data/bref_win_probability.csv", "w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["bref_id", "Season"] + COLUMNS)
        writer.writerows(all_rows)

    # Chadwick register is split into 16 files by the first character of the person key
    id_rows = []
    for letter in "0123456789abcdef":
        text = get_text(f"https://raw.githubusercontent.com/chadwickbureau/register/master/data/people-{letter}.csv")
        for person in csv.DictReader(io.StringIO(text)):
            if person["key_mlbam"] and person["key_bbref"]:
                id_rows.append([person["key_mlbam"], person["key_bbref"], person["key_fangraphs"]])
    with open("data/id_map.csv", "w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["xMLBAMID", "bref_id", "fg_id"])
        writer.writerows(id_rows)
    print(f"ID map: {len(id_rows)} people")


main()

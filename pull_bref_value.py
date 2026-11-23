# Pulls Baseball-Reference season pitching value (RA9, role adjustment, RAA, WAA, leverage adjustment, WAR) for
# every pitcher
# Needs internet, independent of the other scripts (about 5 seconds per season)
# Output: data/bref_value_pitching.csv
import csv
import re
import time
import urllib.request

SEASONS = range(2014, 2027)
COLUMNS = ["p_ip", "p_g", "p_gs", "p_ra9", "p_ra9_role", "p_ra9_avg_pitcher", "p_raa", "p_waa",
           "p_leverage_index_avg_rp", "p_waa_adj", "p_war", "p_rar"]


# Pulls one season of the players_value_pitching table into a list of rows
def pull_season(season):
    url = f"https://www.baseball-reference.com/leagues/majors/{season}-value-pitching.shtml"
    request = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    html = urllib.request.urlopen(request, timeout=60).read().decode("utf-8")
    html = html.replace("<!--", "").replace("-->", "")
    start = html.find('id="players_value_pitching"')
    table = html[start: html.find("</table>", start)]
    season_rows = []
    for row in re.findall(r"<tr[^>]*>(.*?)</tr>", table, re.S):
        bref_id = re.findall(r'data-append-csv="([^"]+)"', row)
        if not bref_id:
            continue
        cells = dict(re.findall(r'data-stat="([^"]+)"[^>]*>(.*?)</t[dh]>', row, re.S))
        values = [re.sub("<[^>]+>", "", cells.get(column, "")) for column in COLUMNS]
        team = re.sub("<[^>]+>", "", cells.get("team_name_abbr", ""))
        season_rows.append([bref_id[0], season, team] + values)
    return season_rows


def main():
    all_rows = []
    for season in SEASONS:
        season_rows = pull_season(season)
        print(f"{season}: {len(season_rows)} rows", flush=True)
        all_rows = all_rows + season_rows
        time.sleep(5)

    with open("data/bref_value_pitching.csv", "w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["bref_id", "Season", "team_ID"] + COLUMNS)
        writer.writerows(all_rows)


main()

# Converts the raw FanGraphs JSON pulls (saved from the browser, see pull_fangraphs.js) into CSVs
# Needs data/raw/fg_role_seasons.json and data/raw/fg_minors.json from pull_fangraphs.js
# Outputs: data/fg_role_seasons.csv and data/fg_minors.csv
import csv
import json
import os

RAW_PATH = "data/raw"
OUT_PATH = "data"


# Writes a list of rows to a CSV with the given column names
def write_csv(file_name, columns, rows):
    with open(os.path.join(OUT_PATH, file_name), "w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(columns)
        writer.writerows(rows)
    print(f"{file_name}: {len(rows)} rows")


def main():
    # starter and reliever lines for every pitcher season, 2014 to 2026
    seasons = json.load(open(os.path.join(RAW_PATH, "fg_role_seasons.json")))
    write_csv("fg_role_seasons.csv", seasons["cols"], seasons["rows"])

    # minor league pitching lines by level
    minors = json.load(open(os.path.join(RAW_PATH, "fg_minors.json")))
    write_csv("fg_minors.csv", minors["cols"], minors["rows"])


main()

# Pulls every Statcast pitch from the starter season of each pitcher in the conversion sample
# Same as pull_relief_pitches.py but for sp_season
# Output: data/savant/starter_season_pitches.csv
# Run after build_sample.R (needs internet, about 8 seconds per pitcher season)
import io
import time
import urllib.request

import pandas as pd

CONVERSIONS_PATH = "data/conversions.csv"
OUT_PATH = "data/savant/starter_season_pitches.csv"
KEEP_COLUMNS = ["pitcher", "player_name", "game_year", "game_date", "game_pk", "pitch_type", "release_speed",
                "stand", "p_throws", "description", "events", "zone", "balls", "strikes", "inning",
                "at_bat_number", "pitch_number", "n_thruorder_pitcher", "woba_value", "woba_denom",
                "estimated_woba_using_speedangle", "delta_run_exp", "pfx_x", "pfx_z"]


# Downloads one pitcher season of regular season pitches from Baseball Savant
def pull_pitcher_season(mlbam_id, season):
    url = ("https://baseballsavant.mlb.com/statcast_search/csv?all=true&type=details&player_type=pitcher"
           f"&hfGT=R%7C&hfSea={season}%7C&pitchers_lookup%5B%5D={mlbam_id}")
    request = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    text = urllib.request.urlopen(request, timeout=60).read().decode("utf-8-sig")
    return pd.read_csv(io.StringIO(text))


def main():
    conversions_df = pd.read_csv(CONVERSIONS_PATH)
    seasons_df = conversions_df[["xMLBAMID", "sp_season", "PlayerName"]].drop_duplicates()

    all_pitches = []
    for row in seasons_df.itertuples():
        try:
            pitches_df = pull_pitcher_season(row.xMLBAMID, row.sp_season)
            print(f"{row.PlayerName} {row.sp_season}: {len(pitches_df)} pitches", flush=True)
            if len(pitches_df) > 0:
                all_pitches.append(pitches_df[KEEP_COLUMNS])
        except Exception as error:
            print(f"{row.PlayerName} {row.sp_season}: FAILED ({error})", flush=True)
        time.sleep(1)

    pd.concat(all_pitches).to_csv(OUT_PATH, index=False)
    print(f"Saved {sum(len(p) for p in all_pitches)} pitches")


main()

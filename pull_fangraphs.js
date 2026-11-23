// Run in the browser console on fangraphs.com, then save result and minors_result to data/raw as JSON
// Pulls starter and reliever lines for every pitcher season from 2014 to 2026, and minor league lines from 2006
// Outputs: data/raw/fg_role_seasons.json and data/raw/fg_minors.json

const keep = ["playerid","xMLBAMID","PlayerName","Throws","Season","Age","TeamNameAbb","G","GS","IP","TBF","Pitches","WAR","SIERA","xFIP","FIP","xERA","K%","BB%","K-BB%","GB%","HR/FB","BABIP","Zone%","F-Strike%","SwStr%","CStr%","O-Swing%","Z-Swing%","O-Contact%","Z-Contact%","Contact%","gmLI","FBv","HardHit%","Barrel%","EV",
"pfxFA%","pfxFT%","pfxFC%","pfxFS%","pfxFO%","pfxSI%","pfxSL%","pfxCU%","pfxKC%","pfxCH%","pfxSC%","pfxKN%","pfxST%","pfxSV%","pfxCV%","pfxEP%",
"pfxvFA","pfxvSI","pfxvFC","pfxvSL","pfxvCU","pfxvCH","pfxvFS","pfxvST","pfxFA-X","pfxFA-Z","pfxSI-X","pfxSI-Z","pfxZone%",
"pfxwFA/C","pfxwSI/C","pfxwFC/C","pfxwSL/C","pfxwCU/C","pfxwCH/C","pfxwFS/C","pfxwST/C",
"sp_stuff","sp_location","sp_pitching","sp_s_FF","sp_s_SI","sp_s_FC","sp_s_SL","sp_s_CU","sp_s_CH","sp_s_FS","sp_s_KC","pb_stuff","pb_command","pb_overall",
"sp_s_FO","sp_l_FF","sp_l_SI","sp_l_FC","sp_l_SL","sp_l_CU","sp_l_CH","sp_l_FS","sp_l_KC",
"pb_s_FF","pb_s_SI","pb_s_FC","pb_s_SL","pb_s_CU","pb_s_CH","pb_s_FS","pb_s_KC",
"WPA","-WPA","+WPA","RE24","REW","pLI","inLI","exLI","WPA/LI","Clutch","RAR","RA9-Wins"];

const sleep = ms => new Promise(res => setTimeout(res, ms));
const rows = [];
for (let season = 2014; season <= 2026; season++) {
  // sta = as a starter, rel = as a reliever
  for (const stats of ["sta", "rel"]) {
    const url = `https://www.fangraphs.com/api/leaders/major-league/data?pos=all&lg=all&qual=0&month=0&team=0&pageitems=2000&pagenum=1&ind=1&rost=0&players=0&sortdir=desc&sortstat=WAR&type=8&stats=${stats}&season=${season}&season1=${season}`;
    const response = await (await fetch(url)).json();
    for (const player of response.data) {
      const row = keep.map(column => player[column] ?? null);
      row.push(stats);
      rows.push(row);
    }
    await sleep(4000);  // FanGraphs returns 429 if these go too fast
  }
}
const result = JSON.stringify({cols: [...keep, "role"], rows: rows});

// Minor league lines by season (one call per season, 2020 had no season)
// Saved as data/raw/fg_minors.json
const minors = [];
for (let season = 2006; season <= 2025; season++) {
  if (season == 2020) continue;
  const url = `https://www.fangraphs.com/api/leaders/minor-league/data?pos=all&level=0&lg=2,4,5,6,7,8,9,10,11,14,12,13,15,16,17,18,30,32&stats=pit&qual=0&type=0&team=&season=${season}&seasonEnd=${season}&org=&ind=0&splitTeam=false`;
  const response = await (await fetch(url)).json();
  for (const player of (response.data || response)) {
    minors.push([player.playerids, player.xMLBAMID, player.PlayerName, season, player.Age, player.G, player.GS, player.IP, player.TBF]);
  }
  await sleep(8000);
}
const minors_result = JSON.stringify({cols: ["playerid", "xMLBAMID", "PlayerName", "Season", "Age", "G", "GS", "IP", "TBF"], rows: minors});

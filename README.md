# Reliever to Starter Conversions

Across 145 MLB reliever to starter conversion attempts from 2015 to 2026, SIERA rose by 0.61 on average and 77% of pitchers got worse. Converting gained about 0.48 WAR over staying in the bullpen, but lost 0.23 WPA and 0.22 wins above average.

---

## Takeaway

Conversions look most useful for teams whose alternative is a replacement level fifth starter or very thin starting depth. Converted pitchers added about 0.48 WAR over staying in the bullpen because they throw more innings, and the typical conversion was only a little worse than the league average starter (0.19 SIERA worse). I did not model team depth, so this part is my interpretation of the results and an area I want to test further in the future.

For teams with decent starting depth, conversions are generally not worth it. SIERA rose by 0.61 on average, 77% of pitchers got worse, WPA and WAA went down, and only 41% finished as league average or better starters.

Elite relievers may be the best conversion candidates. In the 13 attempts by relievers at least a run better than the league average reliever, 10 were league average or better starters and their starter SIERA averaged 0.16 better than the league. That is too few historical comps to judge whether converting an elite reliever pays off without extrapolating, so I treat it as upside to watch and not a conclusion.

## Overview

1. Build a sample of conversions
2. Measure how they went compared to staying in the bullpen
3. Look at what changed after the move
4. Test which relief season traits predict a good starter
5. Predict which 2026 relievers look most like past conversions

Each step is one R Markdown notebook, and the links below open the knitted version.

## 1. Building the Sample ([01_data_and_sample](01_data_and_sample/01_data_and_sample.md))

A conversion attempt is a season with at least 2 starts of 70+ pitches, within three years of a season with 100+ batters faced in relief and starts in 20% or less of games. This gives 145 attempts by 129 pitchers from 2015 to 2026. There is no minimum number of innings or share of games started, since those rules drop attempts that went badly. All of a pitcher's starts that season count toward his results. Pitchers who moved to the rotation partway through a season are a separate group (51 pitcher seasons).

## 2. How Conversions Went ([02_conversion_value](02_conversion_value/02_conversion_value.md))

Each attempt is compared to the expected next season of a similar reliever who stayed in the bullpen. WAR went up because starters throw more innings, but measures compared to an average pitcher (WPA and WAA) went down.

| Measure | Average gain (145 attempts) | Standard error |
|---|---|---|
| WAR | +0.48 | 0.09 |
| WPA | -0.23 | 0.09 |
| WAA (role adjusted) | -0.22 | 0.07 |

Attempts with 5+ starts gained 0.65 WAR, and the 23 attempts with 2 to 4 starts lost 0.46, which could be attributed to poor-performing conversions being forced back into the bullpen.

## 3. What Changed ([03_changes_after_conversion](03_changes_after_conversion/03_changes_after_conversion.md))

Fastball velocity fell 1.1 mph on average. For the 67 attempts with Stuff+ (2020 on), Stuff+ fell 5.1 points while Location+ rose 2.1 points. Pitchers also threw more changeups, sinkers, and cutters and fewer four-seamers.

## 4. What Predicts Success ([04_predicting_success](04_predicting_success/04_predicting_success.md))

- Relief K-BB% and relief SIERA were the strongest predictors of starter SIERA, relative to the league (Spearman correlations of -0.43 and 0.42). No model I tested clearly improved on them, and the relief SIERA relationship held in all 5 versions of the sample I tried (0.41 to 0.59).
- Pitchers with a cushion of at least 1 run (league average starter SIERA minus relief SIERA) were league average or better as starters 70% of the time, compared to 33% for the rest.
- For mid-season moves, K-BB% before and after the move had a correlation of 0.26, compared to 0.45 for the full season conversions.

![SIERA, WAR, and WPA by cushion](04_predicting_success/charts/cushion_bins.png)

## 5. Who Gets Converted ([05_predicting_conversions](05_predicting_conversions/05_predicting_conversions.md))

Teams mostly chose relievers by role and starting history. Those traits separated converts much better than skill did (cross-validated AUC of 0.79 and 0.82, compared to 0.58).

![How converts differ from other relievers](05_predicting_conversions/charts/convert_profile.png)

To predict conversions, I tested each model on 1,398 relief seasons from 2021 to 2025 (64 conversions). Each test year only uses earlier relief seasons and conversions that had already happened.

| Model | Expected conversions | Calibration slope | Log loss | AUC |
|---|---|---|---|---|
| Base rate | 72.3 | | 0.1862 | |
| Logistic regression (19 traits) | 71.0 | 0.82 | 0.1498 | 0.847 |
| Ridge | 74.8 | 0.96 | 0.1506 | 0.847 |
| Logistic regression (4 traits) | 73.5 | 0.83 | 0.1532 | 0.844 |

AUC is averaged over the five test years. I used ridge for the final probabilities because its calibration slope was close to 1, but every model expected too many conversions. Ridge did not beat a simple 4-trait model at ranking pitchers (the 95% interval on the AUC difference is -0.018 to 0.023).

## Limitations

- Only pitchers that teams chose to convert and who made starts in the majors are in the sample, so the results cannot be extended to relievers who have never started.
- The traits and cutoffs were chosen after seeing all of the data, and ridge was chosen on the same test years it is reported on, so the test results are slightly optimistic.
- The slope of starter SIERA on relief SIERA fell from 0.40 for attempts through 2021 to 0.15 after, which could be noise (the trend by season has p = 0.50).
- The 70 pitch cutoff, the 2 start rule, the cushion and Stuff+ cutoffs, and the archetype cutoffs (such as 95 mph) were my choices.
- Some attempts only have a few starts, so their results are noisy.
- Pitchers who are pulled before 70 pitches in every start are not in the sample.
- Recent relief seasons could still become conversions, since the sample allows a three-year gap.

## Future Work

- Add team context to the conversion model. A conversion is the decision of the team the pitcher starts for, and about a third of converts changed teams, so this would need the starter season team and each team's rotation needs.

## Data

FanGraphs (role splits and minor league lines), Baseball Savant (Statcast pitches), Baseball-Reference (WPA), the MLB Stats API (game logs), and the Chadwick Bureau register (player IDs). The data the notebooks read is included in the `data` folder.

## Running the Code

The data is already in the data folder, so the notebooks can be knitted in number order as they are. To pull it again, run the code chunks in 01_data_and_sample.Rmd that are set to eval=FALSE by hand (the FanGraphs pull opens a Chrome window). Each step has its own folder with the notebook, the knitted page, and its results and charts.

## Technologies Used

R Markdown (dplyr, readr, tidyr, ggplot2, ggrepel, gridExtra, glmnet, pROC, splines, rvest, chromote, processx, and jsonlite)

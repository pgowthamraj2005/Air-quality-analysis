# Air Quality Analysis using R

Mini Project for **23CAP317 – R Programming** (MCA, II Year / III Sem / Sec A), Karpagam College of Engineering.
Author: **GOWTHAMRAJ P**

## Overview
Predicts ground-level **ozone (ppb)** in New York from weather conditions (solar radiation, wind speed, temperature, month) and classifies **high-ozone days** (> 50 ppb).

## Data
`aq.csv` – New York daily air quality, May–September 1973 (153 rows, 6 columns: `Ozone`, `Solar.R`, `Wind`, `Temp`, `Month`, `Day`). This is the `airquality` data set distributed with R (Rdatasets CSV copy). Ozone has 37 missing values and Solar.R has 7.

## Mini-project questions and where they are answered
| Question | Script section (`air_quality_analysis.R`) | Word report | Slides |
|---|---|---|---|
| Q1. Data structures, dataset details, objectives | PART 1 | Section 1 | 2-8 |
| Q2. Control structures, functions, lists, data frames | PART 2 | Section 2 | 9-10 |
| Q3. Statistical problems in the project | PART 3 (+ PART 4 cleaning) | Sections 3-4 | 11-17 |
| Q4. R script with different regression models | PART 5, 6, 7 | Section 5 | 18-28 |
| Q5. Apply models, predict output, upload to GitHub | PART 6 (test/CV), PART 8 (new data) | Section 6 | 29-32 |

## What the script does (`air_quality_analysis.R`)
1. Data structures (vector, factor, matrix, list, data frame) and data loading
2. Control structures, user-defined functions, lists, data frames
3. Statistical problems: missing values, duplicates, outliers (IQR), skewness and Shapiro-Wilk, correlation tests, ANOVA, Welch t-test, VIF
4. Cleaning and visualisation (ggplot2, corrplot)
5. 80/20 train–test split
6. Models: multiple linear, log-linear, polynomial + interaction, random forest (ranger), 10-fold × 5 repeated cross-validation
7. Logistic regression for high-ozone days (confusion matrix, ROC/AUC)
8. Predictions for new weather scenarios

## Key results
| Model | Test RMSE | CV RMSE | CV R² |
|---|---|---|---|
| Linear | 17.07 | 20.58 | 0.636 |
| Log-linear | 15.09 | – | – |
| Polynomial + interaction | 15.55 | 18.58 | 0.736 |
| **Random forest** | 15.07 | **16.63** | **0.771** |

Logistic regression: accuracy 90.5 %, AUC 0.978 (21 test days). Wind and temperature are the dominant predictors.

## How to run
```r
install.packages(c("ggplot2", "corrplot", "ranger", "caret", "dplyr"))
```
```bash
Rscript air_quality_analysis.R > output.txt
```
Run from the project folder (the script reads `aq.csv` and writes figures to `plots/`). A seed (`set.seed(2026)`) makes results reproducible. Developed with R 4.3.3.

## Files
- `docs/` – PowerPoint presentation and Word report
- `air_quality_analysis.R` – full analysis script
- `aq.csv` – data
- `output.txt` – console output of one complete run
- `plots/` – 12 figures produced by the script

# ============================================================================
# 00_1_Master_Script.R - runs the project end to end.
#
# By default this reproduces the MAIN RESULTS of the paper: baseline
# specification (492-month window, standard optimization, headline CPI),
# sample 1960-01 .. 2025-12 from the 2026-06 FRED-MD vintage.
#
# Switches live in 00_2_Config.R:
#   VINTAGE_CSV / SAMPLE_END   raw data and sample cut
#   OPT_LEVEL                  "fast" | "standard" | "thorough" forest runtime
#   HP_DEFAULT / HP_CASES      hyper-parameters, per (set, horizon) case
#   USE_DUMMIES                GFC/COVID dummies (FALSE for the paper)
#
# Other specifications (e.g. the six robustness runs: 20/30/40-year windows
# at h = 1 and 3) are driven from 00_3_Run_Specifications.R by flipping flags.
# ============================================================================
rm(list = ls())

## 1. Data ------------------------------------------------------------------
source("01_RScript/01_Data_Transformation.R")
source("01_RScript/02_Data_Cleaning.R")

## 2. Descriptives (Figures 1-2, Table of descriptives) ----------------------
source("01_RScript/07_Descriptives_Clean.R")

## 3. Forecasting models  [SLOW: forests dominate] ---------------------------
# Runs every SPECS entry with run = TRUE (default: baseline only).
source("01_RScript/00_3_Run_Specifications.R")

## 4. Results (read the baseline predictions) --------------------------------
source("01_RScript/51_Cumulative_Errors.R")
source("01_RScript/52_RMSE_Tables_Charts.R")
source("01_RScript/60_Forecast_Tests.R")
source("01_RScript/61_Ensemble_Recursive.R")

## 5. Variable importance and SHAP  [SLOW] -----------------------------------
source("01_RScript/63_VarImp_8Groups.R")
source("01_RScript/64_SHAP_Values.R")            # slowest single script
source("01_RScript/65_Rotation_SHAP.R")

## 6. Robustness --------------------------------------------------------------
# After running the extra window specs in 00_3_Run_Specifications.R:
# source("01_RScript/55_Robustness_Windows.R")
# Core CPI / core PCE target:
# source("01_RScript/70_Robustness_CoreCPI.R")

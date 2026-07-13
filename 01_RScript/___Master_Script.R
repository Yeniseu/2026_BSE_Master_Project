# ============================================================================
# ___Master_Script.R - runs the project end to end.
#
# Every switch lives in 00_Config.R:
#   SAMPLE_END      last month kept (2024-12 now; set 2025-12 to use all data)
#   TRAIN_WINDOW    rolling window, months (492 baseline; 240/360/480 robustness)
#   USE_DUMMIES     add GFC / COVID dummies to every model (FALSE for the paper)
#   SAVE_IMPORTANCE store RF importance, needed for the variable-importance figure
#
# Runtime: the forests dominate. Steps 3 and 5 are the slow ones.
# ============================================================================
rm(list = ls())

## 1. Data ------------------------------------------------------------------
source("01_RScript/01_Data_Transformation.R")
source("01_RScript/02_Data_Cleaning.R")

## 2. Descriptives (Figures 1-2, Table 1) -----------------------------------
source("01_RScript/07_Descriptives_Clean.R")

## 3. Forecasting models  [SLOW] --------------------------------------------
source("01_RScript/10_Benchmarks.R")
source("01_RScript/15_Penalised.R")
source("01_RScript/18_RandomForest.R")
source("01_RScript/20_LLF.R")

## 4. Results ---------------------------------------------------------------
source("01_RScript/50_Assemble_Predictions.R")   # -> Preds/preds_h{1,3}_w*.rds
source("01_RScript/51_Cumulative_Errors.R")      # Appendix A figure
source("01_RScript/52_RMSE_Tables_Charts.R")     # main RMSE figure
source("01_RScript/60_Forecast_Tests.R")         # TODO 1: DM/HLN + Giacomini-Rossi
source("01_RScript/61_Ensemble_Recursive.R")     # TODO 2: recursive ensembles

## 5. Variable importance and SHAP  [SLOW] ----------------------------------
source("01_RScript/63_VarImp_8Groups.R")         # needs SAVE_IMPORTANCE = TRUE
source("01_RScript/64_SHAP_Values.R")            # very slow
source("01_RScript/65_Rotation_SHAP.R")

## 6. Robustness ------------------------------------------------------------
# Set TRAIN_WINDOW to 240, 360, 480 in 00_Config.R and re-run steps 3-4 for
# each, then average across the specifications:
# source("01_RScript/55_Robustness_Windows.R")

# Core CPI / core PCE as alternative targets (TODO 3):
# source("01_RScript/70_Robustness_CoreCPI.R")

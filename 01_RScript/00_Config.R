# ============================================================================
# 00_Config.R - central configuration for the whole project
# Author: Master_O project
# Scope : single source of truth for sample, horizons, models and switches.
#         Every downstream script sources this file first.
# ============================================================================
library(data.table)

## ---- Sample -----------------------------------------------------------------
# NOTE: previously the cleaning script hard-coded 2024-12-31 while the raw
# FRED-MD vintage (2026-01-MD.csv) already runs to 2025-12. Set SAMPLE_END to
# the last month you want to keep; set to NA to use every month available.
SAMPLE_START <- as.Date("1960-01-01")
SAMPLE_END   <- as.Date("2024-12-01")   # <- change to 2025-12-01 after re-running the pipeline

## ---- Forecast design --------------------------------------------------------
# ONE forecasting sample (the old 2001-2015 / 2016-2024 split is gone).
# The model is re-estimated at every origin on a FIXED rolling window of
# TRAIN_WINDOW months, producing forecasts for OOS_START .. SAMPLE_END.
OOS_START    <- as.Date("2001-01-01")
TRAIN_WINDOW <- 492          # months (41 years). Robustness: 240 / 360 / 480
HORIZONS     <- c(1, 3)      # h, in months
N_LAG        <- 4            # number of lags of each predictor
K_FAC        <- 4            # principal components added to the predictor set

## ---- Shock dummies (OFF by default) -----------------------------------------
# Turn ON to add GFC / COVID indicator variables to the predictor set of every
# model. They enter with the same lag structure as any other predictor, so the
# design stays internally consistent.
#
# WARNING: shock dummies are not real-time information. Switching this on makes
# the exercise an in-sample/ex-post decomposition, NOT an honest out-of-sample
# forecast. Keep FALSE for the baseline results in the paper.
USE_DUMMIES  <- FALSE
DUMMY_DEFS   <- list(
  dum_gfc   = c("2007-12-01", "2009-06-01"),   # NBER recession
  dum_covid = c("2020-03-01", "2021-12-01")    # COVID + reopening surge
)

## ---- Sub-periods used in every table / figure -------------------------------
SUBPERIODS <- list(
  "2002-2004" = 2002:2004, "2005-2007" = 2005:2007,
  "2008-2010" = 2008:2010, "2011-2013" = 2011:2013,
  "2014-2016" = 2014:2016, "2017-2019" = 2017:2019,
  "2020-2022" = 2020:2022, "2023-2025" = 2023:2025
)
SHOCK_SUBS <- c("2008-2010", "2020-2022")

## ---- Model space (the 2x2 design) -------------------------------------------
BENCHMARKS <- c("RW", "RSM", "AR")
FAMILIES <- list(
  "Linear Phillips Curve"             = c("La_P", "Ri_P", "EN_P"),
  "Linear with Variable Selection"    = c("La",   "Ri",   "EN"),
  "Non-Linear Phillips Curve"         = c("RF_P", "LLF_P"),
  "Non-Linear and Variable Selection" = c("RF",   "LLF")
)
MODELS <- c(BENCHMARKS, unlist(FAMILIES, use.names = FALSE))

## ---- Labour-market subset (the "Phillips Curve" information set) ------------
LABOR_VARS <- c(
  "HWI","HWIURATIO","CLF16OV","CE16OV","UNRATE","UEMPMEAN","UEMPLT5","UEMP5TO14",
  "UEMP15OV","UEMP15T26","UEMP27OV","CLAIMSx","PAYEMS","USGOOD","CES1021000001",
  "USCONS","MANEMP","DMANEMP","NDMANEMP","SRVPRD","USTPU","USWTRADE","USTRADE",
  "USFIRE","USGOVT","CES0600000007","AWOTMAN","AWHMAN","NAPMEI","CES0600000008",
  "CES2000000008","CES3000000008"
)

## ---- Tuned hyper-parameters (from the optimisation runs; not re-run) --------
# These come from the grid searches in 03_Output/Lasso_Optimization and
# 03_Output/rfres_mtry.rds. Re-running the tuning is slow and is NOT part of
# the standard pipeline.
# SAVE_IMPORTANCE: store per-window Random Forest importance (needed for the
# variable-importance figure, 63). Costs roughly 2x the RF runtime; set FALSE
# if you only want the forecasts.
SAVE_IMPORTANCE <- TRUE

HP <- list(
  lasso_lambda = 0.016912,
  ridge_lambda = 1.57,
  elnet_alpha  = 0.4,
  elnet_lambda = 0.049153,
  rf_mtry      = 52,
  llf_mtry     = 52
)

## ---- Paths ------------------------------------------------------------------
# Figures and tables are written STRAIGHT into the LaTeX folder, under the exact
# names main.tex expects, so nothing has to be copied by hand.
P_IN    <- "02_Input"
P_OUT   <- "03_Output"
P_PRED  <- file.path(P_OUT, "Preds")
P_PAPER <- file.path(P_OUT, "Paper")      # csv side-outputs only
F_DESC  <- "06_Latex/Figures/Descriptives"
F_RMSE  <- "06_Latex/Figures/RMSE"
F_VAR   <- "06_Latex/Figures/Var_Imp"
F_ROB   <- "06_Latex/Figures/Robustness"
F_ROOT  <- "06_Latex/Figures"
T_DIR   <- "06_Latex/Tables"
for (p in c(P_OUT, P_PRED, P_PAPER, F_DESC, F_RMSE, F_VAR, F_ROB, T_DIR))
  dir.create(p, showWarnings = FALSE, recursive = TRUE)

# Forecast files are tagged by training window, so the robustness runs
# (55_Robustness_Windows.R) never overwrite the baseline.
WTAG <- paste0("_w", TRAIN_WINDOW)

## ---- Plot palette -----------------------------------------------------------
MODEL_COLORS <- c(
  "Linear Phillips Curve"             = "#6161BA",
  "Non-Linear Phillips Curve"         = "#34BA66",
  "Linear with Variable Selection"    = "#B84444",
  "Non-Linear and Variable Selection" = "#C8B84A",
  "Ensemble Models"                   = "#3A3A3A"
)

## ---- Helper: map a date to its sub-period label -----------------------------
sub_period <- function(dates) {
  y <- data.table::year(dates)
  out <- rep(NA_character_, length(y))
  for (nm in names(SUBPERIODS)) out[y %in% SUBPERIODS[[nm]]] <- nm
  out
}

## ---- Helper: build the shock-dummy matrix -----------------------------------
make_dummies <- function(dates) {
  if (!USE_DUMMIES) return(NULL)
  D <- sapply(DUMMY_DEFS, function(rng)
    as.numeric(dates >= as.Date(rng[1]) & dates <= as.Date(rng[2])))
  matrix(D, nrow = length(dates), dimnames = list(NULL, names(DUMMY_DEFS)))
}

# ============================================================================
# 00_2_Config.R - central configuration for the whole project
# Scope : single source of truth for data, sample, models, hyper-parameters,
#         optimization level and specification overrides.
#         Every downstream script sources this file first.
# ============================================================================
library(data.table)

## ---- Raw data ----------------------------------------------------------------
# FRED-MD vintage used by 01_Data_Transformation.R and 07_Descriptives_Clean.R.
# NOTE (2026-06 vintage): October 2025 was never published for CPI and ~20 other
# series (federal government shutdown). 02_Data_Cleaning.R fills these cells
# with the Stock-Watson EM/PCA imputation like any other missing value; the
# paper discloses this in the Data section.
VINTAGE_CSV <- "02_Input/2026-06-MD.csv"

## ---- Sample -------------------------------------------------------------------
SAMPLE_START <- as.Date("1960-01-01")
SAMPLE_END   <- as.Date("2025-12-01")   # do NOT use 2026 (vintage runs to 2026-05)

## ---- Specification overrides ---------------------------------------------------
# 00_3_Run_Specifications.R sets .SPEC_* in the global environment before sourcing
# the model scripts, so one R session can run several specifications in a loop.
# Without overrides you get the BASELINE of the paper.
TRAIN_WINDOW <- if (exists(".SPEC_WINDOW")) .SPEC_WINDOW else 492   # months
OPT_LEVEL    <- if (exists(".SPEC_OPT"))    .SPEC_OPT    else "standard"
TARGET       <- if (exists(".SPEC_TARGET")) .SPEC_TARGET else ""    # "" = headline CPI

## ---- Forecast design ------------------------------------------------------------
OOS_START <- as.Date("2001-01-01")
HORIZONS  <- c(1, 3)      # months; both run in one pass
N_LAG     <- 4            # lags of each predictor
K_FAC     <- 4            # principal components appended

## ---- Optimization level ----------------------------------------------------------
# Runtime of the forests. "standard" reproduces the paper exactly (current
# settings). "fast" is for smoke tests and exploratory specs (~3x faster);
# "thorough" for a final polish run. Penalised models are unaffected.
stopifnot(OPT_LEVEL %in% c("fast", "standard", "thorough"))
OPT <- switch(OPT_LEVEL,
  fast     = list(rf_ntree =  150, llf_trees =  150, llf_ci_group = 1),
  standard = list(rf_ntree =  500, llf_trees =  500, llf_ci_group = 2),
  thorough = list(rf_ntree = 1500, llf_trees = 1500, llf_ci_group = 2)
)

## ---- Shock dummies (OFF by default) ----------------------------------------------
# WARNING: not real-time information; switching on makes the exercise an
# ex-post decomposition, not an honest OOS forecast. Keep FALSE for the paper.
USE_DUMMIES <- FALSE
DUMMY_DEFS  <- list(
  dum_gfc   = c("2007-12-01", "2009-06-01"),
  dum_covid = c("2020-03-01", "2021-12-01")
)

## ---- Sub-periods -------------------------------------------------------------------
SUBPERIODS <- list(
  "2002-2004" = 2002:2004, "2005-2007" = 2005:2007,
  "2008-2010" = 2008:2010, "2011-2013" = 2011:2013,
  "2014-2016" = 2014:2016, "2017-2019" = 2017:2019,
  "2020-2022" = 2020:2022, "2023-2025" = 2023:2025
)
SHOCK_SUBS <- c("2008-2010", "2020-2022")

## ---- Model space (the 2x2 design) ---------------------------------------------------
BENCHMARKS <- c("RW", "RSM", "AR")
FAMILIES <- list(
  "Linear Phillips Curve"             = c("La_P", "Ri_P", "EN_P"),
  "Linear with Variable Selection"    = c("La",   "Ri",   "EN"),
  "Non-Linear Phillips Curve"         = c("RF_P", "LLF_P"),
  "Non-Linear and Variable Selection" = c("RF",   "LLF")
)
MODELS <- c(BENCHMARKS, unlist(FAMILIES, use.names = FALSE))

## ---- Labour-market subset ------------------------------------------------------------
LABOR_VARS <- c(
  "HWI","HWIURATIO","CLF16OV","CE16OV","UNRATE","UEMPMEAN","UEMPLT5","UEMP5TO14",
  "UEMP15OV","UEMP15T26","UEMP27OV","CLAIMSx","PAYEMS","USGOOD","CES1021000001",
  "USCONS","MANEMP","DMANEMP","NDMANEMP","SRVPRD","USTPU","USWTRADE","USTRADE",
  "USFIRE","USGOVT","CES0600000007","AWOTMAN","AWHMAN","NAPMEI","CES0600000008",
  "CES2000000008","CES3000000008"
)

## ---- Hyper-parameters -----------------------------------------------------------------
# HP_DEFAULT holds the tuned values used throughout the paper so far (grid
# searches on the pre-2001 validation sample; provenance in
# 03_Output/Lasso_Optimization and rfres_mtry.rds). They are HARD-CODED so the
# pipeline runs immediately with no tuning step.
#
# HP_CASES allows case-specific values: key "<set>_h<horizon>" with
# set in {full, labor}. Any field set there overrides HP_DEFAULT for that case
# only. Fill these in as the per-case tuning runs are done locally; empty
# list() means "use the defaults", which reproduces the current paper.
HP_DEFAULT <- list(
  lasso_lambda = 0.016912,
  ridge_lambda = 1.57,
  elnet_alpha  = 0.4,
  elnet_lambda = 0.049153,
  rf_mtry      = 52,
  llf_mtry     = 52
)
HP_CASES <- list(
  full_h1  = list(),   # e.g. list(lasso_lambda = 0.021, rf_mtry = 40)
  full_h3  = list(),
  labor_h1 = list(),
  labor_h3 = list()
)
get_hp <- function(set, h) {
  key <- paste0(set, "_h", h)
  over <- HP_CASES[[key]]
  if (is.null(over)) over <- list()
  utils::modifyList(HP_DEFAULT, over)
}

## ---- Paths -------------------------------------------------------------------------------
P_IN  <- "02_Input"
P_OUT <- "03_Output"
DATA_FILE <- if (nzchar(TARGET)) sprintf("data_cleaned_%s.rds", TARGET) else "data_cleaned.rds"

.sub    <- if (nzchar(TARGET)) paste0("Robustness_", TARGET) else NULL
.join   <- function(...) do.call(file.path, as.list(c(..., .sub)))
P_PRED  <- .join(P_OUT, "Preds")
P_PAPER <- .join(P_OUT, "Paper")
F_ROOT  <- .join("06_Latex/Figures")
F_DESC  <- file.path(F_ROOT, "Descriptives")
F_RMSE  <- file.path(F_ROOT, "RMSE")
F_VAR   <- file.path(F_ROOT, "Var_Imp")
F_ROB   <- file.path(F_ROOT, "Robustness")
T_DIR   <- .join("06_Latex/Tables")
for (p in c(P_OUT, P_PRED, P_PAPER, F_DESC, F_RMSE, F_VAR, F_ROB, T_DIR))
  dir.create(p, showWarnings = FALSE, recursive = TRUE)

# Forecast files are tagged by training window (and by optimization level when
# it deviates from "standard"), so no specification can overwrite another.
WTAG <- paste0("_w", TRAIN_WINDOW,
               if (OPT_LEVEL == "standard") "" else paste0("_", OPT_LEVEL))

# Per-window RF importance is needed for the variable-importance figure.
SAVE_IMPORTANCE <- TRUE

## ---- Plot palette --------------------------------------------------------------------------
MODEL_COLORS <- c(
  "Linear Phillips Curve"             = "#6161BA",
  "Non-Linear Phillips Curve"         = "#34BA66",
  "Linear with Variable Selection"    = "#B84444",
  "Non-Linear and Variable Selection" = "#C8B84A",
  "Ensemble Models"                   = "#3A3A3A"
)

## ---- Helpers ---------------------------------------------------------------------------------
sub_period <- function(dates) {
  y <- data.table::year(dates)
  out <- rep(NA_character_, length(y))
  for (nm in names(SUBPERIODS)) out[y %in% SUBPERIODS[[nm]]] <- nm
  out
}
make_dummies <- function(dates) {
  if (!USE_DUMMIES) return(NULL)
  D <- sapply(DUMMY_DEFS, function(rng)
    as.numeric(dates >= as.Date(rng[1]) & dates <= as.Date(rng[2])))
  matrix(D, nrow = length(dates), dimnames = list(NULL, names(DUMMY_DEFS)))
}

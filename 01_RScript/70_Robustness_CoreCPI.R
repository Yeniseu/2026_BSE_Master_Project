# ============================================================================
# 70_Robustness_CoreCPI.R   [paper TODO 3]  -- NOT run by default: slow.
#
# Re-runs the whole forecasting exercise with core inflation as the target.
# Headline CPI is dominated by energy at short horizons, so core targets isolate
# the Phillips-Curve-relevant component of the forecast gains.
#
#   CPILFESL  core CPI  (CPI less food and energy)   -- in FRED-MD
#   PCEPI     PCE deflator                           -- in FRED-MD
#
# Usage: set TARGET below, then run. Everything downstream is unchanged, only
# the output directory differs.
#
#   Rscript 01_RScript/70_Robustness_CoreCPI.R CPILFESL
# ============================================================================
source("01_RScript/00_Config.R")

args   <- commandArgs(trailingOnly = TRUE)
TARGET <- if (length(args)) args[1] else "CPILFESL"
stopifnot(TARGET %in% c("CPILFESL", "PCEPI"))

# Route all output to a target-specific folder so the baseline is never touched
P_PRED  <<- file.path(P_OUT,   "Preds", TARGET)
P_PAPER <<- file.path(P_OUT,   "Paper", paste0("Robustness_", TARGET))
dir.create(P_PRED,  showWarnings = FALSE, recursive = TRUE)
dir.create(P_PAPER, showWarnings = FALSE, recursive = TRUE)

# Swap the target: the pipeline always forecasts the column called CPIAUCSL, so
# we rename the chosen core series into that slot and drop the original.
data <- readRDS(file.path(P_IN, "data_cleaned.rds")); setDT(data)
stopifnot(TARGET %in% names(data))

# data_cleaned multiplies CPIAUCSL by 100 (see 02_Data_Cleaning.R); the core
# series still carry the raw log-difference, so scale it the same way.
data[, CPIAUCSL := NULL]
data[, CPIAUCSL := get(TARGET) * 100]
data[, (TARGET) := NULL]
saveRDS(data, file.path(P_IN, paste0("data_cleaned_", TARGET, ".rds")))

cat(sprintf("Target swapped to %s. Now run, in order:\n", TARGET))
cat("  10_Benchmarks.R  15_Penalised.R  18_RandomForest.R  20_LLF.R\n")
cat("  50_Assemble_Predictions.R  52_RMSE_Tables_Charts.R  60_Forecast_Tests.R  61_Ensemble_Recursive.R\n")
cat(sprintf("after pointing them at 02_Input/data_cleaned_%s.rds\n", TARGET))

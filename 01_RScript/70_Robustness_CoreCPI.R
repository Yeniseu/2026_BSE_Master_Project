# ============================================================================
# 70_Robustness_CoreCPI.R   [paper TODO 3]  -- data preparation step
#
# FRED-MD does NOT contain core CPI or core PCE, so the alternative target has
# to be brought in from a separate FRED download and spliced into the cleaned
# panel. This script does the splice; it does NOT run any models.
#
# What it does:
#   1. reads the raw core index from an Excel file in 02_Input/,
#   2. transforms it to the SAME target convention as headline CPI,
#      pi_t = 100 * (log CPI_t - log CPI_{t-1})   (a single log-difference;
#      verified to reproduce data_cleaned$CPIAUCSL exactly),
#   3. replaces the CPIAUCSL column of data_cleaned.rds with the core series,
#      leaving every predictor untouched,
#   4. saves 02_Input/data_cleaned_<SERIES>.rds.
#
# Then, to produce the robustness results:
#   * set  TARGET <- "<SERIES>"  in 00_2_Config.R   (e.g. "CPILFESL")
#   * re-run 10 -> 15 -> 18 -> 20 -> 50 -> 52 -> 60 -> 61
#   Everything routes into 03_Output/.../Robustness_<SERIES> and
#   06_Latex/Figures/Robustness_<SERIES>, so the headline results are untouched.
#
# ---------------------------------------------------------------------------
# Point these two at your download. The Excel file is the standard FRED export
# (a "Monthly" sheet with columns observation_date and the series code).
CORE_SERIES <- "CPILFESL"                              # FRED series code = target
CORE_FILE   <- "02_Input/Core_Inf_CPILFESL.xlsx"       # raw index levels
CORE_SHEET  <- "Monthly"
# ---------------------------------------------------------------------------

source("01_RScript/00_2_Config.R")
library(readxl)

## 1. raw core index --------------------------------------------------------
core <- as.data.table(read_excel(CORE_FILE, sheet = CORE_SHEET))
setnames(core, 1:2, c("date", "level"))
core[, date := as.Date(date)]
core <- core[!is.na(level)]
setorder(core, date)

## 2. same transform as headline: 100 * first difference of log ------------
core[, target := 100 * (log(level) - log(shift(level, 1)))]

## 3. splice into the cleaned panel ----------------------------------------
data <- readRDS(file.path(P_IN, "data_cleaned.rds")); setDT(data)
panel_dates <- data$date
head_mean <- mean(data$CPIAUCSL); head_sd <- sd(data$CPIAUCSL)   # for the comparison print

core_aligned <- core[data, on = "date"]$target        # match panel row for row
n_missing <- sum(is.na(core_aligned))
if (n_missing > 0)
  stop(sprintf("%d panel months have no %s observation (range %s..%s). Extend the download.",
               n_missing, CORE_SERIES, format(min(panel_dates)), format(max(panel_dates))))

data[, CPIAUCSL := core_aligned]                       # swap target, keep predictors
out <- file.path(P_IN, sprintf("data_cleaned_%s.rds", CORE_SERIES))
saveRDS(data, out)

## 4. report ----------------------------------------------------------------
cat(sprintf("Wrote %s\n", out))
cat(sprintf("Target = %s, %d months, %s .. %s\n",
            CORE_SERIES, nrow(data), format(min(panel_dates)), format(max(panel_dates))))
cat(sprintf("pi mean = %.3f, sd = %.3f  (headline for comparison: mean %.3f, sd %.3f)\n",
            mean(data$CPIAUCSL), sd(data$CPIAUCSL), head_mean, head_sd))
cat(sprintf("\nNext: set TARGET <- \"%s\" in 00_2_Config.R, then re-run 10 -> 61.\n", CORE_SERIES))

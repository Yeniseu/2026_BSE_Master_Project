# ============================================================================
# 71_MRF_Comparison.R - RMSE comparison of MRF/MRF_P (19_MRF.R) against every
#   existing model in preds_h{1,3}_w492.rds, over the IDENTICAL set of months
#   MRF was run on (an inner join on date - see 19_MRF.R's SCOPE note; by
#   default the shock+calm scope: 2005-2007, 2008-2010, 2017-2019, 2020-2022).
#
# STANDALONE. Only reads 00_2_Config.R and the .rds files already on disk;
# does not touch 18_RandomForest.R, 20_LLF.R, 50_Assemble_Predictions.R, or
# any other existing script/output. Run any time after both 19_MRF.R and the
# main pipeline (50_Assemble_Predictions.R) have produced their files.
#
# WHY AN INNER JOIN IS THE RIGHT COMPARISON: comparing MRF's RMSE (computed
# over its ~144-month scope) against another model's RMSE reported over the
# FULL ~288/300-month sample would be unfair - the full-sample number is
# diluted by easy calm months MRF never had to forecast. Restricting every
# model's RMSE to the exact same dates as MRF is the only apples-to-apples
# comparison, and mirrors how the paper itself reports RMSE by sub-period
# rather than one pooled number (Section 5.1 / Figure 3).
# ============================================================================
source("01_RScript/00_2_Config.R")

# Picks the widest-scope MRF file available: a full-sample run if you've done
# one (mrf_w492.rds), otherwise the shock+calm default (mrf_w492_shocks.rds).
mrf_file <- if (file.exists(file.path(P_PRED, paste0("mrf", WTAG, ".rds")))) {
  paste0("mrf", WTAG, ".rds")
} else if (file.exists(file.path(P_PRED, paste0("mrf", WTAG, "_shocks.rds")))) {
  paste0("mrf", WTAG, "_shocks.rds")
} else {
  stop("No MRF predictions found in ", P_PRED, " - run 19_MRF.R first.")
}
cat(sprintf("Using MRF file: %s\n", mrf_file))
mrf <- readRDS(file.path(P_PRED, mrf_file))

rmse <- function(real, pred) sqrt(mean((real - pred)^2))

comparison_tables <- list()
subperiod_tables  <- list()

for (h in HORIZONS) {
  hh <- paste0("h", h)
  base <- readRDS(file.path(P_PRED, paste0("preds_", hh, WTAG, ".rds")))

  # Inner join: keep only dates MRF actually has predictions for.
  merged <- merge(base, mrf[[hh]], by = "date")
  cat(sprintf("h=%d: %d months in the comparison (MRF's scope)\n", h, nrow(merged)))

  cols <- c(MODELS, "MRF", "MRF_P")
  cols <- cols[cols %in% names(merged)]   # in case some are missing

  overall <- sapply(cols, function(m) rmse(merged$real, merged[[m]]))
  comparison_tables[[hh]] <- data.table(model = names(overall), rmse = round(overall, 4))[order(rmse)]

  sub_tab <- merged[, lapply(.SD, function(x) round(rmse(real, x), 4)),
                    by = sub, .SDcols = cols]
  setorder(sub_tab, sub)
  subperiod_tables[[hh]] <- sub_tab
}

cat("\n==== Overall RMSE (MRF's scope), h=1 ====\n"); print(comparison_tables$h1)
cat("\n==== Overall RMSE (MRF's scope), h=3 ====\n"); print(comparison_tables$h3)
cat("\n==== By sub-period, h=1 ====\n"); print(subperiod_tables$h1)
cat("\n==== By sub-period, h=3 ====\n"); print(subperiod_tables$h3)

# ---- Save --------------------------------------------------------------
out_dir <- file.path(P_OUT, "Paper", "MRF_Comparison")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
fwrite(comparison_tables$h1, file.path(out_dir, "overall_rmse_h1.csv"))
fwrite(comparison_tables$h3, file.path(out_dir, "overall_rmse_h3.csv"))
fwrite(subperiod_tables$h1,  file.path(out_dir, "subperiod_rmse_h1.csv"))
fwrite(subperiod_tables$h3,  file.path(out_dir, "subperiod_rmse_h3.csv"))
cat(sprintf("\nsaved tables to %s\n", out_dir))

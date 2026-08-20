# ============================================================================
# 56_Spec_Diagnostics.R - open up the robustness average
#
# 55_Robustness_Windows.R averages the eight specifications (4 training windows
# x 2 horizons) into one line per family. That average can hide a single badly
# behaved cell, so this script prints the same numbers specification by
# specification, at three levels of detail:
#
#   (1) family RMSE per specification, for the sub-period of interest
#   (2) the individual models inside a family, to see which one moves
#   (3) an outlier flag: any specification whose RMSE exceeds the median across
#       specifications by more than OUTLIER_MULT
#
# Set FOCUS_SUB / FOCUS_FAMILY below. Writes
# 03_Output/Paper/spec_diagnostics_<sub>.csv and prints everything.
# ============================================================================
source("01_RScript/00_2_Config.R")

FOCUS_SUB    <- "2008-2010"                          # sub-period to inspect
FOCUS_FAMILY <- "Linear with Variable Selection"     # family to open up
OUTLIER_MULT <- 1.5                                  # x median across specs

files <- list.files(P_PRED, pattern = "^preds_h[13]_w[0-9]+\\.rds$", full.names = TRUE)
stopifnot(length(files) > 0)

# ---- one row per specification x sub-period, all models ---------------------
spec <- rbindlist(lapply(files, function(f) {
  m <- regmatches(basename(f), regexec("preds_h([13])_w([0-9]+)", basename(f)))[[1]]
  d <- readRDS(f)[!is.na(sub)]
  r <- d[, lapply(.SD, function(x) sqrt(mean((x - real)^2))), by = sub, .SDcols = MODELS]
  for (fm in names(FAMILIES)) r[[fm]] <- rowMeans(r[, FAMILIES[[fm]], with = FALSE])
  cbind(h = as.integer(m[2]), w = as.integer(m[3]), r)
}))
setorder(spec, sub, h, w)
fwrite(spec, file.path(P_PAPER, sprintf("spec_diagnostics_%s.csv", FOCUS_SUB)))

foc <- spec[sub == FOCUS_SUB]

# ---- (1) family RMSE per specification --------------------------------------
cat(sprintf("\n=== %s : family RMSE by specification ===\n", FOCUS_SUB))
print(foc[, c(.(h = h, w = w), .SD), .SDcols = names(FAMILIES)])
cat("\naverage across the", nrow(foc), "specifications:\n")
print(foc[, lapply(.SD, mean), .SDcols = names(FAMILIES)])

# ---- (2) the individual models inside the family of interest ----------------
mem <- FAMILIES[[FOCUS_FAMILY]]
cat(sprintf("\n=== %s : members of '%s' ===\n", FOCUS_SUB, FOCUS_FAMILY))
print(foc[, c(.(h = h, w = w), .SD), .SDcols = mem])

# ---- (3) outliers: which cell drags the average? ----------------------------
cat("\n=== outlier check (per model, across specifications) ===\n")
flags <- rbindlist(lapply(mem, function(m) {
  v <- foc[[m]]; med <- median(v)
  data.table(model = m, median = med, max = max(v),
             at_h = foc$h[which.max(v)], at_w = foc$w[which.max(v)],
             flagged = max(v) > OUTLIER_MULT * med)
}))
print(flags)

# ---- (4) what the family average looks like without the flagged cell --------
bad <- foc[which.max(rowMeans(foc[, ..mem]))]
cat(sprintf("\nWorst specification for this family: h=%d, w=%d (%.3f)\n",
            bad$h, bad$w, mean(unlist(bad[, ..mem]))))
keep <- foc[!(h == bad$h & w == bad$w)]
cat(sprintf("Family average  all %d specs : %.3f\n", nrow(foc), mean(rowMeans(foc[, ..mem]))))
cat(sprintf("Family average  excluding it : %.3f   (linear PC benchmark: %.3f)\n",
            mean(rowMeans(keep[, ..mem])),
            mean(rowMeans(foc[, FAMILIES[["Linear Phillips Curve"]], with = FALSE]))))

cat(sprintf("\nFull table written to %s\n",
            file.path(P_PAPER, sprintf("spec_diagnostics_%s.csv", FOCUS_SUB))))

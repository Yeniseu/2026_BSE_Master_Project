# ============================================================================
# 50_Assemble_Predictions.R
# Merges every model's forecasts into ONE canonical table per horizon:
#   03_Output/Preds/preds_h1_w492.rds , preds_h3_w492.rds
#   columns: date | real | RW RSM AR La_P Ri_P EN_P La Ri EN RF_P LLF_P RF LLF | sub
# Everything downstream (52, 55, 60, 61) reads only these files, so the paper's
# tables and figures regenerate automatically after any model is re-run.
# ============================================================================
source("01_RScript/00_Config.R")

bm <- readRDS(file.path(P_PRED, paste0("benchmarks", WTAG, ".rds")))
pn <- readRDS(file.path(P_PRED, paste0("penalised",  WTAG, ".rds")))
rf <- readRDS(file.path(P_PRED, paste0("rf",         WTAG, ".rds")))
lf <- readRDS(file.path(P_PRED, paste0("llf",        WTAG, ".rds")))

for (h in HORIZONS) {
  hh <- paste0("h", h)
  d <- Reduce(function(a, b) merge(a, b, by = "date"),
              list(bm[[hh]], pn[[hh]], rf[[hh]], lf[[hh]]))
  setcolorder(d, c("date", "real", MODELS))
  d[, sub := sub_period(date)]
  stopifnot(!any(is.na(d[, ..MODELS])))
  saveRDS(d, file.path(P_PRED, paste0("preds_", hh, WTAG, ".rds")))
  cat(sprintf("preds_%s%s.rds: %d months, %s .. %s\n",
              hh, WTAG, nrow(d), format(min(d$date)), format(max(d$date))))
}

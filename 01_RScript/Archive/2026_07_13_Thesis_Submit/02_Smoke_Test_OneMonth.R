# Author: Ece Tasan (patched v2)
# Date  : 19/05/2026
# Scope : SHAP Values — ONE-MONTH SMOKE TEST
#         Renames the 3 problematic S&P columns at the source so no
#         downstream code has to deal with non-syntactic names.

library(data.table)
library(randomForest)
library(treeshap)

rm(list = ls())
options(print.max = 300, scipen = 30, digits = 5)

source("01_RScript/00_Functions_RF.R")

#### Load & prepare ####
fred <- readRDS("02_Input/data_cleaned.rds")
setDT(fred)

# ---- RENAME problematic columns once, at the source ----
# data_cleaned.rds has three non-syntactic names that break treeshap.
# Rename them here; they will keep these names through all graphs and tables.
old_names <- c("S&P 500", "S&P div yield", "S&P PE ratio")
new_names <- c("SP500",   "SP_div_yield", "SP_PE_ratio")
present   <- old_names %in% colnames(fred)
if (any(present)) {
  setnames(fred, old_names[present], new_names[present])
  cat("Renamed:\n")
  print(data.frame(from = old_names[present], to = new_names[present]))
}

# Sanity check: every name in fred should now be R-syntactic
stopifnot(identical(
  colnames(fred),
  make.names(colnames(fred), unique = TRUE)
))
cat("All column names are R-syntactic. Proceeding.\n\n")

setnames(fred, "CPIAUCSL", "inf")
setcolorder(fred, c("date", "inf"))

best_mtry <- 52

# 20-year window, first OOS period, lag=3 — ONE iteration only
Y <- tail(fred[date < "2016-01-01"], 240 + 180)
Y <- Y[, date := NULL]
Y <- as.matrix(Y)

nprev <- 1   # <- just one month to verify it works

set.seed(123)
shap_smoke <- rf.rolling.window_shap(Y, nprev, 1, 3)

cat("\n--- Smoke test results ---\n")
cat("Prediction:", shap_smoke$pred[1], "\n")
cat("RMSE / MAE:", shap_smoke$errors, "\n")
cat("Length of SHAP vector:", length(shap_smoke$save.shap[[1]]), "\n")

cat("\nTop 10 features by |SHAP|:\n")
sv <- shap_smoke$save.shap[[1]]
print(head(sort(abs(sv), decreasing = TRUE), 10))

cat("\n>>> If you see this line, treeshap works end-to-end. <<<\n")

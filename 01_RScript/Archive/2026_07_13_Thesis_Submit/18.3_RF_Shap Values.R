# Author: Ece Tasan
# Date  : 19/05/2026
# Scope : SHAP Values — 3-month-ahead forecasts only
#         Mirrors the baseline rf1_3 and rf2_3 specifications:
#           * First sample  : 41-year training window, OOS 2001-2015 (nprev=180)
#           * Second sample : 56-year training window, OOS 2016-2024 (nprev=108)
#         Both runs use the same RF tuning (best_mtry = 52).

library(data.table)
library(randomForest)
library(ggplot2)
library(treeshap)

rm(list = ls())
options(print.max = 300, scipen = 30, digits = 5)

source("01_RScript/00_Functions_RF.R")

dir.create("03_Output", showWarnings = FALSE)

#### Load & prepare ####
fred <- readRDS("02_Input/data_cleaned.rds")
setDT(fred)
dim(fred)

# ---- Rename non-syntactic S&P columns at the source ----
# treeshap's randomForest.unify silently rewrites names like "S&P 500" -> "S.P.500"
# via as.data.frame(), which then mismatches model$Feature. Renaming once here
# keeps every downstream object consistent.
old_names <- c("S&P 500", "S&P div yield", "S&P PE ratio")
new_names <- c("SP500",   "SP_div_yield", "SP_PE_ratio")
present   <- old_names %in% colnames(fred)
if (any(present)) setnames(fred, old_names[present], new_names[present])
stopifnot(identical(colnames(fred), make.names(colnames(fred), unique = TRUE)))

setnames(fred, "CPIAUCSL", "inf")
setcolorder(fred, c("date", "inf"))

# Tuning result from baseline RF runs
best_mtry <- 52


################## FIRST SAMPLE (2001-2015) ######################
###########  41-year Training Window, lag = 3  ###################

Y <- fred[date < "2016-01-01"]
Y <- Y[, date := NULL]
Y <- as.matrix(Y)
dim(Y)               # 672 x 126
nprev <- 180         # OOS length 2001-2015

cat("\n===== shap1_3_41 (41-yr window, lag=3, nprev=180) =====\n")
set.seed(123)
shap1_3_41 <- rf.rolling.window_shap(Y, nprev, 1, 3)
saveRDS(shap1_3_41, file = "03_Output/shap1_3_41.rds")
cat("Errors (RMSE / MAE):", shap1_3_41$errors, "\n")


################## SECOND SAMPLE (2016-2024) #####################
###########  56-year Training Window, lag = 3  ###################

Y <- fred
Y <- Y[, date := NULL]
Y <- as.matrix(Y)
dim(Y)               # 780 x 126
nprev <- 108         # OOS length 2016-2024

cat("\n===== shap2_3_56 (56-yr window, lag=3, nprev=108) =====\n")
set.seed(123)
shap2_3_56 <- rf.rolling.window_shap(Y, nprev, 1, 3)
saveRDS(shap2_3_56, file = "03_Output/shap2_3_56.rds")
cat("Errors (RMSE / MAE):", shap2_3_56$errors, "\n")

cat("\n>>> Both 3-month-ahead SHAP runs complete. Outputs in 03_Output/. <<<\n")
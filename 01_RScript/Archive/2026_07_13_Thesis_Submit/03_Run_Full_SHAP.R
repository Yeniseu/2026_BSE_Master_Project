# Author: Ece Tasan (patched v2)
# Date  : 19/05/2026
# Scope : SHAP Values — full rolling-window run

library(data.table)
library(randomForest)
library(treeshap)
library(ggplot2)

rm(list = ls())
options(print.max = 300, scipen = 30, digits = 5)

source("01_RScript/00_Functions_RF.R")

#### Load & prepare ####
fred <- readRDS("02_Input/data_cleaned.rds")
setDT(fred)

# Rename non-syntactic columns once at the source (kept renamed downstream)
old_names <- c("S&P 500", "S&P div yield", "S&P PE ratio")
new_names <- c("SP500",   "SP_div_yield", "SP_PE_ratio")
present   <- old_names %in% colnames(fred)
if (any(present)) setnames(fred, old_names[present], new_names[present])

stopifnot(identical(colnames(fred), make.names(colnames(fred), unique = TRUE)))

setnames(fred, "CPIAUCSL", "inf")
setcolorder(fred, c("date", "inf"))

best_mtry <- 52

# ---- 20-year window, first OOS period, lag=3 ----
Y <- tail(fred[date < "2016-01-01"], 240 + 180)
Y <- Y[, date := NULL]
Y <- as.matrix(Y)
nprev <- 180

set.seed(123)
shap1_3_20 <- rf.rolling.window_shap(Y, nprev, 1, 3)
saveRDS(shap1_3_20, file = "03_Output/shap1_3_20.rds")

# ---- 20-year window, second OOS period, lag=3 ----
Y <- tail(fred, 240 + 108)
Y <- Y[, date := NULL]
Y <- as.matrix(Y)
nprev <- 108

set.seed(123)
shap2_3_20 <- rf.rolling.window_shap(Y, nprev, 1, 3)
saveRDS(shap2_3_20, file = "03_Output/shap2_3_20.rds")

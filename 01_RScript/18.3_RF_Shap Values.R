# Author: Ece Tasan
# Date  : 3/12/2025
# Scope : SHAP Values

library(data.table)
library(randomForest)
library(ggplot2)
library(treeshap)


rm(list = ls())
options(print.max = 300, scipen = 30, digits = 5)

source("01_RScript/00_Functions_RF.R")

#### Load & prepare ####

fred <- readRDS("02_Input/data_cleaned.rds")
setDT(fred)
dim(fred)

setnames(fred, "CPIAUCSL", "inf")
setcolorder(fred, c("date", "inf"))
best_mtry <- 52


# 20-year window, first OOS period, lag=3
Y <- tail(fred[date < "2016-01-01"], 240 + 180)
Y <- Y[, date := NULL]
Y <- as.matrix(Y)
nprev <- 180

set.seed(123)
shap1_3_20 <- rf.rolling.window_shap(Y, nprev, 1, 3)
saveRDS(shap1_3_20, file = "03_Output/shap1_3_20.rds")

# 20-year window, second OOS period, lag=3
Y <- tail(fred, 240 + 108)
Y <- Y[, date := NULL]
Y <- as.matrix(Y)
nprev <- 108

set.seed(123)
shap2_3_20 <- rf.rolling.window_shap(Y, nprev, 1, 3)
saveRDS(shap2_3_20, file = "03_Output/shap2_3_20.rds")
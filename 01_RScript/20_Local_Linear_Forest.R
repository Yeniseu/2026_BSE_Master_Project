# Author: Adapted for Local Linear Forest
# Scope : Apply Local Linear Forest to FRED-MD inflation forecasting

library(data.table)
library(grf)
library(ggplot2)

rm(list = ls())
options(print.max = 300, scipen = 30, digits = 5)

source("01_RScript/00_Functions_LLF.R")

#### Load & prepare ####

fred <- readRDS("02_Input/data_cleaned.rds")
setDT(fred)

setnames(fred, "CPIAUCSL", "inf")
setcolorder(fred, c("date", "inf"))

#### TUNING: validation sample 1991-2000 ####

Y_val <- copy(fred[date < "2001-01-01"])
Y_val_mat <- as.matrix(Y_val[, date := NULL])

# 120 OOS validation observations = 1991-2000
nprev <- 120

# To make mtry grid coherent with the actual design dimension,
# approximate p after PCA+lag with lag = 1:
p_base <- ncol(Y_val_mat) + 4
mtry_grid <- unique(sort(c(
  5, 10, 20, 30, 40, 50,
  round(p_base / 10), round(p_base / 8), round(p_base / 6),
  round(p_base / 4), round(p_base / 3), round(p_base / 2)
)))
mtry_grid <- mtry_grid[mtry_grid <= p_base]

# Run once if you want to retune
# set.seed(123)
# llf_mtry_results <- llf_rolling_window_tune_mtry(
#   Y_raw = Y_val_mat,
#   nprev = nprev,
#   mtry_grid = mtry_grid,
#   target_col = 1,
#   lag = 1,
#   kfac = 4,
#   num.trees = 2000,
#   min.node.size = 5,
#   honesty = TRUE,
#   sample.fraction = 0.5,
#   seed = 123
# )
# saveRDS(llf_mtry_results, file = "03_Output/llfres_mtry.rds")


#### FIRST OOS PERIOD: 2001-2015 ####

Y1 <- copy(fred[date < "2016-01-01"])

# Reproduce your crisis dummy logic
dum1 <- rep(0, nrow(Y1))
dum1[which.min(Y1$inf)] <- 1

Y1_mat <- as.matrix(Y1[, date := NULL])

# 180 OOS observations = 2001-2015
nprev <- 180
set.seed(123)
llf1_1 <- llf_rolling_window(
  Y_raw = Y1_mat,
  nprev = nprev,
  target_col = 1,
  lag = 1,
  kfac = 4,
  num.trees = 2000,
  #mtry = best_mtry,
  min.node.size = 5,
  honesty = TRUE,
  sample.fraction = 0.5,
  tune_ll_lambda = FALSE,      # turn TRUE if you want slower but more tailored fits
  seed = 123,
  dummy_full = dum1,
  plot_results = TRUE
)
llf1_1$errors

set.seed(123)
llf1_3 <- llf_rolling_window(
  Y_raw = Y1_mat,
  nprev = nprev,
  target_col = 1,
  lag = 3,
  kfac = 4,
  num.trees = 2000,
  #mtry = best_mtry,
  min.node.size = 5,
  honesty = TRUE,
  sample.fraction = 0.5,
  tune_ll_lambda = FALSE,
  seed = 123,
  dummy_full = dum1,
  plot_results = TRUE
)
llf1_3$errors

#### SECOND OOS PERIOD: 2016-2024 ####

Y2 <- copy(fred)
Y2_mat <- as.matrix(Y2[, date := NULL])

# 108 OOS observations = 2016-2024
nprev <- 108
set.seed(123)
llf2_1 <- llf_rolling_window(
  Y_raw = Y2_mat,
  nprev = nprev,
  target_col = 1,
  lag = 1,
  kfac = 4,
  num.trees = 2000,
  #mtry = best_mtry,
  min.node.size = 5,
  honesty = TRUE,
  sample.fraction = 0.5,
  tune_ll_lambda = FALSE,
  seed = 123,
  plot_results = TRUE
)
llf2_1$errors

set.seed(123)
llf2_3 <- llf_rolling_window(
  Y_raw = Y2_mat,
  nprev = nprev,
  target_col = 1,
  lag = 3,
  kfac = 4,
  num.trees = 2000,
  #mtry = best_mtry,
  min.node.size = 5,
  honesty = TRUE,
  sample.fraction = 0.5,
  tune_ll_lambda = FALSE,
  seed = 123,
  plot_results = TRUE
)
llf2_3$errors


llf_s1 <- data.table(llf1_1 = llf1_1$pred, llf1_3 = llf1_3$pred)
llf_s2 <- data.table(llf2_1 = llf2_1$pred, llf2_3 = llf2_3$pred)
saveRDS(llf_s1, file = "03_Output/llf_s1.rds")
saveRDS(llf_s2, file = "03_Output/llf_s2.rds")


aa <- readRDS("03_Output/llf_s2.rds")
bb <- data.table(llf2_3 = llf2_3$pred)
llf_s2 <- cbind(aa,bb)
saveRDS(llf_s2, file = "03_Output/llf_s2.rds")




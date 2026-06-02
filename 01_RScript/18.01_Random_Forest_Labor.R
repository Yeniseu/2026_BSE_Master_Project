# Author: Ece Tasan
# Date  : 3/12/2025
# Scope : Apply Random Forest

library(data.table)
library(randomForest)
library(ggplot2)

rm(list = ls())
options(print.max = 300, scipen = 30, digits = 5)

source("01_RScript/00_Functions_RF.R")

#### Load & prepare ####

fred <- readRDS("02_Input/data_cleaned.rds")
setDT(fred)
dim(fred)

setnames(fred, "CPIAUCSL", "inf")
setcolorder(fred, c("date", "inf"))
labor_indicators <- c(
  "HWI","HWIURATIO","CLF16OV","CE16OV","UNRATE","UEMPMEAN","UEMPLT5","UEMP5TO14",
  "UEMP15OV","UEMP15T26","UEMP27OV","CLAIMSx","PAYEMS","USGOOD","CES1021000001",
  "USCONS","MANEMP","DMANEMP","NDMANEMP","SRVPRD","USTPU","USWTRADE","USTRADE",
  "USFIRE","USGOVT","CES0600000007","AWOTMAN","AWHMAN","CES0600000008",
  "CES2000000008","CES3000000008"
)
labor_indicators <- labor_indicators[labor_indicators %in%  names(fred)]
fred <- fred[, .SD, .SDcols=c("date", "inf", labor_indicators)]  # Open for only labor indicators


###################### PREDICTIONS ###############################

################## FIRST SAMPLE (2001-2015) ######################

# Tuning Result: Best mtry result
best_mtry <- 52

###########  41 years Training Window  ########

# FIRST Out of Sample Predictions: 2001-2015
Y <- fred[date < "2016-01-01"]
Y <- Y[, date := NULL]
Y <- as.matrix(Y)
dim(Y)
# Out of Sample Length = 180 (between years 2001-2015)
nprev <- 180

set.seed(123)
rf1_1 <- rf.rolling.window(Y,nprev,1,1)
saveRDS(rf1_1, file= "03_Output/rf1_1_labor.rds")
rf1_1$errors


rf1_3 <- rf.rolling.window(Y,nprev,1,3)
saveRDS(rf1_3, file= "03_Output/rf1_3_labor.rds")
rf1_3$errors


###########  20 years Training Window  ########

best_mtry <- 52
# FIRST OOS PERIOD (2001-2015)
# 20-year training window = 240 months
# First prediction trains on 1981-01 to 2000-12
Y <- tail(fred[date < "2016-01-01"], 240 + 180)
Y <- Y[, date := NULL]
Y <- as.matrix(Y)
nprev <- 180

set.seed(123)
rf1_1_20 <- rf.rolling.window(Y,nprev,1,1)
saveRDS(rf1_1_20, file= "03_Output/rf1_1_20_labor.rds")
rf1_1_20$errors

rf1_3_20 <- rf.rolling.window(Y,nprev,1,3)
saveRDS(rf1_3_20, file= "03_Output/rf1_3_20_labor.rds")
rf1_3_20$errors



###########  30 years Training Window  ########

best_mtry <- 52
# FIRST OOS PERIOD (2001-2015)
# 30-year training window = 360 months
Y <- tail(fred[date < "2016-01-01"], 360 + 180)
Y <- Y[, date := NULL]
Y <- as.matrix(Y)
nprev <- 180

set.seed(123)
rf1_1_30 <- rf.rolling.window(Y,nprev,1,1)
saveRDS(rf1_1_30, file= "03_Output/rf1_1_30_labor.rds")
rf1_1_30$errors

rf1_3_30 <- rf.rolling.window(Y,nprev,1,3)
saveRDS(rf1_3_30, file= "03_Output/rf1_3_30_labor.rds")
rf1_3_30$errors


###########  40 years Training Window  ########

best_mtry <- 52
# FIRST OOS PERIOD (2001-2015)
# 40-year training window = 480 months
Y <- tail(fred[date < "2016-01-01"], 480 + 180)
Y <- Y[, date := NULL]
Y <- as.matrix(Y)
nprev <- 180

set.seed(123)
rf1_1_40 <- rf.rolling.window(Y,nprev,1,1)
saveRDS(rf1_1_40, file= "03_Output/rf1_1_40_labor.rds")
rf1_1_40$errors

rf1_3_40 <- rf.rolling.window(Y,nprev,1,3)
saveRDS(rf1_3_40, file= "03_Output/rf1_3_40_labor.rds")
rf1_3_40$errors


##################### SECOND SAMPLE (2016-2024) ###################

# Tuning Result: Best mtry result
best_mtry <- 52

###########  56 years Training Window  ########

Y <- fred
Y <- Y[, date := NULL]
Y <- as.matrix(Y)
dim(Y)
# Out of Sample Length = 108 (between years 2016-2024)
nprev <- 108

set.seed(123)
rf2_1 <- rf.rolling.window_second(Y,nprev,1,1)
saveRDS(rf2_1, file= "03_Output/rf2_1_labor.rds")
rf2_1$errors


rf2_3 <- rf.rolling.window_second(Y,nprev,1,3)
saveRDS(rf2_3, file= "03_Output/rf2_3_labor.rds")
rf2_3$errors

###########  20 years Training Window  ########

best_mtry <- 52
# SECOND OOS PERIOD (2016-2024)
# 20-year training window = 240 months
# First prediction trains on 1996-01 to 2015-12
Y <- tail(fred, 240 + 108)
Y <- Y[, date := NULL]
Y <- as.matrix(Y)
nprev <- 108

set.seed(123)
rf2_1_20 <- rf.rolling.window_second(Y,nprev,1,1)
saveRDS(rf2_1_20, file= "03_Output/rf2_1_20_labor.rds")
rf2_1_20$errors

rf2_3_20 <- rf.rolling.window_second(Y,nprev,1,3)
saveRDS(rf2_3_20, file= "03_Output/rf2_3_20_labor.rds")
rf2_3_20$errors


###########  30 years Training Window  ########

best_mtry <- 52
# SECOND OOS PERIOD (2016-2024)
# 30-year training window = 360 months
Y <- tail(fred, 360 + 108)
Y <- Y[, date := NULL]
Y <- as.matrix(Y)
nprev <- 108

set.seed(123)
rf2_1_30 <- rf.rolling.window_second(Y,nprev,1,1)
saveRDS(rf2_1_30, file= "03_Output/rf2_1_30_labor.rds")
rf2_1_30$errors

rf2_3_30 <- rf.rolling.window_second(Y,nprev,1,3)
saveRDS(rf2_3_30, file= "03_Output/rf2_3_30_labor.rds")
rf2_3_30$errors


###########  40 years Training Window  ########

best_mtry <- 52
# SECOND OOS PERIOD (2016-2024)
# 40-year training window = 480 months
Y <- tail(fred, 480 + 108)
Y <- Y[, date := NULL]
Y <- as.matrix(Y)
nprev <- 108

set.seed(123)
rf2_1_40 <- rf.rolling.window_second(Y,nprev,1,1)
saveRDS(rf2_1_40, file= "03_Output/rf2_1_40_labor.rds")
rf2_1_40$errors

rf2_3_40 <- rf.rolling.window_second(Y,nprev,1,3)
saveRDS(rf2_3_40, file= "03_Output/rf2_3_40_labor.rds")
rf2_3_40$errors





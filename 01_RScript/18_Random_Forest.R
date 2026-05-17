# Author: Ece Tasan
# Date  : 3/12/2025
# Scope : Apply Random Forest

library(data.table)
library(randomForest)
library(ggplot2)
library(treeshap)

install.packages("treeshap")

rm(list = ls())
options(print.max = 300, scipen = 30, digits = 5)

source("01_RScript/00_Functions_RF.R")

#### Load & prepare ####

fred <- readRDS("02_Input/data_cleaned.rds")
setDT(fred)
dim(fred)

setnames(fred, "CPIAUCSL", "inf")
setcolorder(fred, c("date", "inf"))


#### TUNING ####
Y <- fred[date < "2001-01-01"]
Y <- Y[, date := NULL]
Y <- as.matrix(Y)
dim(Y)

# Validation Data Length = 120 (between years 1991-2000)
nprev <- 120

# mtry grid
p = ncol(Y)*4 # number of features
mtry_grid <- c(2, 3, 5, 8, 10, 15, 25, round(p/10), round(p/8), round(p/6), round(p/4),
               round(p/3), round(p/2))

mtry_grid <- c(2,round(p/20), round(p/10), round(p/5), round(p/3), round(p/2))

results_mtry <- data.frame(
  mtry = mtry_grid,
  rmse = NA_real_,
  mae  = NA_real_
)


#
##
### Note: Parameter selection takes time. The results are calculated once and the
### Note: values from this calculations are used in the following code. If a new
### Note: optimization is wanted, the below code can be run with deleting the "#"s. 
##
#

### Grid search best mtry
#for (k in seq_along(mtry_grid)) {
#  cat("\n==== Testing mtry =", mtry_grid[k], "====\n")
#  
#  set.seed(123)
#  out_k <- rf.rolling.window_tune_mtry(Y, nprev, 1, 1, nfeature = mtry_grid[k])
#  results_mtry$rmse[k] <- out_k$errors["rmse"]
#  results_mtry$mae[k]  <- out_k$errors["mae"]
#}
#
### Save best mtry
#saveRDS(results_mtry, file = "03_Output/rfres_mtry.rds")

# Read saved best mtry
results_mtry <- readRDS("03_Output/rfres_mtry.rds")  # Read saved optimization

## Plot 
best_idx  <- which.min(results_mtry$rmse)
best_mtry <- results_mtry$mtry[best_idx]

plot_mtry <- ggplot(results_mtry, aes(x = mtry, y = rmse)) +
  geom_line() +
  geom_point(size = 2) +
  geom_vline(xintercept = best_mtry, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = results_mtry$mtry) +
  theme_light() +
  theme(
    axis.text.x  = element_text(angle = 90, hjust = 1, size = 14),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

#ggsave(
#  filename = "03_Output/Charts/rf/mtry_tuning.png",
#  plot     = plot_mtry,
#  width    = 12,
#  height   = 6,
#  dpi      = 300
#)


###################### PREDICTIONS ###############################

################## FIRST SAMPLE (2001-2015) ######################

# Tuning Result: Best mtry result
best_mtry <- 52
#best_mtry <- 18

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
saveRDS(rf1_1, file= "03_Output/rf1_1.rds")
rf1_1$errors


rf1_3 <- rf.rolling.window(Y,nprev,1,3)
saveRDS(rf1_3, file= "03_Output/rf1_3.rds")
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
saveRDS(rf1_1_20, file= "03_Output/rf1_1_20.rds")
rf1_1_20$errors

rf1_3_20 <- rf.rolling.window(Y,nprev,1,3)
saveRDS(rf1_3_20, file= "03_Output/rf1_3_20.rds")
rf1_3_20$errors

###########  30 years Training Window  ########

best_mtry <- 52
# FIRST OOS PERIOD (2001-2015)
# 30-year training window = 360 months
# First prediction trains on 1981-01 to 2000-12
Y <- tail(fred[date < "2016-01-01"], 360 + 180)
Y <- Y[, date := NULL]
Y <- as.matrix(Y)
nprev <- 180

set.seed(123)
rf1_1_30 <- rf.rolling.window(Y,nprev,1,1)
saveRDS(rf1_1_30, file= "03_Output/rf1_1_30.rds")
rf1_1_30$errors

rf1_3_30 <- rf.rolling.window(Y,nprev,1,3)
saveRDS(rf1_3_30, file= "03_Output/rf1_3_30.rds")
rf1_3_30$errors


##################### SECOND SAMPLE (2016-2024) ###################

# Tuning Result: Best mtry result
best_mtry <- 52
#best_mtry <- 18


###########  56 years Training Window  ########

Y <- fred
Y <- Y[, date := NULL]
Y <- as.matrix(Y)
dim(Y)
nprev <- 108

set.seed(123)
rf2_1 <- rf.rolling.window_second(Y,nprev,1,1)
saveRDS(rf2_1, file= "03_Output/rf2_1.rds")
rf2_1$errors


rf2_3 <- rf.rolling.window_second(Y,nprev,1,3)
saveRDS(rf2_3, file= "03_Output/rf2_3.rds")
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
saveRDS(rf2_1_20, file= "03_Output/rf2_1_20.rds")
rf2_1_20$errors

rf2_3_20 <- rf.rolling.window_second(Y,nprev,1,3)
saveRDS(rf2_3_20, file= "03_Output/rf2_3_20.rds")
rf2_3_20$errors


###########  30 years Training Window  ########

best_mtry <- 52
# SECOND OOS PERIOD (2016-2024)
# 30-year training window = 360 months
# First prediction trains on 1986-01 to 2015-12
Y <- tail(fred, 360 + 108)
Y <- Y[, date := NULL]
Y <- as.matrix(Y)
nprev <- 108

set.seed(123)
rf2_1_30 <- rf.rolling.window_second(Y,nprev,1,1)
saveRDS(rf2_1_30, file= "03_Output/rf2_1_30.rds")
rf2_1_30$errors

rf2_3_30 <- rf.rolling.window_second(Y,nprev,1,3)
saveRDS(rf2_3_30, file= "03_Output/rf2_3_30.rds")
rf2_3_30$errors








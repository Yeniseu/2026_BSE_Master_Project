# Author: Orhun Ozel
# Date  : 26/11/2025
# Scope : Apply Lasso Ridge ElNet
rm(list = ls())
source("01_RScript/00_Functions_Lasso.R")
library(data.table)
library(glmnet)
library(ggplot2)
library(gt)
library(scales)
options(print.max = 300, scipen = 50, digits = 3)

### Load & prepare
fred <- readRDS("02_Input/data_cleaned.rds")
setDT(fred)
setnames(fred, "CPIAUCSL", "inf")
setcolorder(fred, c("date", "inf"))

fred <- fred[!is.na(inf),]  ## Remove rows if inflation is NA
data <- fred[, -c("date")]  
data <- data[, sapply(data, function(x) sum(is.na(x))==0), with = F]  # Drop cols with NA
data <- as.matrix(data)

fred[, which(date=="2000-12-01")]
fred[, which(date=="2015-12-01")]
dt_s1 <- data[1:fred[, which(date=="2015-12-01")], ]
dt_s2 <- copy(data)

#### Run for different lags and samples
### Parameter Selection Using Sample 1
npred1 <- nrow(dt_s1) - fred[, which(date=="2000-12-01")]  # 180
Y_train_val1 <- dt_s1[1:(nrow(dt_s1)-npred1),]

#
##
### Note: Parameter selection takes time. The results are calculated once and the
### Note: values from this calculations are used in the following code. If a new
### Note: optimization is wanted, the below code can be run with deleting the "#"s. 
##
#
### Parameter Selection Using Sample 1 Lag 1
## Select Lambda for Lasso Lag1
#best_lam_lasso_all_1 <- get_best_lambda(Y_train_val1, npred1, 1, lag=1, alpha=1, nlambda=25)  
## Select Lambda for Ridge Lag1
#best_lam_ridge_all_1 <- get_best_lambda(Y_train_val1, npred1, 1, lag=1, alpha=0, nlambda=25)  
## Select Alpha and Lambda for ElNet Lag1
#best_alp_all_1 <- get_best_alpha(Y_train_val1, npred1, 1, lag=1, alpha_grid="el", lambda="auto", nlambda=25)
## Save results
#saveRDS(best_lam_lasso_all_1, "03_Output/Lasso_Optimization/best_lam_lasso_all_1.rds")
#saveRDS(best_lam_ridge_all_1, "03_Output/Lasso_Optimization/best_lam_ridge_all_1.rds")
#saveRDS(best_alp_all_1      , "03_Output/Lasso_Optimization/best_alp_all_1.rds")
## Read results
best_lam_lasso_all_1 <- readRDS("03_Output/Lasso_Optimization/best_lam_lasso_all_1.rds")
best_lam_ridge_all_1 <- readRDS("03_Output/Lasso_Optimization/best_lam_ridge_all_1.rds")
best_alp_all_1       <- readRDS("03_Output/Lasso_Optimization/best_alp_all_1.rds")

## Define best parameters
blam_l1 <- best_lam_lasso_all_1$best_lam  # 0.016912
blam_r1 <- best_lam_ridge_all_1$best_lam  # 1.57
balp_e1 <- best_alp_all_1$best_alp  # 0.4
blam_e1 <- best_alp_all_1$best_lam  # 0.049153


### Parameter Selection Using Sample 1 Lag 3
## Select Lambda for Lasso Lag3
#best_lam_lasso_all_1_l3 <- get_best_lambda(Y_train_val1, npred1, 1, lag=3, alpha=1, nlambda=25)  
## Select Lambda for Lasso Lag3
#best_lam_ridge_all_1_l3 <- get_best_lambda(Y_train_val1, npred1, 1, lag=3, alpha=0, nlambda=25)  
## Select Alpha and Lambda for ElNet Lag3
#best_alp_all_1_l3 <- get_best_alpha(Y_train_val1, npred1, 1, lag=3, alpha_grid="el", lambda="auto")
#blam_l1_l3 <- best_lam_lasso_all_1_l3$best_lam  # 0.0278
#blam_r1_l3 <- best_lam_ridge_all_1_l3$best_lam  # 2.13
#balp1_e1_l3 <- best_alp_all_1_l3$best_alp  # 
#balp1_e1_l3 <- best_alp_all_1_l3$best_lam  # 


## Sample 1: Train: 1960-01-01:2000-12-01.  Test: 2001-01-01:2015-12-01 
lasso_s1_l1 <- lasso_roll_win(dt_s1, npred1, 1, lag=1, alpha=1      , lambda=blam_l1)
lasso_s1_l3 <- lasso_roll_win(dt_s1, npred1, 1, lag=3, alpha=1      , lambda=blam_l1)
ridge_s1_l1 <- lasso_roll_win(dt_s1, npred1, 1, lag=1, alpha=0      , lambda=blam_r1)
ridge_s1_l3 <- lasso_roll_win(dt_s1, npred1, 1, lag=3, alpha=0      , lambda=blam_r1)
elnet_s1_l1 <- lasso_roll_win(dt_s1, npred1, 1, lag=1, alpha=balp_e1, lambda=blam_e1)
elnet_s1_l3 <- lasso_roll_win(dt_s1, npred1, 1, lag=3, alpha=balp_e1, lambda=blam_e1)
rw_s1_l1 <- sqrt(mean((tail(dt_s1[, "inf"],npred1)-tail(shift(dt_s1[, "inf"],1),npred1))^2))
rw_s1_l3 <- sqrt(mean((tail(dt_s1[, "inf"],npred1)-tail(shift(dt_s1[, "inf"],3),npred1))^2))
window1 <- nrow(dt_s1)-npred1
sm_s1_l1 <- sqrt(mean((tail(dt_s1[, "inf"],npred1)-sapply((1:npred1), function(x) mean(dt_s1[, "inf"][x+(1:window1)-1])))^2))
sm_s1_l3 <- sqrt(mean((tail(dt_s1[, "inf"],npred1)-sapply((1:npred1), function(x) mean(dt_s1[, "inf"][x+(3:window1)-3])))^2))



## Sample 2: Train: 1960-01-01:2015-12-01.  Test: 2016-01-01:2024-12-01
npred2 <- nrow(fred) - fred[, which(date=="2015-12-01")]  # 108 as of 2024-12-01
blam_l2 <- blam_l1
blam_r2 <- blam_r1
blam_e2 <- blam_e1
balp2   <- balp_e1
lasso_s2_l1 <- lasso_roll_win(dt_s2, npred2, 1, lag=1, alpha=1    , lambda=blam_l2)
lasso_s2_l3 <- lasso_roll_win(dt_s2, npred2, 1, lag=3, alpha=1    , lambda=blam_l1)
ridge_s2_l1 <- lasso_roll_win(dt_s2, npred2, 1, lag=1, alpha=0    , lambda=blam_r2)
ridge_s2_l3 <- lasso_roll_win(dt_s2, npred2, 1, lag=3, alpha=0    , lambda=blam_r1) 
elnet_s2_l1 <- lasso_roll_win(dt_s2, npred2, 1, lag=1, alpha=balp2, lambda=blam_e2)
elnet_s2_l3 <- lasso_roll_win(dt_s2, npred2, 1, lag=3, alpha=balp2, lambda=blam_e2)
rw_s2_l1 <- sqrt(mean((tail(dt_s2[, "inf"],npred2)-tail(shift(dt_s2[, "inf"],1),npred2))^2))
rw_s2_l3 <- sqrt(mean((tail(dt_s2[, "inf"],npred2)-tail(shift(dt_s2[, "inf"],3),npred2))^2))
window2 <- nrow(dt_s2)-npred2
sm_s2_l1 <- sqrt(mean((tail(dt_s2[, "inf"],npred2)-sapply((1:npred2), function(x) mean(dt_s2[, "inf"][x+(1:window2)-1])))^2))
sm_s2_l3 <- sqrt(mean((tail(dt_s2[, "inf"],npred2)-sapply((1:npred2), function(x) mean(dt_s2[, "inf"][x+(3:window2)-3])))^2))


### Save Prediction Results
rw_s1_l1_pred <- tail(shift(dt_s1[, "inf"],1),npred1)
rw_s1_l3_pred <- tail(shift(dt_s1[, "inf"],3),npred1)
rw_s2_l1_pred <- tail(shift(dt_s2[, "inf"],1),npred2)
rw_s2_l3_pred <- tail(shift(dt_s2[, "inf"],3),npred2)
lasso_pred_s1 <- data.table(real=lasso_s1_l1$real, 
                            lasso_l1=lasso_s1_l1$pred, lasso_l3=lasso_s1_l3$pred,
                            ridge_l1=ridge_s1_l1$pred, ridge_l3=ridge_s1_l3$pred, 
                            elnet_l1=elnet_s1_l1$pred, elnet_l3=elnet_s1_l3$pred,
                            rw_l1   =rw_s1_l1_pred   , rw_l3   =rw_s1_l3_pred)
lasso_pred_s2 <- data.table(real=lasso_s2_l1$real, 
                            lasso_l1=lasso_s2_l1$pred, lasso_l3=lasso_s2_l3$pred,
                            ridge_l1=ridge_s2_l1$pred, ridge_l3=ridge_s2_l3$pred, 
                            elnet_l1=elnet_s2_l1$pred, elnet_l3=elnet_s2_l3$pred,
                            rw_l1   =rw_s2_l1_pred   , rw_l3   =rw_s2_l3_pred)
saveRDS(lasso_pred_s1, "03_Output/lasso_pred_s1.rds")
saveRDS(lasso_pred_s2, "03_Output/lasso_pred_s2.rds")


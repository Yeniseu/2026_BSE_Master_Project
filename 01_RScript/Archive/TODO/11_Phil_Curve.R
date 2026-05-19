# Author: Orhun Ozel
# Date  : 26/11/2025
# Scope : Apply Lasso Ridge ElNet
rm(list = ls())
library(data.table)
library(glmnet)
library(ggplot2)
library(gt)
library(scales)
library(dynlm)
options(print.max = 300, scipen = 50, digits = 5)

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

a1 <- lm("inf ~ UNRATE", data=fred)
summary(a1)
a2 <- dynlm(inf ~ UNRATE + L(UNRATE,1), data=fred)
summary(a2)



fred_ts <- ts(fred, start = c(1960,1),  frequency = 12)
PC_basic <- formula("inf ~ UNRATE + L(UNRATE, 1)")
a2 <- dynlm(PC_basic, data = fred_ts)
summary(a2)


#npred1 <- nrow(dt_s1) - fred[, which(date=="2000-12-01")]  # 180
#npred2 <- nrow(fred_ts) - which(time(fred_ts) == 2015+11/12)  # "2015-12-01"
PC_basic <- formula("inf ~ UNRATE + L(UNRATE, 1)")
res1 <- frcst_rol_win(fred_ts, 2016, formula = PC_basic)

frc_start_date <- 2016
frcst_rol_win <- function(Y, frc_start_date, formula, steps = 12, plot=T) {
  save.coef <- data.table(NULL)
  save.pred <- Y[, 1:2]  # Create same ts object
  cat("\n iteration \n")
  forecast_dates <- time(Y)[time(Y) >= frc_start_date]
  #browser()
  environment(formula) <- environment()
  for(i in 1:length(forecast_dates)){
    start_t <- time(Y)[i]
    end_t   <- forecast_dates[i] - 1/frequency(Y)
    Y_window  <- window(Y, start = start_t, end = end_t)
    Yf_window <- window(Y, start = end_t+1/frequency(Y), end = end_t+ steps*(1/frequency(Y))) 
    res_model <- dynlm(formula, data = Y_window)
    
    preds    <- predict(res_model, newdata = Yf_window)
    preds_ts <- ts(preds,start=time(Yf_window)[[1]], frequency = 12)
    save.pred <- ts.union(save.pred, preds_ts)
    colnames(save.pred)[ncol(save.pred)] <- paste0("f_",i)
    
    new_coefs <- c("start"=start_t, "end"=end_t, res_model$coefficients)
    save.coef <- rbind(save.coef, as.list(new_coefs))
    
    #cat((1+npred-i),"")
  }
  browser()
  real <- Y[,indice]
  if(plot == T) {
    plot(real,type="l")
    lines(c(rep(NA,length(real)-npred),save.pred),col="red")
  }
  rmse   <- sqrt(mean((tail(real,npred)-save.pred)^2))
  mae    <- mean(abs(tail(real,npred)-save.pred))
  errors <- c("rmse"=rmse,"mae"=mae)
  
  return(list("pred"=as.numeric(save.pred),"real"=tail(real,npred),
              "coef"=save.coef, "coef_names"=lasso$coef_names, "errors"=errors))
}






lasso_s1_l1 <- lasso_roll_win(dt_s1, npred1, 1, lag=1, alpha=1      , lambda=blam_l1)
npred2 <- nrow(fred) - fred[, which(date=="2015-12-01")]  # 108 as of 2024-12-01
lasso_s2_l1 <- lasso_roll_win(dt_s2, npred2, 1, lag=1, alpha=1    , lambda=blam_l2)

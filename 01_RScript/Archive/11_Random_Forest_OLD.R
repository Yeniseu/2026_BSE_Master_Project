# Author: Ece Tasan
# Date: 3/12/2025
# Scope: Apply Random Forest

library(data.table)
library(randomForest)

rm(list = ls())
options(print.max = 300, scipen = 30, digits = 5)

#### Load & prepare ####

fred <- readRDS("02_Input/data_cleaned.rds")
setDT(fred)
dim(fred)

setnames(fred, "CPIAUCSL", "inf")
setcolorder(fred, c("date", "inf"))

#### FUNCTIONS ####

# rf function
runrf=function(Y,indice,lag){
  comp=princomp(scale(Y,scale=FALSE))
  Y2=cbind(Y,comp$scores[,1:4])
  aux=embed(Y2,4+lag)
  y=aux[,indice]
  X=aux[,-c(1:(ncol(Y2)*lag))]  
  
  if(lag==1){
    X.out=tail(aux,1)[1:ncol(X)]  
  }else{
    X.out=aux[,-c(1:(ncol(Y2)*(lag-1)))]
    X.out=tail(X.out,1)[1:ncol(X)]
  }
  
  #browser()
  model=randomForest(X,y,importance=TRUE)
  pred=predict(model,X.out)
  
  return(list("model"=model,"pred"=pred))
}

# rolling window setting
rf.rolling.window=function(Y,nprev,indice=1,lag=1){
  
  save.importance=list()
  save.pred=matrix(NA,nprev,1)
  for(i in nprev:1){
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),]
    lasso=runrf(Y.window,indice,lag)
    save.pred[(1+nprev-i),]=lasso$pred
    save.importance[[i]]=importance(lasso$model)
    cat("iteration",(1+nprev-i),"\n")
  }
  
  real=Y[,indice]
  
  #Plots
  
  plot_dir <- "03_Output/Graphs/plots_rf"
  
  if (!dir.exists("03_Output"))          dir.create("03_Output")
  if (!dir.exists("03_Output/Graphs"))   dir.create("03_Output/Graphs")
  if (!dir.exists(plot_dir))             dir.create(plot_dir)
  
  pred_full <- c(rep(NA, length(real) - nprev), save.pred)
  
  plot_file_full <- file.path(plot_dir, sprintf("rf_lag%d_full.png", lag))
  
  png(plot_file_full,
      width  = 1920,
      height = 1080,
      res    = 300,
      type   = "cairo-png")
  
  plot(real, type = "l",
       main = sprintf("Random Forest Forecast – Lag %d (Full Series)", lag),
       xlab = "Time",
       ylab = "Inflation")
  lines(pred_full, col = "red", lwd = 2)
  legend("topleft",
         legend = c("Actual", "RF Forecast"),
         col    = c("black", "red"),
         lty    = 1,
         bty    = "n")
  
  dev.off()
  
  #Plot 2: Shorter period
  start_idx <- max(1, length(real) - 2 * nprev + 1)
  
  real_zoom <- real[start_idx:length(real)]
  pred_zoom <- pred_full[start_idx:length(real)]
  
  plot_file_zoom <- file.path(plot_dir, sprintf("rf_lag%d_last2nprev.png", lag))
  
  png(plot_file_zoom,
      width  = 1920,
      height = 1080,
      res    = 300,
      type   = "cairo-png")
  
  plot(real_zoom, type = "l",
       main = sprintf("Random Forest Forecast – Lag %d (Last %d Periods)", lag, 2 * nprev),
       xlab = "Time",
       ylab = "Inflation")
  lines(pred_zoom, col = "red", lwd = 2)
  legend("topleft",
         legend = c("Actual", "RF Forecast"),
         col    = c("black", "red"),
         lty    = 1,
         bty    = "n")
  
  dev.off()
  
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2))
  mae=mean(abs(tail(real,nprev)-save.pred))
  errors=c("rmse"=rmse,"mae"=mae)
  
  return(list("pred"=save.pred,"errors"=errors,"save.importance"=save.importance))
}


#### PREDICTIONS ####

# First Out of Sample Predictions: 
# Out of Sample Period : 2001-2015

Y <- fred[date < "2016-01-01"]
Y <- Y[, date := NULL]
Y <- as.matrix(Y)
dim(Y)

# Out of Sample Length = 132
nprev <- 
  
  set.seed(123)
rf1_1 <- rf.rolling.window(Y,nprev,1,1)

rf3_1 <- rf.rolling.window(Y,nprev,1,3)

rf <- rf.rolling.window(Y,nprev,1,6)

rf1$errors
rf1$pred

str(rf1)


2015- 2001 +1

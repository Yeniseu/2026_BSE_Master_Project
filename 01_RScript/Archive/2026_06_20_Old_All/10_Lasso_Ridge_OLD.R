library(data.table)
library(glmnet)
library(caret)
rm(list = ls())
options(print.max = 300, scipen = 30, digits = 5)

### Load & prepare
fred <- readRDS("02_Input/data_cleaned.rds")
setDT(fred)
setnames(fred, "CPIAUCSL", "inf")
setcolorder(fred, c("date", "inf"))

take_lag_matrix <- function(data_matrix, lag_no) {
  X <- as.matrix(data_matrix)
  lagged_matrix <- rbind(
    matrix(NA, lag_no, ncol(X)), 
    X[1:(nrow(X)-lag_no),]
  )
  return(lagged_matrix)
}

all_dt <- fred[, -c("date")]
all_dt <- cbind(all_dt, take_lag_matrix(all_dt, 1), take_lag_matrix(all_dt, 2),
                take_lag_matrix(all_dt, 3), take_lag_matrix(all_dt, 4))

all_dt <- na.omit(all_dt, cols = "inf")  ## Remove rows if inflation is NA
Y    <- all_dt$inf
Xdt  <- all_dt[, -c("inf")]  
Xdt  <- Xdt[, sapply(Xdt, function(x) sum(is.na(x))==0), with = F]  # Drop cols with NA
X    <- as.matrix(Xdt)

### Rolling glmnet with forward TS-CV for lambda


roll_wind_fcast <- function(y, X, window_size, step=1, alpha_lasso, lambda_lasso, anchored=F, sample_skip=0, skip=2) {
  # Create each rolling window, then forecast
  y <- y[(sample_skip+1):length(y)]
  X <- X[(sample_skip+1):nrow(X), , drop=F]
  roll_index <- createTimeSlices(y, window_size, step, fixedWindow = T, skip = skip)
  data_all <- cbind(y, X)
  data_all <- take_lag_matrix(data_all, step)
  train <- lapply(roll_index$train, function(x) data_all[x, ,drop=F])
  test  <- lapply(roll_index$test , function(x) data_all[x, ,drop=F])
  #browser()
  y_true_all <- lapply(test, function(m) m[, "y"][step])
  y_true_all <- as.matrix(list2DF(y_true_all))
  y_hat_all  <- matrix(NA, nrow(y_true_all), ncol(y_true_all))
  for (i in 1:length(train)) {
    all_variables <- train[[i]]
    all_variables <- na.omit(all_variables)
    Y <- all_variables[, "y"]
    X <- all_variables[,  colnames(all_variables) != "y"]
    
    # TS-CV: expanding k-step-ahead within the window
    fit   <- glmnet(X, Y, alpha=alpha_lasso, lambda=lambda_lasso, standardize=T)
    y_hat <- as.numeric(predict(fit, test[[i]][step, colnames(all_variables) != "y"], drop=F))
    y_hat_all[,i] <- y_hat
  }
  bias_mean <- rowMeans(y_true_all - y_hat_all)
  bias_percantage <- abs(bias_mean)/abs(rowMeans(y_true_all))*100
  bias_mean
  bias_percantage
  rmse <- sqrt(rowMeans((y_true_all - y_hat_all)^2))
  rolling_res <- list("rmse" = rmse, "forecast" = y_hat_all, "real" = y_true_all)
  rolling_res
}

get_best_lambda <- function(y, X, window_size, step, alpha_lasso) {
 # browser()
  # Create lambda grid automatically
  lagged_X <- take_lag_matrix(X, 1)
  fit0 <- glmnet(lagged_X[24:window_size, , drop = F], y[24:window_size], 
                 alpha = alpha_lasso, standardize = T, nlambda = 40)
  lambda_grid <- fit0$lambda
  if(alpha_lasso == 1) {  # Remove lambda possibilities that does not change variable count
    lambda_grid <- lambda_grid[fit0$df != shift(fit0$df, fill=-1)]  
  }
  rolling_res <- list(NA)
  rolling_rmse <- NA
  for (i in 1:length(lambda_grid)) {
    #browser()
    rolling_res[[i]] <- roll_wind_fcast(y, X, window_size, step=step, alpha_lasso, 
                                        lambda_grid[i], anchored=F)
    rolling_rmse[i] <- mean(rolling_res[[i]]$rmse[step])
  }
  # best lambda
  best_lam_index <- which.min(rolling_rmse)
  best_lam_and_mse <- c(lambda_grid[best_lam_index], rolling_rmse[best_lam_index])
  return(best_lam_and_mse)
}

rolling_glmnet_ts <- function(y_all, X_all, window_size, train_size, step=1, alpha_lasso) {
  # y_all vector. X_all matrix.
  if(length(y_all) != nrow(X_all)) stop("Y and X lengths does not match")
  y_train <- y_all[1:train_size]
  X_train <- X_all[1:train_size, ,drop=F]
  best_lam_and_rmse <- matrix(NA, length(alpha_lasso), 3) # Store alpha, best lambda and its rmse
  for(i in 1:length(alpha_lasso)) {
    lam_and_rmse <- get_best_lambda(y_train, X_train, window_size, step=1, alpha_lasso[i])
    best_lam_and_rmse[i, ] <- c(alpha_lasso[i], lam_and_rmse)
  }
  best_comb_index <- which.min(best_lam_and_rmse[, 3])
  best_alpha <- best_lam_and_rmse[best_comb_index, 1]
  best_lam   <- best_lam_and_rmse[best_comb_index, 2]

  #fit_full <- glmnet(X, y, alpha=alpha_lasso, lambda=best_lam, standardize=T)
  skip_for_2001_2015 <- train_size-window_size
  skip_for_2016_2025 <- skip_for_2001_2015 + (2016-2001)*12
  
  #browser()
  rmse_sample1 <- NA
  rmse_sample2 <- NA
  for(i in 1:step) {
    roll_fcast_1 <- roll_wind_fcast(y_all, X_all, window_size, step=i, best_alpha,
                                    best_lam, anchored=F, sample_skip = skip_for_2001_2015)
    roll_fcast_2 <- roll_wind_fcast(y_all, X_all, window_size, step=i, best_alpha,
                                    best_lam, anchored=F, sample_skip = skip_for_2016_2025)
    rmse_sample1[i] <- roll_fcast_1$rmse
    rmse_sample2[i] <- roll_fcast_2$rmse
  }
  
  all_res <- list(rmse_sample1, rmse_sample2)
  all_res
}

### Run models
w <- 360  # 30 years of monthly data
t_s <- fred[, which(date=="2000-12-01")]
pred_lasso <- rolling_glmnet_ts(Y, X, window_size=w, train_size=t_s, step=6, alpha_lasso=1)     # Lasso
pred_ridge <- rolling_glmnet_ts(Y, X, window_size=w, train_size=t_s, step=6, alpha_lasso=0)     # Ridge
pred_elnet <- rolling_glmnet_ts(Y, X, window_size=w, train_size=t_s, step=6, alpha_lasso=seq(0, 1, by = 0.1))   # Elastic Net






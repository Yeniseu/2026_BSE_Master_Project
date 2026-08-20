# Author: Adapted for Local Linear Forest forecasting
# Scope : Local Linear Forest functions for rolling OOS inflation forecasts

library(grf)

# ------------------------------------------------------------
# Helper: build lagged design matrix with PCA factors
# ------------------------------------------------------------
# Y_raw: matrix with columns [target first, predictors after]
# lag  : number of lags of all variables/factors used as regressors
# kfac : number of PCA factors extracted from training window only
#
# Returns:
#   X        : training regressors
#   y        : training target
#   X_out    : last available regressor row, used to forecast t+1
# ------------------------------------------------------------
make_llf_design <- function(Y_raw, target_col = 1, lag = 1, kfac = 4) {
  
  stopifnot(is.matrix(Y_raw))
  stopifnot(target_col >= 1, target_col <= ncol(Y_raw))
  stopifnot(lag >= 1)
  
  # PCA on training window only -> no leakage
  pca <- princomp(scale(Y_raw, scale = FALSE))
  fac <- pca$scores[, 1:kfac, drop = FALSE]
  
  Y_aug <- cbind(Y_raw, fac)
  n <- nrow(Y_aug)
  p <- ncol(Y_aug)
  
  if (n <= lag) stop("Training window too short for chosen lag.")
  
  # Build explicit lagged regression design:
  # y_t = f(z_{t-1}, ..., z_{t-lag})
  X_list <- vector("list", lag)
  for (L in 1:lag) {
    X_list[[L]] <- Y_aug[(lag + 1 - L):(n - L), , drop = FALSE]
  }
  
  X <- do.call(cbind, X_list)
  y <- Y_aug[(lag + 1):n, target_col]
  
  # Last observed lag vector -> forecast next period
  X_out_list <- vector("list", lag)
  for (L in 1:lag) {
    X_out_list[[L]] <- matrix(Y_aug[n + 1 - L, ], nrow = 1)
  }
  X_out <- do.call(cbind, X_out_list)
  
  colnames(X) <- unlist(
    lapply(1:lag, function(L) paste0(colnames(Y_aug), "_L", L))
  )
  colnames(X_out) <- colnames(X)
  
  list(
    X = X,
    y = y,
    X_out = X_out,
    pca = pca
  )
}


# ------------------------------------------------------------
# Optional spike dummy for first OOS period
# ------------------------------------------------------------
# This reproduces your "2008-11-01 minimum inflation dummy" logic,
# but keeps it leakage-safe by using only the dummy values present
# in the training window.
# ------------------------------------------------------------
append_dummy_to_design <- function(design, dum_vec, lag = 1) {
  
  stopifnot(length(dum_vec) >= (nrow(design$y) + lag))
  
  # Align dummy with y_t, using dummy_{t-1}, ..., dummy_{t-lag} as regressors
  n_total <- length(dum_vec)
  
  Xdum_list <- vector("list", lag)
  for (L in 1:lag) {
    Xdum_list[[L]] <- dum_vec[(lag + 1 - L):(n_total - L)]
  }
  Xdum <- do.call(cbind, Xdum_list)
  colnames(Xdum) <- paste0("dum_L", 1:lag)
  
  design$X <- cbind(design$X, Xdum)
  design$X_out <- cbind(design$X_out, matrix(rev(tail(dum_vec, lag)), nrow = 1))
  colnames(design$X_out) <- colnames(design$X)
  
  design
}


# ------------------------------------------------------------
# Fit one LLF and forecast next period
# ------------------------------------------------------------
run_llf <- function(Y_raw,
                    target_col = 1,
                    lag = 1,
                    kfac = 4,
                    num.trees = 2000,
                    mtry = NULL,
                    min.node.size = 5,
                    honesty = TRUE,
                    sample.fraction = 0.5,
                    tune_ll_lambda = FALSE,
                    linear.correction.variables = NULL,
                    seed = 123,
                    dum_vec = NULL) {
  
  design <- make_llf_design(
    Y_raw = Y_raw,
    target_col = target_col,
    lag = lag,
    kfac = kfac
  )
  
  if (!is.null(dum_vec)) {
    design <- append_dummy_to_design(design, dum_vec = dum_vec, lag = lag)
  }
  
  X <- design$X
  y <- design$y
  X_out <- design$X_out
  
  if (is.null(mtry)) {
    mtry <- min(ceiling(sqrt(ncol(X)) + 20), ncol(X))
  }
  
  set.seed(seed)
  forest <- ll_regression_forest(
    X = X,
    Y = y,
    num.trees = num.trees,
    mtry = mtry,
    min.node.size = min.node.size,
    honesty = honesty,
    sample.fraction = sample.fraction,
    tune.parameters = "none",
    seed = seed
  )
  
  # Tune only the ridge penalty for local linear prediction if requested
  if (tune_ll_lambda) {
    ll_tune <- tune_ll_regression_forest(
      forest = forest,
      linear.correction.variables = linear.correction.variables
    )
    
    pred <- predict(
      forest,
      newdata = X_out,
      linear.correction.variables = linear.correction.variables,
      ll.lambda = ll_tune$lambda
    )$predictions
    
    out <- list(
      model = forest,
      pred = as.numeric(pred),
      ll_lambda = ll_tune$lambda,
      design = design
    )
  } else {
    pred <- predict(
      forest,
      newdata = X_out,
      linear.correction.variables = linear.correction.variables
    )$predictions
    
    out <- list(
      model = forest,
      pred = as.numeric(pred),
      ll_lambda = NA_real_,
      design = design
    )
  }
  
  return(out)
}


# ------------------------------------------------------------
# Rolling-origin OOS prediction
# ------------------------------------------------------------
# Uses only information available up to each forecast origin.
# No leakage from future rows.
# ------------------------------------------------------------
llf_rolling_window <- function(Y_raw,
                               nprev,
                               target_col = 1,
                               lag = 1,
                               kfac = 4,
                               num.trees = 2000,
                               mtry = NULL,
                               min.node.size = 5,
                               honesty = TRUE,
                               sample.fraction = 0.5,
                               tune_ll_lambda = FALSE,
                               linear.correction.variables = NULL,
                               seed = 123,
                               dummy_full = NULL,
                               plot_results = TRUE) {
  
  stopifnot(is.matrix(Y_raw))
  stopifnot(nprev >= 1, nprev < nrow(Y_raw))
  
  save.pred <- rep(NA_real_, nprev)
  save.lambda <- rep(NA_real_, nprev)
  
  for (i in nprev:1) {
    
    idx_start <- 1 + nprev - i
    idx_end   <- nrow(Y_raw) - i
    
    Y_window <- Y_raw[idx_start:idx_end, , drop = FALSE]
    
    dum_window <- NULL
    if (!is.null(dummy_full)) {
      dum_window <- dummy_full[idx_start:idx_end]
    }
    
    fit_i <- run_llf(
      Y_raw = Y_window,
      target_col = target_col,
      lag = lag,
      kfac = kfac,
      num.trees = num.trees,
      mtry = mtry,
      min.node.size = min.node.size,
      honesty = honesty,
      sample.fraction = sample.fraction,
      tune_ll_lambda = tune_ll_lambda,
      linear.correction.variables = linear.correction.variables,
      seed = seed,
      dum_vec = dum_window
    )
    
    save.pred[idx_start] <- fit_i$pred
    save.lambda[idx_start] <- fit_i$ll_lambda
    
    cat("iteration", idx_start, "\n")
  }
  
  real <- Y_raw[, target_col]
  real_oos <- tail(real, nprev)
  
  rmse <- sqrt(mean((real_oos - save.pred)^2, na.rm = TRUE))
  mae  <- mean(abs(real_oos - save.pred), na.rm = TRUE)
  
  if (plot_results) {
    plot(real, type = "l", main = "LLF rolling OOS forecasts")
    lines(c(rep(NA, length(real) - nprev), save.pred), col = "red")
  }
  
  return(list(
    pred = save.pred,
    real = real,
    errors = c(rmse = rmse, mae = mae),
    lambda = save.lambda
  ))
}


# ------------------------------------------------------------
# Rolling validation for mtry tuning
# ------------------------------------------------------------
llf_rolling_window_tune_mtry <- function(Y_raw,
                                         nprev,
                                         mtry_grid,
                                         target_col = 1,
                                         lag = 1,
                                         kfac = 4,
                                         num.trees = 2000,
                                         min.node.size = 5,
                                         honesty = TRUE,
                                         sample.fraction = 0.5,
                                         seed = 123) {
  
  results <- data.frame(
    mtry = mtry_grid,
    rmse = NA_real_,
    mae  = NA_real_
  )
  
  for (k in seq_along(mtry_grid)) {
    cat("\n==== Testing mtry =", mtry_grid[k], "====\n")
    
    out_k <- llf_rolling_window(
      Y_raw = Y_raw,
      nprev = nprev,
      target_col = target_col,
      lag = lag,
      kfac = kfac,
      num.trees = num.trees,
      mtry = mtry_grid[k],
      min.node.size = min.node.size,
      honesty = honesty,
      sample.fraction = sample.fraction,
      tune_ll_lambda = FALSE,   # keep this FALSE for speed in outer tuning
      seed = seed,
      plot_results = FALSE
    )
    
    results$rmse[k] <- out_k$errors["rmse"]
    results$mae[k]  <- out_k$errors["mae"]
  }
  
  results
}
# ============================================================================
# BENCHMARK MODELS - CLEAN VERSION
# Rolling Mean + AR(4) only
# ============================================================================

# Load data
data <- readRDS("02_Input/data_cleaned.rds")
data_matrix <- as.matrix(data[, -1])
target_col <- which(colnames(data_matrix) == "CPIAUCSL")

# Find date indices
idx_2001 <- which(data$date >= as.Date("2001-01-01"))[1]
idx_2016 <- which(data$date >= as.Date("2016-01-01"))[1]
idx_end <- nrow(data_matrix)

# Calculate test periods
nprev_period1 <- idx_2016 - idx_2001
nprev_period2 <- idx_end - idx_2016 + 1

cat("=== BENCHMARK SETUP ===\n")
cat("Period 1: Train 1960-2000, Test 2001-2015 (", nprev_period1, "obs)\n")
cat("Period 2: Train 1960-2015, Test 2016-2024 (", nprev_period2, "obs)\n")
cat("Horizons: h=1, h=3\n")
cat("Benchmarks: Rolling Mean, AR(4)\n\n")

# ============================================================================
# FUNCTIONS
# ============================================================================

runAR <- function(Y, indice, lag, h){
  Y2 <- cbind(Y[, indice])
  aux <- embed(Y2, h + lag)
  y <- aux[, 1]
  
  if(lag == 1){
    X <- matrix(aux[, h + 1], ncol=1)
    X.out <- tail(X, 1)
  } else {
    X <- aux[, (h+1):(h+lag)]
    X.out <- tail(X, 1)
  }
  
  model <- lm(y ~ X)
  coef <- coef(model)
  pred <- c(1, X.out) %*% coef
  
  return(list("pred" = as.numeric(pred)))
}

ar.rolling.window <- function(Y, nprev, indice, lag, h){
  save.pred <- matrix(NA, nprev, 1)
  
  for(i in nprev:1){
    Y.window <- Y[(1 + nprev - i):(nrow(Y) - i), ]
    
    if(nrow(Y.window) < h + lag + 10){
      next
    }
    
    fact <- runAR(Y.window, indice, lag, h)
    save.pred[(1 + nprev - i), ] <- fact$pred
  }
  
  real <- Y[, indice]
  actual <- tail(real, nprev)
  valid_idx <- !is.na(save.pred)
  mse <- mean((actual[valid_idx] - save.pred[valid_idx])^2)
  
  return(list("mse" = mse, "pred" = save.pred, "actual" = actual))
}

rolling.mean <- function(Y, nprev, indice){
  real <- Y[, indice]
  n <- length(real)
  save.pred <- matrix(NA, nprev, 1)
  
  for(i in 1:nprev){
    train_end <- n - nprev + i - 1
    if(train_end >= 1){
      train_data <- real[1:train_end]
      save.pred[i, ] <- mean(train_data, na.rm = TRUE)
    }
  }
  
  actual <- tail(real, nprev)
  valid_idx <- !is.na(save.pred)
  mse <- mean((actual[valid_idx] - save.pred[valid_idx])^2, na.rm = TRUE)
  
  return(list("mse" = mse, "pred" = save.pred, "actual" = actual))
}

# ============================================================================
# RUN BENCHMARKS
# ============================================================================

data_period1 <- data_matrix[1:(idx_2016-1), ]

cat("\n=== PERIOD 1 (2001-2015) ===\n\n")

cat("h=1:\n")
p1_h1_mean <- rolling.mean(data_period1, nprev_period1, target_col)
cat("  Rolling Mean: MSE =", sprintf("%.6f", p1_h1_mean$mse), "\n")

p1_h1_ar4 <- ar.rolling.window(data_period1, nprev_period1, target_col, lag=4, h=1)
cat("  AR(4): MSE =", sprintf("%.6f", p1_h1_ar4$mse), "\n")

cat("\nh=3:\n")
p1_h3_mean <- rolling.mean(data_period1, nprev_period1, target_col)
cat("  Rolling Mean: MSE =", sprintf("%.6f", p1_h3_mean$mse), "\n")

p1_h3_ar4 <- ar.rolling.window(data_period1, nprev_period1, target_col, lag=4, h=3)
cat("  AR(4): MSE =", sprintf("%.6f", p1_h3_ar4$mse), "\n")

cat("\n=== PERIOD 2 (2016-2024) ===\n\n")

cat("h=1:\n")
p2_h1_mean <- rolling.mean(data_matrix, nprev_period2, target_col)
cat("  Rolling Mean: MSE =", sprintf("%.6f", p2_h1_mean$mse), "\n")

p2_h1_ar4 <- ar.rolling.window(data_matrix, nprev_period2, target_col, lag=4, h=1)
cat("  AR(4): MSE =", sprintf("%.6f", p2_h1_ar4$mse), "\n")

cat("\nh=3:\n")
p2_h3_mean <- rolling.mean(data_matrix, nprev_period2, target_col)
cat("  Rolling Mean: MSE =", sprintf("%.6f", p2_h3_mean$mse), "\n")

p2_h3_ar4 <- ar.rolling.window(data_matrix, nprev_period2, target_col, lag=4, h=3)
cat("  AR(4): MSE =", sprintf("%.6f", p2_h3_ar4$mse), "\n")

# ============================================================================
# RESULTS TABLE
# ============================================================================

benchmark_results <- data.frame(
  Model = rep(c("Rolling Mean", "AR(4)"), 4),
  Period = rep(c(rep("2001-2015", 2), rep("2016-2024", 2)), 2),
  Horizon = c(rep("h=1", 4), rep("h=3", 4)),
  MSE = c(
    p1_h1_mean$mse, p1_h1_ar4$mse,
    p2_h1_mean$mse, p2_h1_ar4$mse,
    p1_h3_mean$mse, p1_h3_ar4$mse,
    p2_h3_mean$mse, p2_h3_ar4$mse
  )
)

cat("\n\n=== BENCHMARK RESULTS ===\n\n")
print(benchmark_results, row.names = FALSE, digits = 6)

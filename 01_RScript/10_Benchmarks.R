# ============================================================================
# 10_Benchmarks.R - Random Walk, Recursive Sample Mean, AR(4)
# Single forecasting sample. Fixes two alignment bugs in the old version:
#   (1) AR: X.out was taken one period too far back, so the AR benchmark was
#       effectively an (h+1)-step forecast and was handicapped at every horizon.
#   (2) Rolling mean: the training set ran to t-1 regardless of h, i.e. a
#       2-month look-ahead at h=3. It now stops at t-h, so h=1 and h=3 differ.
# ============================================================================
source("01_RScript/00_Config.R")

data <- readRDS(file.path(P_IN, "data_cleaned.rds")); setDT(data)
dates <- data$date
Y     <- as.matrix(data[, -"date"])
tcol  <- which(colnames(Y) == "CPIAUCSL")
stopifnot(length(tcol) == 1)

nprev <- sum(dates >= OOS_START)
cat(sprintf("Benchmarks: %d OOS months (%s .. %s), window = %d\n",
            nprev, format(min(dates[dates >= OOS_START])), format(max(dates)), TRAIN_WINDOW))

# ---- AR(p): inf_t = a + sum_j b_j inf_{t-h-j+1}, forecasting inf_{n+1} -------
ar_pred <- function(y, p, h) {
  aux <- embed(y, p + h)                 # row t = [y_t, y_{t-1}, ..., y_{t-(p+h-1)}]
  yy  <- aux[, 1]
  X   <- aux[, (h + 1):(h + p), drop = FALSE]        # y_{t-h} ... y_{t-h-p+1}
  b   <- coef(lm(yy ~ X))
  # to forecast y_{n+1} we need the regressor row for t = n+1:
  # y_{n+1-h}, ..., y_{n+1-h-p+1}  =  columns h:(h+p-1) of the LAST aux row
  x.out <- aux[nrow(aux), h:(h + p - 1)]
  as.numeric(c(1, x.out) %*% b)
}

res <- list()
for (h in HORIZONS) {
  rw <- rsm <- ar4 <- rep(NA_real_, nprev)
  for (i in nprev:1) {
    idx_end <- nrow(Y) - i               # last observed month
    j <- 1 + nprev - i
    y_hist <- Y[1:idx_end, tcol]

    # Random walk: last value observable h months before the target
    rw[j]  <- y_hist[length(y_hist) - h + 1]
    # Recursive sample mean, using only information dated <= target - h
    rsm[j] <- mean(y_hist[1:(length(y_hist) - h + 1)])
    # AR(4) on the fixed rolling window
    ar4[j] <- ar_pred(tail(y_hist, TRAIN_WINDOW), p = 4, h = h)
  }
  real <- tail(Y[, tcol], nprev)
  res[[paste0("h", h)]] <- data.table(
    date = tail(dates, nprev), real = real, RW = rw, RSM = rsm, AR = ar4)
  cat(sprintf("  h=%d  RMSE: RW %.4f | RSM %.4f | AR %.4f\n", h,
              sqrt(mean((real - rw)^2)), sqrt(mean((real - rsm)^2)),
              sqrt(mean((real - ar4)^2))))
}

saveRDS(res, file.path(P_PRED, paste0("benchmarks", WTAG, ".rds")))

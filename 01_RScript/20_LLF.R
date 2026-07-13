# ============================================================================
# 20_LLF.R - Local Linear Forest (grf) on both information sets
#   full panel    -> LLF    (Non-Linear and Variable Selection)
#   labour subset -> LLF_P  (Non-Linear Phillips Curve)
#
# ### IMPORTANT BUG FIX ###
# The old 00_Functions_LLF.R built its own design in which the target was
# ALWAYS the next month and the regressors were lags 1..lag. So calling it with
# lag = 3 produced a ONE-step-ahead forecast using three lags, not a
# three-step-ahead forecast. Because LLF sat in both non-linear families and
# took most of the ensemble weight, this flattered the h=3 results. LLF now uses
# the same make_design() as every other model, so h really means h.
# ============================================================================
source("01_RScript/00_Config.R")
source("01_RScript/00_Functions_Design.R")
library(grf)
set.seed(123)

data <- readRDS(file.path(P_IN, "data_cleaned.rds")); setDT(data)
data <- data[!is.na(CPIAUCSL)]
setnames(data, "CPIAUCSL", "inf"); setcolorder(data, c("date", "inf"))

dates <- data$date
nprev <- sum(dates >= OOS_START)
dum   <- make_dummies(dates)

run_llf <- function(vars, nm) {
  Y <- as.matrix(data[, .SD, .SDcols = c("inf", vars)])
  Y <- Y[, colSums(is.na(Y)) == 0, drop = FALSE]
  out <- list()
  for (h in HORIZONS) {
    fit <- function(des) {
      m <- ll_regression_forest(
        X = des$X, Y = des$y,
        num.trees = 500, mtry = min(HP$llf_mtry, ncol(des$X)),
        min.node.size = 5, honesty = TRUE, sample.fraction = 0.5,
        enable.ll.split = TRUE, seed = 123)
      as.numeric(predict(m, matrix(des$X.out, nrow = 1))$predictions)
    }
    r <- rolling_forecast(Y, nprev, TRAIN_WINDOW, h, fit, indice = 1,
                          nlag = N_LAG, kfac = K_FAC, dum = dum, label = nm)
    out[[paste0("h", h)]] <- setNames(list(r$pred), nm)
    cat(sprintf("%-6s h=%d  RMSE %.4f\n", nm, h, r$errors["rmse"]))
  }
  out
}

full  <- run_llf(setdiff(names(data), c("date", "inf")), "LLF")
labor <- run_llf(intersect(LABOR_VARS, names(data)), "LLF_P")

preds <- list()
for (h in HORIZONS) {
  hh <- paste0("h", h)
  preds[[hh]] <- data.table(date = tail(dates, nprev),
                            as.data.table(c(full[[hh]], labor[[hh]])))
}
saveRDS(preds, file.path(P_PRED, paste0("llf", WTAG, ".rds")))

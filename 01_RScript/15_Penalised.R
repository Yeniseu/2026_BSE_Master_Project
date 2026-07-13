# ============================================================================
# 15_Penalised.R - LASSO / Ridge / Elastic Net
# Runs on BOTH information sets in one pass:
#   full panel     -> "Linear with Variable Selection"  (La,   Ri,   EN)
#   labour subset  -> "Linear Phillips Curve"           (La_P, Ri_P, EN_P)
#
# Fixes vs the old 15_Lasso_Ridge_Elnet_RW.R:
#   * the old runlasso() silently took the LAST column of the panel (VIXCLSx)
#     and treated it as a crisis dummy: it was dropped from the PCA and the lag
#     structure, entered contemporaneously, and was forced to 0 at prediction
#     time. That was a leftover from the Medeiros replication code. Dummies are
#     now explicit and controlled by USE_DUMMIES in 00_Config.R.
#   * single forecasting sample (no 2015 split).
# Hyper-parameter tuning is NOT re-run here; values come from HP in 00_Config.R.
# ============================================================================
source("01_RScript/00_Config.R")
source("01_RScript/00_Functions_Design.R")
library(glmnet)

data <- readRDS(file.path(P_IN, "data_cleaned.rds")); setDT(data)
data <- data[!is.na(CPIAUCSL)]
setnames(data, "CPIAUCSL", "inf")
setcolorder(data, c("date", "inf"))

dates <- data$date
nprev <- sum(dates >= OOS_START)
dum   <- make_dummies(dates)              # NULL unless USE_DUMMIES

SPECS <- list(
  La = list(alpha = 1,               lambda = HP$lasso_lambda),
  Ri = list(alpha = 0,               lambda = HP$ridge_lambda),
  EN = list(alpha = HP$elnet_alpha,  lambda = HP$elnet_lambda)
)

run_set <- function(vars, suffix) {
  Y <- as.matrix(data[, .SD, .SDcols = c("inf", vars)])
  Y <- Y[, colSums(is.na(Y)) == 0, drop = FALSE]
  out <- list()
  for (h in HORIZONS) {
    for (nm in names(SPECS)) {
      s <- SPECS[[nm]]
      fit <- function(des) {
        m <- glmnet(des$X, des$y, alpha = s$alpha, lambda = s$lambda, standardize = TRUE)
        as.numeric(predict(m, matrix(des$X.out, nrow = 1)))
      }
      r <- rolling_forecast(Y, nprev, TRAIN_WINDOW, h, fit, indice = 1,
                            nlag = N_LAG, kfac = K_FAC, dum = dum,
                            label = paste0(nm, suffix))
      out[[paste0("h", h)]][[paste0(nm, suffix)]] <- r$pred
      cat(sprintf("%-6s h=%d  RMSE %.4f\n", paste0(nm, suffix), h, r$errors["rmse"]))
    }
  }
  out
}

full  <- run_set(setdiff(names(data), c("date", "inf")), "")
labor <- run_set(intersect(LABOR_VARS, names(data)), "_P")

preds <- list()
for (h in HORIZONS) {
  hh <- paste0("h", h)
  preds[[hh]] <- data.table(date = tail(dates, nprev),
                            as.data.table(c(full[[hh]], labor[[hh]])))
}
saveRDS(preds, file.path(P_PRED, paste0("penalised", WTAG, ".rds")))

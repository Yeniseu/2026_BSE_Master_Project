# ============================================================================
# 18_RandomForest.R - Random Forest on both information sets
#   full panel    -> RF     (Non-Linear and Variable Selection)
#   labour subset -> RF_P   (Non-Linear Phillips Curve)
#
# Also stores the per-window permutation importance of the full-panel h=3 run,
# which 63_VarImp_8Groups.R turns into the variable-importance figure.
# mtry comes from HP in 00_Config.R; the grid search is NOT re-run.
# ============================================================================
source("01_RScript/00_Config.R")
source("01_RScript/00_Functions_Design.R")
library(randomForest)
set.seed(123)

data <- readRDS(file.path(P_IN, "data_cleaned.rds")); setDT(data)
data <- data[!is.na(CPIAUCSL)]
setnames(data, "CPIAUCSL", "inf"); setcolorder(data, c("date", "inf"))

dates <- data$date
nprev <- sum(dates >= OOS_START)
dum   <- make_dummies(dates)

imp_store <- NULL   # filled by the full-panel h=3 run

run_rf <- function(vars, nm) {
  Y <- as.matrix(data[, .SD, .SDcols = c("inf", vars)])
  Y <- Y[, colSums(is.na(Y)) == 0, drop = FALSE]
  out <- list()

  for (h in HORIZONS) {
    # importance is only needed for the figure, i.e. the full panel at h = 3
    keep_imp <- SAVE_IMPORTANCE && nm == "RF" && h == 3
    imp <- list()

    fit <- function(des) {
      m <- randomForest(des$X, des$y,
                        mtry       = min(HP$rf_mtry, ncol(des$X)),
                        importance = keep_imp)
      if (keep_imp) imp[[length(imp) + 1L]] <<- importance(m)[, 1]   # %IncMSE
      as.numeric(predict(m, matrix(des$X.out, nrow = 1,
                                   dimnames = list(NULL, names(des$X.out)))))
    }

    r <- rolling_forecast(Y, nprev, TRAIN_WINDOW, h, fit, indice = 1,
                          nlag = N_LAG, kfac = K_FAC, dum = dum, label = nm)
    out[[paste0("h", h)]] <- setNames(list(r$pred), nm)
    if (keep_imp) imp_store <<- imp
    cat(sprintf("%-6s h=%d  RMSE %.4f\n", nm, h, r$errors["rmse"]))
  }
  out
}

full  <- run_rf(setdiff(names(data), c("date", "inf")), "RF")
labor <- run_rf(intersect(LABOR_VARS, names(data)), "RF_P")

preds <- list()
for (h in HORIZONS) {
  hh <- paste0("h", h)
  preds[[hh]] <- data.table(date = tail(dates, nprev),
                            as.data.table(c(full[[hh]], labor[[hh]])))
}
saveRDS(preds, file.path(P_PRED, paste0("rf", WTAG, ".rds")))

if (!is.null(imp_store)) {
  saveRDS(list(dates = tail(dates, nprev), importance = imp_store),
          file.path(P_PRED, paste0("rf_importance_h3", WTAG, ".rds")))
  cat("saved per-window RF importance (full panel, h=3)\n")
}

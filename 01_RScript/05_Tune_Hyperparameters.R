# ============================================================================
# 05_Tune_Hyperparameters.R - grid search for the model hyper-parameters
#
# OFF by default. Switch on in 00_2_Config.R:
#     TUNE <- c("lasso", "ridge", "elnet")        # penalised only (minutes)
#     TUNE <- c("lasso", "ridge", "elnet", "rf")  # + random forest (slow)
#     TUNE <- "all"                               # + LLF (very slow)
# then run this script.
#
# DESIGN (no leakage): every candidate is scored by rolling-window RMSE over
# the last TUNE_NPREV months BEFORE OOS_START, using a rolling window of
# TUNE_WINDOW months. The evaluation period 2001+ is never touched, and the
# design matrices come from the same make_design() the forecasts use, so a
# tuned value means the same thing here and in 10/15/18/20.
#
# One search per case: set (full panel / labour subset) x horizon, matching the
# structure of HP_CASES. The script prints a ready-to-paste HP_CASES block and
# saves 03_Output/Tuning/hp_tuned.rds. It never edits the config itself.
# ============================================================================
source("01_RScript/00_2_Config.R")
source("01_RScript/00_Functions_Design.R")
library(glmnet)

MODELS_TO_TUNE <- if (identical(TUNE, "all")) c("lasso","ridge","elnet","rf","llf") else TUNE
if (isFALSE(TUNE) || length(MODELS_TO_TUNE) == 0)
  stop("TUNE is FALSE in 00_2_Config.R - nothing to search.")
if ("rf"  %in% MODELS_TO_TUNE) library(randomForest)
if ("llf" %in% MODELS_TO_TUNE) library(grf)

# forest size while tuning (usually smaller than the production run)
TOPT <- switch(TUNE_OPT,
  fast     = list(rf_ntree =  150, llf_trees =  150, llf_ci_group = 1),
  standard = list(rf_ntree =  500, llf_trees =  500, llf_ci_group = 2),
  thorough = list(rf_ntree = 1500, llf_trees = 1500, llf_ci_group = 2))

## ---- validation sample: strictly before the evaluation period ---------------
data <- readRDS(file.path(P_IN, DATA_FILE)); setDT(data)
data <- data[!is.na(CPIAUCSL)]
setnames(data, "CPIAUCSL", "inf"); setcolorder(data, c("date", "inf"))
data <- data[date < OOS_START]                       # <- the no-leakage cut
dum  <- make_dummies(data$date)

if (TUNE_WINDOW + TUNE_NPREV > nrow(data))
  stop(sprintf("Validation design does not fit: TUNE_WINDOW (%d) + TUNE_NPREV (%d) > %d pre-sample months.",
               TUNE_WINDOW, TUNE_NPREV, nrow(data)))

cat(sprintf("Tuning on %d validation months ending %s, rolling window %d, opt '%s'.\n",
            TUNE_NPREV, format(max(data$date)), TUNE_WINDOW, TUNE_OPT))
cat(sprintf("Models: %s\n\n", paste(MODELS_TO_TUNE, collapse = ", ")))

# rolling-window validation RMSE for one candidate fit function
val_rmse <- function(Y, h, fit, label) {
  r <- rolling_forecast(Y, TUNE_NPREV, TUNE_WINDOW, h, fit, indice = 1,
                        nlag = N_LAG, kfac = K_FAC, dum = dum, label = label)
  as.numeric(r$errors["rmse"])
}

# glmnet's own lambda path on the last validation design (used when the config
# leaves a lambda grid NULL). For ridge the path sits too high, so it is shifted
# down by a factor of five, as in the original tuning code.
auto_lambda <- function(Y, h, alpha) {
  des <- make_design(Y[(nrow(Y) - TUNE_WINDOW + 1):nrow(Y), , drop = FALSE],
                     indice = 1, h = h, nlag = N_LAG, kfac = K_FAC,
                     dum = if (is.null(dum)) NULL else dum[(nrow(Y) - TUNE_WINDOW + 1):nrow(Y), , drop = FALSE])
  g <- glmnet(des$X, des$y, alpha = alpha, standardize = TRUE,
              nlambda = TUNE_GRID$lambda_n)$lambda
  if (alpha == 0) g <- g / 5
  sort(unique(g))
}

## ---- search ---------------------------------------------------------------------
SETS <- list(full  = setdiff(names(data), c("date", "inf")),
             labor = intersect(LABOR_VARS, names(data)))
tuned <- list(); trace <- list()

for (set in names(SETS)) {
  Y <- as.matrix(data[, .SD, .SDcols = c("inf", SETS[[set]])])
  Y <- Y[, colSums(is.na(Y)) == 0, drop = FALSE]

  for (h in HORIZONS) {
    key <- paste0(set, "_h", h); best <- list()
    cat(sprintf("---------- case %s (p = %d) ----------\n", key, ncol(Y)))

    # -- penalised: lasso (alpha = 1) and ridge (alpha = 0) --
    for (m in intersect(c("lasso", "ridge"), MODELS_TO_TUNE)) {
      alpha <- if (m == "lasso") 1 else 0
      grid  <- TUNE_GRID[[paste0(m, "_lambda")]]
      if (is.null(grid)) grid <- auto_lambda(Y, h, alpha)
      rmse <- sapply(grid, function(lam)
        val_rmse(Y, h, function(des) {
          fit <- glmnet(des$X, des$y, alpha = alpha, lambda = lam, standardize = TRUE)
          as.numeric(predict(fit, matrix(des$X.out, nrow = 1)))
        }, label = m))
      best[[paste0(m, "_lambda")]] <- grid[which.min(rmse)]
      trace[[paste0(key, "_", m)]] <- data.table(lambda = grid, rmse = rmse)
      cat(sprintf("  %-6s lambda = %.6g   (RMSE %.4f)\n", m, grid[which.min(rmse)], min(rmse)))
    }

    # -- elastic net: alpha x lambda --
    if ("elnet" %in% MODELS_TO_TUNE) {
      res <- rbindlist(lapply(TUNE_GRID$elnet_alpha, function(a) {
        grid <- TUNE_GRID$elnet_lambda
        if (is.null(grid)) grid <- auto_lambda(Y, h, a)
        data.table(alpha = a, lambda = grid,
                   rmse = sapply(grid, function(lam)
                     val_rmse(Y, h, function(des) {
                       fit <- glmnet(des$X, des$y, alpha = a, lambda = lam, standardize = TRUE)
                       as.numeric(predict(fit, matrix(des$X.out, nrow = 1)))
                     }, label = "elnet")))
      }))
      b <- res[which.min(rmse)]
      best$elnet_alpha <- b$alpha; best$elnet_lambda <- b$lambda
      trace[[paste0(key, "_elnet")]] <- res
      cat(sprintf("  elnet  alpha = %.2f, lambda = %.6g   (RMSE %.4f)\n", b$alpha, b$lambda, b$rmse))
    }

    # -- forests: mtry --
    for (m in intersect(c("rf", "llf"), MODELS_TO_TUNE)) {
      p_des <- (ncol(Y) + K_FAC + (if (is.null(dum)) 0 else ncol(dum))) * N_LAG
      grid  <- unique(pmax(2, round(TUNE_GRID$mtry_frac * p_des)))
      rmse <- sapply(grid, function(mt)
        val_rmse(Y, h, function(des) {
          mt_use <- min(mt, ncol(des$X))
          if (m == "rf") {
            fit <- randomForest(des$X, des$y, mtry = mt_use, ntree = TOPT$rf_ntree)
            as.numeric(predict(fit, matrix(des$X.out, nrow = 1,
                                dimnames = list(NULL, names(des$X.out)))))
          } else {
            fit <- ll_regression_forest(des$X, des$y, num.trees = TOPT$llf_trees,
                     mtry = mt_use, min.node.size = 5, honesty = TRUE,
                     sample.fraction = 0.5, ci.group.size = TOPT$llf_ci_group,
                     enable.ll.split = TRUE, seed = 123)
            as.numeric(predict(fit, matrix(des$X.out, nrow = 1))$predictions)
          }
        }, label = m))
      best[[paste0(m, "_mtry")]] <- grid[which.min(rmse)]
      trace[[paste0(key, "_", m)]] <- data.table(mtry = grid, rmse = rmse)
      cat(sprintf("  %-6s mtry = %d   (RMSE %.4f)\n", m, grid[which.min(rmse)], min(rmse)))
    }

    tuned[[key]] <- best
  }
}

## ---- save + print a paste-ready HP_CASES block --------------------------------------
dir.create(file.path(P_OUT, "Tuning"), showWarnings = FALSE, recursive = TRUE)
saveRDS(list(tuned = tuned, trace = trace, design = list(
  nprev = TUNE_NPREV, window = TUNE_WINDOW, opt = TUNE_OPT,
  validation_end = max(data$date))),
  file.path(P_OUT, "Tuning", "hp_tuned.rds"))

fmt <- function(x) if (is.numeric(x) && x == round(x)) sprintf("%d", x) else sprintf("%.6g", x)
cat("\n\n===== paste into HP_CASES in 00_2_Config.R =====\n\nHP_CASES <- list(\n")
keys <- names(tuned)
for (i in seq_along(keys)) {
  b <- tuned[[keys[i]]]
  body <- if (length(b) == 0) "list()" else
    paste0("list(", paste(sprintf("%s = %s", names(b), sapply(b, fmt)), collapse = ", "), ")")
  cat(sprintf("  %-9s = %s%s\n", keys[i], body, if (i < length(keys)) "," else ""))
}
cat(")\n\nOnly the fields you paste are overridden; the rest fall back to HP_DEFAULT.\n")

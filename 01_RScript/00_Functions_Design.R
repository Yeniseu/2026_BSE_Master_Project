# ============================================================================
# 00_Functions_Design.R
# Scope : ONE design matrix builder + ONE rolling-window driver, shared by
#         every model (penalised linear, Random Forest, Local Linear Forest).
#
# WHY THIS FILE EXISTS
# -------------------
# The models used to build their own design matrices. The penalised models and
# the Random Forest used a direct h-step design; the Local Linear Forest used a
# *one-step-ahead* design regardless of the horizon argument, so "LLF h=3" was
# in fact a 1-step forecast with 3 lags and was not comparable with the other
# h=3 models. Routing every model through the same builder removes that class
# of bug by construction.
#
# THE CONVENTION (direct h-step forecasting)
# ------------------------------------------
#   target      : inf_{n+1}, the first month after the estimation window
#   fitted model: inf_t = f( Z_{t-h}, Z_{t-h-1}, ..., Z_{t-h-nlag+1} )
#   forecast    : evaluate f at ( Z_{n+1-h}, ..., Z_{n+2-h-nlag} )
# so the most recent predictor used is dated n+1-h: exactly h months before the
# target. h = 1 collapses to the usual one-step-ahead design.
# ============================================================================


# ---- Design matrix ---------------------------------------------------------
# Y      : numeric matrix of the estimation window; column `indice` is the target
# h      : forecast horizon in months
# nlag   : number of lags of every predictor
# kfac   : number of principal components appended to the predictor set
# dum    : optional matrix of dummies (same nrow as Y), excluded from the PCA
make_design <- function(Y, indice = 1, h = 1, nlag = 4, kfac = 4, dum = NULL) {

  stopifnot(is.matrix(Y), h >= 1, nlag >= 1)

  # PCA on the estimation window only -> no leakage. Dummies never enter the PCA.
  comp <- princomp(scale(Y, scale = FALSE))
  Z    <- cbind(Y, comp$scores[, seq_len(kfac), drop = FALSE])
  if (!is.null(dum)) Z <- cbind(Z, dum)
  k <- ncol(Z)

  # aux row t = [ Z_t , Z_{t-1} , ... , Z_{t-(nlag+h-1)} ]  (blocks 0 .. nlag+h-1)
  aux <- embed(Z, nlag + h)
  colnames(aux) <- paste0(rep(colnames(Z), times = nlag + h),
                          "_l", rep(0:(nlag + h - 1), each = k))

  y <- aux[, indice]                       # inf_t  (block 0)
  X <- aux[, -(1:(k * h)), drop = FALSE]   # blocks h .. h+nlag-1

  # Forecast row: blocks (h-1) .. (h+nlag-2) of the LAST aux row.
  # For h = 1 this is simply the first nlag blocks.
  j0    <- (h - 1) * k
  X.out <- aux[nrow(aux), (j0 + 1):(j0 + nlag * k)]
  names(X.out) <- colnames(X)

  list(y = y, X = X, X.out = X.out)
}


# ---- Rolling-window driver -------------------------------------------------
# Re-estimates `fit_fun` at every origin on a FIXED window of `window` months
# and returns one forecast per out-of-sample month.
#
# Y       : full matrix (rows ordered in time), column `indice` = target
# nprev   : number of out-of-sample forecasts (the last `nprev` rows of Y)
# window  : training window length in months
# fit_fun : function(design) -> scalar prediction
# dum     : optional dummy matrix aligned with the rows of Y (or NULL)
rolling_forecast <- function(Y, nprev, window, h, fit_fun, indice = 1,
                             nlag = 4, kfac = 4, dum = NULL, label = "") {

  stopifnot(nrow(Y) - nprev >= window)
  save.pred <- rep(NA_real_, nprev)

  for (i in nprev:1) {
    idx_end   <- nrow(Y) - i          # last month in the estimation window
    idx_start <- idx_end - window + 1 # fixed-length rolling window
    j <- 1 + nprev - i                # position of this forecast

    Yw <- Y[idx_start:idx_end, , drop = FALSE]
    Dw <- if (is.null(dum)) NULL else dum[idx_start:idx_end, , drop = FALSE]

    des <- make_design(Yw, indice = indice, h = h, nlag = nlag, kfac = kfac, dum = Dw)
    save.pred[j] <- fit_fun(des)

    if (j %% 25 == 0) cat(sprintf("  [%s h=%d] %d/%d\n", label, h, j, nprev))
  }

  real <- tail(Y[, indice], nprev)
  list(pred   = save.pred,
       real   = real,
       errors = c(rmse = sqrt(mean((real - save.pred)^2)),
                  mae  = mean(abs(real - save.pred))))
}

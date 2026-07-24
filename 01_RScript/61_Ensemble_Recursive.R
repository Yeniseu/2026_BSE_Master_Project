# ============================================================================
# 61_Ensemble_Recursive.R   [paper TODO 2]
# Forecast combinations with RECURSIVE (expanding-window) weights.
#
# FIX: weights used to be estimated once on a pre-2011 holdout and then applied
# to the whole 2001-2024 evaluation window, so every ensemble RMSE dated before
# 2011 -- including the GFC window -- used weights fitted on data that included
# the period being forecast. Weights are now re-estimated at EVERY origin using
# only forecasts dated strictly before it, so all ensemble forecasts are
# genuinely out-of-sample. The price is that the ensemble cannot start until
# MIN_TRAIN months of forecasts exist, so 2002-2004 drops out.
#
#   Best 5        equal weights on the 5 lowest-RMSE models so far
#   Constr. OLS   min ||y - Xw||^2                s.t. w >= 0, sum(w) = 1
#   Constr. Ridge min ||y - Xw||^2 + lambda||w||^2, same constraints
#
# Writes the ensemble weights table, the appendix table of individual models,
# and Figures/RMSE/RMSE_Chart_3_Months_Ensemble.png.
# ============================================================================
source("01_RScript/00_2_Config.R")
library(quadprog)
library(ggplot2)

MIN_TRAIN <- 48   # months of forecast history required before the ensemble starts
LAMBDA    <- 5    # ridge penalty; shrinks the weights toward equality
ENS       <- c("Best 5", "Constr. OLS", "Constr. Ridge")

for (h in HORIZONS) {
  d <- readRDS(file.path(P_PRED, sprintf("preds_h%d%s.rds", h, WTAG)))
  X <- as.matrix(d[, ..MODELS]); y <- d$real; k <- length(MODELS)
  d[, (ENS) := NA_real_]        # pre-allocate ensemble columns by reference
  W <- list()

  for (t in (MIN_TRAIN + 1):nrow(d)) {
    Xt <- X[1:(t - 1), , drop = FALSE]; yt <- y[1:(t - 1)]

    # Best 5: equal weights on the five most accurate models so far
    w5 <- as.numeric(seq_len(k) %in% order(sqrt(colMeans((Xt - yt)^2)))[1:5]) / 5

    # Constrained OLS / Ridge: w >= 0 and sum(w) = 1, solved as a QP
    A <- cbind(rep(1, k), diag(k))          # first column = equality constraint
    b <- c(1, rep(0, k))
    qp <- function(lambda) {
      D <- crossprod(Xt) + (lambda + 1e-8) * diag(k)   # ridge + PD guard
      w <- solve.QP(D, crossprod(Xt, yt), A, b, meq = 1)$solution
      w <- pmax(w, 0); w / sum(w)
    }
    wo <- qp(0); wr <- qp(LAMBDA)

    set(d, i = t, j = "Best 5",        value = sum(X[t, ] * w5))
    set(d, i = t, j = "Constr. OLS",   value = sum(X[t, ] * wo))
    set(d, i = t, j = "Constr. Ridge", value = sum(X[t, ] * wr))
    W[[length(W) + 1L]] <- rbind(w5, wo, wr)
  }

  # weights averaged over the recursive re-estimations
  Wbar <- Reduce(`+`, W) / length(W)
  dimnames(Wbar) <- list(ENS, MODELS)
  wdt <- data.table(Combination = ENS, as.data.table(round(Wbar, 2)))

  # sub-period RMSE, individual models and ensembles
  dd   <- d[!is.na(sub)]
  cols <- c(MODELS, ENS)
  ind  <- dd[, lapply(.SD, function(x) {
    ok <- !is.na(x)
    if (any(ok)) sqrt(mean((x[ok] - real[ok])^2)) else NA_real_
  }), by = sub, .SDcols = cols]
  setorder(ind, sub)

  fam <- ind[, .(sub)]
  for (f in names(FAMILIES)) fam[[f]] <- rowMeans(ind[, FAMILIES[[f]], with = FALSE])
  fam[["Ensemble Models"]] <- rowMeans(ind[, ..ENS])
  fam <- cbind(fam, ind[, ..BENCHMARKS])

  fwrite(wdt, file.path(P_PAPER, sprintf("ensemble_weights_h%d.csv", h)))
  fwrite(ind, file.path(P_PAPER, sprintf("rmse_with_ensemble_h%d.csv", h)))
  cat(sprintf("\n=== h=%d, ensemble starts %s ===\n", h, format(d$date[MIN_TRAIN + 1])))
  print(wdt); print(fam)

  if (h != 3) next

  # ---- LaTeX: ensemble weights table -------------------------------------
  writeLines(c(
    paste0("\\begin{tabular}{l", strrep("c", length(MODELS)), "}"),
    "\\toprule",
    paste(c("Combination", gsub("_", "\\\\_", MODELS)), collapse = " & "), "\\\\",
    "\\midrule",
    apply(wdt, 1, function(r) paste0(paste(r, collapse = " & "), " \\\\")),
    "\\bottomrule", "\\end{tabular}"),
    file.path(T_DIR, "Ensemble_Weights.tex"))

  # ---- LaTeX: appendix table of individual models + ensembles -------------
  tex <- copy(ind)
  num <- setdiff(names(tex), "sub")
  tex[, (num) := lapply(.SD, function(x) ifelse(is.na(x), "--", sprintf("%.3f", x))),
      .SDcols = num]
  writeLines(c(
    paste0("\\begin{tabular}{l", strrep("c", length(num)), "}"),
    "\\toprule",
    paste(c("Sub-period", gsub("_", "\\\\_", num)), collapse = " & "), "\\\\",
    "\\midrule",
    apply(tex, 1, function(r) paste0(paste(r, collapse = " & "), " \\\\")),
    "\\bottomrule", "\\end{tabular}"),
    file.path(T_DIR, "ShockTable_Step3_Ensemble.tex"))

  # ---- figure -------------------------------------------------------------
  pl <- melt(fam[, c("sub", names(FAMILIES), "Ensemble Models"), with = FALSE],
             id.vars = "sub", variable.name = "Model", value.name = "RMSE")
  gg <- ggplot(pl[!is.na(RMSE)], aes(sub, RMSE, group = Model, colour = Model)) +
    annotate("rect", xmin = 2.5, xmax = 3.5, ymin = -Inf, ymax = Inf,
             fill = "darkgrey", alpha = 0.2) +
    annotate("rect", xmin = 6.5, xmax = 7.5, ymin = -Inf, ymax = Inf,
             fill = "darkgrey", alpha = 0.2) +
    geom_line(linewidth = 1) + geom_point(size = 2) +
    scale_colour_manual(values = MODEL_COLORS) +
    labs(x = NULL, y = "RMSE") +
    theme_minimal() +
    theme(axis.text.x    = element_text(angle = 90, hjust = 1),
          legend.position = "top", legend.title = element_blank(),
          plot.title    = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5))

  ggsave(file.path(F_RMSE, "RMSE_Chart_3_Months_Ensemble.png"), gg, width = 7, height = 5)
}

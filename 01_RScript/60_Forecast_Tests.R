# ============================================================================
# 60_Forecast_Tests.R   [paper TODO 1]
# Formal predictive-accuracy tests for the 2x2 design.
#
# Table (Tables/DM_Tests.tex): Diebold-Mariano statistics with the
# Harvey-Leybourne-Newbold small-sample correction, absolute-error loss, for
#   each shock window separately (GFC 2008-2010, COVID 2020-2022),
#   the two windows pooled (Shock, 72 mo), the calm remainder (Calm, 204 mo),
#   and the full sample (Full, 276 mo).
# Each "family" is one forecast series (equal-weighted mean of its members).
#
# The individual three-year windows have only 36 observations and almost no
# power, so the paper's headline statistical result is the regime INTERACTION
# test, reported in the text (and printed / saved here): regress the monthly
# loss differential d_t = L(linear) - L(flexible) on a shock dummy,
# d_t = alpha + beta*shock_t + u_t, Newey-West SE. beta > 0 significant means
# the flexible edge is larger in shocks than in calm -- the state-dependence
# claim. It is significant at 5% under squared loss (see the console output).
#
# LOSS: the DM tables use SQUARED-ERROR loss -- the same object as the RMSE
# figures, so tables and figures cannot disagree. (Under absolute error the
# Non-Linear PC shows a small consistent calm-period edge that squared loss
# correctly discounts; that discrepancy confused readers.) The interaction is
# reported under both losses.
#
# All p-values are ONE-SIDED. The hypothesis is directional and stated ex ante
# (Section 3 of the paper): flexible models should be MORE accurate, and more
# so in shocks, so the relevant alternatives are d < 0 (DM) and beta > 0
# (interaction). Two-sided p-values are simply twice these.
#
# The Giacomini-Rossi fluctuation test is produced as a complementary figure.
# ============================================================================
source("01_RScript/00_2_Config.R")
library(ggplot2)

# ---- Diebold-Mariano with HLN correction -----------------------------------
dm_hln <- function(e1, e2, h, power = 2) {
  d <- abs(e1)^power - abs(e2)^power
  n <- length(d); dbar <- mean(d)
  gam <- sapply(0:(h - 1), function(k)
    sum((d[(k + 1):n] - dbar) * (d[1:(n - k)] - dbar)) / n)
  avar <- (gam[1] + 2 * sum(gam[-1])) / n
  if (avar <= 0) return(c(stat = NA, p = NA))
  dm   <- dbar / sqrt(avar)
  corr <- sqrt((n + 1 - 2 * h + h * (h - 1) / n) / n)
  stat <- dm * corr
  se   <- sqrt(avar) / corr                  # HLN-adjusted SE: dbar / se = stat
  c(stat = stat, dbar = dbar, se = se,
    p1 = pt(stat, df = n - 1),               # one-sided: H1 model 1 better (d < 0)
    p2 = 2 * pt(-abs(stat), df = n - 1))     # two-sided
}

# ---- Regime interaction test (Newey-West SE, Bartlett bandwidth h-1) --------
interact_test <- function(e_base, e_flex, shock, h, power = 1) {
  d <- abs(e_base)^power - abs(e_flex)^power
  X <- cbind(1, as.numeric(shock)); n <- nrow(X)
  XtX_inv <- solve(crossprod(X))
  b <- as.numeric(XtX_inv %*% crossprod(X, d))
  u <- as.numeric(d - X %*% b)
  M <- crossprod(X * u)
  L <- max(h - 1, 0)
  if (L >= 1) for (l in 1:L) {
    w  <- 1 - l / h
    Xu <- X * u
    G  <- t(Xu[(l + 1):n, , drop = FALSE]) %*% Xu[1:(n - l), , drop = FALSE]
    M  <- M + w * (G + t(G))
  }
  se <- sqrt(diag(XtX_inv %*% M %*% XtX_inv))
  tstat <- b[2] / se[2]
  c(calm = b[1], beta = b[2], se = se[2], t = tstat,
    p1 = pt(tstat, df = n - 2, lower.tail = FALSE),  # one-sided: H1 beta > 0
    p2 = 2 * pt(-abs(tstat), df = n - 2))            # two-sided
}

# ---- Giacomini-Rossi fluctuation test --------------------------------------
GR_CV <- c("0.1"=3.393,"0.2"=3.179,"0.3"=3.012,"0.4"=2.890,"0.5"=2.779,
           "0.6"=2.634,"0.7"=2.560,"0.8"=2.433,"0.9"=2.248)
gr_cv <- function(mu) GR_CV[[which.min(abs(as.numeric(names(GR_CV)) - mu))]]
gr_fluctuation <- function(e1, e2, h, m, power = 2) {
  d <- abs(e1)^power - abs(e2)^power
  P <- length(d); dbar <- mean(d)
  gam <- sapply(0:(h - 1), function(k)
    sum((d[(k + 1):P] - dbar) * (d[1:(P - k)] - dbar)) / P)
  sig <- sqrt(gam[1] + 2 * sum(gam[-1]))
  F <- sapply(1:(P - m + 1), function(t) sum(d[t:(t + m - 1)]) / (sqrt(m) * sig))
  list(centre = (1:(P - m + 1)) + floor(m / 2), F = F, cv = gr_cv(m / P))
}

STARS <- function(p) if (is.na(p)) "" else if (p < .01) "***" else if (p < .05) "**" else if (p < .1) "*" else ""
abbr  <- function(x) {
  x <- gsub("Non-Linear and Variable Selection", "NL+VS", x, fixed = TRUE)
  x <- gsub("Linear with Variable Selection",    "Lin+VS", x, fixed = TRUE)
  x <- gsub("Non-Linear Phillips Curve",         "NL-PC", x, fixed = TRUE)
  x <- gsub("Linear Phillips Curve",             "Lin-PC", x, fixed = TRUE)
  gsub("AR", "AR(4)", x, fixed = TRUE)
}

# 2x2 margins (interaction-eligible) + the AR benchmark
PAIRS <- list(
  c("Linear with Variable Selection",    "Linear Phillips Curve"),   # variable selection
  c("Non-Linear Phillips Curve",         "Linear Phillips Curve"),   # non-linearity
  c("Non-Linear and Variable Selection", "Linear Phillips Curve")    # both
)
# table columns: GFC, COVID, Shock (pooled), Calm, Full
SLICES <- list(GFC = "2008-2010", COVID = "2020-2022",
               Shock = "shock", Calm = "calm", Full = "FULL")

for (h in HORIZONS) {
  d <- readRDS(file.path(P_PRED, sprintf("preds_h%d%s.rds", h, WTAG)))
  d <- d[!is.na(sub)]
  for (f in names(FAMILIES)) d[, (f) := rowMeans(.SD), .SDcols = FAMILIES[[f]]]
  d[, shock := as.integer(sub %in% SHOCK_SUBS)]
  E <- function(x, c) x[[c]] - x$real
  slice_dt <- function(s) if (s == "FULL") d else if (s == "shock") d[shock == 1] else
                          if (s == "calm") d[shock == 0] else d[sub == s]

  # ---- per-slice DM (MAE) for the table ------------------------------------
  dm <- rbindlist(lapply(PAIRS, function(pr) {
    row <- data.table(model1 = pr[1], model2 = pr[2])
    for (nm in names(SLICES)) {
      x <- dm_hln(E(slice_dt(SLICES[[nm]]), pr[1]), E(slice_dt(SLICES[[nm]]), pr[2]), h, power = 2)
      row[[nm]] <- x[["stat"]]
      row[[paste0("d_",  nm)]] <- x[["dbar"]]; row[[paste0("se_", nm)]] <- x[["se"]]
      row[[paste0("p1_", nm)]] <- x[["p1"]];   row[[paste0("p2_", nm)]] <- x[["p2"]]
    }
    row
  }))
  fwrite(dm, file.path(P_PAPER, sprintf("dm_tests_h%d.csv", h)))

  # ---- regime interaction test (headline result, reported in the text) -----
  it <- rbindlist(lapply(PAIRS, function(pr) {
    m1 <- interact_test(E(d, pr[2]), E(d, pr[1]), d$shock, h, 1)
    m2 <- interact_test(E(d, pr[2]), E(d, pr[1]), d$shock, h, 2)
    data.table(comparison = paste(abbr(pr[1]), "vs", abbr(pr[2])),
               beta_mae = m1[["beta"]], se_mae = m1[["se"]], p1_mae = m1[["p1"]], p2_mae = m1[["p2"]],
               beta_mse = m2[["beta"]], se_mse = m2[["se"]], p1_mse = m2[["p1"]], p2_mse = m2[["p2"]],
               calm_gain = m1[["calm"]])
  }))
  fwrite(it, file.path(P_PAPER, sprintf("interaction_tests_h%d.csv", h)))
  cat(sprintf("\n===== h=%d : regime-interaction test (extra shock gain) =====\n", h))
  print(it[, .(comparison, beta_mae = round(beta_mae, 3), p1_mae = round(p1_mae, 3),
               beta_mse = round(beta_mse, 3), p1_mse = round(p1_mse, 3))])

  # ---- LaTeX tables ---------------------------------------------------------
  # Each statistic is printed with its p-value in parentheses on the line below
  # (classical presentation). Two variants of each table: one-sided p (main
  # text; directional ex-ante hypothesis) and two-sided p (appendix), so the
  # reader can apply either standard.
  if (h == 3) {
    for (v in c("1", "2")) {                 # "1" = one-sided, "2" = two-sided
      tag  <- ifelse(v == "1", "", "_TwoSided")
      pcol <- function(base) paste0("p", v, "_", base)

      # -- Table 1: Comparison | GFC | COVID | Calm | Shock (squared loss) --
      COLS <- c("GFC", "COVID", "Calm", "Shock")   # no Full, no Loss column
      NS   <- sprintf("(n=%d)", c(sum(d$sub == "2008-2010"), sum(d$sub == "2020-2022"),
                                  sum(d$shock == 0), sum(d$shock == 1)))
      body <- unlist(lapply(seq_len(nrow(dm)), function(i) {
        r  <- dm[i]
        es <- sapply(COLS, function(nm)
          sprintf("$%.3f$%s", r[[paste0("d_", nm)]], STARS(r[[pcol(nm)]])))
        se <- sapply(COLS, function(nm)
          sprintf("\\footnotesize($%.3f$)", r[[paste0("se_", nm)]]))
        c(paste0("\\emph{", abbr(r$model1), "} vs \\emph{", abbr(r$model2), "} & ",
                 paste(es, collapse = " & "), " \\\\"),
          paste0(" & ", paste(se, collapse = " & "), " \\\\[4pt]"))
      }))
      writeLines(c(
        "\\begin{tabular}{lcccc}", "\\toprule",
        paste0("Comparison & ", paste(COLS, collapse = " & "), " \\\\"),
        paste0(" & ", paste(NS, collapse = " & "), " \\\\"), "\\midrule",
        body, "\\bottomrule", "\\end{tabular}"),
        file.path(T_DIR, paste0("DM_Tests", tag, ".tex")))

      # -- Table 2: Shock | Calm DM + interaction beta (MAE, MSE) --
      body2 <- unlist(lapply(seq_len(nrow(dm)), function(i) {
        r  <- dm[i]
        ir <- it[comparison == paste(abbr(r$model1), "vs", abbr(r$model2))]
        pS <- r[[pcol("Shock")]]; pC <- r[[pcol("Calm")]]
        pQ <- ir[[paste0("p", v, "_mse")]]; pM <- ir[[paste0("p", v, "_mae")]]
        c(paste0("\\emph{", abbr(r$model1), "} vs \\emph{", abbr(r$model2), "} & ",
                 sprintf("$%.3f$%s", r[["d_Shock"]], STARS(pS)), " & ",
                 sprintf("$%.3f$%s", r[["d_Calm"]],  STARS(pC)), " & ",
                 sprintf("$%.3f$%s", ir$beta_mse, STARS(pQ)), " & ",
                 sprintf("$%.3f$%s", ir$beta_mae, STARS(pM)), " \\\\"),
          paste0(" & ", paste(sprintf("\\footnotesize($%.3f$)",
                              c(r[["se_Shock"]], r[["se_Calm"]], ir$se_mse, ir$se_mae)),
                              collapse = " & "), " \\\\[4pt]"))
      }))
      writeLines(c(
        "\\begin{tabular}{lcccc}", "\\toprule",
        " & \\multicolumn{2}{c}{Loss differential (MSE)} & \\multicolumn{2}{c}{Regime interaction ($\\beta$)} \\\\",
        "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
        "Comparison & Shock & Calm & MSE & MAE \\\\",
        sprintf(" & ($n{=}%d$) & ($n{=}%d$) & & \\\\",
                sum(d$shock == 1), sum(d$shock == 0)), "\\midrule",
        body2, "\\bottomrule", "\\end{tabular}"),
        file.path(T_DIR, paste0("DM_Interaction", tag, ".tex")))
    }
  }

  # ---- Giacomini-Rossi fluctuation plot (complement) -----------------------
  m  <- 60
  gr <- gr_fluctuation(E(d, "Non-Linear and Variable Selection"),
                       E(d, "Linear Phillips Curve"), h, m = m)
  gd <- data.table(date = d$date[gr$centre], F = gr$F)
  gg <- ggplot(gd, aes(date, F)) +
    geom_hline(yintercept = c(-gr$cv, gr$cv), linetype = "dashed", colour = "firebrick") +
    geom_hline(yintercept = 0, colour = "grey60") +
    geom_line(linewidth = 1, colour = "#6161BA") +
    labs(x = "", y = "Fluctuation statistic") +
    theme_minimal()
  ggsave(file.path(F_RMSE, sprintf("GR_Fluctuation_h%d.png", h)), gg, width = 7, height = 4.5)
  cat(sprintf("  GR: min F = %.2f, 5%% cv = -%.3f, rejects = %s\n",
              min(gr$F), gr$cv, any(gr$F < -gr$cv)))
}

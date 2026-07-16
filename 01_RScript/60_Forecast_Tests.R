# ============================================================================
# 60_Forecast_Tests.R   [paper TODO 1]
# Diebold-Mariano tests with the Harvey-Leybourne-Newbold small-sample
# correction, plus the Giacomini-Rossi (2010) fluctuation test.
#
# A "family forecast" is the equal-weighted average of the family's members, so
# each family is one forecast series. Both squared-error and absolute-error loss
# are reported: inside the shock windows a handful of months dominate the
# squared loss differential (in the COVID window four months account for ~77% of
# it), so MAE loss is the more reliable of the two.
# ============================================================================
source("01_RScript/00_Config.R")
library(ggplot2)

# ---- Diebold-Mariano with HLN correction -----------------------------------
# d_t = L(e1_t) - L(e2_t);  negative statistic => model 1 is more accurate.
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
  c(stat = stat, p = 2 * pt(-abs(stat), df = n - 1))
}

# ---- Giacomini-Rossi fluctuation test --------------------------------------
# Rolling DM statistic; two-sided 5% critical values from GR (2010, Table 1),
# indexed by mu = m/P.
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

for (h in HORIZONS) {
  d <- readRDS(file.path(P_PRED, sprintf("preds_h%d%s.rds", h, WTAG)))
  d <- d[!is.na(sub)]
  for (f in names(FAMILIES)) d[, (f) := rowMeans(.SD), .SDcols = FAMILIES[[f]]]
  d[, regime := ifelse(sub %in% SHOCK_SUBS, "shock", "calm")]
  E <- function(x, c) x[[c]] - x$real

  PAIRS <- list(
    c("Non-Linear and Variable Selection", "Linear Phillips Curve"),
    c("Linear with Variable Selection",    "Linear Phillips Curve"),
    c("Non-Linear Phillips Curve",         "Linear Phillips Curve"),
    c("Non-Linear and Variable Selection", "AR")
  )
  SLICES <- c(as.list(names(SUBPERIODS)), list("shock", "calm", "FULL"))

  res <- rbindlist(lapply(PAIRS, function(pr) rbindlist(lapply(SLICES, function(s) {
    g <- if (s == "FULL") d else if (s %in% c("shock","calm")) d[regime == s] else d[sub == s]
    if (nrow(g) < 10) return(NULL)
    rbindlist(lapply(c(1, 2), function(pw) {
      t <- dm_hln(E(g, pr[1]), E(g, pr[2]), h, power = pw)
      data.table(model1 = pr[1], model2 = pr[2], slice = s, n = nrow(g),
                 loss = ifelse(pw == 1, "MAE", "MSE"),
                 stat = t[["stat"]], p = t[["p"]], sig = STARS(t[["p"]]))
    }))
  }))))
  fwrite(res, file.path(P_PAPER, sprintf("dm_tests_h%d.csv", h)))
  cat(sprintf("\n===== DM-HLN, h=%d =====\n", h)); print(res[loss == "MAE"])

  # ---- LaTeX table (MAE loss, the headline comparisons) --------------------
  if (h == 3) {
    keep <- c("2008-2010","2020-2022","shock","calm","FULL")
    lines <- c("\\begin{tabular}{llccccc}", "\\toprule",
      "Comparison & Loss & GFC & COVID & Shock & Calm & Full \\\\",
      " & & (n=36) & (n=36) & (n=72) & (n=204) & (n=276) \\\\", "\\midrule")
    for (pr in PAIRS) {
      r <- res[loss == "MAE" & model1 == pr[1] & model2 == pr[2]]
      cells <- sapply(keep, function(s) {
        x <- r[slice == s]
        if (nrow(x) == 0) "--" else sprintf("$%.2f$%s", x$stat, x$sig)
      })
      lines <- c(lines, paste0(
        "\\emph{", gsub("Non-Linear and Variable Selection","NL+VS",
                   gsub("Linear with Variable Selection","Lin+VS",
                   gsub("Non-Linear Phillips Curve","NL-PC",
                   gsub("Linear Phillips Curve","Lin-PC", pr[1])))),
        "} vs \\emph{", gsub("Linear Phillips Curve","Lin-PC", pr[2]), "} & MAE & ",
        paste(cells, collapse = " & "), " \\\\"))
    }
    lines <- c(lines, "\\bottomrule", "\\end{tabular}")
    writeLines(lines, file.path(T_DIR, "DM_Tests.tex"))
  }

  # ---- Giacomini-Rossi fluctuation plot ------------------------------------
  m  <- 60
  gr <- gr_fluctuation(E(d, "Non-Linear and Variable Selection"),
                       E(d, "Linear Phillips Curve"), h, m = m)
  gd <- data.table(date = d$date[gr$centre], F = gr$F)
  gg <- ggplot(gd, aes(date, F)) +
    geom_hline(yintercept = c(-gr$cv, gr$cv), linetype = "dashed", colour = "firebrick") +
    geom_hline(yintercept = 0, colour = "grey60") +
    geom_line(linewidth = 1, colour = "#6161BA") +
    labs(title = "Giacomini-Rossi fluctuation test",
         subtitle = sprintf("NL+VS vs Linear Phillips Curve, h=%d, m=%d (5%% band)", h, m),
         x = "", y = "Fluctuation statistic") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = .5, face = "bold"),
          plot.subtitle = element_text(hjust = .5))
  ggsave(file.path(F_RMSE, sprintf("GR_Fluctuation_h%d.png", h)), gg, width = 7, height = 4.5)
  cat(sprintf("  GR: min F = %.2f, 5%% cv = -%.3f, rejects = %s\n",
              min(gr$F), gr$cv, any(gr$F < -gr$cv)))
}

# ============================================================================
# 52_RMSE_Tables_Charts.R - sub-period RMSE figure for the 2x2 design
#
# FIX: the old script computed a sub-period RMSE as the MEAN OF THE YEARLY
# RMSEs. By Jensen's inequality that understates the true RMSE (it reported
# 0.47 for the Linear PC in the GFC window when the pooled value is 0.55).
# RMSE is now pooled over all months in the sub-period: sqrt(mean(e^2)).
#
# Writes Figures/RMSE/RMSE_Chart_{1,3}_Months.png straight into 06_Latex.
# ============================================================================
source("01_RScript/00_Config.R")
library(ggplot2)

for (h in HORIZONS) {
  d <- readRDS(file.path(P_PRED, sprintf("preds_h%d%s.rds", h, WTAG)))
  d <- d[!is.na(sub)]

  # RMSE of each individual model, pooled within sub-period
  ind <- d[, lapply(.SD, function(x) sqrt(mean((x - real)^2))),
           by = sub, .SDcols = MODELS]
  setorder(ind, sub)

  # family RMSE = mean of the RMSEs of that family's members
  fam <- ind[, .(sub)]
  for (f in names(FAMILIES)) fam[[f]] <- rowMeans(ind[, FAMILIES[[f]], with = FALSE])
  fam <- cbind(fam, ind[, ..BENCHMARKS])

  fwrite(ind, file.path(P_PAPER, sprintf("rmse_individual_h%d.csv", h)))
  fwrite(fam, file.path(P_PAPER, sprintf("rmse_family_h%d.csv", h)))

  pl <- melt(fam[, c("sub", names(FAMILIES)), with = FALSE],
             id.vars = "sub", variable.name = "Model", value.name = "RMSE")

  gg <- ggplot(pl, aes(sub, RMSE, group = Model, colour = Model)) +
    annotate("rect", xmin = 2.5, xmax = 3.5, ymin = -Inf, ymax = Inf,
             fill = "darkgrey", alpha = 0.2) +
    annotate("rect", xmin = 6.5, xmax = 7.5, ymin = -Inf, ymax = Inf,
             fill = "darkgrey", alpha = 0.2) +
    annotate("text", x = "2008-2010", y = Inf, label = "GFC", vjust = 1.5, size = 3) +
    annotate("text", x = "2020-2022", y = Inf, label = "COVID", vjust = 1.5, size = 3) +
    geom_line(linewidth = 1) + geom_point(size = 2) +
    scale_colour_manual(values = MODEL_COLORS) +
    labs(title = "Out-of-sample RMSE", subtitle = paste0(h, "-month horizon"),
         x = NULL, y = "RMSE") +
    theme_minimal() +
    theme(axis.text.x    = element_text(angle = 90, hjust = 1),
          legend.position = "top", legend.title = element_blank(),
          plot.title    = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5))

  ggsave(file.path(F_RMSE, sprintf("RMSE_Chart_%d_Months.png", h)),
         gg, width = 7, height = 5)

  cat(sprintf("\n=== h=%d family RMSE (pooled) ===\n", h)); print(fam)
}

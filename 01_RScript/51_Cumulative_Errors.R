# ============================================================================
# 51_Cumulative_Errors.R - Appendix A figure
# Cumulative absolute forecast error, one representative model per family plus
# the two time-series benchmarks. Writes Figures/cum_err_all_step3_models2.png.
# ============================================================================
source("01_RScript/00_2_Config.R")
library(ggplot2)

# one representative model per family, plus the benchmarks
SHOW <- c("RSM" = "Rolling mean",
          "AR"  = "AR(4)",
          "La_P"= "Linear Phillips Curve",
          "La"  = "Linear + Var. Selection",
          "RF_P"= "Non-Linear Phillips Curve",
          "RF"  = "Non-Linear + Var. Selection")

for (h in HORIZONS) {
  d <- readRDS(file.path(P_PRED, sprintf("preds_h%d%s.rds", h, BASE_WTAG)))

  cum <- d[, c(list(date = date),
               lapply(.SD, function(x) cumsum(abs(x - real)))),
           .SDcols = names(SHOW)]

  pl <- melt(cum, id.vars = "date", variable.name = "Model",
             value.name = "cum_abs_err")
  pl[, Model := factor(SHOW[as.character(Model)], levels = SHOW)]

  gg <- ggplot(pl, aes(date, cum_abs_err, colour = Model)) +
    geom_line(linewidth = 0.9) +
    scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
    labs(x = NULL, y = "Cumulative absolute forecast error", colour = NULL) +
    theme_light() +
    theme(legend.position = "top") +
    guides(colour = guide_legend(nrow = 2))

  ggsave(file.path(F_ROOT, sprintf("cum_err_all_step%d_models2.png", h)),
         gg, width = 7, height = 4)
  cat(sprintf("h=%d: cumulative-error figure written\n", h))
}

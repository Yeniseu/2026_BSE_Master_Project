# ============================================================================
# 55_Robustness_Windows.R - Section 5.4 robustness figure
# Averages the sub-period RMSE across every training-window length that has
# been run, at both horizons, and plots the result.
#
# To produce the inputs, set TRAIN_WINDOW in 00_Config.R to each of 240, 360,
# 480 (and the 492 baseline) and re-run 10 / 15 / 18 / 20 / 50 each time. This
# script then picks up whatever preds_h*_w*.rds files exist.
#
# Writes Figures/Robustness/RMSE_Average_Chart.png.
# ============================================================================
source("01_RScript/00_Config.R")
library(ggplot2)

files <- list.files(P_PRED, pattern = "^preds_h[13]_w[0-9]+\\.rds$", full.names = TRUE)
stopifnot(length(files) > 0)
cat("Averaging over", length(files), "specifications:\n")
cat(paste0("  ", basename(files), collapse = "\n"), "\n")

rmse_one <- function(f) {
  d <- readRDS(f)[!is.na(sub)]
  ind <- d[, lapply(.SD, function(x) sqrt(mean((x - real)^2))),
           by = sub, .SDcols = MODELS]
  out <- ind[, .(sub)]
  for (fm in names(FAMILIES)) out[[fm]] <- rowMeans(ind[, FAMILIES[[fm]], with = FALSE])
  setorder(out, sub)
  out
}

all <- lapply(files, rmse_one)
avg <- copy(all[[1]])
for (fm in names(FAMILIES))
  avg[[fm]] <- rowMeans(sapply(all, function(x) x[[fm]]))

fwrite(avg, file.path(P_PAPER, "rmse_robustness_average.csv"))
print(avg)

pl <- melt(avg, id.vars = "sub", variable.name = "Model", value.name = "RMSE")
gg <- ggplot(pl, aes(sub, RMSE, group = Model, colour = Model)) +
  annotate("rect", xmin = 2.5, xmax = 3.5, ymin = -Inf, ymax = Inf,
           fill = "darkgrey", alpha = 0.2) +
  annotate("rect", xmin = 6.5, xmax = 7.5, ymin = -Inf, ymax = Inf,
           fill = "darkgrey", alpha = 0.2) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_colour_manual(values = MODEL_COLORS) +
  labs(title = "Out-of-sample RMSE",
       subtitle = sprintf("Average of %d specifications", length(files)),
       x = NULL, y = "RMSE") +
  theme_minimal() +
  theme(axis.text.x    = element_text(angle = 90, hjust = 1),
        legend.position = "top", legend.title = element_blank(),
        plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

ggsave(file.path(F_ROB, "RMSE_Average_Chart.png"), gg, width = 7, height = 5)

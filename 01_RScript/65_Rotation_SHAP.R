# ============================================================================
# 65_Rotation_SHAP.R - Section 5.3 figure
# Relative |SHAP| contribution of a supply-price group and a labour-market
# group to the Random Forest's h=3 forecast. Each series is smoothed over 12
# months and divided by its own full-sample mean, so 1.0 = average reliance.
#
# Input : 03_Output/Preds/shap_h3_w*.rds (from 64_SHAP_Values.R)
# Output: Figures/Var_Imp/rotation_shap_relative.png
# ============================================================================
source("01_RScript/00_2_Config.R")
library(ggplot2)

obj   <- readRDS(file.path(P_PRED, paste0("shap_h3", WTAG, ".rds")))
shaps <- obj$shap

SUPPLY <- c("OILPRICEx", "WPSID61", "WPSID62", "PPICMM")            # oil + producer prices
LABOUR <- c("HWIURATIO", "PAYEMS", "UNRATE", "CLAIMSx")            # slack + payrolls

# feature names are "<var>_l<k>": keep every lag of the group's variables
keys <- function(vars, nms) nms[sub("_l[0-9]+$", "", nms) %in% vars]

nms <- names(shaps[[1]])
s_keys <- keys(SUPPLY, nms); l_keys <- keys(LABOUR, nms)
stopifnot(length(s_keys) > 0, length(l_keys) > 0)
cat(sprintf("supply keys: %d | labour keys: %d\n", length(s_keys), length(l_keys)))

share <- function(v, k) sum(abs(v[k])) / sum(abs(v))

dt <- data.table(
  date   = obj$dates,
  supply = vapply(shaps, share, numeric(1), k = s_keys),
  labour = vapply(shaps, share, numeric(1), k = l_keys)
)
dt[, supply := frollmean(supply, 12, align = "right")]
dt[, labour := frollmean(labour, 12, align = "right")]
dt[, supply := supply / mean(supply, na.rm = TRUE)]
dt[, labour := labour / mean(labour, na.rm = TRUE)]

pl <- melt(dt, id.vars = "date", variable.name = "Group", value.name = "rel")
pl[, Group := factor(Group, levels = c("supply", "labour"),
                     labels = c("Supply prices (oil, PPI)",
                                "Labour market (v/u, payrolls, unemployment, claims)"))]

gg <- ggplot(pl[!is.na(rel)], aes(date, rel, colour = Group)) +
  annotate("rect", xmin = as.Date("2020-02-01"), xmax = as.Date("2022-06-01"),
           ymin = -Inf, ymax = Inf, fill = "#CCCCDD", alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = c("#6161BA", "#34BA66"), name = NULL) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(x = NULL, y = "Relative |SHAP| contribution") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank()) +
  guides(colour = guide_legend(nrow = 2))

ggsave(file.path(F_VAR, "rotation_shap_relative.png"), gg,
       width = 7, height = 3.5, dpi = 300)
cat("SHAP rotation figure written\n")

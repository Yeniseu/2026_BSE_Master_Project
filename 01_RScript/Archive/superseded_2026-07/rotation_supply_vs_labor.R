# Author: Ece Tasan
# Date  : 5/26/2026
# Scope : Two-panel "do inflation drivers rotate?" chart.
#         For each user-defined group, plot its 12-month rolling RF variable
#         importance share divided by its own historical average, so 1.0 means
#         "at its own average". Shock windows (GFC, COVID) shaded.
#
#         3-month horizon, full-sample RF (rf1_3.rds + rf2_3.rds).
#         Lags summed per variable using the row-block layout verified in
#         18.2_RF_Var_Imp_G2.R (HWIURATIO -> rows 22, 152, 282, 412).

library(data.table)
library(ggplot2)
library(patchwork)
library(scales)

rm(list = ls())
options(scipen = 30, digits = 5)


# ============================================================
# USER INPUT — change these two vectors to control the groups
# ============================================================

SUPPLY_VARS <- c(
  "OILPRICEx",     # crude oil, spliced WTI/Cushing  -> direct supply-cost shock
  "WPSID61",       # PPI: Intermediate Materials      -> upstream cost pressure
  "WPSID62",       # PPI: Crude Materials             -> raw-input cost
  "PPICMM"         # PPI: Metals and metal products   -> commodity-cycle pressure
)

LABOR_VARS <- c(
  "UNRATE",        # Civilian unemployment rate
  "CLAIMSx",       # Initial claims
  "PAYEMS",        # Nonfarm payrolls
  "HWIURATIO"      # Vacancy / unemployment ratio (v/u)
)

SUPPLY_LABEL <- "Supply-side price variables\n(Oil, PPI (Intermediates, crude materials, metals))"
LABOR_LABEL  <- "Labor market variables\n(UNRATE, Claims, Payrolls, HWIURATIO)"

SMOOTH_WINDOW <- 12L


# ============================================================
# Load data + RF results
# ============================================================

fred <- readRDS("02_Input/data_cleaned.rds")
setDT(fred)
setnames(fred, "CPIAUCSL", "inf")
setcolorder(fred, c("date", "inf"))

rf1_3 <- readRDS("03_Output/rf1_3.rds")   # OOS 2001-01..2015-12 (180 windows)
rf2_3 <- readRDS("03_Output/rf2_3.rds")   # OOS 2016-01..2024-12 (108 windows)


# ============================================================
# Feature ordering (must match runrf)
# ============================================================

var_names    <- setdiff(names(fred), "date")
pc_names     <- paste0("Comp.", 1:4)
y2_names     <- c(var_names, pc_names)
n_y2         <- length(y2_names)          # 130
n_lag_blocks <- 4L                        # lags 3..6
stopifnot(n_y2 == 130L)

# Sanity-check user input before doing the heavy work
missing_supply <- setdiff(SUPPLY_VARS, y2_names)
missing_labor  <- setdiff(LABOR_VARS,  y2_names)
if (length(missing_supply) > 0)
  stop("Not in dataset (SUPPLY_VARS): ", paste(missing_supply, collapse = ", "))
if (length(missing_labor) > 0)
  stop("Not in dataset (LABOR_VARS): ",  paste(missing_labor,  collapse = ", "))


# ============================================================
# Core: share of a target variable set within total RF importance
# ============================================================
# imp_mat is (n_y2 * n_lag_blocks) x 2. Col 1 = %IncMSE.
# Reshape with byrow=TRUE so each column corresponds to one variable across
# its four lag blocks, then sum down columns.

share_for <- function(imp_mat, target_vars,
                      y2_names, n_y2 = 130L, n_lag_blocks = 4L) {
  vals <- imp_mat[, 1]
  vals[vals < 0] <- 0
  stopifnot(length(vals) == n_y2 * n_lag_blocks)

  lag_mat <- matrix(vals, nrow = n_lag_blocks, ncol = n_y2, byrow = TRUE)
  colnames(lag_mat) <- y2_names

  per_var <- colSums(lag_mat)
  total   <- sum(per_var)
  if (total <= 0) return(0)
  sum(per_var[target_vars]) / total
}


# ============================================================
# Apply across all rolling windows
# ============================================================

oos_dates_1 <- tail(fred$date[fred$date < as.Date("2016-01-01")], 180)
oos_dates_2 <- tail(fred$date, 108)
stopifnot(length(rf1_3$save.importance) == length(oos_dates_1))
stopifnot(length(rf2_3$save.importance) == length(oos_dates_2))

shares_one <- function(rf_result, oos_dates, target_vars) {
  vapply(
    seq_along(rf_result$save.importance),
    function(i) share_for(rf_result$save.importance[[i]], target_vars, y2_names),
    numeric(1)
  )
}

dt <- data.table(
  date          = c(oos_dates_1, oos_dates_2),
  supply_share  = c(shares_one(rf1_3, oos_dates_1, SUPPLY_VARS),
                    shares_one(rf2_3, oos_dates_2, SUPPLY_VARS)),
  labor_share   = c(shares_one(rf1_3, oos_dates_1, LABOR_VARS),
                    shares_one(rf2_3, oos_dates_2, LABOR_VARS))
)
setorder(dt, date)


# ============================================================
# 12-month rolling mean -> divide by own average -> relative importance
# ============================================================

dt[, supply_smooth := frollmean(supply_share, SMOOTH_WINDOW, align = "right")]
dt[, labor_smooth  := frollmean(labor_share,  SMOOTH_WINDOW, align = "right")]

supply_avg <- mean(dt$supply_smooth, na.rm = TRUE)
labor_avg  <- mean(dt$labor_smooth,  na.rm = TRUE)

dt[, supply_rel := supply_smooth / supply_avg]
dt[, labor_rel  := labor_smooth  / labor_avg]

cat(sprintf("Supply group : avg absolute share = %.3f%%\n", 100 * supply_avg))
cat(sprintf("Labor  group : avg absolute share = %.3f%%\n", 100 * labor_avg))


# ============================================================
# Shock bands
# ============================================================

gfc_start <- as.Date("2007-12-01")
gfc_end   <- as.Date("2009-06-01")
cov_start <- as.Date("2020-02-01")
cov_end   <- as.Date("2022-06-01")


# ============================================================
# Single-panel builder
# ============================================================

build_panel <- function(d, yvar, label, line_color, show_x = TRUE) {
  d <- d[!is.na(get(yvar))]

  ymin <- 0
  ymax <- max(1.5, max(d[[yvar]], na.rm = TRUE) * 1.05)

  p <- ggplot(d, aes(x = date, y = .data[[yvar]])) +
    # shock shading
    annotate("rect", xmin = gfc_start, xmax = gfc_end,
             ymin = ymin, ymax = ymax, fill = "#e74c3c", alpha = 0.12) +
    annotate("rect", xmin = cov_start, xmax = cov_end,
             ymin = ymin, ymax = ymax, fill = "#f39c12", alpha = 0.12) +

    # blue above 1.0, red below 1.0
    geom_ribbon(aes(ymin = 1, ymax = pmax(.data[[yvar]], 1)),
                fill = line_color, alpha = 0.20) +
    geom_ribbon(aes(ymin = pmin(.data[[yvar]], 1), ymax = 1),
                fill = "#e74c3c", alpha = 0.12) +

    geom_hline(yintercept = 1, linetype = "dashed",
               color = "grey40", linewidth = 0.5) +
    geom_line(color = line_color, linewidth = 1.0) +

    annotate("label", x = min(d$date), y = ymax,
             label = label,
             hjust = 0, vjust = 1, size = 3.6,
             label.size = 0.3,
             fill  = alpha("white", 0.85),
             color = line_color, fontface = "plain",
             label.padding = unit(0.35, "lines")) +

    scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                 limits = c(min(dt$date), max(dt$date)),
                 expand = expansion(mult = c(0.01, 0.02))) +
    scale_y_continuous(limits = c(ymin, ymax),
                       breaks = seq(0, ceiling(ymax * 5) / 5, 0.2),
                       labels = label_number(accuracy = 0.1),
                       expand = expansion(mult = c(0, 0))) +
    labs(x = NULL, y = "Relative importance\n(1.0 = own average)") +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey90"),
      plot.margin = margin(t = 4, r = 12, b = 4, l = 6)
    )

  if (!show_x) p <- p + theme(axis.text.x  = element_blank(),
                              axis.ticks.x = element_blank())
  p
}


# ============================================================
# Assemble two-panel chart
# ============================================================

p_top <- build_panel(dt, "supply_rel", SUPPLY_LABEL,
                     line_color = "#c0392b", show_x = FALSE)
p_bot <- build_panel(dt, "labor_rel",  LABOR_LABEL,
                     line_color = "#2980b9", show_x = TRUE)

p <- (p_top / p_bot) +
  plot_annotation(
    title    = "Do inflation drivers rotate during shocks?",
    subtitle = paste0("Relative RF importance: supply-side prices vs labor ",
                      "market | ", SMOOTH_WINDOW, "-month rolling | ",
                      "3-month horizon"),
    theme = theme(
      plot.title    = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, colour = "grey25")
    )
  )

print(p)

outdir <- "03_Output/Charts/rf"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
outfile <- file.path(outdir, "rotation_supply_vs_labor.png")
ggsave(outfile, plot = p, width = 14, height = 7, dpi = 300)
cat(sprintf("\nSaved: %s\n", outfile))

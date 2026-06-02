# Author: Ece Tasan
# Date  : 5/26/2026
# Scope : Two SHAP-based rotation charts (3-month horizon):
#           (1) Relative |SHAP| importance, 12-month rolling, 1.0 = own full-OOS mean.
#           (2) Signed SHAP, 12-month rolling mean of summed signed SHAP
#               -> tells whether the group pushed the forecast UP (>0) or DOWN (<0).
#
#         Inputs:
#           03_Output/shap1_3_41.rds  (first  OOS sample, 41-yr training, lag=3)
#           03_Output/shap2_3_56.rds  (second OOS sample, 56-yr training, lag=3)
#
#         SHAP data shape (per saved window, from 00_Functions_RF.R):
#           save.shap[[i]] : named numeric vector of length 520
#           names()        : paste0(var, "_lag", k), k in 3..6
#           First 130 entries are lag-3 of all y2_names, next 130 are lag-4, etc.

library(data.table)
library(ggplot2)
library(patchwork)
library(scales)

rm(list = ls())
options(scipen = 30, digits = 5)


# ============================================================
# USER INPUT — control variable groups
# ============================================================

#"WPSFD49207",    # PPI: Finished Goods
#"WPSFD49502",    # PPI: Finished Consumer Goods

SUPPLY_VARS <- c(
  "OILPRICEx",     # crude oil (WTI/Cushing spliced)
  "PPICMM",         # PPI: Metals and metal products   -> commodity-cycle pressure
  "WPSID61",       # PPI: Intermediate Materials
  "WPSID62"        # PPI: Crude Materials
)

LABOR_VARS <- c(
  "HWIURATIO",     # v/u ratio
  "PAYEMS",        # Nonfarm payrolls
  "UNRATE",        # Civilian unemployment rate
  "CLAIMSx"        # Initial claims
)

SUPPLY_TITLE <- "Supply-side prices  (oil + producer prices: intermediate, crude materials, metals)"
LABOR_TITLE  <- "Labor market  (HWIURATIO, PAYEMS, UNRATE, CLAIMSx)"

SMOOTH_WINDOW <- 12L
HORIZON_LAG   <- 3L                              # lag used in runrf_shap for h=3


# ============================================================
# Load saved SHAP runs
# ============================================================

shap1 <- readRDS("03_Output/shap1_3_41.rds")
shap2 <- readRDS("03_Output/shap2_3_56.rds")

# Recover the OOS dates from the FRED date column (same convention as before)
fred <- readRDS("02_Input/data_cleaned.rds")
setDT(fred)
oos_dates_1 <- tail(fred$date[fred$date < as.Date("2016-01-01")],
                    length(shap1$save.shap))      # 180
oos_dates_2 <- tail(fred$date, length(shap2$save.shap))   # 108

# Sanity: every saved SHAP vector should have feature names of the form var_lagK
stopifnot(!is.null(names(shap1$save.shap[[1]])))
stopifnot(!is.null(names(shap2$save.shap[[1]])))


# ============================================================
# Build per-variable lag-key vectors for each target group
# ------------------------------------------------------------
# We sum the four lag entries of each variable, then sum across variables in
# the group. For lag = 3 these are "_lag3", "_lag4", "_lag5", "_lag6".
# ============================================================

lag_suffixes <- paste0("_lag", seq(HORIZON_LAG, HORIZON_LAG + 3L))

expand_lags <- function(vars) {
  as.vector(outer(vars, lag_suffixes, paste0))
}

supply_keys <- expand_lags(SUPPLY_VARS)
labor_keys  <- expand_lags(LABOR_VARS)

# Validate against the first SHAP vector we have
present_keys <- names(shap1$save.shap[[1]])
miss_s <- setdiff(supply_keys, present_keys)
miss_l <- setdiff(labor_keys,  present_keys)
if (length(miss_s) > 0)
  stop("Missing SHAP keys for SUPPLY_VARS: ", paste(miss_s, collapse = ", "))
if (length(miss_l) > 0)
  stop("Missing SHAP keys for LABOR_VARS: ",  paste(miss_l, collapse = ", "))

cat(sprintf("Supply group keys: %d (= %d vars x 4 lags)\n",
            length(supply_keys), length(SUPPLY_VARS)))
cat(sprintf("Labor  group keys: %d (= %d vars x 4 lags)\n",
            length(labor_keys),  length(LABOR_VARS)))


# ============================================================
# Per-window aggregations
# ============================================================
# For each rolling window i:
#   |SHAP| share of group  =  sum(|shap[group_keys]|) / sum(|shap|)
#   Signed SHAP of group   =  sum(shap[group_keys])
#
# Note: signed contributions are SUMMED (not averaged) so the magnitude
# represents the group's total push on the predicted m/m CPI change.

abs_share <- function(shap_vec, group_keys) {
  total <- sum(abs(shap_vec))
  if (!is.finite(total) || total == 0) return(NA_real_)
  sum(abs(shap_vec[group_keys])) / total
}

signed_sum <- function(shap_vec, group_keys) {
  sum(shap_vec[group_keys])
}

agg_one <- function(shap_list, oos_dates) {
  data.table(
    date         = oos_dates,
    supply_abs   = vapply(shap_list, abs_share,  numeric(1), group_keys = supply_keys),
    labor_abs    = vapply(shap_list, abs_share,  numeric(1), group_keys = labor_keys),
    supply_signed= vapply(shap_list, signed_sum, numeric(1), group_keys = supply_keys),
    labor_signed = vapply(shap_list, signed_sum, numeric(1), group_keys = labor_keys)
  )
}

dt <- rbind(
  agg_one(shap1$save.shap, oos_dates_1),
  agg_one(shap2$save.shap, oos_dates_2)
)
setorder(dt, date)


# ============================================================
# 12-month rolling means
# ============================================================

dt[, supply_abs_sm    := frollmean(supply_abs,    SMOOTH_WINDOW, align = "right")]
dt[, labor_abs_sm     := frollmean(labor_abs,     SMOOTH_WINDOW, align = "right")]
dt[, supply_signed_sm := frollmean(supply_signed, SMOOTH_WINDOW, align = "right")]
dt[, labor_signed_sm  := frollmean(labor_signed,  SMOOTH_WINDOW, align = "right")]

# Relative-importance series: divide by own full-OOS smoothed mean
supply_abs_mean <- mean(dt$supply_abs_sm, na.rm = TRUE)
labor_abs_mean  <- mean(dt$labor_abs_sm,  na.rm = TRUE)
dt[, supply_rel := supply_abs_sm / supply_abs_mean]
dt[, labor_rel  := labor_abs_sm  / labor_abs_mean]

cat(sprintf("\nMean |SHAP| share — supply : %.4f%%\n", 100 * supply_abs_mean))
cat(sprintf("Mean |SHAP| share — labor  : %.4f%%\n",   100 * labor_abs_mean))


# ============================================================
# Shock bands
# ============================================================

gfc_start <- as.Date("2008-09-01")
gfc_end   <- as.Date("2009-12-31")
cov_start <- as.Date("2020-03-01")
cov_end   <- as.Date("2022-12-31")


# ============================================================
# Common axis range from the data so both panels in a chart align
# ============================================================

x_lims <- c(min(dt$date), max(dt$date))


# ============================================================
# Panel builder for the relative-importance chart
# ============================================================

build_rel_panel <- function(d, yvar, title, line_color, show_x = TRUE) {
  d2 <- d[!is.na(get(yvar))]
  ymax <- max(1.2, max(d2[[yvar]], na.rm = TRUE) * 1.05)

  p <- ggplot(d2, aes(x = date, y = .data[[yvar]])) +
    annotate("rect", xmin = gfc_start, xmax = gfc_end,
             ymin = 0, ymax = ymax, fill = "grey75", alpha = 0.45) +
    annotate("rect", xmin = cov_start, xmax = cov_end,
             ymin = 0, ymax = ymax, fill = "grey75", alpha = 0.45) +
    geom_ribbon(aes(ymin = 1, ymax = pmax(.data[[yvar]], 1)),
                fill = line_color, alpha = 0.22) +
    geom_ribbon(aes(ymin = pmin(.data[[yvar]], 1), ymax = 1),
                fill = line_color, alpha = 0.08) +
    geom_hline(yintercept = 1, linetype = "dashed",
               color = "grey40", linewidth = 0.5) +
    geom_line(color = line_color, linewidth = 1.0) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                 limits = x_lims,
                 expand = expansion(mult = c(0.01, 0.02))) +
    scale_y_continuous(limits = c(0, ymax),
                       breaks = seq(0, ceiling(ymax * 2) / 2, 0.5),
                       expand = expansion(mult = c(0, 0))) +
    labs(title = title, x = NULL,
         y = "Relative |SHAP|\n(1.0 = own average)") +
    theme_minimal(base_size = 10) +
    theme(
      plot.title         = element_text(face = "bold", size = 10),
      axis.title.y       = element_text(size = 8, colour = "grey25"),
      axis.text          = element_text(size = 8),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(color = "grey90"),
      plot.margin        = margin(t = 4, r = 12, b = 4, l = 6)
    )

  if (!show_x) p <- p + theme(axis.text.x  = element_blank(),
                              axis.ticks.x = element_blank())
  p
}


# ============================================================
# Panel builder for the signed-SHAP chart
# ============================================================

build_signed_panel <- function(d, yvar, title, line_color, show_x = TRUE) {
  d2 <- d[!is.na(get(yvar))]
  yabs <- max(abs(d2[[yvar]]), na.rm = TRUE)
  ymax <-  yabs * 1.08
  ymin <- -ymax

  p <- ggplot(d2, aes(x = date, y = .data[[yvar]])) +
    annotate("rect", xmin = gfc_start, xmax = gfc_end,
             ymin = ymin, ymax = ymax, fill = "grey75", alpha = 0.45) +
    annotate("rect", xmin = cov_start, xmax = cov_end,
             ymin = ymin, ymax = ymax, fill = "grey75", alpha = 0.45) +
    geom_ribbon(aes(ymin = 0, ymax = pmax(.data[[yvar]], 0)),
                fill = line_color, alpha = 0.22) +
    geom_ribbon(aes(ymin = pmin(.data[[yvar]], 0), ymax = 0),
                fill = line_color, alpha = 0.08) +
    geom_hline(yintercept = 0, color = "grey25", linewidth = 0.5) +
    geom_line(color = line_color, linewidth = 1.0) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                 limits = x_lims,
                 expand = expansion(mult = c(0.01, 0.02))) +
    scale_y_continuous(labels = label_number(accuracy = 0.001,
                                             style_positive = "plus"),
                       limits = c(ymin, ymax),
                       expand = expansion(mult = c(0, 0))) +
    labs(title = title, x = NULL,
         y = "Signed SHAP\n(CPI m/m, p.p.)") +
    theme_minimal(base_size = 10) +
    theme(
      plot.title         = element_text(face = "bold", size = 10),
      axis.title.y       = element_text(size = 8, colour = "grey25"),
      axis.text          = element_text(size = 8),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(color = "grey90"),
      plot.margin        = margin(t = 4, r = 12, b = 4, l = 6)
    )

  if (!show_x) p <- p + theme(axis.text.x  = element_blank(),
                              axis.ticks.x = element_blank())
  p
}


# ============================================================
# Chart 1 — Relative |SHAP| importance
# ============================================================

p1_top <- build_rel_panel(dt, "supply_rel", SUPPLY_TITLE,
                          line_color = "#c0392b", show_x = FALSE)
p1_bot <- build_rel_panel(dt, "labor_rel",  LABOR_TITLE,
                          line_color = "#2980b9", show_x = TRUE)

chart1 <- (p1_top / p1_bot) +
  plot_annotation(
    title    = "Do inflation drivers rotate during shocks?",
    subtitle = "Relative SHAP importance, 12-month rolling, 3-month forecast horizon",
    caption  = paste0("Grey bands: GFC (2008-09 to 2009-12), COVID period ",
                      "(2020-03 to 2022-12).\n",
                      "Each line: 12-month rolling mean of summed |SHAP| over ",
                      "the listed variables, divided by that series' full-OOS mean."),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(colour = "grey25", size = 10),
      plot.caption  = element_text(hjust = 0, colour = "grey40", size = 8)
    )
  )


# ============================================================
# Chart 2 — Direction of effect (signed SHAP)
# ============================================================

p2_top <- build_signed_panel(dt, "supply_signed_sm", SUPPLY_TITLE,
                             line_color = "#c0392b", show_x = FALSE)
p2_bot <- build_signed_panel(dt, "labor_signed_sm",  LABOR_TITLE,
                             line_color = "#2980b9", show_x = TRUE)

chart2 <- (p2_top / p2_bot) +
  plot_annotation(
    title    = "Direction of effect: how each group pushed the 3-month inflation forecast",
    subtitle = "Signed SHAP, 12-month rolling mean. Above zero = pushed forecast up; below zero = pulled it down.",
    caption  = paste0("Grey bands: GFC (2008-09 to 2009-12), COVID period ",
                      "(2020-03 to 2022-12).\n",
                      "Each line: 12-month rolling mean of summed signed SHAP ",
                      "over the listed variables. Sum (not average): magnitudes ",
                      "are total contribution to monthly inflation forecast."),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(colour = "grey25", size = 10),
      plot.caption  = element_text(hjust = 0, colour = "grey40", size = 8)
    )
  )


# ============================================================
# Save
# ============================================================

outdir <- "03_Output/Charts/rf"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

out1 <- file.path(outdir, "rotation_shap_relative.png")
out2 <- file.path(outdir, "rotation_shap_signed.png")

ggsave(out1, plot = chart1, width = 14, height = 8, dpi = 300)
ggsave(out2, plot = chart2, width = 14, height = 8, dpi = 300)

print(chart1)
print(chart2)

cat(sprintf("\nSaved: %s\n", out1))
cat(sprintf("Saved: %s\n",   out2))

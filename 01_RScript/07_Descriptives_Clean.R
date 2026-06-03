# ============================================================================
# 07_Descriptives_Clean.R
# Master's Thesis: The State-Dependent Phillips Curve
# Authors: E. Tasan Ozel, E. Infante, O. Ozel, M. Bratkowska, Z. Pehlivan
# Scope: Produces the figures and the regime-level descriptive statistics
#        table used in the Data & Descriptives section of the thesis.
#
# Inputs : 02_Input/data_cleaned.rds  (transformed/stationary panel)
#          02_Input/2026-01-MD.csv    (raw FRED-MD vintage, for *levels*
#                                      of UNRATE and HWIURATIO)
# Outputs: 03_Output/Paper/Descriptives/
#            fig1_inflation_unemp_panel.pdf  (3-panel: pi, v/u, rolling corr)
#            fig2_phillips_scatter_by_regime.pdf
#            tab1_descriptives_by_regime.tex
#            tab1_descriptives_by_regime.csv
#
# Important convention
# --------------------
# CPIAUCSL is stationary-transformed (log-diff x 100), which is the modelling
# target pi_t.  UNRATE and HWIURATIO carry transformation code 2 in FRED-MD,
# so the cleaned panel stores them as *first differences*.  For the
# descriptives we want them in LEVELS (unemployment %, vacancy-to-unemployment
# ratio), so we re-read them from the raw csv.  This is purely a display
# choice -- forecasting models continue to use the stationary cleaned series.
#
# Style: uses data.table.  ggplot2 only for plots.
# ============================================================================

# ---- 0. Packages ----------------------------------------------------------
required <- c("data.table", "ggplot2", "patchwork", "zoo",
              "scales", "moments", "xtable")
to_install <- setdiff(required, rownames(installed.packages()))
if (length(to_install)) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}
invisible(lapply(required, library, character.only = TRUE))

# ---- 1. Load data ---------------------------------------------------------
data_cleaned <- readRDS("02_Input/data_cleaned.rds")
# Defensive: an older 01_Data_Transformation.R attached class
# c("data.frame","fredmd"), which trips vctrs::vec_slice().  Strip it.
class(data_cleaned) <- "data.frame"
DT <- as.data.table(data_cleaned)
DT <- DT[, .(date, CPIAUCSL)]            # only pi_t from the cleaned panel

# Re-load raw csv to get UNRATE and HWIURATIO in *levels*
raw <- fread("02_Input/2026-01-MD.csv", skip = 2, header = FALSE)
hdr <- names(fread("02_Input/2026-01-MD.csv", nrows = 0))
setnames(raw, hdr)
setnames(raw, "sasdate", "date")
raw[, date := as.Date(date, format = "%m/%d/%Y")]
raw <- raw[!is.na(date), .(date, UNRATE, HWIURATIO)]

DT <- merge(DT, raw, by = "date", all.x = TRUE)

dir.create("03_Output/Paper/Descriptives", recursive = TRUE, showWarnings = FALSE)

# ---- 2. Build working sample ---------------------------------------------
DT <- DT[date >= as.Date("2002-01-01") & date <= as.Date("2024-12-31")]
DT[, `:=`(
  infl_mom = CPIAUCSL,             # monthly % inflation
  infl_ann = CPIAUCSL * 12,        # annualized %, for readability
  vu_ratio = HWIURATIO             # level: vacancies / unemployed
)]

# Regime classification.  Two shock windows; the rest are pooled
# visually but kept as labelled sub-periods in the descriptive table.
DT[, regime := fifelse(date <  as.Date("2008-01-01"), "Other periods (pre-2008)",
              fifelse(date <  as.Date("2010-01-01"), "2008-2009 GFC",
              fifelse(date <  as.Date("2020-01-01"), "Other periods (2010-2019)",
              fifelse(date <  as.Date("2023-01-01"), "2020-2022 COVID",
                                                    "Other periods (post-2022)"))))]

regime_levels <- c("Other periods (pre-2008)",
                   "2008-2009 GFC",
                   "Other periods (2010-2019)",
                   "2020-2022 COVID",
                   "Other periods (post-2022)")
DT[, regime := factor(regime, levels = regime_levels)]

# Binary collapse used in the scatter (Shock vs Other)
DT[, regime2 := fifelse(regime %in% c("2008-2009 GFC", "2020-2022 COVID"),
                        as.character(regime),
                        "Other periods")]
DT[, regime2 := factor(regime2,
                       levels = c("Other periods",
                                  "2008-2009 GFC",
                                  "2020-2022 COVID"))]

shocks <- data.table(
  shock = c("GFC", "COVID"),
  start = as.Date(c("2008-01-01", "2020-01-01")),
  end   = as.Date(c("2009-12-31", "2022-12-31"))
)

# ---- 3. Descriptive statistics table by regime ---------------------------
desc_tbl <- DT[, .(
  N       = .N,
  Mean    = mean(infl_mom,  na.rm = TRUE),
  SD      = sd(infl_mom,    na.rm = TRUE),
  Min     = min(infl_mom,   na.rm = TRUE),
  Max     = max(infl_mom,   na.rm = TRUE),
  Skew    = moments::skewness(infl_mom, na.rm = TRUE),
  Kurt    = moments::kurtosis(infl_mom, na.rm = TRUE) - 3,
  MeanAnn = mean(infl_ann,  na.rm = TRUE),
  CorrPC  = cor(infl_mom, UNRATE, use = "pairwise.complete.obs")
), by = regime]

full_row <- DT[, .(
  regime  = "Full sample (2002-2024)",
  N       = .N,
  Mean    = mean(infl_mom, na.rm = TRUE),
  SD      = sd(infl_mom,   na.rm = TRUE),
  Min     = min(infl_mom,  na.rm = TRUE),
  Max     = max(infl_mom,  na.rm = TRUE),
  Skew    = moments::skewness(infl_mom, na.rm = TRUE),
  Kurt    = moments::kurtosis(infl_mom, na.rm = TRUE) - 3,
  MeanAnn = mean(infl_ann, na.rm = TRUE),
  CorrPC  = cor(infl_mom, UNRATE, use = "pairwise.complete.obs")
)]

desc_tbl <- rbindlist(list(desc_tbl, full_row), use.names = TRUE)

fwrite(desc_tbl, "03_Output/Paper/Descriptives/tab1_descriptives_by_regime.csv")

tex_tbl <- copy(desc_tbl)
num_cols <- c("Mean","SD","Min","Max","Skew","Kurt","MeanAnn","CorrPC")
tex_tbl[, (num_cols) := lapply(.SD, function(x) formatC(x, format="f", digits=2)),
        .SDcols = num_cols]

print(xtable::xtable(
  tex_tbl,
  caption = paste("Descriptive statistics of monthly CPI",
                  "inflation (\\%, log-difference \\(\\times\\) 100)",
                  "by sub-period, 2002:M1--2024:M12."),
  label   = "tab:desc_regime",
  align   = c("l","l","r","r","r","r","r","r","r","r","r")),
  include.rownames = FALSE,
  booktabs = TRUE,
  file = "03_Output/Paper/Descriptives/tab1_descriptives_by_regime.tex")

cat("\nRegime descriptive table:\n"); print(desc_tbl)

# ---- 4. Rolling 36-month Phillips correlation ----------------------------
setorder(DT, date)
DT[, roll_corr := zoo::rollapplyr(
  data       = cbind(infl_mom, vu_ratio),
  width      = 36,
  FUN        = function(x) cor(x[, 1], x[, 2]),
  by.column  = FALSE,
  fill       = NA
)]

# ---- 5. Figure 1: 3-panel stack (inflation, v/u ratio, rolling corr) -----
# Common silent shock-band layer.
shock_bands <- list(
  geom_rect(data = shocks,
            aes(xmin = start, xmax = end,
                ymin = -Inf, ymax =  Inf),
            fill = "grey75", alpha = 0.30,
            inherit.aes = FALSE),
  annotate("text", x = as.Date("2009-01-01"), y =  Inf,
           label = "GFC",   vjust = 1.4, size = 3, colour = "grey25"),
  annotate("text", x = as.Date("2021-06-01"), y =  Inf,
           label = "COVID", vjust = 1.4, size = 3, colour = "grey25")
)

base_theme <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        plot.title = element_text(size = 11, face = "bold", hjust = 0.5))

p1a <- ggplot(DT, aes(x = date, y = infl_mom)) +
  shock_bands +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_line(colour = "#08306b", linewidth = 0.6) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(title = "(a) Monthly CPI Inflation",
       y = "%") +
  base_theme

p1b <- ggplot(DT, aes(x = date, y = vu_ratio)) +
  shock_bands +
  geom_hline(yintercept = 1, colour = "grey40",
             linewidth = 0.3, linetype = "dashed") +
  geom_line(colour = "#08519c", linewidth = 0.6) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(title = "(b) Vacancy / Unemployment ratio (v/u)",
       y = "ratio") +
  base_theme

p1c <- ggplot(DT, aes(x = date, y = roll_corr)) +
  shock_bands +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_line(colour = "#08306b", linewidth = 0.6) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(title = expression(bold("(c) Correlation Between Inflation and v/u (36-month rolling)")),
       y = expression(rho)) +
  base_theme

fig1 <- p1a / p1b / p1c
fig1
ggsave("03_Output/Paper/Descriptives/fig1_inflation_unemp_panel.pdf",
       fig1, width = 7.2, height = 7.5)

# ---- 6. Figure 2: Phillips scatter (level UNRATE) by regime --------------
p2 <- ggplot(DT, aes(x = vu_ratio, y = infl_mom, colour = regime2)) +
  geom_point(alpha = 0.55, size = 2.0, stroke = 0) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
  scale_colour_manual(values = c(
    "Other periods"   = "black",
    "2008-2009 GFC"   = "red",
    "2020-2022 COVID" = "orange")) +
  labs(x = "Vacancy to Unemployment Ratio",
       y = "Monthly CPI Inflation",
       colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 9),
        panel.grid.minor = element_blank())
p2
ggsave("03_Output/Paper/Descriptives/fig2_phillips_scatter_by_regime.pdf",
       p2, width = 7.2, height = 4.4)

p3 <- ggplot(DT[regime2 != "2008-2009 GFC"], aes(x = vu_ratio, y = infl_mom, colour = regime2)) +
  geom_point(alpha = 0.55, size = 2.0, stroke = 0) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_colour_manual(values = c(
    "Other periods"   = "black",
    "2008-2009 GFC"   = "red",
    "2020-2022 COVID" = "orange")) +
  labs(x = "Vacancy to Unemployment Ratio",
       y = "Monthly CPI Inflation",
       colour = NULL) +
  theme_minimal(base_size = 11) + coord_cartesian(ylim = c(-2, 2)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 9),
        panel.grid.minor = element_blank())

p4 <- ggplot(DT[regime2 != "2020-2022 COVID"], aes(x = vu_ratio, y = infl_mom, colour = regime2)) +
  geom_point(alpha = 0.55, size = 2.0, stroke = 0) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_colour_manual(values = c(
    "Other periods"   = "black",
    "2008-2009 GFC"   = "red",
    "2020-2022 COVID" = "orange")) +
  labs(x = "Vacancy to Unemployment Ratio",
       y = "",
       colour = NULL) +
  theme_minimal(base_size = 11) + coord_cartesian(ylim = c(-2, 2)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 9),
        panel.grid.minor = element_blank())

fig2 <- p3 * p4
fig2
ggsave("03_Output/Paper/Descriptives/fig2_phillips_scatter_2charts.pdf",
       fig2, width = 7, height = 4)
       


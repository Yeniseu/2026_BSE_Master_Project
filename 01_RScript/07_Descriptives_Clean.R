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

cat("\nAll outputs written to 03_Output/Paper/Descriptives/\n")



# ============================================================================
# ============================================================================
# ============================================================================
# 07_Descriptives_Clean.R
# Master's Thesis: The State-Dependent Phillips Curve
# Authors: E. Tasan Ozel, E. Infante, O. Ozel, M. Bratkowska, Z. Pehlivan
# Economist retheme by Emilia
# ============================================================================

# ---- 0. Packages ----------------------------------------------------------
required <- c("data.table", "ggplot2", "patchwork", "zoo",
              "scales", "moments", "xtable")
to_install <- setdiff(required, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(required, library, character.only = TRUE))


# ============================================================
# Palette
# ============================================================

COL_INFL  <- "#6161BA"   # periwinkle  — inflation line
COL_VU    <- "#34BA66"   # green       — v/u ratio line
COL_CORR  <- "#6161BA"   # periwinkle  — rolling correlation
COL_OTHER <- "#555566"   # dark slate  — scatter: other periods
COL_GFC   <- "#BA3B35"   # deep red    — scatter: GFC
COL_COVID <- "#BAA237"   # warm gold   — scatter: COVID
COL_BAND  <- "#CCCCDD"   # violet-grey — shock bands
COL_TEXT  <- "#1a1a2e"   # near-black  — all text
COL_RULE  <- "#6161BA"   # top border accent

scatter_colors <- c(
  "Other periods"   = COL_OTHER,
  "2008-2009 GFC"   = COL_GFC,
  "2020-2022 COVID" = COL_COVID
)


# ============================================================
# Economist base theme
# ============================================================

theme_ec <- function(base_size = 11, legend = "none") {
  theme_minimal(base_size = base_size, base_family = "serif") %+replace%
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.margin      = margin(t = 10, r = 14, b = 6, l = 8),
      
      plot.title    = element_text(face = "bold",   size = 12,
                                   family = "serif", hjust = 0,
                                   colour = COL_TEXT,
                                   margin = margin(b = 3)),
      plot.subtitle = element_text(face = "italic", size = 10,
                                   family = "serif", hjust = 0,
                                   colour = "#444455",
                                   margin = margin(b = 8)),
      plot.caption  = element_text(face = "italic", size = 9,
                                   family = "serif", hjust = 0,
                                   colour = "#444455",
                                   margin = margin(t = 6)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      
      axis.title = element_text(family = "serif", face = "italic",
                                size = 10, colour = COL_TEXT),
      axis.text  = element_text(family = "serif", size = 10,
                                colour = COL_TEXT),
      axis.line.x = element_line(colour = "#AAAABC", linewidth = 0.35),
      axis.line.y = element_blank(),
      axis.ticks  = element_blank(),
      
      panel.grid.major.y = element_line(colour = "#EEEEF2", linewidth = 0.28),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      
      legend.position   = legend,
      legend.title      = element_blank(),
      legend.text       = element_text(family = "serif", size = 10,
                                       colour = COL_TEXT),
      legend.key.width  = unit(1.6, "lines"),
      legend.key.height = unit(0.5, "lines"),
      legend.margin     = margin(0, 0, 4, 0)
    )
}

ec_annotation_theme <- theme(
  plot.title    = element_text(face = "bold",   size = 15,
                               family = "serif", hjust = 0,
                               colour = COL_TEXT,
                               margin = margin(t = 12, b = 3)),
  plot.subtitle = element_text(face = "italic", size = 11,
                               family = "serif", hjust = 0,
                               colour = "#444455",
                               margin = margin(b = 10)),
  plot.caption  = element_text(face = "italic", size = 9,
                               family = "serif", hjust = 0,
                               colour = "#444455",
                               margin = margin(t = 8)),
  plot.background = element_rect(fill = "white", colour = COL_RULE,
                                 linewidth = 2.2)
)


# ---- 1. Load data (unchanged) --------------------------------------------
data_cleaned <- readRDS("02_Input/data_cleaned.rds")
class(data_cleaned) <- "data.frame"
DT <- as.data.table(data_cleaned)
DT <- DT[, .(date, CPIAUCSL)]

raw <- fread("02_Input/2026-01-MD.csv", skip = 2, header = FALSE)
hdr <- names(fread("02_Input/2026-01-MD.csv", nrows = 0))
setnames(raw, hdr)
setnames(raw, "sasdate", "date")
raw[, date := as.Date(date, format = "%m/%d/%Y")]
raw <- raw[!is.na(date), .(date, UNRATE, HWIURATIO)]

DT <- merge(DT, raw, by = "date", all.x = TRUE)
dir.create("03_Output/Paper/Descriptives", recursive = TRUE, showWarnings = FALSE)


# ---- 2. Build working sample (unchanged) ---------------------------------
DT <- DT[date >= as.Date("2002-01-01") & date <= as.Date("2024-12-31")]
DT[, `:=`(
  infl_mom = CPIAUCSL,
  infl_ann = CPIAUCSL * 12,
  vu_ratio = HWIURATIO
)]

regime_levels <- c("Other periods (pre-2008)", "2008-2009 GFC",
                   "Other periods (2010-2019)", "2020-2022 COVID",
                   "Other periods (post-2022)")
DT[, regime := fifelse(date <  as.Date("2008-01-01"), "Other periods (pre-2008)",
                       fifelse(date <  as.Date("2010-01-01"), "2008-2009 GFC",
                               fifelse(date <  as.Date("2020-01-01"), "Other periods (2010-2019)",
                                       fifelse(date <  as.Date("2023-01-01"), "2020-2022 COVID",
                                               "Other periods (post-2022)"))))]
DT[, regime := factor(regime, levels = regime_levels)]

DT[, regime2 := fifelse(regime %in% c("2008-2009 GFC", "2020-2022 COVID"),
                        as.character(regime), "Other periods")]
DT[, regime2 := factor(regime2,
                       levels = c("Other periods", "2008-2009 GFC", "2020-2022 COVID"))]

shocks <- data.table(
  shock = c("GFC", "COVID"),
  start = as.Date(c("2008-01-01", "2020-01-01")),
  end   = as.Date(c("2009-12-31", "2022-12-31"))
)


# ---- 3. Descriptive statistics table (unchanged) -------------------------
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
  include.rownames = FALSE, booktabs = TRUE,
  file = "03_Output/Paper/Descriptives/tab1_descriptives_by_regime.tex")

cat("\nRegime descriptive table:\n"); print(desc_tbl)


# ---- 4. Rolling 36-month correlation (unchanged) -------------------------
setorder(DT, date)
DT[, roll_corr := zoo::rollapplyr(
  data      = cbind(infl_mom, vu_ratio),
  width     = 36,
  FUN       = function(x) cor(x[, 1], x[, 2]),
  by.column = FALSE,
  fill      = NA
)]


# ---- 5. Figure 1: 3-panel stack ------------------------------------------

shock_bands <- list(
  geom_rect(data = shocks,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = COL_BAND, alpha = 0.45, inherit.aes = FALSE),
  annotate("text", x = as.Date("2009-01-01"), y = Inf,
           label = "GFC",   vjust = 1.6, size = 3,
           family = "serif", fontface = "italic", colour = "#444455"),
  annotate("text", x = as.Date("2021-06-01"), y = Inf,
           label = "COVID", vjust = 1.6, size = 3,
           family = "serif", fontface = "italic", colour = "#444455")
)

x_scale <- scale_x_date(date_breaks = "3 years", date_labels = "%Y",
                        expand = expansion(mult = c(0.01, 0.01)))

p1a <- ggplot(DT, aes(x = date, y = infl_mom)) +
  shock_bands +
  geom_hline(yintercept = 0, colour = "#AAAABC", linewidth = 0.35) +
  geom_line(colour = COL_INFL, linewidth = 0.7) +
  x_scale +
  labs(title = "(a) Monthly CPI inflation", y = "%") +
  theme_ec()

p1b <- ggplot(DT, aes(x = date, y = vu_ratio)) +
  shock_bands +
  geom_hline(yintercept = 1, colour = "#AAAABC",
             linewidth = 0.35, linetype = "22") +
  geom_line(colour = COL_VU, linewidth = 0.7) +
  x_scale +
  labs(title = "(b) Vacancy / Unemployment ratio (v/u)", y = "ratio") +
  theme_ec()

p1c <- ggplot(DT, aes(x = date, y = roll_corr)) +
  shock_bands +
  geom_hline(yintercept = 0, colour = "#AAAABC", linewidth = 0.35) +
  geom_line(colour = COL_CORR, linewidth = 0.7) +
  x_scale +
  labs(title = expression(bold("(c) Correlation between inflation and v/u (36-month rolling)")),
       y = expression(rho)) +
  theme_ec()

fig1 <- (p1a / p1b / p1c) +
  plot_annotation(
    title    = "Inflation dynamics and labour market, 2002–2024",
    subtitle = "Monthly CPI inflation, vacancy-unemployment ratio, and rolling correlation",
    caption  = "Shaded bands: GFC (2008–09) and COVID (2020–22) periods.",
    theme    = ec_annotation_theme
  )

fig1
ggsave("03_Output/Paper/Descriptives/fig1_inflation_unemp_panel.pdf",
       fig1, width = 7.2, height = 7.5, bg = "white")


# ---- 6. Figure 2: full scatter (all three regimes) -----------------------

p2 <- ggplot(DT, aes(x = vu_ratio, y = infl_mom, colour = regime2)) +
  geom_point(alpha = 0.50, size = 2.0, stroke = 0) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  scale_colour_manual(
    values = scatter_colors,
    guide  = guide_legend(override.aes = list(size = 3, alpha = 0.85, linetype = 0))
  ) +
  labs(
    title    = "The Phillips curve by regime",
    subtitle = "Monthly CPI inflation vs. vacancy-unemployment ratio, 2002–2024",
    x        = "Vacancy to unemployment ratio",
    y        = "Monthly CPI inflation (%)",
    caption  = "OLS fit lines estimated separately per regime."
  ) +
  theme_ec(legend = "bottom") +
  theme(
    legend.justification = "left",
    plot.background = element_rect(fill = "white", colour = COL_RULE,
                                   linewidth = 2.2)
  )

p2
ggsave("03_Output/Paper/Descriptives/fig2_phillips_scatter_by_regime.pdf",
       p2, width = 7.2, height = 4.8, bg = "white")


# ---- 7. Figure 3: side-by-side scatters (excl. one shock each) ----------
# Shared scatter layer builder to keep both panels consistent
build_scatter <- function(data, title) {
  ggplot(data, aes(x = vu_ratio, y = infl_mom, colour = regime2)) +
    geom_point(alpha = 0.50, size = 2.0, stroke = 0) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1.0) +
    scale_colour_manual(
      values = scatter_colors,
      guide  = guide_legend(override.aes = list(size = 3, alpha = 0.85, linetype = 0))
    ) +
    coord_cartesian(ylim = c(-2, 2)) +
    labs(title = title,
         x = "Vacancy to unemployment ratio",
         y = "Monthly CPI inflation (%)") +
    theme_ec(legend = "bottom") +
    theme(legend.justification = "left")
}

p3 <- build_scatter(DT[regime2 != "2008-2009 GFC"],
                    "  ")
p4 <- build_scatter(DT[regime2 != "2020-2022 COVID"],
                    "  ") +
  labs(y = "")   # suppress y-label on right panel

fig2 <- (p3 | p4) +
  plot_annotation(
    title    = "Phillips curve robustness: excluding shock periods",
    subtitle = "OLS fit by regime; y-axis clipped to [−2, 2] for comparability",
    caption  = "Left: COVID retained, GFC excluded. Right: GFC retained, COVID excluded.",
    theme    = ec_annotation_theme
  )

fig2
ggsave("03_Output/Paper/Descriptives/fig2_phillips_scatter_2charts.pdf",
       fig2, width = 10, height = 5, bg = "white")

DT[, cor(infl_mom, vu_ratio)]
DT[between(date, "2008-01-01", "2010-12-31"), cor(infl_mom, vu_ratio)]
DT[between(date, "2020-01-01", "2022-12-31"), cor(infl_mom, vu_ratio)]
DT[!(between(date, "2008-01-01", "2010-12-31") | between(date, "2020-01-01", "2022-12-31")), cor(infl_mom, vu_ratio)]


# ============================================================================
# 07_Descriptives_Clean.R
# Master's Thesis: The State-Dependent Phillips Curve
# Authors: E. Tasan Ozel, E. Infante, O. Ozel, M. Bratkowska, Z. Pehlivan
#
# Scope : Figures 1-2 and the regime-level descriptive statistics table.
#
# Inputs : 02_Input/data_cleaned.rds   (stationary panel)
#          VINTAGE_CSV (00_2_Config.R)   (raw vintage, for LEVELS of UNRATE, HWIURATIO)
# Outputs: 06_Latex/Figures/Descriptives/
#            fig1_inflation_unemp_panel.pdf     (pi, v/u, 36m rolling corr)
#            fig2_phillips_scatter_2charts.pdf  (used in the paper)
#            fig2_phillips_scatter_by_regime.pdf
#            tab1_descriptives_by_regime.{tex,csv}
#
# Convention: CPIAUCSL is log-diff x 100 (the modelling target). UNRATE and
# HWIURATIO carry FRED-MD transformation code 2, so the cleaned panel holds them
# as first differences; for the descriptives we want LEVELS, so they are re-read
# from the raw csv. Display choice only -- the models use the stationary series.
# ============================================================================
source("01_RScript/00_2_Config.R")


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
COL_OTHER <- "#1a1a2e"   # dark slate  — scatter: Normal Periods
COL_GFC   <- "red"   # deep red    — scatter: GFC
COL_COVID <- "#6161BA"   # warm gold   — scatter: COVID
COL_BAND  <- "#CCCCDD"   # violet-grey — shock bands
COL_TEXT  <- "#1a1a2e"   # near-black  — all text
COL_RULE  <- "#6161BA"   # top border accent

scatter_colors <- c(
  "Calm Periods"   = COL_OTHER,
  "Normal Periods"   = COL_OTHER,
  "2008-10 GFC"   = COL_GFC,
  "2020-22 COVID" = COL_COVID
)


# ============================================================
# Economist base theme
# ============================================================

theme_ec <- function(base_size = 11, legend = "none", size = 10) {
  theme_minimal(base_size = base_size, base_family = "Computer Modern") %+replace%
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.margin      = margin(t = 10, r = 14, b = 6, l = 8),
      
      plot.title    = element_text(face = "bold",   size = size+2,
                                   family = "serif", hjust = 0,
                                   colour = COL_TEXT,
                                   margin = margin(b = 3)),
      plot.subtitle = element_text(face = "italic", size = size,
                                   family = "serif", hjust = 0,
                                   colour = "#444455",
                                   margin = margin(b = 8)),
      plot.caption  = element_text(face = "italic", size = size-1,
                                   family = "serif", hjust = 0,
                                   colour = "#444455",
                                   margin = margin(t = 6)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      
      axis.title = element_text(family = "serif", face = "italic",
                                size = size, colour = COL_TEXT),
      axis.text  = element_text(family = "serif", size = size,
                                colour = COL_TEXT),
      axis.line.x = element_line(colour = "#AAAABC", linewidth = 0.35),
      axis.line.y = element_blank(),
      axis.ticks  = element_blank(),
      
      panel.grid.major.y = element_line(colour = "#EEEEF2", linewidth = 0.28),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      
      legend.position   = legend,
      legend.title      = element_blank(),
      legend.text       = element_text(family = "serif", size = size,
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
)


# ---- 1. Load data (unchanged) --------------------------------------------
data_cleaned <- readRDS("02_Input/data_cleaned.rds")
class(data_cleaned) <- "data.frame"
DT <- as.data.table(data_cleaned)
DT <- DT[, .(date, CPIAUCSL)]

raw <- fread(VINTAGE_CSV, skip = 2, header = FALSE)
hdr <- names(fread(VINTAGE_CSV, nrows = 0))
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

regime_levels <- c("Normal Periods (pre-2008)", "2008-10 GFC",
                   "Normal Periods (2010-2019)", "2020-22 COVID",
                   "Normal Periods (post-2022)")
DT[, regime := fifelse(date <  as.Date("2008-01-01"), "Normal Periods (pre-2008)",
                       fifelse(date <  as.Date("2011-01-01"), "2008-10 GFC",
                      fifelse(date <  as.Date("2020-01-01"), "Normal Periods (2010-2019)",
                      fifelse(date <  as.Date("2023-01-01"), "2020-22 COVID",
                                               "Normal Periods (post-2022)"))))]
DT[, regime := factor(regime, levels = regime_levels)]

DT[, regime2 := fifelse(regime %in% c("2008-10 GFC", "2020-22 COVID"),
                        as.character(regime), "Normal Periods")]
DT[, regime2 := factor(regime2,
                       levels = c("Normal Periods", "2008-10 GFC", "2020-22 COVID"))]

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
fwrite(desc_tbl, file.path(F_DESC, "tab1_descriptives_by_regime.csv"))

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
  file = file.path(F_DESC, "tab1_descriptives_by_regime.tex"))

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
           family = "serif"),
  annotate("text", x = as.Date("2021-06-01"), y = Inf,
           label = "COVID", vjust = 1.6, size = 3,
           family = "serif")
)

x_scale <- scale_x_date(date_breaks = "3 years", date_labels = "%Y",
                        expand = expansion(mult = c(0.01, 0.01)))

p1a <- ggplot(DT, aes(x = date, y = infl_mom)) +
  shock_bands +
  geom_hline(yintercept = 0, colour = "#AAAABC", linewidth = 0.35) +
  geom_line(colour = COL_INFL, linewidth = 0.7) +
  x_scale +
  labs(title = "(a) Monthly CPI Inflation", y = "%", x=NULL) +
  theme_ec()

p1b <- ggplot(DT, aes(x = date, y = vu_ratio)) +
  shock_bands +
  geom_hline(yintercept = 1, colour = "#AAAABC",
             linewidth = 0.35, linetype = "22") +
  geom_line(colour = COL_VU, linewidth = 0.7) +
  x_scale +
  labs(title = "(b) Vacancy / Unemployment ratio (v/u)", y = "ratio", x=NULL) +
  theme_ec() 

p1c <- ggplot(DT, aes(x = date, y = roll_corr)) +
  shock_bands +
  geom_hline(yintercept = 0, colour = "#AAAABC", linewidth = 0.35) +
  geom_line(colour = COL_CORR, linewidth = 0.7) +
  x_scale +
  labs(title = expression(bold("(c) Correlation between inflation and v/u (36-month rolling)")),
       y = expression(rho), x=NULL) +
  theme_ec()

fig1 <- (p1a / p1b / p1c) +
  plot_annotation(
    #title    = "Inflation dynamics and labour market, 2002–2024",
    #subtitle = "Monthly CPI Inflation, vacancy-unemployment ratio, and rolling correlation",
    theme    = ec_annotation_theme
  )

fig1
ggsave(file.path(F_DESC, "fig1_inflation_unemp_panel.pdf"),
       fig1, width = 6, height = 6, bg = "white")


# ---- 6. Figure 2: full scatter (all three regimes) -----------------------
#DT[regime2 == "Normal Periods", regime2 := "Calm Periods"]
p2 <- ggplot(DT, aes(x = vu_ratio, y = infl_mom, colour = regime2)) +
  geom_point(alpha = 0.40, size = 2.0, stroke = 0) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.25) +
  scale_colour_manual(
    values = scatter_colors,
    guide  = guide_legend(nrow=1, override.aes = list(size = 3, alpha = 0.85, linetype = 0))
  ) +
  labs(x="Vacancy to unemployment ratio", y="Monthly CPI Inflation (%)"
  ) +
  theme_ec(legend = "top", size = 20)
p2

p2 <- ggplot(DT, aes(x = vu_ratio, y = infl_mom, colour = regime2)) +
  geom_point(alpha = 0.40, size = 2.0, stroke = 0) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.25) +
  scale_colour_manual(
    values = scatter_colors,
    guide  = guide_legend(nrow=1, override.aes = list(size = 3, alpha = 0.85, linetype = 0))
  ) +
  coord_cartesian(ylim = c(-1.67, 1.245)) + # Sets the exact y-axis boundaries
  labs(
    #title    = "The Phillips curve by regime",
    #subtitle = "Monthly CPI Inflation vs. vacancy-unemployment ratio, 2002–2024",
    x        = "Vacancy to unemployment ratio",
    y        = "Monthly CPI Inflation (%)",
    #caption  = "OLS fit lines estimated separately per regime."
  ) +
  theme_ec(legend = "top", size = 20) +
  theme(
    legend.margin = margin(t = -1, r = 90, b = 0, l = 0, unit = "pt"),
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
    plot.margin = margin(t = 0, r = 5.5, b = 5.5, l = 5.5, unit = "pt") 
  )
p2
ggsave(file.path(F_DESC, "fig2_phillips_scatter_by_regime.pdf"),
       p2, width = 7, height = 7, bg = "white")


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
         y = "Monthly CPI Inflation (%)") +
    theme_ec(legend = "top") +
    theme(legend.justification = "left")
}

p3 <- build_scatter(DT[regime2 != "2008-10 GFC"],
                    "  ")
p4 <- build_scatter(DT[regime2 != "2020-22 COVID"],
                    "  ") +
  labs(y = "")   # suppress y-label on right panel

fig2 <- (p3 | p4) +
  plot_annotation(
    theme    = ec_annotation_theme
  )

fig2
ggsave(file.path(F_DESC, "fig2_phillips_scatter_2charts.pdf"),
       fig2, width = 6.5, height = 3.5, bg = "white")

DT[, cor(infl_mom, vu_ratio)]
#DT[between(date, "2008-01-01", "2010-12-31"), cor(infl_mom, vu_ratio)]
#DT[between(date, "2020-01-01", "2022-12-31"), cor(infl_mom, vu_ratio)]
#DT[!(between(date, "2008-01-01", "2010-12-31") | between(date, "2020-01-01", "2022-12-31")), cor(infl_mom, vu_ratio)]


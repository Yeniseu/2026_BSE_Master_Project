# -*- coding: utf-8 -*-
# =============================================================================
# Master Thesis: Comprehensive Descriptive & Creative Macroeconomic Visualizations
# Topic: Comparing Machine Learning Models for US Inflation Forecasting: Evidence from FRED-MD
# Base Dataset: data_cleaned.rds (Direct Output from 02_Data_Cleaning.R)
# Language: Full Academic English (Titles, Labels, and Structural Variables)
# =============================================================================

# 1. AUTOMATIC DEPENDENCY MANAGEMENT
required_packages <- c("tidyverse", "scales", "lubridate", "zoo", "ggridges", "ggcorrplot", "viridis")
missing_packages  <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

library(tidyverse)
library(scales)
library(lubridate)
library(zoo)
library(ggridges)
library(ggcorrplot)
library(viridis)

# 2. STRICT BASELINE LOADING FROM 'data_cleaned.rds'
# Checks both root directory and the standard '02_Input' directory structure
possible_paths <- c("data_cleaned.rds", "02_Input/data_cleaned.rds", "/content/data_cleaned.rds")
target_path <- NULL

for(path in possible_paths) {
  if(file.exists(path)) {
    target_path <- path
    break
  }
}

if (is.null(target_path)) {
  cat("Current workspace files:\n")
  print(list.files())
  stop("CRITICAL ERROR: 'data_cleaned.rds' not found. Please drag and drop the clean rds file into the Colab left panel.")
}

cat("SUCCESS: Establishing master dataset from verified clean source:", target_path, "\n")
data_base <- readRDS(target_path) %>% as_tibble()

# Fix non-standard syntactic names (e.g., S&P 500, yields) for R syntax compatibility
colnames(data_base) <- make.names(colnames(data_base))

# Create export directory for the thesis manuscript appendix
dir.create("Thesis_Final_Outputs", showWarnings = FALSE)

# 3. ULTRA-READABLE ACADEMIC PRINT THEME
theme_thesis_ultra <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 15, hjust = 0.5, margin = margin(b = 12)),
      plot.subtitle = element_text(color = "grey30", size = 11, hjust = 0.5, margin = margin(b = 15)),
      axis.title.x = element_text(face = "bold", size = 13, margin = margin(t = 10)),
      axis.title.y = element_text(face = "bold", size = 13, margin = margin(r = 10)),
      axis.text = element_text(color = "black", size = 11),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey93"),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
}

# Official NBER Business Cycle Recessions for Shading (Sample Phase: 1960 - 2024)
recessions <- data.frame(
  start = as.Date(c("1960-04-01","1969-12-01","1973-11-01","1980-01-01","1981-07-01","1990-07-01","2001-03-01","2007-12-01","2020-02-01")),
  end   = as.Date(c("1961-02-01","1970-11-01","1975-03-01","1980-07-01","1982-11-01","1991-03-01","2001-11-01","2009-06-01","2020-04-01"))
)

# Segment data into historical monetary policy regimes matching macroeconomic literature
data_regimes <- data_base %>%
  mutate(cpi_ann = CPIAUCSL * 12, # Annualized MoM inflation rate proxy
         Regime = case_when(
           date < "1984-01-01" ~ "Great Inflation Era (1960-1983)",
           date >= "1984-01-01" & date < "2008-01-01" ~ "Great Moderation (1984-2007)",
           date >= "2008-01-01" & date < "2020-01-01" ~ "Post-GFC Low Inflation (2008-2019)",
           date >= "2020-01-01" ~ "Pandemic & Recovery (2020-2024)",
           TRUE ~ NA_character_
         )) %>%
  mutate(Regime = factor(Regime, levels = c("Great Inflation Era (1960-1983)", "Great Moderation (1984-2007)", "Post-GFC Low Inflation (2008-2019)", "Pandemic & Recovery (2020-2024)")))

# =============================================================================
# PART A: CORE DESCRIPTIVE VISUALIZATIONS
# =============================================================================

# FIGURE 1: INFLATION HEATMAP
infl_heatmap_data <- data_base %>% mutate(Year = year(date), Month = month(date, label = TRUE))
p1 <- ggplot(infl_heatmap_data, aes(x = Month, y = Year, fill = CPIAUCSL * 12)) +
  geom_tile(color = "white", linewidth = 0.05) +
  scale_fill_distiller(palette = "RdYlBu", direction = -1, name = "Annualized % ") +
  scale_y_continuous(breaks = seq(1960, 2024, by = 5)) +
  labs(title = "Figure 1: US Inflation Heatmap (Year-over-Month Profiles)", subtitle = "Analysis of MoM annualized inflation clusters and historical regime persistence parameters", x = "Month", y = "Year") + theme_thesis_ultra()
print(p1)
ggsave("Thesis_Final_Outputs/Figure1_Heatmap.png", p1, width = 12, height = 11, dpi = 300)

# FIGURE 2: TIME-VARYING PHILLIPS CURVE
data_base <- data_base %>% mutate(phillips_corr = rollapplyr(data.frame(UNRATE, CPIAUCSL), width = 36, function(x) cor(x[,1], x[,2]), by.column = FALSE, fill = NA))
p2 <- ggplot(data_base, aes(x = date)) +
  geom_rect(data = recessions, aes(xmin = start, xmax = end, ymin = -1, ymax = 1), fill = "grey90", alpha = 0.6, inherit.aes = FALSE) +
  geom_hline(yintercept = 0, linetype = "longdash", color = "darkred", linewidth = 0.8) +
  geom_line(aes(y = phillips_corr), color = "#1a365d", linewidth = 1.5) + labs(title = "Figure 2: Dynamic Time-Varying Phillips Curve Relationship", subtitle = "36-Month Rolling Pearson Correlation between Unemployment Rate and CPI Inflation (Shaded areas indicate NBER Recessions)", x = "Year", y = "Correlation Coefficient (Pearson R)") + theme_thesis_ultra()
print(p2)
ggsave("Thesis_Final_Outputs/Figure2_Phillips_Rolling.png", p2, width = 13, height = 6.5, dpi = 300)

# FIGURE 3: INFLATION DENSITY BY ECONOMIC REGIMES
p3 <- ggplot(data_regimes, aes(x = cpi_ann, y = fct_rev(Regime), fill = Regime)) +
  geom_density_ridges(alpha = 0.75, scale = 1.3, rel_min_height = 0.005, color = "white", linewidth = 0.7) +
  scale_fill_viridis_d(option = "plasma", end = 0.85) + labs(title = "Figure 3: Inflation Density Distributions across Historical Economic Regimes", subtitle = "Structural modifications in macro-volatility trends and central tendencies across timeline transitions", x = "Annualized Monthly Inflation (%)", y = NULL) + theme_thesis_ultra() + theme(legend.position = "none")
print(p3)
ggsave("Thesis_Final_Outputs/Figure3_Regime_Ridges.png", p3, width = 12, height = 7, dpi = 300)

# FIGURE 4: MACRO-FINANCIAL CONNECTIVITY MATRIX
corr_mat <- cor(data_base %>% select(CPIAUCSL, UNRATE, INDPRO, FEDFUNDS, HOUST, M2SL, GS10), use = "pairwise.complete.obs")
p4 <- ggcorrplot(corr_mat, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 4.5, colors = c("#4575b4", "white", "#d73027"), title = "Figure 4: Macro-Financial Structural Connectivity Matrix") + theme_thesis_ultra() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
print(p4)
ggsave("Thesis_Final_Outputs/Figure4_CorrMatrix.png", p4, width = 11, height = 11, dpi = 300)


# =============================================================================
# PART B: ADVANCED CREATIVE VISUALIZATIONS
# =============================================================================

# FIGURE 5: INFLATION VOLATILITY FAN CHART
p5 <- data_regimes %>% mutate(roll_mean = rollmean(cpi_ann, k = 12, fill = NA, align = "right"), roll_sd = rollapply(cpi_ann, width = 12, FUN = sd, fill = NA, align = "right")) %>% filter(!is.na(roll_sd)) %>%
  ggplot(aes(x = date)) + geom_rect(data = recessions, aes(xmin = start, xmax = end, ymin = -25, ymax = 35), fill = "grey95", alpha = 0.5, inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = roll_mean - 2*roll_sd, ymax = roll_mean + 2*roll_sd), fill = "#3182bd", alpha = 0.15) + geom_ribbon(aes(ymin = roll_mean - roll_sd, ymax = roll_mean + roll_sd), fill = "#3182bd", alpha = 0.35) + geom_line(aes(y = roll_mean), color = "#08519c", linewidth = 1.2) + labs(title = "Figure 5: Inflation Volatility Clusters and Conditional Variance Bands", subtitle = "Solid line represents 12-month rolling mean; shaded areas depict empirical ±1 and ±2 standard deviation bands", x = "Year", y = "Annualized Monthly Inflation (%)") + theme_thesis_ultra()
print(p5)
ggsave("Thesis_Final_Outputs/Figure5_Volatility_Fan.png", p5, width = 13, height = 6.5, dpi = 300)

# FIGURE 6: MACROECONOMIC MOMENTUM PHASE DIAGRAM
p6 <- data_base %>% mutate(IP_Growth = (INDPRO - lag(INDPRO, 12)) / lag(INDPRO, 12) * 100, Unrate_Change = UNRATE - lag(UNRATE, 12), Decade = factor(paste0(10 * (year(date) %/% 10), "s"))) %>% filter(!is.na(IP_Growth) & !is.na(Unrate_Change)) %>%
  ggplot(aes(x = Unrate_Change, y = IP_Growth, color = Decade)) + geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") + geom_path(alpha = 0.25, color = "grey30") + geom_point(alpha = 0.7, size = 2) + scale_color_viridis_d(option = "turbo", name = "Historical Decade ") + labs(title = "Figure 6: Macroeconomic Momentum Structural Phase Diagram", subtitle = "Joint trajectories of industrial production growth and labor market slack vectors", x = "12-Month Absolute Change in Unemployment Rate (Percentage Points)", y = "Year-over-Year Industrial Production Growth (%)") + theme_thesis_ultra() + theme(legend.position = "right")
print(p6)
ggsave("Thesis_Final_Outputs/Figure6_Macro_Momentum.png", p6, width = 12, height = 8, dpi = 300)

# FIGURE 7: LEAD-LAG CROSS-CORRELATION ANALYSIS
cross_corr <- ccf(data_base$CPIAUCSL, data_base$FEDFUNDS, lag.max = 24, plot = FALSE)
df_ccf <- data.frame(Lag = cross_corr$lag, Correlation = cross_corr$acf)
p7 <- ggplot(df_ccf, aes(x = Lag, y = Correlation)) + geom_hline(yintercept = 0, color = "grey40") + geom_hline(yintercept = c(2/sqrt(nrow(data_base)), -2/sqrt(nrow(data_base))), linetype = "dotted", color = "darkred", linewidth = 0.8) + geom_segment(aes(xend = Lag, yend = 0), color = "#cb181d", linewidth = 1.5) + geom_point(color = "#67000d", size = 2.5) + scale_x_continuous(breaks = seq(-24, 24, by = 4)) + labs(title = "Figure 7: Cross-Correlation Function (CCF): Inflation vs. Monetary Policy Rate", subtitle = "Negative Lags: CPI leads Fed Funds Rate | Positive Lags: Policy Rate leads Inflation Dynamics", x = "Lag Horizon (Months)", y = "Cross-Correlation Coefficient") + theme_thesis_ultra()
print(p7)
ggsave("Thesis_Final_Outputs/Figure7_Lead_Lag_Analysis.png", p7, width = 12, height = 6.5, dpi = 300)

# FIGURE 8: TAIL RISK ANALYSIS (NORMAL Q-Q PLOT BY ERA)
p8 <- data_regimes %>% filter(!is.na(cpi_ann)) %>% ggplot(aes(sample = cpi_ann)) + stat_qq_line(color = "darkred", linewidth = 0.8, linetype = "longdash") + stat_qq(aes(color = Regime), alpha = 0.6, size = 1.8) + facet_wrap(~Regime, scales = "free", nrow = 2) + scale_color_viridis_d(option = "viridis", end = 0.8) + labs(title = "Figure 8: Empirical Normal Q-Q Plots of US Inflation Shocks by Era", subtitle = "Severe deviations establish heavy structural tails, justifying non-linear Machine Learning models", x = "Theoretical Quantiles (Standard Normal)", y = "Observed Sample Quantiles") + theme_thesis_ultra() + theme(legend.position = "none")
print(p8)
ggsave("Thesis_Final_Outputs/Figure8_Tail_Risk_QQ.png", p8, width = 13, height = 9, dpi = 300)

cat("\n=============================================================================")
cat("\n[SUCCESS] Baseline established from data_cleaned.rds. All 8 figures saved inside 'Thesis_Final_Outputs/'!")
cat("\n=============================================================================\n")

# =============================================================================
# Master Thesis: Comprehensive Descriptive & Creative Macroeconomic Visualizations
# Topic: Comparing Machine Learning Models for US Inflation Forecasting
# Base Dataset: data_cleaned.rds (Loaded directly from root or 02_Input panel)
# Language: Full Academic English inside plots and labels
# =============================================================================

# 1. DEPENDENCY MANAGEMENT
required_packages <- c("tidyverse", "scales", "lubridate", "zoo", "ggridges", "ggcorrplot", "viridis")
missing_packages  <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

library(tidyverse)
library(scales)
library(lubridate)
library(zoo)
library(ggridges)
library(ggcorrplot)
library(viridis)

# 2. INTELLIGENT ROUTING FOR 'data_cleaned.rds'
# Checks both root directory and the '02_Input' directory structure from your thesis scripts
possible_paths <- c("data_cleaned.rds", "02_Input/data_cleaned.rds", "/content/data_cleaned.rds")
target_path <- NULL

for(path in possible_paths) {
  if(file.exists(path)) {
    target_path <- path
    break
  }
}

if (is.null(target_path)) {
  cat("Current directory file list:\n")
  print(list.files())
  stop("CRITICAL ERROR: 'data_cleaned.rds' not found. Please ensure it is fully uploaded to the left panel.")
}

cat("SUCCESS: Base data established from verified clean source:", target_path, "\n")
data_base <- readRDS(target_path) %>% as_tibble()

# Fix S&P 500 and other non-standard syntactic names automatically for R stability
colnames(data_base) <- make.names(colnames(data_base))

# Create output folder for the thesis document appendix
dir.create("Thesis_Final_Outputs", showWarnings = FALSE)

# 3. HIGH-READABILITY ACADEMIC PLOT THEME
theme_thesis_ultra <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 15, hjust = 0.5, margin = margin(b = 12)),
      plot.subtitle = element_text(color = "grey30", size = 11, hjust = 0.5, margin = margin(b = 15)),
      axis.title.x = element_text(face = "bold", size = 13, margin = margin(t = 10)),
      axis.title.y = element_text(face = "bold", size = 13, margin = margin(r = 10)),
      axis.text = element_text(color = "black", size = 11),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey93"),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
}

# Official NBER Business Cycle Horizons for Shading
recessions <- data.frame(
  start = as.Date(c("1960-04-01","1969-12-01","1973-11-01","1980-01-01","1981-07-01","1990-07-01","2001-03-01","2007-12-01","2020-02-01")),
  end   = as.Date(c("1961-02-01","1970-11-01","1975-03-01","1980-07-01","1982-11-01","1991-03-01","2001-11-01","2009-06-01","2020-04-01"))
)

# Build historical policy regimes based on data timeframe
data_regimes <- data_base %>%
  mutate(cpi_ann = CPIAUCSL * 12, # Annualized MoM metric proxy
         Regime = case_when(
           date < "1984-01-01" ~ "Great Inflation Era (1960-1983)",
           date >= "1984-01-01" & date < "2008-01-01" ~ "Great Moderation (1984-2007)",
           date >= "2008-01-01" & date < "2020-01-01" ~ "Post-GFC Low Inflation (2008-2019)",
           date >= "2020-01-01" ~ "Pandemic & Recovery (2020-2024)",
           TRUE ~ NA_character_
         )) %>%
  mutate(Regime = factor(Regime, levels = c("Great Inflation Era (1960-1983)", "Great Moderation (1984-2007)", "Post-GFC Low Inflation (2008-2019)", "Pandemic & Recovery (2020-2024)")))

# =============================================================================
# DESCRIPTIVE VISUALIZATION GENERATION
# =============================================================================

# FIGURE 1: INFLATION HEATMAP
infl_heatmap_data <- data_base %>% mutate(Year = year(date), Month = month(date, label = TRUE))
p1 <- ggplot(infl_heatmap_data, aes(x = Month, y = Year, fill = CPIAUCSL * 12)) +
  geom_tile(color = "white", linewidth = 0.05) +
  scale_fill_distiller(palette = "RdYlBu", direction = -1, name = "Annualized % ") +
  scale_y_continuous(breaks = seq(1960, 2024, by = 5)) +
  labs(title = "Figure 1: US Inflation Heatmap (Year-over-Month Profiles)", subtitle = "Analysis of MoM annualized inflation clusters and historical regime persistence parameters", x = "Month", y = "Year") + theme_thesis_ultra()
print(p1)
ggsave("Thesis_Final_Outputs/Figure1_Heatmap.png", p1, width = 12, height = 11, dpi = 300)

# FIGURE 2: TIME-VARYING PHILLIPS CURVE
data_base <- data_base %>% mutate(phillips_corr = rollapplyr(data.frame(UNRATE, CPIAUCSL), width = 36, function(x) cor(x[,1], x[,2]), by.column = FALSE, fill = NA))
p2 <- ggplot(data_base, aes(x = date)) +
  geom_rect(data = recessions, aes(xmin = start, xmax = end, ymin = -1, ymax = 1), fill = "grey90", alpha = 0.6, inherit.aes = FALSE) +
  geom_hline(yintercept = 0, linetype = "longdash", color = "darkred", linewidth = 0.8) +
  geom_line(aes(y = phillips_corr), color = "#1a365d", linewidth = 1.5) + labs(title = "Figure 2: Dynamic Time-Varying Phillips Curve Relationship", subtitle = "36-Month Rolling Pearson Correlation between Unemployment Rate and CPI Inflation (Shaded areas indicate NBER Recessions)", x = "Year", y = "Correlation Coefficient (Pearson R)") + theme_thesis_ultra()
print(p2)
ggsave("Thesis_Final_Outputs/Figure2_Phillips_Rolling.png", p2, width = 13, height = 6.5, dpi = 300)

# FIGURE 3: INFLATION DENSITY BY ECONOMIC REGIMES
p3 <- ggplot(data_regimes, aes(x = cpi_ann, y = fct_rev(Regime), fill = Regime)) +
  geom_density_ridges(alpha = 0.75, scale = 1.3, rel_min_height = 0.005, color = "white", linewidth = 0.7) +
  scale_fill_viridis_d(option = "plasma", end = 0.85) + labs(title = "Figure 3: Inflation Density Distributions across Historical Economic Regimes", subtitle = "Structural modifications in macro-volatility trends and central tendencies across timeline transitions", x = "Annualized Monthly Inflation (%)", y = NULL) + theme_thesis_ultra() + theme(legend.position = "none")
print(p3)
ggsave("Thesis_Final_Outputs/Figure3_Regime_Ridges.png", p3, width = 12, height = 7, dpi = 300)

# FIGURE 4: MACRO-FINANCIAL CONNECTIVITY MATRIX
corr_mat <- cor(data_base %>% select(CPIAUCSL, UNRATE, INDPRO, FEDFUNDS, HOUST, M2SL, GS10), use = "pairwise.complete.obs")
p4 <- ggcorrplot(corr_mat, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 4.5, colors = c("#4575b4", "white", "#d73027"), title = "Figure 4: Macro-Financial Structural Connectivity Matrix") + theme_thesis_ultra() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
print(p4)
ggsave("Thesis_Final_Outputs/Figure4_CorrMatrix.png", p4, width = 11, height = 11, dpi = 300)

# FIGURE 5: INFLATION VOLATILITY FAN CHART
p5 <- data_regimes %>% mutate(roll_mean = rollmean(cpi_ann, k = 12, fill = NA, align = "right"), roll_sd = rollapply(cpi_ann, width = 12, FUN = sd, fill = NA, align = "right")) %>% filter(!is.na(roll_sd)) %>%
  ggplot(aes(x = date)) + geom_rect(data = recessions, aes(xmin = start, xmax = end, ymin = -25, ymax = 35), fill = "grey95", alpha = 0.5, inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = roll_mean - 2*roll_sd, ymax = roll_mean + 2*roll_sd), fill = "#3182bd", alpha = 0.15) + geom_ribbon(aes(ymin = roll_mean - roll_sd, ymax = roll_mean + roll_sd), fill = "#3182bd", alpha = 0.35) + geom_line(aes(y = roll_mean), color = "#08519c", linewidth = 1.2) + labs(title = "Figure 5: Inflation Volatility Clusters and Conditional Variance Bands", subtitle = "Solid line represents 12-month rolling mean; shaded areas depict empirical ±1 and ±2 standard deviation bands", x = "Year", y = "Annualized Monthly Inflation (%)") + theme_thesis_ultra()
print(p5)
ggsave("Thesis_Final_Outputs/Figure5_Volatility_Fan.png", p5, width = 13, height = 6.5, dpi = 300)

# FIGURE 6: MACROECONOMIC MOMENTUM PHASE DIAGRAM
p6 <- data_base %>% mutate(IP_Growth = (INDPRO - lag(INDPRO, 12)) / lag(INDPRO, 12) * 100, Unrate_Change = UNRATE - lag(UNRATE, 12), Decade = factor(paste0(10 * (year(date) %/% 10), "s"))) %>% filter(!is.na(IP_Growth) & !is.na(Unrate_Change)) %>%
  ggplot(aes(x = Unrate_Change, y = IP_Growth, color = Decade)) + geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") + geom_path(alpha = 0.25, color = "grey30") + geom_point(alpha = 0.7, size = 2) + scale_color_viridis_d(option = "turbo", name = "Historical Decade ") + labs(title = "Figure 6: Macroeconomic Momentum Structural Phase Diagram", subtitle = "Joint trajectories of industrial production growth and labor market slack vectors", x = "12-Month Absolute Change in Unemployment Rate (Percentage Points)", y = "Year-over-Year Industrial Production Growth (%)") + theme_thesis_ultra() + theme(legend.position = "right")
print(p6)
ggsave("Thesis_Final_Outputs/Figure6_Macro_Momentum.png", p6, width = 12, height = 8, dpi = 300)

# FIGURE 7: LEAD-LAG CROSS-CORRELATION ANALYSIS
cross_corr <- ccf(data_base$CPIAUCSL, data_base$FEDFUNDS, lag.max = 24, plot = FALSE)
df_ccf <- data.frame(Lag = cross_corr$lag, Correlation = cross_corr$acf)
p7 <- ggplot(df_ccf, aes(x = Lag, y = Correlation)) + geom_hline(yintercept = 0, color = "grey40") + geom_hline(yintercept = c(2/sqrt(nrow(data_base)), -2/sqrt(nrow(data_base))), linetype = "dotted", color = "darkred", linewidth = 0.8) + geom_segment(aes(xend = Lag, yend = 0), color = "#cb181d", linewidth = 1.5) + geom_point(color = "#67000d", size = 2.5) + scale_x_continuous(breaks = seq(-24, 24, by = 4)) + labs(title = "Figure 7: Cross-Correlation Function (CCF): Inflation vs. Monetary Policy Rate", subtitle = "Negative Lags: CPI leads Fed Funds Rate | Positive Lags: Policy Rate leads Inflation Dynamics", x = "Lag Horizon (Months)", y = "Cross-Correlation Coefficient") + theme_thesis_ultra()
print(p7)
ggsave("Thesis_Final_Outputs/Figure7_Lead_Lag_Analysis.png", p7, width = 12, height = 6.5, dpi = 300)

# FIGURE 8: TAIL RISK ANALYSIS (NORMAL Q-Q PLOT BY ERA)
p8 <- data_regimes %>% filter(!is.na(cpi_ann)) %>% ggplot(aes(sample = cpi_ann)) + stat_qq_line(color = "darkred", linewidth = 0.8, linetype = "longdash") + stat_qq(aes(color = Regime), alpha = 0.6, size = 1.8) + facet_wrap(~Regime, scales = "free", nrow = 2) + scale_color_viridis_d(option = "viridis", end = 0.8) + labs(title = "Figure 8: Empirical Normal Q-Q Plots of US Inflation Shocks by Era", subtitle = "Severe deviations establish heavy structural tails, justifying non-linear Machine Learning models", x = "Theoretical Quantiles (Standard Normal)", y = "Observed Sample Quantiles") + theme_thesis_ultra() + theme(legend.position = "none")
print(p8)
ggsave("Thesis_Final_Outputs/Figure8_Tail_Risk_QQ.png", p8, width = 13, height = 9, dpi = 300)

cat("\n=============================================================================")
cat("\n[SUCCESS] Baseline established from data_cleaned.rds. 8 figures saved inside 'Thesis_Final_Outputs/'!")
cat("\n=============================================================================\n")

# =============================================================================
# Master Thesis: Advanced & Custom Macroeconomic Visualizations for ML Pipeline
# Topic: Comparing Machine Learning Models for US Inflation Forecasting
# Base Dataset: data_cleaned.rds (Direct Output from PCA Imputation)
# Language: Full Academic English (Advanced Statistical Plots)
# Fixes: Resolved syntax error in %in% operator and pipe symbols
# =============================================================================

# 1. DEPENDENCY CHECK (Using standard tidyverse to avoid package masking)
required_packages <- c("tidyverse", "scales", "lubridate", "zoo", "ggridges", "viridis")
missing_packages  <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages) > 0) install.packages(missing_packages, repos = "https://cloud.r-project.org")

library(tidyverse)
library(scales)
library(lubridate)
library(zoo)
library(ggridges)
library(viridis)

# 2. LOAD CLEANED BASE DATASET
possible_paths <- c("data_cleaned.rds", "02_Input/data_cleaned.rds", "/content/data_cleaned.rds")
target_path <- NULL
for(path in possible_paths) { if(file.exists(path)) { target_path <- path; break } }

if (is.null(target_path)) {
  stop("CRITICAL ERROR: 'data_cleaned.rds' not found. Please check your left file panel.")
}

data_base <- readRDS(target_path) %>% as_tibble()
colnames(data_base) <- make.names(colnames(data_base)) # Fix syntactical names like S&P 500
dir.create("Thesis_New_Visualizations", showWarnings = FALSE)

# Custom High-End Academic Theme
theme_thesis_advanced <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 12)),
      plot.subtitle = element_text(color = "grey30", size = 10.5, hjust = 0.5, margin = margin(b = 15)),
      axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 10)),
      axis.title.y = element_text(face = "bold", size = 12, margin = margin(r = 10)),
      axis.text = element_text(color = "black", size = 10.5),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey93"),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
      legend.position = "bottom"
    )
}

# Official NBER Business Cycle Recessions for Background Shading
recessions <- data.frame(
  start = as.Date(c("1960-04-01","1969-12-01","1973-11-01","1980-01-01","1981-07-01","1990-07-01","2001-03-01","2007-12-01","2020-02-01")),
  end   = as.Date(c("1961-02-01","1970-11-01","1975-03-01","1980-07-01","1982-11-01","1991-03-01","2001-11-01","2009-06-01","2020-04-01"))
)

# =============================================================================
# NEW GRAPH 1: HIGH-DIMENSIONAL DATA VOLATILITY CLUSTERING (THE ML JUSTIFICATION)
# =============================================================================
cat("Generating New Graph 1 (Volatility Clustering Across 127 Features)...\n")
all_numeric_vars <- data_base %>% select(-any_of("date"))

rolling_sd_all <- all_numeric_vars %>%
  mutate(across(everything(), ~ rollapplyr(.x, width = 12, FUN = sd, fill = NA))) %>%
  mutate(date = data_base$date) %>%
  filter(!is.na(date)) %>%
  pivot_longer(cols = -date, names_to = "Variable", values_to = "Rolling_SD") %>%
  filter(!is.na(Rolling_SD))

macro_volatility_summary <- rolling_sd_all %>%
  group_by(date) %>%
  summarise(
    Median_Vol = median(Rolling_SD, na.rm = TRUE),
    Upper_Vol = quantile(Rolling_SD, 0.75, na.rm = TRUE),
    Lower_Vol = quantile(Rolling_SD, 0.25, na.rm = TRUE)
  )

max_upper_y <- max(macro_volatility_summary$Upper_Vol, na.rm = TRUE) * 1.1

p_new1 <- ggplot(macro_volatility_summary, aes(x = date)) +
  geom_rect(data = recessions, aes(xmin = start, xmax = end, ymin = 0, ymax = max_upper_y),
            fill = "grey90", alpha = 0.6, inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = Lower_Vol, ymax = Upper_Vol), fill = "#8884d8", alpha = 0.3) +
  geom_line(aes(y = Median_Vol), color = "#4d44b5", linewidth = 1.3) +
  labs(
    title = "Figure 1: Systemic Macroeconomic Volatility Dispersion (127 FRED-MD Features)",
    subtitle = "Shaded area represents the 25th-75th percentile cross-sectional variance band; Dark line marks the panel median.",
    x = "Year", y = "12-Month Rolling Standard Deviation (Normalized Index)"
  ) + theme_thesis_advanced()

ggsave("Thesis_New_Visualizations/Advanced_Figure1_Panel_Volatility.png", p_new1, width = 12, height = 6, dpi = 300)


# =============================================================================
# NEW GRAPH 2: TIME-VARYING NON-LINEARITY (THE RANDOM FOREST JUSTIFICATION)
# =============================================================================
cat("Generating New Graph 2 (Non-Linear LOESS Shifts)...\n")
p_new2 <- data_base %>%
  mutate(Decade = factor(paste0(10 * (year(date) %/% 10), "s"))) %>%
  filter(Decade %in% c("1970s", "1990s", "2010s", "2020s")) %>%
  ggplot(aes(x = UNRATE, y = CPIAUCSL, color = Decade)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.5) +
  scale_color_viridis_d(option = "viridis", end = 0.85) +
  labs(
    title = "Figure 2: Structural Non-Linear Shifts in the Empirical Phillips Space",
    subtitle = "LOESS non-parametric curves illustrate why rigid linear models (OLS) fail compared to Random Forest architectures.",
    x = "Unemployment Rate (Standardized Scale)", y = "Target Inflation Feature (CPIAUCSL Transformation)"
  ) + theme_thesis_advanced() + theme(legend.position = "right")

ggsave("Thesis_New_Visualizations/Advanced_Figure2_NonLinear_Regimes.png", p_new2, width = 11, height = 6.5, dpi = 300)


# =============================================================================
# NEW GRAPH 3: INFLATION MOMENTUM DRIFT (CROSS-TAIL BEHAVIOR)
# =============================================================================
cat("Generating New Graph 3 (Ridge Tail Drift Analysis)...\n")
data_ridges <- data_base %>%
  mutate(Year_Group = case_when(
    date < "1970-01-01" ~ "1960-1969 Foundations",
    date >= "1970-01-01" & date < "1983-01-01" ~ "1970-1982 Great Stagflation",
    date >= "1983-01-01" & date < "2007-01-01" ~ "1983-2006 Moderation Era",
    date >= "2007-01-01" & date < "2020-01-01" ~ "2007-2019 Post-GFC Cycle",
    date >= "2020-01-01" ~ "2020-2024 Pandemic Shock & Inflation Surge",
    TRUE ~ NA_character_
  )) %>% filter(!is.na(Year_Group))

p_new3 <- ggplot(data_ridges, aes(x = CPIAUCSL, y = Year_Group, fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01, linewidth = 0.5, color = "white") +
  scale_fill_viridis_c(option = "inferno", direction = 1, name = "Shock Severity ") +
  labs(
    title = "Figure 3: Empirical Density Drift and Tail Asymmetry of Target Inflation",
    subtitle = "Visualizing the emergence of fat tails and structural skewness changes across critical modeling horizons.",
    x = "Transformed Target Variable (CPIAUCSL Growth Rate)", y = NULL
  ) + theme_thesis_advanced() + theme(legend.position = "right")

ggsave("Thesis_New_Visualizations/Advanced_Figure3_Tail_Drift.png", p_new3, width = 12, height = 6.5, dpi = 300)


# =============================================================================
# NEW GRAPH 4: PRE-PREDICTIVE MACRO MATRIX LEAD-LAG STRUCTURE
# =============================================================================
cat("Generating New Graph 4 (Predictive Lead-Lag Covariance Grid)...\n")
core_features <- c("UNRATE", "INDPRO", "FEDFUNDS", "HOUST", "M2SL")
lag_horizons <- 0:6
ccm_list <- list()

for(v in core_features) {
  corrs <- sapply(lag_horizons, function(l) {
    cor(data_base$CPIAUCSL, lag(data_base[[v]], l), use = "pairwise.complete.obs")
  })
  ccm_list[[v]] <- data.frame(Variable = v, Lag = lag_horizons, Correlation = corrs)
}

ccm_df <- do.call(rbind, ccm_list)

p_new4 <- ggplot(ccm_df, aes(x = factor(Lag), y = Variable, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(low = "#4575b4", mid = "white", high = "#d73027", midpoint = 0, name = "Pearson R ") +
  geom_text(aes(label = round(Correlation, 2)), size = 4.5, fontface = "bold") +
  labs(
    title = "Figure 4: Predictive Lead-Lag Covariance Grid (t to t-6 Horizons)",
    subtitle = "Correlation of current CPIAUCSL with lagged historical features; highlights optimal lag bounds for AR templates.",
    x = "Lag Interval (Months, t - k)", y = "Predictor Variable Name"
  ) + theme_thesis_advanced()

ggsave("Thesis_New_Visualizations/Advanced_Figure4_Lag_Covariance.png", p_new4, width = 11, height = 7, dpi = 300)

cat("\n=============================================================================")
cat("\n[SUCCESS] 4 entirely new, advanced Machine Learning-justifying plots saved in 'Thesis_New_Visualizations/'!")
cat("\n=============================================================================\n")

# =============================================================================
# Master Thesis: Advanced & Custom Macroeconomic Visualizations for ML Pipeline
# Topic: Comparing Machine Learning Models for US Inflation Forecasting
# Base Dataset: data_cleaned.rds (Direct Output from PCA Imputation)
# Language: Full Academic English (Advanced Statistical Plots)
# Fixes: Resolved syntax error in %in% operator and pipe symbols
# =============================================================================

# 1. DEPENDENCY CHECK (Using standard tidyverse to avoid package masking)
required_packages <- c("tidyverse", "scales", "lubridate", "zoo", "ggridges", "viridis")
missing_packages  <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages) > 0) install.packages(missing_packages, repos = "https://cloud.r-project.org")

library(tidyverse)
library(scales)
library(lubridate)
library(zoo)
library(ggridges)
library(viridis)

# 2. LOAD CLEANED BASE DATASET
possible_paths <- c("data_cleaned.rds", "02_Input/data_cleaned.rds", "/content/data_cleaned.rds")
target_path <- NULL
for(path in possible_paths) { if(file.exists(path)) { target_path <- path; break } }

if (is.null(target_path)) {
  stop("CRITICAL ERROR: 'data_cleaned.rds' not found. Please check your left file panel.")
}

data_base <- readRDS(target_path) %>% as_tibble()
colnames(data_base) <- make.names(colnames(data_base)) # Fix syntactical names like S&P 500
dir.create("Thesis_New_Visualizations", showWarnings = FALSE)

# Custom High-End Academic Theme
theme_thesis_advanced <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 12)),
      plot.subtitle = element_text(color = "grey30", size = 10.5, hjust = 0.5, margin = margin(b = 15)),
      axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 10)),
      axis.title.y = element_text(face = "bold", size = 12, margin = margin(r = 10)),
      axis.text = element_text(color = "black", size = 10.5),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey93"),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
      legend.position = "bottom"
    )
}

# Official NBER Business Cycle Recessions for Background Shading
recessions <- data.frame(
  start = as.Date(c("1960-04-01","1969-12-01","1973-11-01","1980-01-01","1981-07-01","1990-07-01","2001-03-01","2007-12-01","2020-02-01")),
  end   = as.Date(c("1961-02-01","1970-11-01","1975-03-01","1980-07-01","1982-11-01","1991-03-01","2001-11-01","2009-06-01","2020-04-01"))
)

# =============================================================================
# NEW GRAPH 1: HIGH-DIMENSIONAL DATA VOLATILITY CLUSTERING (THE ML JUSTIFICATION)
# =============================================================================
cat("Generating New Graph 1 (Volatility Clustering Across 127 Features)...\n")
all_numeric_vars <- data_base %>% select(-any_of("date"))

rolling_sd_all <- all_numeric_vars %>%
  mutate(across(everything(), ~ rollapplyr(.x, width = 12, FUN = sd, fill = NA))) %>%
  mutate(date = data_base$date) %>%
  filter(!is.na(date)) %>%
  pivot_longer(cols = -date, names_to = "Variable", values_to = "Rolling_SD") %>%
  filter(!is.na(Rolling_SD))

macro_volatility_summary <- rolling_sd_all %>%
  group_by(date) %>%
  summarise(
    Median_Vol = median(Rolling_SD, na.rm = TRUE),
    Upper_Vol = quantile(Rolling_SD, 0.75, na.rm = TRUE),
    Lower_Vol = quantile(Rolling_SD, 0.25, na.rm = TRUE)
  )

max_upper_y <- max(macro_volatility_summary$Upper_Vol, na.rm = TRUE) * 1.1

p_new1 <- ggplot(macro_volatility_summary, aes(x = date)) +
  geom_rect(data = recessions, aes(xmin = start, xmax = end, ymin = 0, ymax = max_upper_y),
            fill = "grey90", alpha = 0.6, inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = Lower_Vol, ymax = Upper_Vol), fill = "#8884d8", alpha = 0.3) +
  geom_line(aes(y = Median_Vol), color = "#4d44b5", linewidth = 1.3) +
  labs(
    title = "Figure 1: Systemic Macroeconomic Volatility Dispersion (127 FRED-MD Features)",
    subtitle = "Shaded area represents the 25th-75th percentile cross-sectional variance band; Dark line marks the panel median.",
    x = "Year", y = "12-Month Rolling Standard Deviation (Normalized Index)"
  ) + theme_thesis_advanced()

ggsave("Thesis_New_Visualizations/Advanced_Figure1_Panel_Volatility.png", p_new1, width = 12, height = 6, dpi = 300)


# =============================================================================
# NEW GRAPH 2: TIME-VARYING NON-LINEARITY (THE RANDOM FOREST JUSTIFICATION)
# =============================================================================
cat("Generating New Graph 2 (Non-Linear LOESS Shifts)...\n")
p_new2 <- data_base %>%
  mutate(Decade = factor(paste0(10 * (year(date) %/% 10), "s"))) %>%
  filter(Decade %in% c("1970s", "1990s", "2010s", "2020s")) %>%
  ggplot(aes(x = UNRATE, y = CPIAUCSL, color = Decade)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.5) +
  scale_color_viridis_d(option = "viridis", end = 0.85) +
  labs(
    title = "Figure 2: Structural Non-Linear Shifts in the Empirical Phillips Space",
    subtitle = "LOESS non-parametric curves illustrate why rigid linear models (OLS) fail compared to Random Forest architectures.",
    x = "Unemployment Rate (Standardized Scale)", y = "Target Inflation Feature (CPIAUCSL Transformation)"
  ) + theme_thesis_advanced() + theme(legend.position = "right")

ggsave("Thesis_New_Visualizations/Advanced_Figure2_NonLinear_Regimes.png", p_new2, width = 11, height = 6.5, dpi = 300)


# =============================================================================
# NEW GRAPH 3: INFLATION MOMENTUM DRIFT (CROSS-TAIL BEHAVIOR)
# =============================================================================
cat("Generating New Graph 3 (Ridge Tail Drift Analysis)...\n")
data_ridges <- data_base %>%
  mutate(Year_Group = case_when(
    date < "1970-01-01" ~ "1960-1969 Foundations",
    date >= "1970-01-01" & date < "1983-01-01" ~ "1970-1982 Great Stagflation",
    date >= "1983-01-01" & date < "2007-01-01" ~ "1983-2006 Moderation Era",
    date >= "2007-01-01" & date < "2020-01-01" ~ "2007-2019 Post-GFC Cycle",
    date >= "2020-01-01" ~ "2020-2024 Pandemic Shock & Inflation Surge",
    TRUE ~ NA_character_
  )) %>% filter(!is.na(Year_Group))

p_new3 <- ggplot(data_ridges, aes(x = CPIAUCSL, y = Year_Group, fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01, linewidth = 0.5, color = "white") +
  scale_fill_viridis_c(option = "inferno", direction = 1, name = "Shock Severity ") +
  labs(
    title = "Figure 3: Empirical Density Drift and Tail Asymmetry of Target Inflation",
    subtitle = "Visualizing the emergence of fat tails and structural skewness changes across critical modeling horizons.",
    x = "Transformed Target Variable (CPIAUCSL Growth Rate)", y = NULL
  ) + theme_thesis_advanced() + theme(legend.position = "right")

ggsave("Thesis_New_Visualizations/Advanced_Figure3_Tail_Drift.png", p_new3, width = 12, height = 6.5, dpi = 300)


# =============================================================================
# NEW GRAPH 4: PRE-PREDICTIVE MACRO MATRIX LEAD-LAG STRUCTURE
# =============================================================================
cat("Generating New Graph 4 (Predictive Lead-Lag Covariance Grid)...\n")
core_features <- c("UNRATE", "INDPRO", "FEDFUNDS", "HOUST", "M2SL")
lag_horizons <- 0:6
ccm_list <- list()

for(v in core_features) {
  corrs <- sapply(lag_horizons, function(l) {
    cor(data_base$CPIAUCSL, lag(data_base[[v]], l), use = "pairwise.complete.obs")
  })
  ccm_list[[v]] <- data.frame(Variable = v, Lag = lag_horizons, Correlation = corrs)
}

ccm_df <- do.call(rbind, ccm_list)

p_new4 <- ggplot(ccm_df, aes(x = factor(Lag), y = Variable, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(low = "#4575b4", mid = "white", high = "#d73027", midpoint = 0, name = "Pearson R ") +
  geom_text(aes(label = round(Correlation, 2)), size = 4.5, fontface = "bold") +
  labs(
    title = "Figure 4: Predictive Lead-Lag Covariance Grid (t to t-6 Horizons)",
    subtitle = "Correlation of current CPIAUCSL with lagged historical features; highlights optimal lag bounds for AR templates.",
    x = "Lag Interval (Months, t - k)", y = "Predictor Variable Name"
  ) + theme_thesis_advanced()

ggsave("Thesis_New_Visualizations/Advanced_Figure4_Lag_Covariance.png", p_new4, width = 11, height = 7, dpi = 300)

cat("\n=============================================================================")
cat("\n[SUCCESS] 4 entirely new, advanced Machine Learning-justifying plots saved in 'Thesis_New_Visualizations/'!")
cat("\n=============================================================================\n")
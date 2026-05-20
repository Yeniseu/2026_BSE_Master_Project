# ============================================================================
# DESCRIPTIVE STATISTICS AND EXPLORATORY DATA ANALYSIS
# Inflation Forecasting Using Machine Learning Methods
# Author: Emilia Infante
# Date: 28.11.2025
# ============================================================================

library(corrplot)

# ----------------------------------------------------------------------------
# Summary Statistics
# ----------------------------------------------------------------------------
summary(data_transformed)

# ----------------------------------------------------------------------------
# Figure 1: Temporal Evolution of US Monthly Inflation Rate
# ----------------------------------------------------------------------------

plot(data_transformed$date, data_transformed$CPIAUCSL, 
     type = "l", 
     main = "US Monthly Inflation Rate (1960-2025)",
     xlab = "Date", 
     ylab = "Inflation Rate (%)",
     col = "steelblue",
     lwd = 1.5,
     las = 1)
abline(h = 0, col = "red", lty = 2, lwd = 1.2)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray90")

# ----------------------------------------------------------------------------
# Figure 2: Distribution of Monthly Inflation Rates
# ----------------------------------------------------------------------------

hist(data_transformed$CPIAUCSL, 
     breaks = 50,
     main = "Distribution of US Monthly Inflation Rates",
     xlab = "Inflation Rate (%)",
     ylab = "Frequency",
     col = "lightblue",
     border = "white",
     las = 1)
abline(v = mean(data_transformed$CPIAUCSL, na.rm = TRUE), 
       col = "red", lwd = 2, lty = 2)
legend("topright", 
       legend = c("Mean"), 
       col = c("red"), 
       lty = 2, 
       lwd = 2,
       bty = "n")


# ============================================
# CORRELATION TABLE: ALL VARIABLES WITH INFLATION BY GROUP
# ============================================

# Get all numeric variables
all_vars <- names(data_transformed)[sapply(data_transformed, is.numeric)]
all_vars <- all_vars[all_vars != "CPIAUCSL"]  # Remove inflation itself

# Calculate correlations
all_correlations <- sapply(all_vars, function(var) {
  cor(data_transformed$CPIAUCSL, data_transformed[[var]], 
      use = "pairwise.complete.obs")
})

# Create full table
full_correlation_table <- data.frame(
  Variable = names(all_correlations),
  Correlation = round(all_correlations, 3)
)

# Sort by absolute correlation
full_correlation_table <- full_correlation_table[order(-abs(full_correlation_table$Correlation)), ]
rownames(full_correlation_table) <- NULL

# Print
print(full_correlation_table)

# ----------------------------------------------------------------------------
# Variable Selection for Correlation Analysis
# ----------------------------------------------------------------------------
# Variables selected based on both empirical correlations and economic theory

selected_vars <- c(
  "CPIAUCSL",       # Consumer Price Index (dependent variable)
  
  # High correlation group (supply-side, top predictors)
  "WPSFD49207",     # PPI: Finished Goods (r=0.55, rank 10)
  "WPSID61",        # PPI: Intermediate Materials (r=0.55, rank 9)
  "WPSID62",        # PPI: Crude Materials (r=0.36, rank 12)
  "OILPRICEx",      # Crude Oil Prices (r=0.28, rank 13)
  
  # Demand-side indicators
  "ACOGNO",         # New Orders: Consumer Goods (r=0.38, rank 11)
  "RETAILx",        # Retail Sales (r=0.17, rank 21)
  
  # Labor market (Phillips curve)
  "UNRATE",         # Unemployment Rate (r=-0.07, theory-based)
  "PAYEMS",         # Total Employment (r=0.09, rank 36)
  
  # Output/Production
  "INDPRO",         # Industrial Production (r=0.05, theory-based)
  
  # Monetary policy & financial conditions
  "FEDFUNDS",       # Federal Funds Rate (r=0.04, policy instrument)
  "M2REAL",         # Real M2 Money Supply (r=-0.19, rank 17)
  "GS10"            # 10-Year Treasury Rate (r=0.07, financial conditions)
)

# ----------------------------------------------------------------------------
# Correlation Matrix Computation
# ----------------------------------------------------------------------------
cor_matrix <- cor(data_transformed[, selected_vars], 
                  use = "pairwise.complete.obs")

cat("\nCorrelation Matrix:\n")
print(round(cor_matrix, 2))

# ----------------------------------------------------------------------------
# Figure: Correlation Matrix Heatmap
# ----------------------------------------------------------------------------

corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.7,
         addCoef.col = "black",
         number.cex = 0.5,
         col = colorRampPalette(c("#2166AC", "white", "#B2182B"))(200),
         title = "Correlation Matrix: Inflation and Key Macroeconomic Predictors",
         mar = c(0,0,2,0))

# ============================================================================
# End of Descriptive Analysis
# ============================================================================
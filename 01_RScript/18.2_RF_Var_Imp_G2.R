# Author: Ece Tasan
# Date  : 5/2025
# Scope : Rolling variable importance plots — single variable or custom group
#         Variable names are attached on-the-fly from known feature ordering.
#         Lags of the same variable are summed before computing the share.
#         Change ONLY the "USER INPUT" section to switch variables/groups.

library(data.table)
library(ggplot2)

rm(list = ls())
options(scipen = 30, digits = 5)


# ============================================================
# USER INPUT — change this section only
# ============================================================

# --- Option A: Single variable ---
# Set GROUP_MODE = FALSE and put the variable name below
GROUP_MODE    <- FALSE
VARIABLE_NAME <- "PPICMM"    # e.g. "CPITRNSL", "OILPRICEx", "PAYEMS"

# --- Option B: Custom group ---
# Set GROUP_MODE = TRUE, list variables, and give the group a name
# GROUP_MODE <- TRUE
# GROUP_NAME <- "Supply-side prices"
# GROUP_VARS <- c("OILPRICEx", "WPSID61", "WPSFD49207", "PPICMM", "WPSID62")

# --- Rolling window smoothing (months) ---
SMOOTH_WINDOW <- 12


# ============================================================
# Load data & saved RF results
# ============================================================

fred  <- readRDS("02_Input/data_cleaned.rds")
setDT(fred)
setnames(fred, "CPIAUCSL", "inf")
setcolorder(fred, c("date", "inf"))

first <- readRDS("03_Output/rf1_3.rds")   # OOS 2001-01 to 2015-12
socend <- readRDS("03_Output/rf2_3.rds")   # OOS 2016-01 to 2024-12


# ============================================================
# Build feature name mapping — matches exact order inside runrf
# ============================================================
# Inside runrf (lag=3):
#   Y2   = cbind(Y, 4 PCs)               → 130 columns
#   aux  = embed(Y2, 7)                   → 7 lag blocks of 130
#   X    = aux[, -(1:(130*3))]            → keeps lags 3,4,5,6  → 520 columns
#
# Importance matrix rows follow this exact order:
#   Rows   1-130 : lag-3 of [y2_names[1], y2_names[2], ..., y2_names[130]]
#   Rows 131-260 : lag-4 of same
#   Rows 261-390 : lag-5 of same
#   Rows 391-520 : lag-6 of same
#
# Verified against R console: HWIURATIO = y2_names[22]
#   → importance rows 22, 152, 282, 412  ✓

lag_param <- 3                                    # must match lag used in runrf
n_lag_blocks <- 4                                 # always 4 lag blocks in X

var_names <- setdiff(names(fred), "date")         # 126 vars, "inf" first
pc_names  <- paste0("Comp.", 1:4)
y2_names  <- c(var_names, pc_names)               # 130 variables
n_y2      <- length(y2_names)                     # 130
n_feat    <- n_y2 * n_lag_blocks                  # 520 total features

# Named feature vector: attach variable name + lag label to every row of imp matrix
lag_labels <- paste0("lag", seq(lag_param, lag_param + n_lag_blocks - 1))  # lag3..lag6
feat_names <- paste0(
  rep(y2_names, times = n_lag_blocks),            # var repeats across lag blocks
  "_",
  rep(lag_labels, each = n_y2)                    # lag label repeats within block
)
# feat_names[1]   = "inf_lag3"
# feat_names[2]   = "RPI_lag3"
# feat_names[22]  = "HWIURATIO_lag3"
# feat_names[152] = "HWIURATIO_lag4"  ... etc.

cat(sprintf("Feature matrix: %d variables x %d lag blocks = %d features\n",
            n_y2, n_lag_blocks, n_feat))
cat(sprintf("Lag blocks used: %s\n\n", paste(lag_labels, collapse = ", ")))


# ============================================================
# Validate user input
# ============================================================

if (!GROUP_MODE) {
  if (!VARIABLE_NAME %in% y2_names)
    stop(paste0("'", VARIABLE_NAME, "' not found.\n",
                "Available variables:\n",
                paste(y2_names, collapse = ", ")))
  plot_label  <- VARIABLE_NAME
  target_vars <- VARIABLE_NAME
  
} else {
  missing_vars <- GROUP_VARS[!GROUP_VARS %in% y2_names]
  if (length(missing_vars) > 0)
    stop(paste0("Variables not found: ", paste(missing_vars, collapse = ", ")))
  plot_label  <- GROUP_NAME
  target_vars <- GROUP_VARS
  cat(sprintf("Group '%s' contains %d variables:\n", GROUP_NAME, length(GROUP_VARS)))
  cat(paste(GROUP_VARS, collapse = ", "), "\n\n")
}


# ============================================================
# Core function: attach names, sum lags, compute share
# ============================================================

extract_share <- function(imp_mat, target_vars, feat_names, y2_names, n_y2, n_lag_blocks) {
  # imp_mat  : (520 x 2) matrix. Column 1 = %IncMSE, Column 2 = IncNodePurity.
  # Step 1   : attach feature names as rownames so we can identify each row
  # Step 2   : take %IncMSE, clip negatives to 0
  # Step 3   : for each of the 130 variables, sum its importance across all 4 lags
  # Step 4   : compute the target variable(s) share of total importance
  
  rownames(imp_mat) <- feat_names                  # attach names on-the-fly
  
  inc_mse <- imp_mat[, 1]                          # %IncMSE column
  inc_mse[inc_mse < 0] <- 0                        # clip small negatives to 0
  
  # Reshape into (n_lag_blocks x n_y2) matrix, then sum down the rows
  # Each column = one variable; each row = one lag block
  # byrow=TRUE fills: row1 = features 1..130 (lag3), row2 = 131..260 (lag4), etc.
  lag_mat <- matrix(inc_mse, nrow = n_lag_blocks, ncol = n_y2, byrow = TRUE)
  colnames(lag_mat) <- y2_names                    # column = variable name
  
  # One importance value per variable (summed across all 4 lags)
  per_var <- colSums(lag_mat)                      # named vector, length 130
  
  total <- sum(per_var)
  if (total == 0) return(0)
  
  # Share of target variable(s) in total importance
  sum(per_var[target_vars]) / total
}


# ============================================================
# Apply across all rolling windows
# ============================================================

oos_dates_1 <- tail(fred$date[fred$date < as.Date("2016-01-01")], 180)
oos_dates_2 <- tail(fred$date, 108)

shares_1 <- vapply(
  first$save.importance,
  function(m) extract_share(m, target_vars, feat_names, y2_names, n_y2, n_lag_blocks),
  numeric(1)
)

shares_2 <- vapply(
  second$save.importance,
  function(m) extract_share(m, target_vars, feat_names, y2_names, n_y2, n_lag_blocks),
  numeric(1)
)

dt <- data.table(
  date  = c(oos_dates_1, oos_dates_2),
  share = c(shares_1, shares_2)
)


# ============================================================
# Rolling mean & relative importance
# ============================================================

dt[, smooth_abs := frollmean(share, SMOOTH_WINDOW, align = "right")]
overall_avg <- mean(dt$smooth_abs, na.rm = TRUE)
dt[, smooth_rel := smooth_abs / overall_avg]

cat(sprintf("Variable/Group : %s\n",   plot_label))
cat(sprintf("Avg importance : %.4f%%\n", overall_avg * 100))
cat(sprintf("Smoothed range : %.3fx to %.3fx (relative)\n",
            min(dt$smooth_rel, na.rm = TRUE),
            max(dt$smooth_rel, na.rm = TRUE)))


# ============================================================
# Recession / shock bands
# ============================================================

shock_bands <- data.table(
  label = c("GFC",          "COVID surge"),
  start = as.Date(c("2007-12-01", "2020-02-01")),
  end   = as.Date(c("2009-06-01", "2022-06-01"))
)


# ============================================================
# Plot
# ============================================================

main_color <- "#2980B9"

p <- ggplot(dt[!is.na(smooth_rel)], aes(x = date, y = smooth_rel)) +
  
  # Shock shading
  geom_rect(data = shock_bands,
            aes(xmin = start, xmax = end, ymin = 0, ymax = Inf, fill = label),
            inherit.aes = FALSE, alpha = 0.12) +
  scale_fill_manual(values = c("GFC" = "red", "COVID surge" = "orange"),
                    name = NULL) +
  
  # Area above/below average
  geom_ribbon(aes(ymin = 1, ymax = pmax(smooth_rel, 1)),
              fill = main_color, alpha = 0.22) +
  geom_ribbon(aes(ymin = pmin(smooth_rel, 1), ymax = 1),
              fill = "#E74C3C", alpha = 0.15) +
  
  # Main line
  geom_line(color = main_color, linewidth = 1.0) +
  
  # Average reference line
  geom_hline(yintercept = 1, linetype = "dashed",
             color = "black", linewidth = 0.8, alpha = 0.6) +
  annotate("text",
           x     = min(dt$date[!is.na(dt$smooth_rel)]),
           y     = 1.02,
           label = "Historical average (1.0)",
           hjust = 0, vjust = 0, size = 3, color = "grey30") +
  
  scale_x_date(date_breaks = "2 years", date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(ylim = c(0, NA)) +
  
  labs(
    title    = paste0(plot_label,
                      " — RF variable importance relative to own average"),
    subtitle = paste0(SMOOTH_WINDOW, "-month rolling average | ",
                      "3-month forecast horizon | ",
                      "Blue = above average | Red = below average"),
    x = NULL,
    y = "Relative importance\n(1.0 = own historical average)"
  ) +
  theme_light(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 9, color = "grey40"),
    legend.position  = "bottom",
    legend.key.size  = unit(0.5, "cm"),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(size = 10)
  )

print(p)

# Save
safe_name <- gsub("[^A-Za-z0-9_]", "_", plot_label)
outfile   <- paste0("03_Output/Charts/rf/importance_", safe_name, ".png")
ggsave(outfile, plot = p, width = 13, height = 5, dpi = 300)
cat(sprintf("\nSaved: %s\n", outfile))
# Author: Ece Tasan
# Date  : 5/12/2026
# Scope : Variable importance by 3-year period, 3-month horizon.
#         Stacked bar chart in the style of Medeiros et al. (2021) Figure 7,
#         but with periods on the x-axis instead of forecast horizons.

library(data.table)
library(ggplot2)

rm(list = ls())
options(scipen = 30, digits = 5)


#### Load data & saved RF results ####

fred <- readRDS("02_Input/data_cleaned.rds")
setDT(fred)
setnames(fred, "CPIAUCSL", "inf")
setcolorder(fred, c("date", "inf"))

# Saved RF results for the 3-month horizon (lag = 3 in runrf)
rf1_3 <- readRDS("03_Output/rf1_3.rds")  # OOS 2001-01 to 2015-12 (nprev = 180)
rf2_3 <- readRDS("03_Output/rf2_3.rds")  # OOS 2016-01 to 2024-12 (nprev = 108)


#### Map FRED variables to Medeiros groups (Appendix A, Tables 13-20) ####

group_map <- list(
  output_income = c("RPI","W875RX1","INDPRO","IPFPNSS","IPFINAL","IPCONGD","IPDCONGD",
                    "IPNCONGD","IPBUSEQ","IPMAT","IPDMAT","IPNMAT","IPMANSICS",
                    "IPB51222s","IPB51222S","IPFUELS","NAPMPI","CUMFNS"),
  labor         = c("HWI","HWIURATIO","CLF16OV","CE16OV","UNRATE","UEMPMEAN","UEMPLT5",
                    "UEMP5TO14","UEMP15OV","UEMP15T26","UEMP27OV","CLAIMSx","PAYEMS",
                    "USGOOD","CES1021000001","USCONS","MANEMP","DMANEMP","NDMANEMP",
                    "SRVPRD","USTPU","USWTRADE","USTRADE","USFIRE","USGOVT",
                    "CES0600000007","AWOTMAN","AWHMAN","NAPMEI","CES0600000008",
                    "CES2000000008","CES3000000008"),
  housing       = c("HOUST","HOUSTNE","HOUSTMW","HOUSTS","HOUSTW",
                    "PERMIT","PERMITNE","PERMITMW","PERMITS","PERMITW"),
  consumption   = c("DPCERA3M086SBEA","CMRMTSPLx","RETAILx","NAPM","NAPMNOI","NAPMSDI",
                    "NAPMII","ACOGNO","AMDMNOx","ANDENOx","AMDMUOx","BUSINVx",
                    "ISRATIOx","UMCSENTx"),
  money         = c("M1SL","M2SL","M2REAL","AMBSL","TOTRESNS","NONBORRES","BUSLOANS",
                    "REALLN","NONREVSL","CONSPI","MZMSL","DTCOLNVHFNM","DTCTHFNM",
                    "INVEST"),
  interest_fx   = c("FEDFUNDS","CP3Mx","TB3MS","TB6MS","GS1","GS5","GS10","AAA","BAA",
                    "COMPAPFFx","TB3SMFFM","TB6SMFFM","T1YFFM","T5YFFM","T10YFFM",
                    "AAAFFM","BAAFFM","TWEXMMTH","TWEXAFEGSMTHx","EXSZUSx","EXJPUSx",
                    "EXUSUKx","EXCAUSx"),
  prices        = c("WPSFD49207","WPSFD49502","WPSID61","WPSID62","OILPRICEx","PPICMM",
                    "NAPMPRI","CPIAPPSL","CPITRNSL","CPIMEDSL","CUSR0000SAC",
                    "CUUR0000SAD","CUSR0000SAS","CPIULFSL","CUUR0000SA0L2",
                    "CUSR0000SA0L5","PCEPI","DDURRG3M086SBEA","DNDGRG3M086SBEA",
                    "DSERRG3M086SBEA"),
  stocks        = c("S&P 500","S&P: indust","S&P div yield","S&P PE ratio",
                    "S.P.500","S.P..indust","S.P.div.yield","S.P.PE.ratio","VXOCLSx")
)

# Build a single variable -> group lookup vector
var_to_group <- unlist(lapply(names(group_map), function(g) {
  setNames(rep(g, length(group_map[[g]])), group_map[[g]])
}))

# Order in Y2 (inside runrf): [inf, all other fred vars, Comp.1..Comp.4]
var_names <- setdiff(names(fred), "date")        # "inf" is first
pc_names  <- paste0("Comp.", 1:4)
y2_names  <- c(var_names, pc_names)

# Assign each Y2 column to a group
group_of_var <- function(v) {
  if (v %in% pc_names) return("factors")
  if (v == "inf")      return("ar")              # lagged inflation = AR terms
  g <- var_to_group[v]
  if (is.na(g))        return("other")
  g
}
group_lookup <- vapply(y2_names, group_of_var, character(1))

# Print anything that didn't get mapped, so it can be fixed if needed
unmapped <- y2_names[group_lookup == "other"]
if (length(unmapped) > 0) {
  cat("Variables not mapped to any group (assigned to 'other'):\n")
  print(unmapped)
}


#### Aggregate importance from one rolling window ####

# In runrf with lag = 3, X has 4 lag blocks of Y2 (lags 3, 4, 5, 6).
# Column order: (lag-3 of all Y2), (lag-4 of all Y2), (lag-5...), (lag-6...).
# So row k of the importance matrix corresponds to y2_names[((k-1) %% n_y2) + 1].

aggregate_importance <- function(imp_mat, y2_names, group_lookup) {
  imp_vals <- imp_mat[, 1]                       # %IncMSE column (permutation OOB)
  imp_vals[imp_vals < 0] <- 0                    # clip small negatives
  
  n_y2   <- length(y2_names)
  n_lags <- length(imp_vals) / n_y2              # = 4 for lag = 3
  
  # Reshape: rows = lag blocks, cols = variables
  mat <- matrix(imp_vals, nrow = n_lags, ncol = n_y2, byrow = TRUE)
  colnames(mat) <- y2_names
  
  per_var   <- colSums(mat)                      # one value per variable
  per_group <- tapply(per_var, group_lookup, sum)
  per_group / sum(per_group)                     # normalize so shares sum to 1
}


#### Build long-format table: one row per (date, group) ####

oos_dates_1 <- tail(fred$date[fred$date < as.Date("2016-01-01")], 180)
oos_dates_2 <- tail(fred$date, 108)

build_shares <- function(rf_result, oos_dates) {
  rbindlist(lapply(seq_along(rf_result$save.importance), function(i) {
    s <- aggregate_importance(rf_result$save.importance[[i]], y2_names, group_lookup)
    data.table(date = oos_dates[i], group = names(s), share = as.numeric(s))
  }))
}

shares <- rbind(
  build_shares(rf1_3, oos_dates_1),
  build_shares(rf2_3, oos_dates_2)
)


#### Bucket dates into 3-year periods (matching presentation) ####

periods <- data.table(
  period = c("2002-2004","2005-2007","2008-2010","2011-2013",
             "2014-2016","2017-2019","2020-2022","2023-2025"),
  start  = as.Date(c("2002-01-01","2005-01-01","2008-01-01","2011-01-01",
                     "2014-01-01","2017-01-01","2020-01-01","2023-01-01")),
  end    = as.Date(c("2004-12-31","2007-12-31","2010-12-31","2013-12-31",
                     "2016-12-31","2019-12-31","2022-12-31","2025-12-31"))
)

shares[, period := NA_character_]
for (k in seq_len(nrow(periods))) {
  shares[date >= periods$start[k] & date <= periods$end[k], period := periods$period[k]]
}
shares <- shares[!is.na(period)]

# Average shares within each (period, group), then re-normalize per period
period_shares <- shares[, .(share = mean(share)), by = .(period, group)]
period_shares[, share := share / sum(share), by = period]


#### Plot ####

group_order  <- c("ar","output_income","labor","housing","consumption",
                  "money","interest_fx","prices","stocks","factors","other")
group_labels <- c("ar","output-income","employment","housing","consumption",
                  "money","interest-exchange","prices","stocks","factors","other")
group_colors <- c("black","red","forestgreen","blue","cyan",
                  "magenta","gold","grey75","orange","darkred","grey40")

period_shares[, group  := factor(group,  levels = group_order, labels = group_labels)]
period_shares[, period := factor(period, levels = periods$period)]

plot_vi <- ggplot(period_shares, aes(x = period, y = share, fill = group)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = setNames(group_colors, group_labels), drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Importance share", fill = NULL,
       title    = "Random Forest variable importance by period",
       subtitle = "3-month horizon; variables grouped as in Medeiros et al. (2021)") +
  theme_light(base_size = 13) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 12),
    legend.position = "bottom",
    legend.text  = element_text(size = 11),
    plot.title   = element_text(face = "bold")
  )

ggsave(
  filename = "03_Output/Charts/rf/variable_importance_by_period.pngby_period.png",
  plot     = plot_vi,
  width    = 12,
  height   = 6,
  dpi      = 300
)

print(plot_vi)
# Author: Ece Tasan
# Date  : 5/26/2026
# Scope : Line chart of Random Forest variable-group importance over time,
#         12-month rolling, 3-month horizon. Uses the 8 Medeiros et al. (2021)
#         FRED-MD groups (Tables 13-20). "ar" (lagged CPI) and "factors" (PCs)
#         are excluded; "inf" is folded into the Prices group, since Medeiros
#         classifies CPIAUCSL under Prices (Group 7).
#
#         Lags of the same variable are summed before computing the share, then
#         shares are aggregated by group and re-normalised so the eight plotted
#         lines add to 100% in every window.
#
#         All NAPM/ISM (PMI) series are missing in the 2026 FRED-MD vintage —
#         the script lists them in the mapping so the lookup is safe, but they
#         will not contribute. Diagnostic prints show what each group covers
#         and which variables fell through to "other" (then dropped).

library(data.table)
library(ggplot2)
library(scales)

rm(list = ls())
options(scipen = 30, digits = 5)


# ============================================================
# Load data and saved RF results (full-sample training)
# ============================================================

fred <- readRDS("02_Input/data_cleaned.rds")
setDT(fred)
setnames(fred, "CPIAUCSL", "inf")
setcolorder(fred, c("date", "inf"))

rf1_3 <- readRDS("03_Output/rf1_3.rds")   # OOS 2001-01 .. 2015-12  (180 windows)
rf2_3 <- readRDS("03_Output/rf2_3.rds")   # OOS 2016-01 .. 2024-12  (108 windows)


# ============================================================
# Feature ordering (must match runrf)
# ============================================================

var_names    <- setdiff(names(fred), "date")
pc_names     <- paste0("Comp.", 1:4)
y2_names     <- c(var_names, pc_names)
n_y2         <- length(y2_names)
n_lag_blocks <- 4L
stopifnot(n_y2 == 130L)


# ============================================================
# Medeiros et al. (2021) 8 groups (FRED-MD Appendix A, Tables 13-20)
# ============================================================
# Series flagged with a trailing "(missing)" comment are the NAPM/ISM PMI-family
# series that were discontinued by ISM around 2019 and are absent from the 2026
# vintage. Listed for transparency; they harmlessly map to their group but the
# importance lookup will not find them.

group_map <- list(

  output_income = c(
    "RPI","W875RX1","INDPRO","IPFPNSS","IPFINAL","IPCONGD","IPDCONGD",
    "IPNCONGD","IPBUSEQ","IPMAT","IPDMAT","IPNMAT","IPMANSICS",
    "IPB51222s","IPB51222S","IPFUELS","CUMFNS",
    "NAPMPI"                                       # PMI: missing
  ),

  employment    = c(
    "HWI","HWIURATIO","CLF16OV","CE16OV","UNRATE","UEMPMEAN","UEMPLT5",
    "UEMP5TO14","UEMP15OV","UEMP15T26","UEMP27OV","CLAIMSx","PAYEMS",
    "USGOOD","CES1021000001","USCONS","MANEMP","DMANEMP","NDMANEMP",
    "SRVPRD","USTPU","USWTRADE","USTRADE","USFIRE","USGOVT",
    "CES0600000007","AWOTMAN","AWHMAN",
    "CES0600000008","CES2000000008","CES3000000008",
    "NAPMEI"                                       # PMI employment: missing
  ),

  housing       = c(
    "HOUST","HOUSTNE","HOUSTMW","HOUSTS","HOUSTW",
    "PERMIT","PERMITNE","PERMITMW","PERMITS","PERMITW"
  ),

  consumption   = c(
    "DPCERA3M086SBEA","CMRMTSPLx","RETAILx",
    "ACOGNO","AMDMNOx","ANDENOx","AMDMUOx",
    "BUSINVx","ISRATIOx","UMCSENTx",
    "NAPM","NAPMNOI","NAPMSDI","NAPMII"            # PMI family: missing
  ),

  money         = c(
    "M1SL","M2SL","M2REAL","AMBSL","BOGMBASE","TOTRESNS","NONBORRES",
    "BUSLOANS","REALLN","NONREVSL","CONSPI","MZMSL",
    "DTCOLNVHFNM","DTCTHFNM","INVEST"
  ),

  interest_fx   = c(
    "FEDFUNDS","CP3Mx","TB3MS","TB6MS","GS1","GS5","GS10","AAA","BAA",
    "COMPAPFFx","TB3SMFFM","TB6SMFFM","T1YFFM","T5YFFM","T10YFFM",
    "AAAFFM","BAAFFM",
    "TWEXMMTH","TWEXAFEGSMTHx","EXSZUSx","EXJPUSx","EXUSUKx","EXCAUSx"
  ),

  prices        = c(
    "inf","CPIAUCSL",                              # CPI = Group 7 in Medeiros
    "WPSFD49207","WPSFD49502","WPSID61","WPSID62",
    "OILPRICEx","PPICMM",
    "CPIAPPSL","CPITRNSL","CPIMEDSL",
    "CUSR0000SAC","CUUR0000SAD","CUSR0000SAS","CPIULFSL",
    "CUUR0000SA0L2","CUSR0000SA0L5",
    "PCEPI","DDURRG3M086SBEA","DNDGRG3M086SBEA","DSERRG3M086SBEA",
    "NAPMPRI"                                      # PMI prices: missing
  ),

  stocks        = c(
    "S&P 500","S&P: indust","S&P div yield","S&P PE ratio",
    "S.P.500","S.P..indust","S.P.div.yield","S.P.PE.ratio",
    "SP500","SP_div_yield","SP_PE_ratio",
    "VXOCLSx"
  )
)

var_to_group <- unlist(lapply(names(group_map), function(g)
  setNames(rep(g, length(group_map[[g]])), group_map[[g]])
))

# "ar" (inf as lagged target) and "factors" (PCs) are not plotted.
# Folding inf into prices (per Medeiros) means "ar" no longer exists separately.
# PCs go to "other" and get dropped.
group_of_var <- function(v) {
  if (v %in% pc_names) return("other")            # PCs -> dropped
  g <- var_to_group[v]
  if (is.na(g))        return("other")
  g
}
group_lookup <- vapply(y2_names, group_of_var, character(1))

cat("Variables per group:\n")
print(table(group_lookup))
cat("\nVariables ending up in 'other' (excluded from plot):\n")
print(y2_names[group_lookup == "other"])


# ============================================================
# Aggregate one rolling window: sum 4 lags per variable, then by group
# ============================================================

aggregate_importance <- function(imp_mat, y2_names, group_lookup,
                                 n_y2 = 130L, n_lag_blocks = 4L) {
  vals <- imp_mat[, 1]                              # %IncMSE
  vals[vals < 0] <- 0
  stopifnot(length(vals) == n_y2 * n_lag_blocks)

  lag_mat <- matrix(vals, nrow = n_lag_blocks, ncol = n_y2, byrow = TRUE)
  colnames(lag_mat) <- y2_names

  per_var   <- colSums(lag_mat)
  per_group <- tapply(per_var, group_lookup, sum)
  per_group / sum(per_group)                        # raw shares incl. "other"
}


# ============================================================
# Long-format table across all rolling windows
# ============================================================

oos_dates_1 <- tail(fred$date[fred$date < as.Date("2016-01-01")], 180)
oos_dates_2 <- tail(fred$date, 108)
stopifnot(length(rf1_3$save.importance) == length(oos_dates_1))
stopifnot(length(rf2_3$save.importance) == length(oos_dates_2))

build_shares <- function(rf_result, oos_dates) {
  rbindlist(lapply(seq_along(rf_result$save.importance), function(i) {
    s <- aggregate_importance(rf_result$save.importance[[i]],
                              y2_names, group_lookup)
    data.table(date = oos_dates[i], group = names(s), share = as.numeric(s))
  }))
}

shares <- rbind(
  build_shares(rf1_3, oos_dates_1),
  build_shares(rf2_3, oos_dates_2)
)

# Drop "other" (PCs / unmapped) and re-normalise so the 8 plotted groups
# add to 100% within each window.
shares <- shares[group != "other"]
shares[, share := share / sum(share), by = date]
shares[, pct   := 100 * share]


# ============================================================
# 12-month right-aligned rolling mean (per group)
# ============================================================

setorder(shares, group, date)
shares[, pct_roll := frollmean(pct, n = 12, align = "right"), by = group]


# ============================================================
# Cosmetic labels + Medeiros-style colors
# ============================================================

group_order  <- c("output_income","employment","housing","consumption",
                  "money","interest_fx","prices","stocks")
group_labels <- c("Output & income","Employment","Housing","Consumption",
                  "Money & credit","Interest & exchange","Prices","Stocks")
group_colors <- c("#e74c3c",        # red    -> output-income
                  "#2ecc71",        # green  -> employment
                  "#1f4e79",        # blue   -> housing
                  "#56b4e9",        # cyan   -> consumption
                  "#e84393",        # magenta-> money
                  "#f1c40f",        # yellow -> interest-exchange
                  "#7f8c8d",        # grey   -> prices
                  "#e67e22")        # orange -> stocks

shares[, group := factor(group, levels = group_order, labels = group_labels)]


# ============================================================
# Shock highlight bands
# ============================================================

gfc_start <- as.Date("2007-12-01")
gfc_end   <- as.Date("2009-06-01")
gfc_line  <- as.Date("2008-09-15")
cov_start <- as.Date("2020-02-01")
cov_end   <- as.Date("2022-06-01")
cov_line  <- as.Date("2020-03-01")


# ============================================================
# Plot
# ============================================================

p <- ggplot(shares[!is.na(pct_roll)], aes(x = date, y = pct_roll,
                                          colour = group)) +
  annotate("rect", xmin = gfc_start, xmax = gfc_end,
           ymin = -Inf, ymax = Inf, fill = "#e74c3c", alpha = 0.08) +
  annotate("rect", xmin = cov_start, xmax = cov_end,
           ymin = -Inf, ymax = Inf, fill = "#f39c12", alpha = 0.08) +
  geom_vline(xintercept = gfc_line, linetype = "dashed",
             colour = "#c0392b", linewidth = 0.4) +
  geom_vline(xintercept = cov_line, linetype = "dashed",
             colour = "#e67e22", linewidth = 0.4) +
  annotate("text", x = gfc_line, y = Inf, label = "Lehman",
           vjust = 1.5, hjust = -0.05, size = 3.2, colour = "#c0392b") +
  annotate("text", x = cov_line, y = Inf, label = "COVID",
           vjust = 1.5, hjust = -0.05, size = 3.2, colour = "#e67e22") +

  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = setNames(group_colors, group_labels),
                      name = NULL) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.04))) +
  scale_y_continuous(labels = label_number(accuracy = 0.1),
                     expand = expansion(mult = c(0.02, 0.08))) +
  labs(
    title    = "Variable group importance over time — 12-month rolling average",
    subtitle = "3-month horizon Random Forest",
    x = NULL,
    y = "Importance share (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(colour = "grey30"),
    legend.position  = "bottom",
    legend.key.width = unit(0.9, "cm"),
    legend.text      = element_text(size = 9),
    legend.margin    = margin(t = 6, b = 2),
    panel.grid.minor = element_blank(),
    plot.margin      = margin(t = 10, r = 18, b = 6, l = 10)
  ) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE))

print(p)

outdir <- "03_Output/Charts/rf"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
outfile <- file.path(outdir, "line_chart_var_imp_8groups.png")
ggsave(outfile, plot = p, width = 14, height = 6, dpi = 300)
cat(sprintf("\nSaved: %s\n", outfile))

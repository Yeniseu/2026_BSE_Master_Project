# ============================================================================
# 63_VarImp_8Groups.R - Section 5.3 figure
# 12-month rolling Random Forest variable importance, aggregated into the eight
# FRED-MD groups of McCracken & Ng (2016) / Medeiros et al. (2021).
#
# Input : 03_Output/Preds/rf_importance_h3_w*.rds (written by 18_RandomForest.R)
# Output: Figures/Var_Imp/line_chart_var_imp_8groups.png
#
# The four lags of a variable are summed before the shares are computed. The
# principal components are dropped, and CPI is folded into Prices (its FRED-MD
# group), so the eight plotted lines are re-normalised to 100% in every window.
# ============================================================================
source("01_RScript/00_2_Config.R")
library(ggplot2)
library(scales)

imp_file <- file.path(P_PRED, paste0("rf_importance_h3", WTAG, ".rds"))
stopifnot(file.exists(imp_file))
obj   <- readRDS(imp_file)
dates <- obj$dates
imps  <- obj$importance
stopifnot(length(dates) == length(imps))

# ---- the eight FRED-MD groups ----------------------------------------------
GROUPS <- list(
  output_income = c("RPI","W875RX1","INDPRO","IPFPNSS","IPFINAL","IPCONGD","IPDCONGD",
                    "IPNCONGD","IPBUSEQ","IPMAT","IPDMAT","IPNMAT","IPMANSICS",
                    "IPB51222s","IPB51222S","IPFUELS","CUMFNS","NAPMPI"),
  employment    = c("HWI","HWIURATIO","CLF16OV","CE16OV","UNRATE","UEMPMEAN","UEMPLT5",
                    "UEMP5TO14","UEMP15OV","UEMP15T26","UEMP27OV","CLAIMSx","PAYEMS",
                    "USGOOD","CES1021000001","USCONS","MANEMP","DMANEMP","NDMANEMP",
                    "SRVPRD","USTPU","USWTRADE","USTRADE","USFIRE","USGOVT",
                    "CES0600000007","AWOTMAN","AWHMAN","CES0600000008",
                    "CES2000000008","CES3000000008","NAPMEI"),
  housing       = c("HOUST","HOUSTNE","HOUSTMW","HOUSTS","HOUSTW",
                    "PERMIT","PERMITNE","PERMITMW","PERMITS","PERMITW"),
  consumption   = c("DPCERA3M086SBEA","CMRMTSPLx","RETAILx","ACOGNO","AMDMNOx",
                    "ANDENOx","AMDMUOx","BUSINVx","ISRATIOx","UMCSENTx",
                    "NAPM","NAPMNOI","NAPMSDI","NAPMII"),
  money         = c("M1SL","M2SL","M2REAL","AMBSL","BOGMBASE","TOTRESNS","NONBORRES",
                    "BUSLOANS","REALLN","NONREVSL","CONSPI","MZMSL",
                    "DTCOLNVHFNM","DTCTHFNM","INVEST"),
  interest_fx   = c("FEDFUNDS","CP3Mx","TB3MS","TB6MS","GS1","GS5","GS10","AAA","BAA",
                    "COMPAPFFx","TB3SMFFM","TB6SMFFM","T1YFFM","T5YFFM","T10YFFM",
                    "AAAFFM","BAAFFM","TWEXMMTH","TWEXAFEGSMTHx","EXSZUSx","EXJPUSx",
                    "EXUSUKx","EXCAUSx"),
  prices        = c("inf","CPIAUCSL","WPSFD49207","WPSFD49502","WPSID61","WPSID62",
                    "OILPRICEx","PPICMM","CPIAPPSL","CPITRNSL","CPIMEDSL",
                    "CUSR0000SAC","CUUR0000SAD","CUSR0000SAS","CPIULFSL",
                    "CUUR0000SA0L2","CUSR0000SA0L5","PCEPI","DDURRG3M086SBEA",
                    "DNDGRG3M086SBEA","DSERRG3M086SBEA","NAPMPRI"),
  stocks        = c("S&P 500","S&P: indust","S&P div yield","S&P PE ratio",
                    "S.P.500","S.P..indust","S.P.div.yield","S.P.PE.ratio","VXOCLSx")
)
var2group <- unlist(lapply(names(GROUPS), function(g)
  setNames(rep(g, length(GROUPS[[g]])), GROUPS[[g]])))

# ---- importance -> group shares, one row per rolling window ------------------
# Feature names are "<var>_l<k>"; strip the lag suffix and sum over lags.
shares <- rbindlist(lapply(seq_along(imps), function(i) {
  v <- imps[[i]]
  v[v < 0] <- 0                                   # negative %IncMSE -> 0
  base <- sub("_l[0-9]+$", "", names(v))
  per_var   <- tapply(v, base, sum)
  g         <- var2group[names(per_var)]
  per_var   <- per_var[!is.na(g)]                 # drops the PCs and dummies
  per_group <- tapply(per_var, g[!is.na(g)], sum)
  data.table(date  = dates[i],
             group = names(per_group),
             pct   = 100 * as.numeric(per_group) / sum(per_group))
}))

setorder(shares, group, date)
shares[, pct_roll := frollmean(pct, 12, align = "right"), by = group]

LAB <- c(output_income = "Output & income", employment = "Employment",
         housing = "Housing", consumption = "Consumption",
         money = "Money & credit", interest_fx = "Interest & exchange",
         prices = "Prices", stocks = "Stocks")
COL <- c("#6161BA","#34BA66","#B84444","#C8B84A","#3A3A3A","#E07B3A","#9B59B6","#5BB8C4")
shares[, group := factor(LAB[group], levels = LAB)]

gg <- ggplot(shares[!is.na(pct_roll)], aes(date, pct_roll, colour = group)) +
  annotate("rect", xmin = as.Date("2007-12-01"), xmax = as.Date("2009-06-01"),
           ymin = -Inf, ymax = Inf, fill = "#CCCCDD", alpha = 0.5) +
  annotate("rect", xmin = as.Date("2020-02-01"), xmax = as.Date("2022-06-01"),
           ymin = -Inf, ymax = Inf, fill = "#CCCCDD", alpha = 0.5) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = setNames(COL, LAB), name = NULL) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  labs(x = NULL, y = "Importance share (%)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank()) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE))

ggsave(file.path(F_VAR, "line_chart_var_imp_8groups.png"), gg,
       width = 7, height = 3.5, dpi = 300)
cat("variable-importance figure written\n")
print(shares[, .(mean_pct = mean(pct, na.rm = TRUE)), by = group][order(-mean_pct)])

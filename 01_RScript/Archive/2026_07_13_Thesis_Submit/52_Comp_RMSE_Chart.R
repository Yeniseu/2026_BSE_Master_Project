# Author: Ece Tasan, Orhun Ozel
# Date: 5/12/2025
# Scope: Rolling RMSE for Model Comparison

rm(list = ls())
library(ggplot2)
library(data.table)
library(gt)
library(glmnet)
library(quadprog)
library(kableExtra)
library(readxl)
options(scipen=30, digits=3)
options(datatable.print.trunc.cols = T)
options(datatable.print.nrows      = 15)


option <- 1
if (option==1) {path = ""}
if (option==2) {path = "_20"}
if (option==3) {path = "_30"}
if (option==4) {path = "_40"}

# ── Shared colour palette (matches RMSE_Chart_3_Months_Ensemble.png) ──────────
model_colors <- c(
  "Linear Phillips Curve"              = "#6161BA",   # salmon / pink-red
  "Non-Linear Phillips Curve"          = "#34BA66",   # green
  "Linear with Variable Selection"     = "#B84444",   # olive / yellow-green
  "Non-Linear and Var. Selection"      = "#C8B84A",   # cyan / teal  (ensemble charts)
  "Non-Linear and Variable Selection"  = "#C8B84A",   # cyan / teal  (non-ensemble chart)
  "Ensemble Models"                    = "#3A3A3A",    # orchid / purple
  "Cleveland FED"                      = "#E07B3A"   
)
# ─────────────────────────────────────────────────────────────────────────────



#### Prepare Data and Functions for Sample 1 -----------------------------------
cl_fed <- read_xlsx("02_Input/FED_Cleveland_MoM.xlsx", sheet="CL_FED") |> as.data.table()
cl_fed <- cl_fed[, .(date = as.Date(Date), cl_fed = `CPI Inflation`)]
setkey(cl_fed, "date")

lasso1 <- readRDS(paste0("03_Output/lasso_pred_s1", path, ".rds"))
lasso1_1 <- lasso1[, c("real", "lasso_l1", "ridge_l1", "elnet_l1", "rw_l1")]
setnames(lasso1_1, c("real", "lasso_l1", "ridge_l1", "elnet_l1", "rw_l1"),
         c("real", "LASSO", "Ridge", "ElNet", "RW"))
lasso1_3 <- lasso1[, c("real", "lasso_l3", "ridge_l3", "elnet_l3", "rw_l3")]
setnames(lasso1_3, c("real", "lasso_l3", "ridge_l3", "elnet_l3", "rw_l3"),
         c("real", "LASSO", "Ridge", "ElNet", "RW"))
lasso1_labor <- readRDS(paste0("03_Output/lasso_pred_s1_labor_indicators",path ,".rds"))
lasso1_1_labor <- lasso1_labor[, c("lasso_l1", "ridge_l1", "elnet_l1")]
setnames(lasso1_1_labor, c("lasso_l1", "ridge_l1", "elnet_l1"),
         c("LASSO_L", "Ridge_L", "ElNet_L"))
lasso1_3_labor <- lasso1_labor[, c("lasso_l3", "ridge_l3", "elnet_l3")]
setnames(lasso1_3_labor, c("lasso_l3", "ridge_l3", "elnet_l3"),
         c("LASSO_L", "Ridge_L", "ElNet_L"))


mean1_h1 <- readRDS("03_Output/AR_SM/p1_h1_mean.rds")
mean1_h1 <- mean1_h1$pred
mean1_h1 <- as.data.table(mean1_h1)
setnames(mean1_h1, "V1", "RSM")
mean1_h3 <- readRDS("03_Output/AR_SM/p1_h3_mean.rds")
mean1_h3 <- mean1_h3$pred
mean1_h3 <- as.data.table(mean1_h3)
setnames(mean1_h3, "V1", "RSM")


p1_h1_ar4 <- readRDS("03_Output/AR_SM/p1_h1_ar4.rds")
p1_h1_ar4 <- p1_h1_ar4$pred
p1_h1_ar4 <- as.data.table(p1_h1_ar4)
setnames(p1_h1_ar4, "V1", "AR")
p1_h3_ar4 <- readRDS("03_Output/AR_SM/p1_h3_ar4.rds")
p1_h3_ar4 <- p1_h3_ar4$pred
p1_h3_ar4 <- as.data.table(p1_h3_ar4)
setnames(p1_h3_ar4, "V1", "AR")


rf1_1 <- readRDS(paste0("03_Output/rf1_1",path,".rds"))
rf1_1 <- rf1_1$pred
rf1_1 <- as.data.table(rf1_1)
setnames(rf1_1, "V1", "RF")
rf1_3 <- readRDS(paste0("03_Output/rf1_3",path,".rds"))
rf1_3 <- rf1_3$pred
rf1_3 <- as.data.table(rf1_3)
setnames(rf1_3, "V1", "RF")
rf1_1_labor <- readRDS(paste0("03_Output/rf1_1", path, "_labor.rds"))
rf1_1_labor <- rf1_1_labor$pred
rf1_1_labor <- as.data.table(rf1_1_labor)
setnames(rf1_1_labor, "V1", "RF_L")
rf1_3_labor <- readRDS(paste0("03_Output/rf1_3", path, "_labor.rds"))
rf1_3_labor <- rf1_3_labor$pred
rf1_3_labor <- as.data.table(rf1_3_labor)
setnames(rf1_3_labor, "V1", "RF_L")


llf1   <- readRDS(paste0("03_Output/llf_s1", path, ".rds"))
llf1_1 <- llf1[, .(LLF = llf1_1)]
llf1_3 <- llf1[, .(LLF = llf1_3)]
llf1_labor   <- readRDS(paste0("03_Output/llf_s1_labor",path,".rds"))
llf1_1_labor <- llf1_labor[, .(LLF_L = llf1_1)]
llf1_3_labor <- llf1_labor[, .(LLF_L = llf1_3)]


all1_1 <- cbind(lasso1_1, mean1_h1, p1_h1_ar4, rf1_1, llf1_1, lasso1_1_labor, rf1_1_labor, llf1_1_labor)
all1_3 <- cbind(lasso1_3, mean1_h3, p1_h3_ar4, rf1_3, llf1_3, lasso1_3_labor, rf1_3_labor, llf1_3_labor)

dates <- seq(as.Date("2001-01-01"), as.Date("2015-12-01"), by = "month")
all1_1[, date := dates]
all1_3[, date := dates]
setcolorder(all1_1, "date")
setcolorder(all1_3, "date")
all1_1
all1_3

# Calculate Errors
all1_1_err <- copy(all1_1)
cols <- setdiff(names(all1_1_err), c("real", "date"))
all1_1_err[, (cols) := lapply(.SD, function(x) x - real), .SDcols = cols]
all1_1_err_long <- melt(all1_1_err[, -c("real")], id.vars = "date") 

all1_1_rmse_yearly <- copy(all1_1_err)
all1_1_rmse_yearly[, date := year(date)]
all1_1_rmse_yearly <- all1_1_rmse_yearly[, (cols) := lapply(.SD, function(x) sqrt(mean(x^2))), by = "date", .SDcols=cols]
all1_1_rmse_yearly <- unique(all1_1_rmse_yearly[, -c("real")])
setnames(all1_1_rmse_yearly, "date", "Year")
all1_1_rmse_yearly_long <- melt(all1_1_rmse_yearly, id.vars = "Year") 
all1_1_rmse_yearly

ggplot(all1_1_err_long[variable %in% c("AR", "RSM", "Ridge")],
       aes(x = date, y=value, color=variable, group=variable)) +
  geom_line(size = 0.8) +
  theme_light() +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y") +
  labs(
    y = "Cumulative absolute error",
    colour = "Model"
  )

caption <- "Forecast RMSE (Yearly Average Periods)"
ggplot(all1_1_rmse_yearly_long, aes(x=Year, y=value, color=variable, group=variable)) +
  geom_point(size = 4) + 
  scale_color_viridis_d(option = "H") +
  labs(title=caption, x="Year", y="Output Per Worker Index", color="") +
  theme_minimal(base_family="Computer Modern") + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
#ggsave(paste0("03_Output/Exercise c/Growth_Acc_Cumulative_hclc", path,".png"), width = 7, height = 5)

gt_table <- function(data, title, subtitle) {
  res <- data |>
    gt() |>
    tab_header(title=md(title), subtitle=subtitle) |>
    cols_align(align = "center") |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |> 
    data_color(columns = -Year, direction = "row",    
               palette = c("dodgerblue", "white", "firebrick")) |>
    tab_options(table.font.names = "Consolas")
  return(res)
}


gt_table_shocks <- function(data, title, subtitle) {
  res <- data |>
    gt() |>
    tab_header(title=md(title), subtitle=subtitle) |>
    cols_align(align = "center") |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |> 
    tab_style(
      style = list(cell_borders(sides = c("top", "bottom"), color = "black", 
                                weight = px(3)), style = cell_fill(color = "#E8E8E8")),
      locations = cells_body(rows = Year %in% c("2008-2010", "2020-2022", "2008-10", "2020-22", "Average", "Average All", "Av. After 2010"))
    ) |>
    data_color(columns = -Year, direction = "row",    
               palette = c("dodgerblue", "white", "firebrick")) |>
    tab_options(table.font.names = "Consolas")
  return(res)
}



## Comparisons RMSE Year by Year  
title    <- "**Yearly RMSE by Model**"
(Sample1_Step1 <- gt_table(all1_1_rmse_yearly, title, "3-Step Ahead"))
#gtsave(Sample1_Step1, paste0("03_Output/RMSE",path,"/Sample1_Step3",path,".png"))

# Calculate Errors 3 Step
all1_3_err <- copy(all1_3)
cols <- setdiff(names(all1_3_err), c("real", "date"))
all1_3_err[, (cols) := lapply(.SD, function(x) x - real), .SDcols = cols]
all1_3_err_long <- melt(all1_3_err[, -c("real")], id.vars = "date") 

all1_3_rmse_yearly <- copy(all1_3_err)
all1_3_rmse_yearly[, date := year(date)]
all1_3_rmse_yearly <- all1_3_rmse_yearly[, (cols) := lapply(.SD, function(x) sqrt(mean(x^2))), by = "date", .SDcols=cols]
all1_3_rmse_yearly <- unique(all1_3_rmse_yearly[, -c("real")])
setnames(all1_3_rmse_yearly, "date", "Year")
all1_3_rmse_yearly_long <- melt(all1_3_rmse_yearly, id.vars = "Year") 
all1_3_rmse_yearly


ggplot(all1_3_err_long[variable %in% c("AR", "RSM", "Ridge")],
       aes(x = date, y=value, color=variable, group=variable)) +
  geom_line(size = 0.8) +
  theme_light() +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y") +
  labs(
    y = "Cumulative absolute error",
    colour = "Model"
  )

caption <- "Forecast RMSE (Yearly Average Periods)"
ggplot(all1_3_rmse_yearly_long, aes(x=Year, y=value, color=variable, group=variable)) +
  geom_point(size = 4) + 
  scale_color_viridis_d(option = "H") +
  labs(title=caption, x="Year", y="Output Per Worker Index", color="") +
  theme_minimal(base_family="Computer Modern") + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
#ggsave(paste0("03_Output/Exercise c/Growth_Acc_Cumulative_hclc", path,".png"), width=7, height=5)



(Sample1_Step3 <- gt_table(all1_3_rmse_yearly, title, "3-Step Ahead"))
#gtsave(Sample1_Step3, paste0("03_Output/RMSE",path,"/Sample1_Step3",path,".png")





#### Prepare Data and Functions for Sample 2 -----------------------------------
lasso2 <- readRDS(paste0("03_Output/lasso_pred_s2",path,".rds"))
lasso2_1 <- lasso2[, c("real", "lasso_l1", "ridge_l1", "elnet_l1", "rw_l1")]
setnames(lasso2_1, c("real", "lasso_l1", "ridge_l1", "elnet_l1", "rw_l1"),
         c("real", "LASSO", "Ridge", "ElNet", "RW"))
lasso2_3 <- lasso2[, c("real", "lasso_l3", "ridge_l3", "elnet_l3", "rw_l3")]
setnames(lasso2_3, c("real", "lasso_l3", "ridge_l3", "elnet_l3", "rw_l3"),
         c("real", "LASSO", "Ridge", "ElNet", "RW"))
lasso2_labor <- readRDS(paste0("03_Output/lasso_pred_s2_labor_indicators",path ,".rds"))
lasso2_1_labor <- lasso2_labor[, c("lasso_l1", "ridge_l1", "elnet_l1")]
setnames(lasso2_1_labor, c("lasso_l1", "ridge_l1", "elnet_l1"),
         c("LASSO_L", "Ridge_L", "ElNet_L"))
lasso2_3_labor <- lasso2_labor[, c("lasso_l3", "ridge_l3", "elnet_l3")]
setnames(lasso2_3_labor, c("lasso_l3", "ridge_l3", "elnet_l3"),
         c("LASSO_L", "Ridge_L", "ElNet_L"))


mean2_h1 <- readRDS("03_Output/AR_SM/p2_h1_mean.rds")
mean2_h1 <- mean2_h1$pred
mean2_h1 <- as.data.table(mean2_h1)
setnames(mean2_h1, "V1", "RSM")
mean2_h3 <- readRDS("03_Output/AR_SM/p2_h3_mean.rds")
mean2_h3 <- mean2_h3$pred
mean2_h3 <- as.data.table(mean2_h3)
setnames(mean2_h3, "V1", "RSM")
p2_h1_ar4 <- readRDS("03_Output/AR_SM/p2_h1_ar4.rds")
p2_h1_ar4 <- p2_h1_ar4$pred
p2_h1_ar4 <- as.data.table(p2_h1_ar4)
setnames(p2_h1_ar4, "V1", "AR")
p2_h3_ar4 <- readRDS("03_Output/AR_SM/p2_h3_ar4.rds")
p2_h3_ar4 <- p2_h3_ar4$pred
p2_h3_ar4 <- as.data.table(p2_h3_ar4)
setnames(p2_h3_ar4, "V1", "AR")



rf2_1 <- readRDS(paste0("03_Output/rf2_1",path,".rds"))
rf2_1 <- rf2_1$pred
rf2_1 <- as.data.table(rf2_1)
setnames(rf2_1, "V1", "RF")
rf2_3 <- readRDS("03_Output/rf2_3.rds")
rf2_3 <- readRDS(paste0("03_Output/rf2_3",path,".rds"))
rf2_3 <- rf2_3$pred
rf2_3 <- as.data.table(rf2_3)
setnames(rf2_3, "V1", "RF")
rf2_1_labor <- readRDS(paste0("03_Output/rf2_1", path, "_labor.rds"))
rf2_1_labor <- rf2_1_labor$pred
rf2_1_labor <- as.data.table(rf2_1_labor)
setnames(rf2_1_labor, "V1", "RF_L")
rf2_3_labor <- readRDS(paste0("03_Output/rf2_3", path, "_labor.rds"))
rf2_3_labor <- rf2_3_labor$pred
rf2_3_labor <- as.data.table(rf2_3_labor)
setnames(rf2_3_labor, "V1", "RF_L")

llf2   <- readRDS(paste0("03_Output/llf_s2", path, ".rds"))
llf2_1 <- llf2[, .(LLF = llf2_1)]
llf2_3 <- llf2[, .(LLF = llf2_3)]
llf2_labor   <- readRDS(paste0("03_Output/llf_s2_labor",path,".rds"))
llf2_1_labor  <- llf2_labor[, .(LLF_L = llf2_1)]
llf2_3_labor  <- llf2_labor[, .(LLF_L = llf2_3)]


all2_1 <- cbind(lasso2_1, mean2_h1, p2_h1_ar4, rf2_1, llf2_1, lasso2_1_labor, rf2_1_labor, llf2_1_labor)
all2_3 <- cbind(lasso2_3, mean2_h3, p2_h3_ar4, rf2_3, llf2_3, lasso2_3_labor, rf2_3_labor, llf2_3_labor)

dates <- seq(
  from = as.Date("2016-01-01"),
  to   = as.Date("2024-12-01"),
  by   = "month"
)


dates <- seq(as.Date("2016-01-01"), as.Date("2024-12-01"), by = "month")
all2_1[, date := dates]
all2_3[, date := dates]
setcolorder(all2_1, "date")
setcolorder(all2_3, "date")
all2_1
all2_3

# Calculate Errors
all2_1_err <- copy(all2_1)
cols <- setdiff(names(all2_1_err), c("real", "date"))
all2_1_err[, (cols) := lapply(.SD, function(x) x - real), .SDcols = cols]
all2_1_err_long <- melt(all2_1_err[, -c("real")], id.vars = "date") 

all2_1_rmse_yearly <- copy(all2_1_err)
all2_1_rmse_yearly[, date := year(date)]
all2_1_rmse_yearly <- all2_1_rmse_yearly[, (cols) := lapply(.SD, function(x) sqrt(mean(x^2))), by = "date", .SDcols=cols]
all2_1_rmse_yearly <- unique(all2_1_rmse_yearly[, -c("real")])
setnames(all2_1_rmse_yearly, "date", "Year")
all2_1_rmse_yearly_long <- melt(all2_1_rmse_yearly, id.vars = "Year") 
all2_1_rmse_yearly

ggplot(all2_1_err_long[variable %in% c("AR", "RSM", "RF")],
       aes(x = date, y=value, color=variable, group=variable)) +
  geom_line(size = 0.8) +
  theme_light() +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y") +
  labs(
    y = "Cumulative absolute error",
    colour = "Model"
  )

caption <- "Forecast RMSE (Yearly Average Periods)"
ggplot(all2_1_rmse_yearly_long, aes(x=Year, y=value, color=variable, group=variable)) +
  geom_point(size = 4) + 
  scale_color_viridis_d(option = "H") +
  labs(title=caption, x="Year", y="Output Per Worker Index", color="") +
  theme_minimal(base_family="Computer Modern") + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
#ggsave(paste0("03_Output/Exercise c/Growth_Acc_Cumulative_hclc", path, ".png"), width=7, height=5)
#gtsave(Sample2_Step1, filename = paste0("03_Output/RMSE", path, "/Sample2_Step1", path, ".png"))




# Calculate Errors
all2_3_err <- copy(all2_3)
cols <- setdiff(names(all2_3_err), c("real", "date"))
all2_3_err[, (cols) := lapply(.SD, function(x) x - real), .SDcols = cols]
all2_3_err_long <- melt(all2_3_err[, -c("real")], id.vars = "date") 

all2_3_rmse_yearly <- copy(all2_3_err)
all2_3_rmse_yearly[, date := year(date)]
all2_3_rmse_yearly <- all2_3_rmse_yearly[, (cols) := lapply(.SD, function(x) sqrt(mean(x^2))), by = "date", .SDcols=cols]
all2_3_rmse_yearly <- unique(all2_3_rmse_yearly[, -c("real")])
setnames(all2_3_rmse_yearly, "date", "Year")
all2_3_rmse_yearly_long <- melt(all2_3_rmse_yearly, id.vars = "Year") 
all2_3_rmse_yearly

ggplot(all2_3_err_long[variable %in% c("AR", "RSM", "RF")],
       aes(x = date, y=value, color=variable, group=variable)) +
  geom_line(size = 0.8) +
  theme_light() +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y") +
  labs(
    y = "Cumulative absolute error",
    colour = "Model"
  )

caption <- "Forecast RMSE (Yearly Average Periods)"
ggplot(all2_3_rmse_yearly_long, aes(x=Year, y=value, color=variable, group=variable)) +
  geom_point(size = 4) + 
  scale_color_viridis_d(option = "H") +
  labs(title=caption, x="Year", y="Output Per Worker Index", color="") +
  theme_minimal(base_family="Computer Modern") + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
#ggsave(paste0("03_Output/Exercise c/Growth_Acc_Cumulative_hclc", path, ".png"), width=7, height=5)
#gtsave(Sample2_Step3, filename = "03_Output/RMSE/Sample2_Step3.png")



#### Results Grouped Over Shock vs Normal Years --------------------------------
# Option 1
all_rmse_shock <- rbind(all1_1_rmse_yearly, all2_1_rmse_yearly)
shock_years <- c(2008, 2009, 2020, 2022)
all_rmse_shock[, Year_tmp := Year %in% shock_years]
# Option 2
all_rmse_shock <- rbind(all1_1_rmse_yearly, all2_1_rmse_yearly)
shock_years <- c(2008, 2009, 2010, 2020, 2021, 2022)
all_rmse_shock[, Year_tmp := Year %in% shock_years]
# Option 3
all_rmse_shock <- rbind(all1_1_rmse_yearly, all2_1_rmse_yearly)
all_rmse_shock[, Year_tmp := 0]
all_rmse_shock[Year %in% c(2008, 2009, 2010), Year_tmp := 1]
all_rmse_shock[Year %in% c(2020, 2021, 2022), Year_tmp := 2]
# Option 4
all_rmse_shock <- rbind(all1_1_rmse_yearly, all2_1_rmse_yearly)
all_rmse_shock[, Year_tmp := 0]
all_rmse_shock[Year %in% 2008:2010, Year_tmp := 1]
all_rmse_shock[Year %in% 2011:2019, Year_tmp := 2]
all_rmse_shock[Year %in% 2020:2022, Year_tmp := 3]
all_rmse_shock[Year %in% 2023:2030, Year_tmp := 4]
# Option 5
all_rmse_shock <- rbind(all1_1_rmse_yearly, all2_1_rmse_yearly)
all_rmse_shock[Year %in% 2002:2004, Year_tmp := "2002-2004"]
all_rmse_shock[Year %in% 2005:2007, Year_tmp := "2005-2007"]
all_rmse_shock[Year %in% 2008:2010, Year_tmp := "2008-2010"]
all_rmse_shock[Year %in% 2011:2013, Year_tmp := "2011-2013"]
all_rmse_shock[Year %in% 2014:2016, Year_tmp := "2014-2016"]
all_rmse_shock[Year %in% 2017:2019, Year_tmp := "2017-2019"]
all_rmse_shock[Year %in% 2020:2022, Year_tmp := "2020-2022"]
all_rmse_shock[Year %in% 2023:2025, Year_tmp := "2023-2025"]
# Get the res
all_rmse_shock[, Year := Year_tmp]
all_rmse_shock[, Year_tmp := NULL]
all_rmse_shock <- all_rmse_shock[, lapply(.SD, mean), by=Year]
title_shock   <- "**Out of Sample RMSE**"
(shock_table <- gt_table_shocks(all_rmse_shock[!is.na(Year)], title_shock, "1-Step Ahead"))
gtsave(shock_table, filename = paste0("03_Output/RMSE", path, "/ShockTable_Step1", path, ".png"))



# Shock versus normal 3 SStep
# Option 1
all_rmse_shock <- rbind(all1_3_rmse_yearly, all2_3_rmse_yearly)
shock_years <- c(2008, 2009, 2020, 2022)
all_rmse_shock[, Year_tmp := Year %in% shock_years]
# Option 2
all_rmse_shock <- rbind(all1_3_rmse_yearly, all2_3_rmse_yearly)
shock_years <- c(2008, 2009, 2010, 2020, 2021, 2022)
all_rmse_shock[, Year_tmp := Year %in% shock_years]
# Option 3
all_rmse_shock <- rbind(all1_3_rmse_yearly, all2_3_rmse_yearly)
all_rmse_shock[, Year_tmp := 0]
all_rmse_shock[Year %in% c(2008, 2009, 2010), Year_tmp := 1]
all_rmse_shock[Year %in% c(2020, 2021, 2022), Year_tmp := 2]
# Option 4
all_rmse_shock <- rbind(all1_3_rmse_yearly, all2_3_rmse_yearly)
all_rmse_shock[, Year_tmp := "2001-2007"]
all_rmse_shock[Year %in% 2008:2010, Year_tmp := "2008-2010"]
all_rmse_shock[Year %in% 2011:2019, Year_tmp := "2011-2019"]
all_rmse_shock[Year %in% 2020:2022, Year_tmp := "2020-2012"]
all_rmse_shock[Year %in% 2023:2030, Year_tmp := "2023-2030"]
# Option 5
all_rmse_shock <- rbind(all1_3_rmse_yearly, all2_3_rmse_yearly)
all_rmse_shock[Year %in% 2002:2004, Year_tmp := "2002-2004"]
all_rmse_shock[Year %in% 2005:2007, Year_tmp := "2005-2007"]
all_rmse_shock[Year %in% 2008:2010, Year_tmp := "2008-2010"]
all_rmse_shock[Year %in% 2011:2013, Year_tmp := "2011-2013"]
all_rmse_shock[Year %in% 2014:2016, Year_tmp := "2014-2016"]
all_rmse_shock[Year %in% 2017:2019, Year_tmp := "2017-2019"]
all_rmse_shock[Year %in% 2020:2022, Year_tmp := "2020-2022"]
all_rmse_shock[Year %in% 2023:2025, Year_tmp := "2023-2025"]
# Get the res
all_rmse_shock[, Year := Year_tmp]
all_rmse_shock[, Year_tmp := NULL]
all_rmse_shock <- all_rmse_shock[, lapply(.SD, mean), by=Year]
title_shock   <- "**Out of Sample RMSE**"
(shock_table <- gt_table_shocks(all_rmse_shock[!is.na(Year)], title_shock, "3-Step Ahead"))
gtsave(shock_table, filename = paste0("03_Output/RMSE", path, "/ShockTable_Step3", path, ".png"))



#### Grouped Model Results ------------------------------------------------------
mean_errs <- colMeans(all_rmse_shock[, -c("Year")])
weights_first   <- (1/(mean_errs/max(mean_errs)))^4
weights <- weights_first
shock_table_weighted <- copy(all_rmse_shock)

VarSels_labor <- c("LASSO_L", "Ridge_L", "ElNet_L")
shock_table_weighted[, Phil_Lasso := weighted.mean(.SD, weights[names(weights) %in% VarSels_labor]),
                     .SDcols = VarSels_labor, by=seq(nrow(shock_table_weighted))]

VarSels <- c("LASSO", "Ridge", "ElNet")
shock_table_weighted[, VarSel := weighted.mean(.SD, weights[names(weights) %in% VarSels]),
                     .SDcols = VarSels, by=seq(nrow(shock_table_weighted))]

nonlin <- c("RF_L", "LLF_L")
shock_table_weighted[, NonLin := weighted.mean(.SD, weights[names(weights) %in% nonlin]),
                     .SDcols = nonlin, by=seq(nrow(shock_table_weighted))]

VarSel_NonLins <- c("RF", "LLF")
shock_table_weighted[, VarSel_NonLin := weighted.mean(.SD, weights[names(weights) %in% VarSel_NonLins]),
                     .SDcols = VarSel_NonLins, by=seq(nrow(shock_table_weighted))]
shock_table_weighted <- shock_table_weighted[, .(Year, Phil_Lasso, VarSel, NonLin, VarSel_NonLin)]

setnames(shock_table_weighted, c("Phil_Lasso", "VarSel", "NonLin", "VarSel_NonLin"), c("Linear Phillips Curve", "Linear with Variable Selection", "Non-Linear Phillips Curve", "Non-Linear and Variable Selection"))
shock_table_weighted <- rbind(shock_table_weighted, data.table(t(c(NA, colMeans(shock_table_weighted[,-1])))), use.names=F)
shock_table_weighted[10, 1] <- "Average All"
(shock_table_wei <- gt_table_shocks(shock_table_weighted[!is.na(Year)], title_shock, "3-Step Ahead"))
gtsave(shock_table_wei, filename = paste0("03_Output/RMSE", path, "/ShockTable_Step3_Grouped", path, ".png"))
gtsave(shock_table_wei, filename = paste0("03_Output/Paper/RMSE", path, "/ShockTable_Step3_Grouped", path, ".png"))


# Convert to long format
plot_rmse <- shock_table_weighted[!is.na(Year)][Year!="Average All"]
plot_rmse <- melt(plot_rmse, id.vars = "Year", variable.name = "Model", value.name = "RMSE")
ggplot(plot_rmse, aes(x = Year, y = RMSE, group = Model, color = Model)) +
  geom_line(size = 1) + geom_point(size = 2) + theme_minimal(base_family="Computer Modern") +
  scale_color_manual(values = model_colors) +
  annotate("rect", xmin = 2.5, xmax = 3.5 , ymin = -Inf, ymax = Inf, fill = "darkgrey", alpha = 0.2) +
  annotate("rect", xmin = 6.5, xmax = 7.5 , ymin = -Inf, ymax = Inf, fill = "darkgrey", alpha = 0.2) +
  annotate("text", x = "2008-2010", y = 0.55, label = "GFC", size = 3) +
  annotate("text", x = "2020-2022", y = 0.55, label = "COVID", size = 3) +
  labs(title = "Out of Sample RMSE", subtitle = "3-Months Ahead", x = "", y = "RMSE") +
  guides(color = guide_legend(nrow = 2)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",  legend.title = element_blank()
  )
ggsave(paste0("03_Output/RMSE", path, "/RMSE_Chart_3_Months", path, ".png"), width = 7, height = 5)
ggsave(paste0("03_Output/Paper/RMSE", path, "/RMSE_Chart_3_Months", path, ".png"), width = 7, height = 5)
# Phillips Curve
# Adaptive Variable Selection (VarSel)
# Non-Linearities (Non-Lin)
# Non-Lin and VarSel
plot_rmse_only2 <- shock_table_weighted[!is.na(Year)][Year!="Average All"]
plot_rmse_only2 <- plot_rmse_only2[, c("Year", "Linear Phillips Curve", "Non-Linear and Variable Selection")]
plot_rmse_only2 <- melt(plot_rmse_only2, id.vars = "Year", variable.name = "Model", value.name = "RMSE")
ggplot(plot_rmse_only2, aes(x = Year, y = RMSE, group = Model, color = Model)) +
  geom_line(size = 1) + geom_point(size = 2) + theme_minimal(base_family="Computer Modern") +
  scale_color_manual(values = model_colors) +
  annotate("rect", xmin = 2.5, xmax = 3.5 , ymin = -Inf, ymax = Inf, fill = "darkgrey", alpha = 0.2) +
  annotate("rect", xmin = 6.5, xmax = 7.5 , ymin = -Inf, ymax = Inf, fill = "darkgrey", alpha = 0.2) +
  annotate("text", x = "2008-2010", y = 0.55, label = "GFC", size = 3) +
  annotate("text", x = "2020-2022", y = 0.55, label = "COVID", size = 3) +
  labs(title = "Out of Sample RMSE", subtitle = "3-Months Ahead", x = "", y = "RMSE") +
  guides(color = guide_legend(nrow = 2)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",  legend.title = element_blank()
  )
ggsave(paste0("03_Output/Paper/RMSE", path, "/RMSE_Chart_3_Months_Only2", path, ".png"), width = 7, height = 5)





#### Forecast Combination / Ensemble Forecasting 3 Step ------------------------
## Simple Average Best 5
all_3     <- rbind(all1_3, all2_3)
all_3step <- copy(all_3)
models <- setdiff(names(all_3step), c("real", "date"))
all_errors <- all_3step[, lapply(.SD, function(x) x - real), .SDcols = models]
all_performance <- sapply(all_errors, function(x) sqrt(mean(x^2)))
best_5 <- names(all_performance[order(all_performance)][1:5])
best_5


## Weights From Constrained Regression 
X <- as.matrix(all_3step[date < "2010-12-31", ..models])
y <- all_3step[date < "2010-12-31", real]
Dmat <- t(X) %*% X
dvec <- t(X) %*% y
# Constraints: sum(w)=1 and w>=0
Amat <- cbind(rep(1, ncol(X)), diag(ncol(X)))
bvec <- c(1, rep(0, ncol(X)))
fit_qp <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
w1 <- fit_qp$solution
names(w1) <- colnames(X)
round(w1,2)

## Weights From Constrained Ridge Regression 
lambda <- 5  # tune this
Dmat <- t(X) %*% X + lambda * diag(ncol(X))
dvec <- t(X) %*% y
Amat <- cbind(rep(1, ncol(X)), diag(ncol(X)))
bvec <- c(1, rep(0, ncol(X)))
fit_qp <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
w2 <- fit_qp$solution
names(w2) <- colnames(X)
round(w2,2)

## Descriptive Table
best_5
round(w1,2)
round(w2,2)
best_5_rbind <- round(w1,2)
best_5_rbind[names(best_5_rbind) %in% best_5] <- 1
best_5_rbind[!names(best_5_rbind) %in% best_5] <- 0
frcst_comb <- rbind(best_5_rbind, round(w1,2), round(w2,2)) |> as.data.table()
frcst_comb <- cbind(data.table(method=c("Best 5", "Constrained OLS", "Constrained Ridge")), frcst_comb)
setcolorder(frcst_comb, c("method", "RW", "RSM", "AR", "LASSO_L", "Ridge_L", "ElNet_L",
                          "LASSO", "Ridge", "ElNet", "RF_L", "LLF_L", "RF", "LLF"))
frcst_comb
tab <- copy(frcst_comb)
latex_table <- kbl(tab,format = "latex",booktabs = TRUE,digits = 2,align = c("l", rep("c", ncol(tab) - 1)),
                   caption = "Forecast Combination Weights",label = "tab:forecast_combination_weights",
                   linesep = "", escape = FALSE) %>%
  add_header_above(
    c(" " = 1,
      "Benchmark Classic Models" = 3,
      "Linear Phillips Curve" = 3,
      "Linear with Variable Selection" = 3,
      "Non-Linear Phil. Curve" = 2,
      "Non-Linear Var.Sel." = 2
    ), bold = TRUE) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"), position = "center", font_size = 8)
frcst_comb
writeLines(latex_table, paste0("03_Output/RMSE", path, "/variable_groupings", path, ".tex"))

### Add these into the forecast table
all_3[, best_5 := rowMeans(.SD), .SDcols = best_5]
#all_3[, const_1 :=  weighted.mean(as.matrix((.SD[1,]))[1,], w1), .SDcols = names(w1), by=seq(.N)]
#all_3[, const_2 :=  weighted.mean(as.matrix((.SD[1,]))[1,], w2), .SDcols = names(w2), by=seq(.N)]

tmp <- all_3[, names(w1), with=F]
tmp_wei <- matrix(w1, nrow=nrow(tmp), ncol = length(w1), byrow = T)
all_3[, const_1 := rowSums(tmp*tmp_wei)]

tmp <- all_3[, names(w2), with=F]
tmp_wei <- matrix(w2, nrow=nrow(tmp), ncol = length(w2), byrow = T)
all_3[, const_2 := rowSums(tmp*tmp_wei)]

### Look at the results
# Calculate Errors
all_3_err <- copy(all_3)
cols <- setdiff(names(all_3_err), c("real", "date"))
all_3_err[, (cols) := lapply(.SD, function(x) x - real), .SDcols = cols]

all_3_rmse_yearly <- copy(all_3_err)
all_3_rmse_yearly[, date := year(date)]
all_3_rmse_yearly <- all_3_rmse_yearly[, (cols) := lapply(.SD, function(x) sqrt(mean(x^2))), by = "date", .SDcols=cols]
all_3_rmse_yearly <- unique(all_3_rmse_yearly[, -c("real")])
setnames(all_3_rmse_yearly, "date", "Year")
all_3_rmse_yearly_long <- melt(all_3_rmse_yearly, id.vars = "Year") 
all_3_rmse_yearly

# Shock versus normal 3 Step
# Option 5
all_rmse_ensemble <- copy(all_3_rmse_yearly)
all_rmse_ensemble[Year %in% 2002:2004, Year_tmp := "2002-2004"]
all_rmse_ensemble[Year %in% 2005:2007, Year_tmp := "2005-2007"]
all_rmse_ensemble[Year %in% 2008:2010, Year_tmp := "2008-2010"]
all_rmse_ensemble[Year %in% 2011:2013, Year_tmp := "2011-2013"]
all_rmse_ensemble[Year %in% 2014:2016, Year_tmp := "2014-2016"]
all_rmse_ensemble[Year %in% 2017:2019, Year_tmp := "2017-2019"]
all_rmse_ensemble[Year %in% 2020:2022, Year_tmp := "2020-2022"]
all_rmse_ensemble[Year %in% 2023:2025, Year_tmp := "2023-2025"]
# Get the res
all_rmse_ensemble[, Year := Year_tmp]
all_rmse_ensemble[, Year_tmp := NULL]
all_rmse_ensemble <- all_rmse_ensemble[, lapply(.SD, mean), by=Year]
title_shock   <- "**Out of Sample RMSE**"
all_rmse_ensemble <- all_rmse_ensemble[!is.na(Year)]
all_rmse_ensemble <- rbind(all_rmse_ensemble, t(colMeans(all_rmse_ensemble[, -c("Year")])), fill=T)
all_rmse_ensemble[.N, Year := "Average"]
setcolorder(all_rmse_ensemble, c("Year","RW", "RSM", "AR", "LASSO_L", "Ridge_L", "ElNet_L",
                                 "LASSO", "Ridge", "ElNet", "RF_L", "LLF_L", "RF", "LLF", "best_5", "const_1", "const_2"))
all_rmse_ensemble[, Year := gsub("-20", "-", Year)]
round_cols <- setdiff(names(all_rmse_ensemble), "Year")
all_rmse_ensemble[, (round_cols) := lapply(.SD, function(x) round(x,2)), .SDcols = round_cols]
#(shock_table_ensemble <- gt_table_shocks(all_rmse_ensemble[!is.na(Year)], title_shock, "3-Step Ahead"))
(shock_table_ensemble <- all_rmse_ensemble[!is.na(Year)] |>  gt() |>
    tab_header(title = md(title_shock), subtitle = "3-Step Ahead") |>
    tab_spanner(label = "Linear Phillips Curve",columns = c(LASSO_L, Ridge_L, ElNet_L)) |>
    tab_spanner(label = "Benchmark Classic Models",columns = c(RW, RSM, AR)) |>
    tab_spanner(label = "Non-Linear Var.Sel.", columns = c(RF, LLF)) |>
    tab_spanner(label = "Linear with Variable Selection", columns = c(LASSO, Ridge, ElNet)) |>
    tab_spanner(label = "Non-Linear Phil. Curve", columns = c(RF_L, LLF_L)) |>
    tab_spanner(label = "Ensemble/Combined Models", columns = c(best_5, const_1, const_2) ) |>
    cols_align(align = "center") |>
    tab_options(table.font.names = "Consolas") |>
    tab_style(style = list(
      cell_borders(sides = c("top", "bottom"), color = "black", weight = px(3)), style = cell_fill(color = "#E8E8E8") ),
      locations = cells_body(rows = Year %in% c("2008-10", "2020-22", "Average"))) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
    data_color(columns = -Year, direction = "row", palette = c("dodgerblue", "white", "firebrick")))
gtsave(shock_table_ensemble, filename = paste0("03_Output/RMSE", path, "/ShockTable_Step3_Ensemble", path, ".html"))
gtsave(shock_table_ensemble, filename = paste0("03_Output/Paper/RMSE", path, "/ShockTable_Step3_Ensemble", path, ".html"))
shock_table_ensemble |> as_latex() |> cat()
writeLines(as_latex(shock_table_ensemble)[[1]], paste0("03_Output/Paper/RMSE", path, "/ShockTable_Step3_Ensemble", path, ".tex"))


(shock_table <- all_rmse_ensemble[!is.na(Year), c("Year", "LASSO_L",	"Ridge_L",	"ElNet_L",	"LASSO",	"Ridge",	"ElNet",	"RF_L",	"LLF_L",	"RF",	"LLF")] |>  gt() |>
    tab_header(title = md(title_shock), subtitle = "3-Step Ahead") |>
    tab_spanner(label = "Linear Phillips Curve",columns = c(LASSO_L, Ridge_L, ElNet_L)) |>
    tab_spanner(label = "Non-Linear Var.Sel.", columns = c(RF, LLF)) |>
    tab_spanner(label = "Linear with Variable Selection", columns = c(LASSO, Ridge, ElNet)) |>
    tab_spanner(label = "Non-Linear Phil. Curve", columns = c(RF_L, LLF_L)) |>
    cols_align(align = "center") |>
    tab_options(table.font.names = "Consolas") |>
    tab_style(style = list(
      cell_borders(sides = c("top", "bottom"), color = "black", weight = px(3)), style = cell_fill(color = "#E8E8E8") ),
      locations = cells_body(rows = Year %in% c("2008-10", "2020-22", "Average"))) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
    data_color(columns = -Year, direction = "row", palette = c("dodgerblue", "white", "firebrick")))
#gtsave(shock_table, filename = paste0("03_Output/RMSE", path, "/ShockTable_Step3", path, ".html"))
gtsave(shock_table, filename = paste0("03_Output/Paper/RMSE", path, "/ShockTable_Step3", path, ".html"))
shock_table |> as_latex() |> cat()
writeLines(as_latex(shock_table)[[1]], paste0("03_Output/Paper/RMSE", path, "/ShockTable_Step3", path, ".tex"))


### Weighted Model Results
mean_errs <- colMeans(all_rmse_ensemble[Year!="Average", -c("Year")])
weights_first
weights_new   <- (1/(mean_errs/max(mean_errs)))^4
weights_second <- c(weights_first, weights_new[(length(weights_new)-2):length(weights_new)])
weights <- weights_second
#weights[names(weights)=="RF"] <- 0
#weights[names(weights)=="RF_L"] <- 0
shock_table_ens_wei <- copy(all_rmse_ensemble)
VarSels_labor <- c("LASSO_L", "Ridge_L", "ElNet_L")
shock_table_ens_wei[, Phil_Lasso := weighted.mean(.SD, weights[names(weights) %in% VarSels_labor]),
                    .SDcols = VarSels_labor, by=seq(nrow(shock_table_ens_wei))]
VarSels <- c("LASSO", "Ridge", "ElNet")
shock_table_ens_wei[, VarSel := weighted.mean(.SD, weights[names(weights) %in% VarSels]),
                    .SDcols = VarSels, by=seq(nrow(shock_table_ens_wei))]
nonlin <- c("RF_L", "LLF_L")
shock_table_ens_wei[, NonLin := weighted.mean(.SD, weights[names(weights) %in% nonlin]),
                    .SDcols = nonlin, by=seq(nrow(shock_table_ens_wei))]
VarSel_NonLins <- c("RF", "LLF")
shock_table_ens_wei[, VarSel_NonLin := weighted.mean(.SD, weights[names(weights) %in% VarSel_NonLins]),
                    .SDcols = VarSel_NonLins, by=seq(nrow(shock_table_ens_wei))]
Ensembles <- c("best_5",	"const_1",	"const_2")
shock_table_ens_wei[, Ensembles := weighted.mean(.SD, weights[names(weights) %in% Ensembles]),
                    .SDcols = Ensembles, by=seq(nrow(shock_table_ens_wei))]
shock_table_ens_wei <- shock_table_ens_wei[, .(Year, Phil_Lasso, VarSel, NonLin, VarSel_NonLin, Ensembles)]
#shock_table_ens_wei <- shock_table_ens_wei[, .(Year, Phil_Lasso, VarSel_NonLin, Ensembles)]

setnames(shock_table_ens_wei, c("Phil_Lasso", "VarSel", "NonLin", "VarSel_NonLin", "Ensembles"), c("Linear Phillips Curve", "Linear with Variable Selection", "Non-Linear Phillips Curve", "Non-Linear and Var. Selection", "Ensemble Models"))
shock_table_ens_wei <- shock_table_ens_wei[Year != "Average"]
shock_table_ens_wei <- rbind(shock_table_ens_wei, data.table(t(c(NA, colMeans(shock_table_ens_wei[-(1:3),-1])))), use.names=F)
shock_table_ens_wei[9, 1] <- "Av. After 2010"
shock_table_ens_wei <- rbind(shock_table_ens_wei, data.table(t(c(NA, colMeans(shock_table_ens_wei[,-1])))), use.names=F)
shock_table_ens_wei[10, 1] <- "Average All"
(shock_table_wei <- gt_table_shocks(shock_table_ens_wei[!is.na(Year)], title_shock, "3-Step Ahead"))
gtsave(shock_table_wei, filename = paste0("03_Output/RMSE", path, "/ShockTable_Step3_Grouped_Ensemble", path, ".png"))
gtsave(shock_table_wei, filename = paste0("03_Output/Paper/RMSE", path, "/ShockTable_Step3_Grouped_Ensemble", path, ".png"))
saveRDS(shock_table_ens_wei, paste0("03_Output/Paper/RMSE_Average/Step3_Grouped_Ensemble", path, ".rds"))

# Plot RMSE Figure with Ensemble
plot_rmse <- shock_table_ens_wei[!is.na(Year)][!Year %in% c("Average All", "Av. After 2010")]
plot_rmse <- melt(plot_rmse, id.vars = "Year", variable.name = "Model", value.name = "RMSE")
ggplot(plot_rmse, aes(x = Year, y = RMSE, group = Model, color = Model)) +
  annotate("rect", xmin = 2.5, xmax = 3.5 , ymin = -Inf, ymax = Inf, fill = "darkgrey", alpha = 0.2) +
  annotate("rect", xmin = 6.5, xmax = 7.5 , ymin = -Inf, ymax = Inf, fill = "darkgrey", alpha = 0.2) +
  annotate("text", x = "2008-10", y = 0.55, label = "GFC", size = 3) +
  annotate("text", x = "2020-22", y = 0.55, label = "COVID", size = 3) +
  geom_line(size = 1) +  geom_point(size = 2) + theme_minimal(base_family="Computer Modern") +
  scale_color_manual(values = model_colors) +
  labs(title = "Out of Sample RMSE", subtitle = "3-Months Ahead", x = "", y = "RMSE") +
  guides(color = guide_legend(nrow = 2)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top", 
    legend.title = element_blank()
  )
ggsave(paste0("03_Output/RMSE", path, "/RMSE_Chart_3_Months_Ensemble", path, ".png"), width = 7, height = 5)
ggsave(paste0("03_Output/Paper/RMSE", path, "/RMSE_Chart_3_Months_Ensemble", path, ".png"), width = 7, height = 5)




#### Forecast Combination / Ensemble Forecasting 1 Step ------------------------
## Simple Average Best 5
all_1     <- rbind(all1_1, all2_1)
all_1step <- copy(all_1)
models <- setdiff(names(all_1step), c("real", "date"))
all_errors <- all_1step[, lapply(.SD, function(x) x - real), .SDcols = models]
all_performance <- sapply(all_errors, function(x) sqrt(mean(x^2)))
best_5 <- names(all_performance[order(all_performance)][1:5])
best_5


## Weights From Constrained Regression 
X <- as.matrix(all_1step[date < "2010-12-31", ..models])
y <- all_1step[date < "2010-12-31", real]
Dmat <- t(X) %*% X
dvec <- t(X) %*% y
# Constraints: sum(w)=1 and w>=0
Amat <- cbind(rep(1, ncol(X)), diag(ncol(X)))
bvec <- c(1, rep(0, ncol(X)))
fit_qp <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
w1 <- fit_qp$solution
names(w1) <- colnames(X)
round(w1,2)

## Weights From Constrained Ridge Regression 
lambda <- 5  # tune this
Dmat <- t(X) %*% X + lambda * diag(ncol(X))
dvec <- t(X) %*% y
Amat <- cbind(rep(1, ncol(X)), diag(ncol(X)))
bvec <- c(1, rep(0, ncol(X)))
fit_qp <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
w2 <- fit_qp$solution
names(w2) <- colnames(X)
round(w2,2)

## Descriptive Table
best_5
round(w1,2)
round(w2,2)
best_5_rbind <- round(w1,2)
best_5_rbind[names(best_5_rbind) %in% best_5] <- 1
best_5_rbind[!names(best_5_rbind) %in% best_5] <- 0
frcst_comb <- rbind(best_5_rbind, round(w1,2), round(w2,2)) |> as.data.table()
frcst_comb <- cbind(data.table(method=c("Best 5", "Constrained OLS", "Constrained Ridge")), frcst_comb)
setcolorder(frcst_comb, c("method", "RW", "RSM", "AR", "LASSO_L", "Ridge_L", "ElNet_L",
                          "LASSO", "Ridge", "ElNet", "RF_L", "LLF_L", "RF", "LLF"))
frcst_comb
tab <- copy(frcst_comb)
latex_table <- kbl(tab,format = "latex",booktabs = TRUE,digits = 2,align = c("l", rep("c", ncol(tab) - 1)),
                   caption = "Forecast Combination Weights",label = "tab:forecast_combination_weights",
                   linesep = "", escape = FALSE) %>%
  add_header_above(
    c(" " = 1,
      "Benchmark Classic Models" = 3,
      "Linear Phillips Curve" = 3,
      "Linear with Variable Selection" = 3,
      "Non-Linear Phil. Curve" = 2,
      "Non-Linear Var.Sel." = 2
    ), bold = TRUE) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"), position = "center", font_size = 8)
frcst_comb
writeLines(latex_table, paste0("03_Output/RMSE", path, "/variable_groupings", path, ".tex"))

### Add these into the forecast table
all_1[, best_5 := rowMeans(.SD), .SDcols = best_5]
#all_1[, const_1 :=  weighted.mean(as.matrix((.SD[1,]))[1,], w1), .SDcols = names(w1), by=seq(.N)]
#all_1[, const_2 :=  weighted.mean(as.matrix((.SD[1,]))[1,], w2), .SDcols = names(w2), by=seq(.N)]

tmp <- all_1[, names(w1), with=F]
tmp_wei <- matrix(w1, nrow=nrow(tmp), ncol = length(w1), byrow = T)
all_1[, const_1 := rowSums(tmp*tmp_wei)]

tmp <- all_1[, names(w2), with=F]
tmp_wei <- matrix(w2, nrow=nrow(tmp), ncol = length(w2), byrow = T)
all_1[, const_2 := rowSums(tmp*tmp_wei)]

### Look at the results
# Calculate Errors
all_1_err <- copy(all_1)
cols <- setdiff(names(all_1_err), c("real", "date"))
all_1_err[, (cols) := lapply(.SD, function(x) x - real), .SDcols = cols]

all_1_rmse_yearly <- copy(all_1_err)
all_1_rmse_yearly[, date := year(date)]
all_1_rmse_yearly <- all_1_rmse_yearly[, (cols) := lapply(.SD, function(x) sqrt(mean(x^2))), by = "date", .SDcols=cols]
all_1_rmse_yearly <- unique(all_1_rmse_yearly[, -c("real")])
setnames(all_1_rmse_yearly, "date", "Year")
all_1_rmse_yearly_long <- melt(all_1_rmse_yearly, id.vars = "Year") 
all_1_rmse_yearly

# Shock versus normal 3 Step
# Option 5
all_rmse_ensemble <- copy(all_1_rmse_yearly)
all_rmse_ensemble[Year %in% 2002:2004, Year_tmp := "2002-2004"]
all_rmse_ensemble[Year %in% 2005:2007, Year_tmp := "2005-2007"]
all_rmse_ensemble[Year %in% 2008:2010, Year_tmp := "2008-2010"]
all_rmse_ensemble[Year %in% 2011:2013, Year_tmp := "2011-2013"]
all_rmse_ensemble[Year %in% 2014:2016, Year_tmp := "2014-2016"]
all_rmse_ensemble[Year %in% 2017:2019, Year_tmp := "2017-2019"]
all_rmse_ensemble[Year %in% 2020:2022, Year_tmp := "2020-2022"]
all_rmse_ensemble[Year %in% 2023:2025, Year_tmp := "2023-2025"]
# Get the res
all_rmse_ensemble[, Year := Year_tmp]
all_rmse_ensemble[, Year_tmp := NULL]
all_rmse_ensemble <- all_rmse_ensemble[, lapply(.SD, mean), by=Year]
title_shock   <- "**Out of Sample RMSE**"
all_rmse_ensemble <- all_rmse_ensemble[!is.na(Year)]
all_rmse_ensemble <- rbind(all_rmse_ensemble, t(colMeans(all_rmse_ensemble[, -c("Year")])), fill=T)
all_rmse_ensemble[.N, Year := "Average"]
setcolorder(all_rmse_ensemble, c("Year","RW", "RSM", "AR", "LASSO_L", "Ridge_L", "ElNet_L",
                                 "LASSO", "Ridge", "ElNet", "RF_L", "LLF_L", "RF", "LLF", "best_5", "const_1", "const_2"))
all_rmse_ensemble[, Year := gsub("-20", "-", Year)]
round_cols <- setdiff(names(all_rmse_ensemble), "Year")
all_rmse_ensemble[, (round_cols) := lapply(.SD, function(x) round(x,2)), .SDcols = round_cols]
#(shock_table_ensemble <- gt_table_shocks(all_rmse_ensemble[!is.na(Year)], title_shock, "1-Months Ahead"))
(shock_table_ensemble <- all_rmse_ensemble[!is.na(Year)] |>  gt() |>
    tab_header(title = md(title_shock), subtitle = "1-Months Ahead") |>
    tab_spanner(label = "Linear Phillips Curve",columns = c(LASSO_L, Ridge_L, ElNet_L)) |>
    tab_spanner(label = "Benchmark Classic Models",columns = c(RW, RSM, AR)) |>
    tab_spanner(label = "Non-Linear Var.Sel.", columns = c(RF, LLF)) |>
    tab_spanner(label = "Linear with Variable Selection", columns = c(LASSO, Ridge, ElNet)) |>
    tab_spanner(label = "Non-Linear Phil. Curve", columns = c(RF_L, LLF_L)) |>
    tab_spanner(label = "Ensemble/Combined Models", columns = c(best_5, const_1, const_2) ) |>
    cols_align(align = "center") |>
    tab_options(table.font.names = "Consolas") |>
    tab_style(style = list(
      cell_borders(sides = c("top", "bottom"), color = "black", weight = px(3)), style = cell_fill(color = "#E8E8E8") ),
      locations = cells_body(rows = Year %in% c("2008-10", "2020-22", "Average"))) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
    data_color(columns = -Year, direction = "row", palette = c("dodgerblue", "white", "firebrick")))
gtsave(shock_table_ensemble, filename = paste0("03_Output/RMSE", path, "/ShockTable_Step1_Ensemble", path, ".html"))
gtsave(shock_table_ensemble, filename = paste0("03_Output/Paper/RMSE", path, "/ShockTable_Step1_Ensemble", path, ".html"))
shock_table_ensemble |> as_latex() |> cat()
writeLines(shock_table_ensemble |> as_latex(), paste0("03_Output/RMSE", path, "/ShockTable_Step1_Ensemble", path, ".tex"))


### Weighted Model Results
mean_errs <- colMeans(all_rmse_ensemble[Year!="Average", -c("Year")])
weights_first
weights_new   <- (1/(mean_errs/max(mean_errs)))^4
weights_second <- c(weights_first, weights_new[(length(weights_new)-2):length(weights_new)])
weights <- weights_second
shock_table_ens_wei <- copy(all_rmse_ensemble)
VarSels_labor <- c("LASSO_L", "Ridge_L", "ElNet_L")
shock_table_ens_wei[, Phil_Lasso := weighted.mean(.SD, weights[names(weights) %in% VarSels_labor]),
                    .SDcols = VarSels_labor, by=seq(nrow(shock_table_ens_wei))]
VarSels <- c("LASSO", "Ridge", "ElNet")
shock_table_ens_wei[, VarSel := weighted.mean(.SD, weights[names(weights) %in% VarSels]),
                    .SDcols = VarSels, by=seq(nrow(shock_table_ens_wei))]
nonlin <- c("RF_L", "LLF_L")
shock_table_ens_wei[, NonLin := weighted.mean(.SD, weights[names(weights) %in% nonlin]),
                    .SDcols = nonlin, by=seq(nrow(shock_table_ens_wei))]
VarSel_NonLins <- c("RF", "LLF")
shock_table_ens_wei[, VarSel_NonLin := weighted.mean(.SD, weights[names(weights) %in% VarSel_NonLins]),
                    .SDcols = VarSel_NonLins, by=seq(nrow(shock_table_ens_wei))]
Ensembles <- c("best_5",	"const_1",	"const_2")
shock_table_ens_wei[, Ensembles := weighted.mean(.SD, weights[names(weights) %in% Ensembles]),
                    .SDcols = Ensembles, by=seq(nrow(shock_table_ens_wei))]
shock_table_ens_wei <- shock_table_ens_wei[, .(Year, Phil_Lasso, VarSel, NonLin, VarSel_NonLin, Ensembles)]
#shock_table_ens_wei <- shock_table_ens_wei[, .(Year, Phil_Lasso, VarSel_NonLin, Ensembles)]

setnames(shock_table_ens_wei, c("Phil_Lasso", "VarSel", "NonLin", "VarSel_NonLin", "Ensembles"), c("Linear Phillips Curve", "Linear with Variable Selection", "Non-Linear Phillips Curve", "Non-Linear and Var. Selection", "Ensemble Models"))
shock_table_ens_wei <- shock_table_ens_wei[Year != "Average"]
shock_table_ens_wei <- rbind(shock_table_ens_wei, data.table(t(c(NA, colMeans(shock_table_ens_wei[-(1:3),-1])))), use.names=F)
shock_table_ens_wei[9, 1] <- "Av. After 2010"
shock_table_ens_wei <- rbind(shock_table_ens_wei, data.table(t(c(NA, colMeans(shock_table_ens_wei[,-1])))), use.names=F)
shock_table_ens_wei[10, 1] <- "Average All"
(shock_table_wei <- gt_table_shocks(shock_table_ens_wei[!is.na(Year)], title_shock, "1-Months Ahead"))
gtsave(shock_table_wei, filename = paste0("03_Output/RMSE", path, "/ShockTable_Step1_Grouped_Ensemble", path, ".png"))
gtsave(shock_table_wei, filename = paste0("03_Output/Paper/RMSE", path, "/ShockTable_Step1_Grouped_Ensemble", path, ".png"))
saveRDS(shock_table_ens_wei, paste0("03_Output/Paper/RMSE_Average/Step1_Grouped_Ensemble", path, ".rds"))

# Plot RMSE Figure with Ensemble
plot_rmse <- shock_table_ens_wei[!is.na(Year)][!Year %in% c("Average All", "Av. After 2010")]
plot_rmse <- melt(plot_rmse, id.vars = "Year", variable.name = "Model", value.name = "RMSE")
ggplot(plot_rmse, aes(x = Year, y = RMSE, group = Model, color = Model)) +
  annotate("rect", xmin = 2.5, xmax = 3.5 , ymin = -Inf, ymax = Inf, fill = "darkgrey", alpha = 0.2) +
  annotate("rect", xmin = 6.5, xmax = 7.5 , ymin = -Inf, ymax = Inf, fill = "darkgrey", alpha = 0.2) +
  annotate("text", x = "2008-10", y = 0.55, label = "GFC", size = 3) +
  annotate("text", x = "2020-22", y = 0.55, label = "COVID", size = 3) +
  geom_line(size = 1) +  geom_point(size = 2) + theme_minimal(base_family="Computer Modern") +
  scale_color_manual(values = model_colors) +
  labs(title = "Out of Sample RMSE", subtitle = "1-Months Ahead", x = "", y = "RMSE") +
  guides(color = guide_legend(nrow = 2)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top", 
    legend.title = element_blank()
  )
ggsave(paste0("03_Output/RMSE", path, "/RMSE_Chart_1_Months_Ensemble", path, ".png"), width = 10, height = 6)
ggsave(paste0("03_Output/Paper/RMSE", path, "/RMSE_Chart_1_Months_Ensemble", path, ".png"), width = 10, height = 6)




#### Compare with FED   --------------------------------------------------------
### Cleaveland FED 1-Step Ahead
# Build the Ensemble Models column from all_1 (same weighted combo used in shock_table_ens_wei)
all_1_fed <- copy(all_1)
tmp <- all_1_fed[, names(w2), with = F]
tmp_wei <- matrix(w2, nrow = nrow(tmp), ncol = length(w2), byrow = T)
Ensembles_cols <- c("best_5", "const_1", "const_2")
tmp_ens <- all_1_fed[, ..Ensembles_cols]
ens_weights <- weights[names(weights) %in% Ensembles_cols]
tmp_ens_wei <- matrix(ens_weights, nrow = nrow(tmp_ens), ncol = length(ens_weights), byrow = T)
all_1_fed[, Ensemble_Models := rowSums(tmp_ens * tmp_ens_wei) / sum(ens_weights)]

# Merge with Cleveland FED on date (inner join — only overlapping dates)
all_1_fed <- merge(all_1_fed[, .(date, real, Ensemble_Models)], cl_fed, by = "date")

# Compute yearly RMSE for Ensemble and CL_FED
all_1_fed_err <- copy(all_1_fed)
all_1_fed_err[, Ensemble_err := Ensemble_Models - real]
all_1_fed_err[, cl_fed_err   := cl_fed - real]

all_fed_rmse_yearly <- all_1_fed_err[, .(
  `Ensemble Models` = sqrt(mean(Ensemble_err^2)),
  `Cleveland FED`   = sqrt(mean(cl_fed_err^2))
), by = .(Year = year(date))]

# Group into 3-year periods (same as rest of script)
all_fed_rmse_yearly[Year %in% 2002:2004, Year_tmp := "2002-2004"]
all_fed_rmse_yearly[Year %in% 2005:2007, Year_tmp := "2005-2007"]
all_fed_rmse_yearly[Year %in% 2008:2010, Year_tmp := "2008-2010"]
all_fed_rmse_yearly[Year %in% 2011:2013, Year_tmp := "2011-2013"]
all_fed_rmse_yearly[Year %in% 2014:2016, Year_tmp := "2014-2016"]
all_fed_rmse_yearly[Year %in% 2017:2019, Year_tmp := "2017-2019"]
all_fed_rmse_yearly[Year %in% 2020:2022, Year_tmp := "2020-2022"]
all_fed_rmse_yearly[Year %in% 2023:2025, Year_tmp := "2023-2025"]

all_fed_rmse_yearly[, Year := Year_tmp]
all_fed_rmse_yearly[, Year_tmp := NULL]
all_fed_rmse_yearly <- all_fed_rmse_yearly[!is.na(Year), lapply(.SD, mean), by = Year]

# Round
round_cols <- setdiff(names(all_fed_rmse_yearly), "Year")
all_fed_rmse_yearly[, (round_cols) := lapply(.SD, function(x) round(x, 3)), .SDcols = round_cols]

# Add summary rows: Av. After 2010 and Average All
post2010_rows <- all_fed_rmse_yearly[Year %in% c("2011-2013", "2014-2016", "2017-2019", "2020-2022", "2023-2025")]
av_all        <- data.table(Year = "Average All",    t(round(colMeans(all_fed_rmse_yearly[, -"Year"]), 3)))
all_fed_rmse_yearly <- rbind(all_fed_rmse_yearly, av_all)

#gt_table_shocks_dif_color <- function(data, title, subtitle) {
#  res <- data |>
#    gt() |>
#    tab_header(title=md(title), subtitle=subtitle) |>
#    cols_align(align = "center") |>
#    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |> 
#    tab_style(
#      style = list(cell_borders(sides = c("top", "bottom"), color = "black", 
#                                weight = px(3)), style = cell_fill(color = "#E8E8E8")),
#      locations = cells_body(rows = Year %in% c("2008-2010", "2020-2022", "2008-10", "2020-22", "Average", "Average All", "Av. After 2010"))
#    ) |>
#    data_color(columns = -Year, direction = "row",    
#               palette = c("#6161BA", "white", "#B84444")) |>
#    tab_options(table.font.names = "Consolas")
#  return(res)
#}
# Render the gt table (same style as shock_table_wei)
title_shock <- "**Out of Sample RMSE**"
(shock_table_fed <- gt_table_shocks(all_fed_rmse_yearly, title_shock, "1-Months Ahead — Ensemble vs. Cleveland FED"))
gtsave(shock_table_fed, filename = paste0("03_Output/Paper/RMSE", path, "/CL_FED_Comparison", path, ".png"))


# Plot RMSE Figure with Ensemble
plot_rmse <- all_fed_rmse_yearly[!is.na(Year)][!Year %in% c("Average All", "Av. After 2010")]
plot_rmse <- melt(plot_rmse, id.vars = "Year", variable.name = "Model", value.name = "RMSE")
ggplot(plot_rmse, aes(x = Year, y = RMSE, group = Model, color = Model)) +
  annotate("rect", xmin = 3.5, xmax = 4.5 , ymin = -Inf, ymax = Inf, fill = "darkgrey", alpha = 0.2) +
  annotate("text", x = "2020-2022", y = 0.4, label = "COVID", size = 3) +
  geom_line(size = 1) +  geom_point(size = 2) + theme_minimal(base_family="Computer Modern") +
  scale_color_manual(values = model_colors) +
  labs(title = "Out of Sample RMSE", subtitle = "3-Months Ahead", x = "", y = "RMSE") +
  guides(color = guide_legend(nrow = 2)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top", 
    legend.title = element_blank()
  )
ggsave(paste0("03_Output/Paper/RMSE", path, "/CL_FED_Comparison_chart", path, ".png"), width = 7, height = 5)

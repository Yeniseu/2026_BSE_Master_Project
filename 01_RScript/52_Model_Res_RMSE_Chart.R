# Author: Ece Tasan
# Date: 5/12/2025
# Scope: Rolling RMSE for Model Comparison

rm(list = ls())
library(ggplot2)
library(data.table)
library(gt)
options(scipen=30, digits=3)
#### Comparison for First Out of Sample Period ####

lasso1 <- readRDS("03_Output/lasso_pred_s1.rds")

lasso1_1 <- lasso1[, c("real", "lasso_l1", "ridge_l1", "elnet_l1", "rw_l1")]
setnames(lasso1_1, c("real", "lasso_l1", "ridge_l1", "elnet_l1", "rw_l1"),
         c("real", "LASSO", "Ridge", "ElNet", "RW"))

lasso1_3 <- lasso1[, c("real", "lasso_l3", "ridge_l3", "elnet_l3", "rw_l3")]
setnames(lasso1_3, c("real", "lasso_l3", "ridge_l3", "elnet_l3", "rw_l3"),
         c("real", "LASSO", "Ridge", "ElNet", "RW"))

mean1_h1 <- readRDS("03_Output/p1_h1_mean.rds")
mean1_h1 <- mean1_h1$pred
mean1_h1 <- as.data.table(mean1_h1)
setnames(mean1_h1, "V1", "RSM")

mean1_h3 <- readRDS("03_Output/p1_h3_mean.rds")
mean1_h3 <- mean1_h3$pred
mean1_h3 <- as.data.table(mean1_h3)
setnames(mean1_h3, "V1", "RSM")

p1_h1_ar4 <- readRDS("03_Output/p1_h1_ar4.rds")
p1_h1_ar4 <- p1_h1_ar4$pred
p1_h1_ar4 <- as.data.table(p1_h1_ar4)
setnames(p1_h1_ar4, "V1", "AR")

p1_h3_ar4 <- readRDS("03_Output/p1_h3_ar4.rds")
p1_h3_ar4 <- p1_h3_ar4$pred
p1_h3_ar4 <- as.data.table(p1_h3_ar4)
setnames(p1_h3_ar4, "V1", "AR")

rf1_1 <- readRDS("03_Output/rf1_1.rds")
rf1_1 <- rf1_1$pred
rf1_1 <- as.data.table(rf1_1)
setnames(rf1_1, "V1", "RF")

rf1_3 <- readRDS("03_Output/rf1_3.rds")
rf1_3 <- rf1_3$pred
rf1_3 <- as.data.table(rf1_3)
setnames(rf1_3, "V1", "RF")

all1_1 <- cbind(lasso1_1, mean1_h1, p1_h1_ar4, rf1_1)
all1_3 <- cbind(lasso1_3, mean1_h3, p1_h3_ar4, rf1_3)

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
  theme_minimal() + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
#ggsave("03_Output/Exercise c/Growth_Acc_Cumulative_hclc.png", width=7, height=4)

(Sample1_Step1 <- all1_1_rmse_yearly |>
  round(2) |>
  gt() |>
  tab_header(title=md("**Yearly RMSE by Model**"), subtitle="1-Step Ahead Out of Sample") |>
  cols_align(align = "center") |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |> 
  data_color(columns = -Year, direction = "row",    
             palette = c("dodgerblue", "white", "firebrick")) |>
  tab_options(table.font.names = "Consolas"))
gtsave(Sample1_Step1, filename = "03_Output/RMSE/Sample1_Step1.png")


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
  theme_minimal() + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
#ggsave("03_Output/Exercise c/Growth_Acc_Cumulative_hclc.png", width=7, height=4)

(Sample1_Step3 <- all1_3_rmse_yearly[,-c("RW")] |>
  round(2) |>
  gt() |>
  tab_header(title=md("**Yearly RMSE by Model**"), subtitle="3-Step Ahead Out of Sample") |>
  cols_align(align = "center") |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |> 
  data_color(columns = -Year, direction = "row",    
             palette = c("dodgerblue", "white", "firebrick")) |>
  tab_options(table.font.names = "Consolas"))
gtsave(Sample1_Step3, filename = "03_Output/RMSE/Sample1_Step3.png")
















#### Comparison for Second Out of Sample Period ####
lasso2 <- readRDS("03_Output/lasso_pred_s2.rds")

lasso2_1 <- lasso2[, c("real", "lasso_l1", "ridge_l1", "elnet_l1", "rw_l1")]
setnames(lasso2_1, c("real", "lasso_l1", "ridge_l1", "elnet_l1", "rw_l1"),
         c("real", "LASSO", "Ridge", "ElNet", "RW"))

lasso2_3 <- lasso2[, c("real", "lasso_l3", "ridge_l3", "elnet_l3", "rw_l3")]
setnames(lasso2_3, c("real", "lasso_l3", "ridge_l3", "elnet_l3", "rw_l3"),
         c("real", "LASSO", "Ridge", "ElNet", "RW"))

mean2_h1 <- readRDS("03_Output/p2_h1_mean.rds")
mean2_h1 <- mean2_h1$pred
mean2_h1 <- as.data.table(mean2_h1)
setnames(mean2_h1, "V1", "RSM")

mean2_h3 <- readRDS("03_Output/p2_h3_mean.rds")
mean2_h3 <- mean2_h3$pred
mean2_h3 <- as.data.table(mean2_h3)
setnames(mean2_h3, "V1", "RSM")

p2_h1_ar4 <- readRDS("03_Output/p2_h1_ar4.rds")
p2_h1_ar4 <- p2_h1_ar4$pred
p2_h1_ar4 <- as.data.table(p2_h1_ar4)
setnames(p2_h1_ar4, "V1", "AR")

p2_h3_ar4 <- readRDS("03_Output/p2_h3_ar4.rds")
p2_h3_ar4 <- p2_h3_ar4$pred
p2_h3_ar4 <- as.data.table(p2_h3_ar4)
setnames(p2_h3_ar4, "V1", "AR")

rf2_1 <- readRDS("03_Output/rf2_1.rds")
rf2_1 <- rf2_1$pred
rf2_1 <- as.data.table(rf2_1)
setnames(rf2_1, "V1", "RF")

rf2_3 <- readRDS("03_Output/rf2_3.rds")
rf2_3 <- rf2_3$pred
rf2_3 <- as.data.table(rf2_3)
setnames(rf2_3, "V1", "RF")

all2_1 <- cbind(lasso2_1, mean2_h1, p2_h1_ar4, rf2_1)
all2_3 <- cbind(lasso2_3, mean2_h3, p2_h3_ar4, rf2_3)

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
  theme_minimal() + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
#ggsave("03_Output/Exercise c/Growth_Acc_Cumulative_hclc.png", width=7, height=4)


(Sample2_Step1 <- all2_1_rmse_yearly |>
  round(2) |>
  gt() |>
  cols_align(align = "center") |>
  tab_header(title=md("**Yearly RMSE by Model**"), subtitle="1-Step Ahead Out of Sample") |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |> 
  data_color(columns = -Year, direction = "row",    
             palette = c("dodgerblue", "white", "firebrick")) |>
  tab_options(table.font.names = "Consolas"))
gtsave(Sample2_Step1, filename = "03_Output/RMSE/Sample2_Step1.png")






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
  theme_minimal() + 
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"))
#ggsave("03_Output/Exercise c/Growth_Acc_Cumulative_hclc.png", width=7, height=4)


(Sample2_Step3 <- all2_3_rmse_yearly |>
  round(2) |>
  gt() |>
  tab_header(title=md("**Yearly RMSE by Model**"), subtitle="3-Step Ahead Out of Sample") |>
  cols_align(align = "center") |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |> 
  data_color(columns = -Year, direction = "row",    
             palette = c("dodgerblue", "white", "firebrick")) |>
  tab_options(table.font.names = "Consolas"))
gtsave(Sample2_Step3, filename = "03_Output/RMSE/Sample2_Step3.png")


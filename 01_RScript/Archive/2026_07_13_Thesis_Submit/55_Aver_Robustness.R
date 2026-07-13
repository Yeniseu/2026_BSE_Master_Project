# Author: Ece Tasan, Orhun Ozel
# Date: 5/12/2025
# Scope: Get Average Robustness Table and Figure
library(ggplot2)
library(data.table)
library(gt)
library(glmnet)
library(quadprog)
library(kableExtra)

rm(list = ls())
options(scipen=30, digits=3)
options(datatable.print.trunc.cols = T)
options(datatable.print.nrows      = 15)

# ── Shared colour palette (matches RMSE_Chart_3_Months_Ensemble.png) ──────────
model_colors <- c(
  "Linear Phillips Curve"              = "#F8766D",   # salmon / pink-red
  "Non-Linear Phillips Curve"          = "#00BA38",   # green
  "Linear with Variable Selection"     = "#B79F00",   # olive / yellow-green
  "Non-Linear and Var. Selection"      = "#00BFC4",   # cyan / teal  (ensemble charts)
  "Non-Linear and Variable Selection"  = "#00BFC4",   # cyan / teal  (non-ensemble chart)
  "Ensemble Models"                    = "#C77CFF"    # orchid / purple
)
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

#### Average robustness  
base_3 <- readRDS("03_Output/Paper/RMSE_Average/Step3_Grouped_Ensemble.rds")
base_1 <- readRDS("03_Output/Paper/RMSE_Average/Step1_Grouped_Ensemble.rds")
rb40_3 <- readRDS("03_Output/Paper/RMSE_Average/Step3_Grouped_Ensemble_40.rds")
rb40_1 <- readRDS("03_Output/Paper/RMSE_Average/Step1_Grouped_Ensemble_40.rds")
rb30_3 <- readRDS("03_Output/Paper/RMSE_Average/Step3_Grouped_Ensemble_30.rds")
rb30_1 <- readRDS("03_Output/Paper/RMSE_Average/Step1_Grouped_Ensemble_30.rds")

base_3_m <- base_3[, -c("Year")] |> as.matrix()
base_1_m <- base_1[, -c("Year")] |> as.matrix()
rb40_3_m <- rb40_3[, -c("Year")] |> as.matrix()
rb40_1_m <- rb40_1[, -c("Year")] |> as.matrix()
rb30_3_m <- rb30_3[, -c("Year")] |> as.matrix()
rb30_1_m <- rb30_1[, -c("Year")] |> as.matrix()

rob_aver <- (base_3_m + base_1_m + rb40_3_m + rb40_1_m + rb30_3_m + rb30_1_m)/6
rob_aver <- cbind(base_3[, .(Year)], as.data.table(rob_aver))

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

title_shock <- "**Out of Sample RMSE**"
(shock_table_wei     <- gt_table_shocks(base_3[!is.na(Year)], title_shock, "1-Months Ahead"))
(shock_table_average <- gt_table_shocks(rob_aver[!is.na(Year)], title_shock, "1-Months Ahead"))
gtsave(shock_table_average, "03_Output/Paper/RMSE_Average/RMSE_Average_Table.png")


# Plot RMSE Figure with Ensemble
plot_rmse <- rob_aver[!is.na(Year)][!Year %in% c("Average All", "Av. After 2010")]
plot_rmse <- melt(plot_rmse, id.vars = "Year", variable.name = "Model", value.name = "RMSE")
ggplot(plot_rmse, aes(x = Year, y = RMSE, group = Model, color = Model)) +
  annotate("rect", xmin = 2.5, xmax = 3.5 , ymin = -Inf, ymax = Inf, fill = "darkgrey", alpha = 0.2) +
  annotate("rect", xmin = 6.5, xmax = 7.5 , ymin = -Inf, ymax = Inf, fill = "darkgrey", alpha = 0.2) +
  annotate("text", x = "2008-10", y = 0.55, label = "GFC", size = 3) +
  annotate("text", x = "2020-22", y = 0.55, label = "COVID", size = 3) +
  geom_line(size = 1) +  geom_point(size = 2) + theme_minimal(base_family = "Computer Modern") +
  scale_color_manual(values = model_colors) +
  labs(title = "Out of Sample RMSE", subtitle = "Average of 6 Different Specifications", x = "", y = "RMSE") +
  guides(color = guide_legend(nrow = 2)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top", 
    legend.title = element_blank()
  )
ggsave("03_Output/Paper/RMSE_Average/RMSE_Average_Chart.png", width = 7, height = 5)


(shock_table_wei <- gt_table_shocks(base_3[!is.na(Year)], title_shock, "1-Months Ahead"))
(shock_table_wei <- gt_table_shocks(base_1[!is.na(Year)], title_shock, "1-Months Ahead"))
(shock_table_wei <- gt_table_shocks(rb40_3[!is.na(Year)], title_shock, "1-Months Ahead"))
(shock_table_wei <- gt_table_shocks(rb40_1[!is.na(Year)], title_shock, "1-Months Ahead"))
(shock_table_wei <- gt_table_shocks(rb30_3[!is.na(Year)], title_shock, "1-Months Ahead"))
(shock_table_wei <- gt_table_shocks(rb30_1[!is.na(Year)], title_shock, "1-Months Ahead"))




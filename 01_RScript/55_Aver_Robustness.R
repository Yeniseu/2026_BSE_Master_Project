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
  geom_line(size = 1) +  geom_point(size = 2) + theme_minimal() +
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


############# graphhhhhhhhhhh
# Author: Ece Tasan, Orhun Ozel (original) — Economist retheme by Emilia
# Date: 5/12/2025 (updated)
# Scope: Average Robustness Table and Figure — Economist style

library(ggplot2)
library(data.table)
library(gt)
library(glmnet)
library(quadprog)

# restart R
rm(list = ls())
options(scipen = 30, digits = 3)
options(datatable.print.trunc.cols = T)
options(datatable.print.nrows      = 15)


# ============================================================
# Palette — same family as SHAP charts
# #6161BA  periwinkle   (Linear Phillips Curve)
# #34BA66  green        (Non-Linear Phillips Curve)
# #BAA237  warm gold    (Linear with Variable Selection)
# #BA3B35  deep red     (Non-Linear and Var. Selection)
# #555566  dark slate   (Ensemble Models)
# ============================================================

model_colors <- c(
  "Linear Phillips Curve"             = "#6161BA",
  "Non-Linear Phillips Curve"         = "#34BA66",
  "Linear with Variable Selection"    = "#BAA237",
  "Non-Linear and Var. Selection"     = "#BA3B35",
  "Non-Linear and Variable Selection" = "#BA3B35",
  "Ensemble Models"                   = "#555566"
)

COL_TEXT <- "#1a1a2e"
COL_RULE <- "#6161BA"


# ============================================================
# Economist base theme (same as SHAP charts)
# ============================================================

theme_ec <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "serif") %+replace%
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.margin      = margin(t = 14, r = 16, b = 10, l = 10),
      
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
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      
      axis.title = element_text(family = "serif", face = "italic",
                                size = 10, colour = COL_TEXT),
      axis.text  = element_text(family = "serif", size = 10,
                                colour = COL_TEXT),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.line.x = element_line(colour = "#AAAABC", linewidth = 0.35),
      axis.line.y = element_blank(),
      axis.ticks  = element_blank(),
      
      panel.grid.major.y = element_line(colour = "#EEEEF2", linewidth = 0.28),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      
      legend.position    = "top",
      legend.justification = "left",
      legend.title       = element_blank(),
      legend.text        = element_text(family = "serif", size = 10,
                                        colour = COL_TEXT),
      legend.key.width   = unit(1.8, "lines"),
      legend.key.height  = unit(0.5, "lines"),
      legend.margin      = margin(0, 0, 4, 0),
      legend.spacing.x   = unit(0.4, "cm"),
      
      # Green top rule via plot border
      plot.background = element_rect(fill      = "white",
                                     colour    = COL_RULE,
                                     linewidth = 2.2)
    )
}


# ============================================================
# Load data (unchanged)
# ============================================================

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

rob_aver <- (base_3_m + base_1_m + rb40_3_m + rb40_1_m + rb30_3_m + rb30_1_m) / 6
rob_aver <- cbind(base_3[, .(Year)], as.data.table(rob_aver))


# ============================================================
# gt table — rethemed
# ============================================================

gt_table_shocks <- function(data, title, subtitle) {
  data |>
    gt() |>
    tab_header(title = md(title), subtitle = subtitle) |>
    cols_align(align = "center") |>
    tab_style(
      style     = cell_text(weight = "bold", font = google_font("Source Serif 4")),
      locations = cells_column_labels()
    ) |>
    tab_style(
      style     = cell_text(font = google_font("Source Serif 4")),
      locations = cells_body()
    ) |>
    tab_style(
      style = list(
        cell_borders(sides = c("top", "bottom"), color = "#6161BA", weight = px(2)),
        cell_fill(color = "#F0F0F8")
      ),
      locations = cells_body(
        rows = Year %in% c("2008-2010", "2020-2022", "2008-10",
                           "2020-22", "Average", "Average All", "Av. After 2010")
      )
    ) |>
    data_color(
      columns  = -Year,
      direction = "row",
      palette  = c("#6161BA", "white", "#BA3B35")   # periwinkle → white → red
    ) |>
    tab_options(
      table.font.names          = "Georgia, serif",
      table.border.top.color    = "#6161BA",
      table.border.top.width    = px(3),
      table.border.bottom.color = "#6161BA",
      column_labels.border.bottom.color = "#1a1a2e",
      column_labels.border.bottom.width = px(1.5),
      heading.title.font.size   = px(16),
      heading.subtitle.font.size = px(13),
      heading.align             = "left"
    )
}

title_shock <- "**Out of Sample RMSE**"
(shock_table_average <- gt_table_shocks(rob_aver[!is.na(Year)], title_shock,
                                        "Average of 6 Specifications"))
gtsave(shock_table_average,
       "03_Output/Paper/RMSE_Average/RMSE_Average_Table.png")


# ============================================================
# RMSE line chart — rethemed
# ============================================================

plot_rmse <- rob_aver[!is.na(Year)][!Year %in% c("Average All", "Av. After 2010")]
plot_rmse <- melt(plot_rmse, id.vars = "Year",
                  variable.name = "Model", value.name = "RMSE")

ggplot(plot_rmse, aes(x = Year, y = RMSE, group = Model, color = Model)) +
  # Shock bands
  annotate("rect", xmin = 2.5, xmax = 3.5,
           ymin = -Inf, ymax = Inf, fill = "#CCCCDD", alpha = 0.55) +
  annotate("rect", xmin = 6.5, xmax = 7.5,
           ymin = -Inf, ymax = Inf, fill = "#CCCCDD", alpha = 0.55) +
  # Shock labels
  annotate("text", x = "2008-10", y = 0.55, label = "GFC",
           size = 3.2, family = "serif", colour = "#444455", fontface = "italic") +
  annotate("text", x = "2020-22", y = 0.55, label = "COVID",
           size = 3.2, family = "serif", colour = "#444455", fontface = "italic") +
  geom_line(linewidth = 0.95, lineend = "round") +
  geom_point(size = 2.2) +
  scale_color_manual(
    values = model_colors,
    guide  = guide_legend(nrow = 2,
                          override.aes = list(linewidth = 1.4, size = 2.5))
  ) +
  labs(
    title    = "Out of Sample RMSE",
    subtitle = "Average of 6 different specifications",
    x        = NULL,
    y        = "RMSE",
    caption  = "Shaded bands: GFC (2008–10) and COVID (2020–22) periods."
  ) +
  theme_ec()

ggsave("03_Output/Paper/RMSE_Average/RMSE_Average_Chart.png",
       width = 9, height = 5.5, dpi = 300, bg = "white")


# ============================================================
# Individual shock tables (unchanged logic, rethemed)
# ============================================================

(gt_table_shocks(base_3[!is.na(Year)], title_shock, "3-Month Ahead — Base (56yr)"))
(gt_table_shocks(base_1[!is.na(Year)], title_shock, "1-Month Ahead — Base (56yr)"))
(gt_table_shocks(rb40_3[!is.na(Year)], title_shock, "3-Month Ahead — 40yr window"))
(gt_table_shocks(rb40_1[!is.na(Year)], title_shock, "1-Month Ahead — 40yr window"))
(gt_table_shocks(rb30_3[!is.na(Year)], title_shock, "3-Month Ahead — 30yr window"))
(gt_table_shocks(rb30_1[!is.na(Year)], title_shock, "1-Month Ahead — 30yr window"))

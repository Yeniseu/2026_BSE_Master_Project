# Author: Emilia
# Scope : Realized CPI (FRED quarterly) vs SPF median forecasts — motivation slide

rm(list = ls())
library(readxl)
library(data.table)
library(ggplot2)
library(fredr)
fredr_set_key("5a32a7fd46f7d868f44d20560b817b96")

# ---- 1. SPF individual headline CPI forecasts -> quarterly median ----
spf <- as.data.table(read_excel("C:/Users/emili/Downloads/Individual_CPI.xlsx"))
cpi_cols <- c("CPI1","CPI2","CPI3","CPI4","CPI5","CPI6")
spf[, (cpi_cols) := lapply(.SD, function(x) suppressWarnings(as.numeric(x))),
    .SDcols = cpi_cols]
spf[, qdate := as.Date(paste0(YEAR, "-", (QUARTER-1)*3 + 1, "-01"))]

spf_med <- spf[, .(
  nowcast = median(CPI2, na.rm = TRUE),
  h4q     = median(CPI6, na.rm = TRUE)
), by = qdate]

spf_med[, target_now := qdate]
spf_med[, target_h4q := as.Date(sapply(qdate, function(d) seq(d, by="3 months", length.out=5)[5]))]


# ---- 2. Realized CPI from FRED, quarterly, q/q annualized ----
cpi_q <- as.data.table(fredr(
  series_id          = "CPIAUCSL",
  observation_start  = as.Date("1965-01-01"),
  frequency          = "q",
  aggregation_method = "avg"
))
setnames(cpi_q, c("date", "value"), c("qdate", "level"))
cpi_q <- cpi_q[, .(qdate, level)]
setorder(cpi_q, qdate)
cpi_q[, realized := 400 * log(level / shift(level, 1))]

# ---- 3. Stack ----
plot_dt <- rbind(
  cpi_q[,   .(date = qdate,      value = realized, series = "Realized")],
  spf_med[, .(date = target_now, value = nowcast,  series = "SPF nowcast")],
  spf_med[, .(date = target_h4q, value = h4q,      series = "SPF, 4 quarters earlier")]
)
plot_dt <- plot_dt[!is.na(value) & date >= as.Date("2000-01-01") & date <= as.Date("2025-12-31")]

# ---- 4. Bands and annotations ----
# Merge COVID + post-pandemic into ONE band; that solves the label collision
# and is also more honest (the COVID dip and the surge are one continuous
# regime-change episode that the framework misses).
shock_bands <- data.table(
  xmin  = as.Date(c("2008-07-01", "2010-01-01", "2020-04-01")),
  xmax  = as.Date(c("2009-06-30", "2015-12-31", "2022-12-31")),
  label = c("GFC oil spike\n& deflation",
            "Missing disinflation",
            "COVID shock &\npost-pandemic surge")
)

# Peak annotations: pulled UP and to the side, never on top of band labels
annot <- data.table(
  date  = as.Date(c("2008-10-01", "2022-04-01")),
  y     = c(-9.27, 9.30),
  date_lab = as.Date(c("2010-04-01", "2018-10-01")),  # label position
  y_lab    = c(-7.5, 9.5),
  text  = c("2008Q4: -9.3% realized\n(forecast a year earlier: +2.2%)",
            "2022Q2: +9.3% realized\n(forecast a year earlier: +2.25%)"),
  hjust = c(0, 1)
)

annot_pts <- data.table(
  date = as.Date(c("2008-10-01", "2022-04-01")),
  y    = c(-9.27, 9.30)
)

# ---- 5. Plot ----
p <- ggplot() +
  # shock shading
  geom_rect(data = shock_bands,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey85", alpha = 0.45) +
  # Band labels at the BOTTOM of the chart (out of the way of the lines)
  geom_text(data = shock_bands,
            aes(x = xmin + (xmax - xmin)/2, y = -11.3, label = label),
            size = 2.8, color = "grey30", fontface = "italic", lineheight = 0.9) +
  # Reference lines
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.3) +
  geom_hline(yintercept = 2, color = "grey70", linewidth = 0.25, linetype = "dashed") +
  annotate("text", x = as.Date("2000-04-01"), y = 2.6,
           label = "2% target", size = 2.6, color = "grey50", hjust = 0) +
  # Series
  geom_line(data = plot_dt,
            aes(x = date, y = value, color = series, linewidth = series)) +
  # Peak callouts: line from point to label, then label
  geom_segment(data = data.table(
    x    = as.Date(c("2008-10-01", "2022-04-01")),
    xend = as.Date(c("2010-04-01", "2018-10-01")),
    y    = c(-9.27, 9.30),
    yend = c(-7.7, 9.4)),
    aes(x = x, xend = xend, y = y, yend = yend),
    color = "grey40", linewidth = 0.3) +
  geom_point(data = annot_pts, aes(x = date, y = y),
             color = "black", size = 1.8) +
  geom_label(data = annot,
             aes(x = date_lab, y = y_lab, label = text, hjust = hjust),
             size = 2.9, lineheight = 0.95, color = "grey15",
             label.size = 0, fill = "white", alpha = 0.9,
             label.padding = unit(0.15, "lines")) +
  scale_color_manual(values = c("Realized"                = "black",
                                "SPF nowcast"             = "#2c7fb8",
                                "SPF, 4 quarters earlier" = "#d7301f")) +
  scale_linewidth_manual(values = c("Realized" = 0.9,
                                    "SPF nowcast" = 0.6,
                                    "SPF, 4 quarters earlier" = 0.7),
                         guide = "none") +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(breaks = seq(-9, 9, 3), limits = c(-12.5, 11.5)) +
  labs(x = NULL,
       y = "CPI inflation (%, q/q annualized)",
       color = NULL,
       title = "Realized US inflation vs. SPF median forecast",
       subtitle = "US headline CPI. SPF median forecast vs. realized.",
       caption = "Source: Survey of Professional Forecasters (Philadelphia Fed); CPIAUCSL (FRED).") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        legend.margin = margin(b = -5),
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "grey30", size = 10),
        plot.caption = element_text(color = "grey50", size = 8, hjust = 0),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

print(p)

ggsave("03_Output/SPF/SPF_vs_Realized_Motivation.png", p,
       width = 10, height = 5, dpi = 300, bg = "white")


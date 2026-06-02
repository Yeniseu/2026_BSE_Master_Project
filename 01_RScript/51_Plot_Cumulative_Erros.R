# Author: Ece Tasan
# Date: 5/12/2025
# Scope: Rolling RMSE for Model Comparison

rm(list = ls())
library(ggplot2)
library(data.table)

option <- 2
path   <-  ""
models_1 <- c("RW", "RSM", "AR", "LASSO", "Ridge", "ElNet", "RF", "LLF")
models_2 <- c("RSM", "AR", "ElNet_L", "ElNet","LLF_L", "LLF")
if (option==1) {models <- models_1; path_save = "_models1"}
if (option==2) {models <- models_2; path_save = "_models2"}

#### Comparison for First Out of Sample Period ####
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

#### Calculate Cumulative Absolute Errors

#### Sample 1, Horizon 1 ####
all1_1_err <- copy(all1_1)
cols <- setdiff(names(all1_1_err), "real")
all1_1_err[, (cols) := lapply(.SD, function(x) abs(x - real)), .SDcols = cols]
all1_1_err[, real := NULL]

all1_1_cum <- copy(all1_1_err)
all1_1_cum[, (names(all1_1_cum)) := lapply(.SD, cumsum)]

dates <- seq(
  from = as.Date("2001-01-01"),
  to   = as.Date("2015-12-01"),
  by   = "month"
)

stopifnot(length(dates) == nrow(all1_1_cum))
all1_1_cum[, date := dates]
setcolorder(all1_1_cum, c("date", intersect(c("RW", "RSM", "AR"), names(all1_1_cum))))

# wide -> long
all1_1_cum_long <- melt(
  all1_1_cum,
  id.vars = "date",
  variable.name = "model",
  value.name = "cum_abs_error"
)
all1_1_cum_long <- all1_1_cum_long[model %in% models]

# plot
cum_err_p1_h1 <- ggplot(all1_1_cum_long, 
                        aes(x = date, y = cum_abs_error, colour = model)) +
  geom_line(size = 0.9) +
  theme_light() +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y") +
  labs(
    title = "Cumulative Absolute Errors (2001–2015, horizon = 1)",
    x = "",
    y = "Cumulative absolute error",
    colour = "Model"
  ) +
  theme(
    plot.title  = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x  = element_text(size = 14, angle = 0, hjust = 0.5),
    axis.text.y  = element_text(size = 12),
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 13)
  )
cum_err_p1_h1

ggsave(
  filename = paste0("03_Output/Charts/cum_err_p1_h1", path_save, ".png"),
  plot     = cum_err_p1_h1,
  width    = 12,
  height   = 6,
  dpi      = 300
)


#### Sample 1, Horizon 3 ####
all1_3_err <- copy(all1_3)
cols <- setdiff(names(all1_3_err), "real")
all1_3_err[, (cols) := lapply(.SD, function(x) abs(x - real)), .SDcols = cols]
all1_3_err[, real := NULL]

all1_3_cum <- copy(all1_3_err)
all1_3_cum[, (names(all1_3_cum)) := lapply(.SD, cumsum)]

dates <- seq(
  from = as.Date("2001-01-01"),
  to   = as.Date("2015-12-01"),
  by   = "month"
)

stopifnot(length(dates) == nrow(all1_3_cum))
all1_3_cum[, date := dates]
setcolorder(all1_3_cum, c("date", intersect(c("RW", "RSM", "AR"), names(all1_3_cum))))

# wide -> long
all1_3_cum_long <- melt(
  all1_3_cum,
  id.vars = "date",
  variable.name = "model",
  value.name = "cum_abs_error"
)
all1_3_cum_long <- all1_3_cum_long[model %in% models]

# plot
ggplot(all1_3_cum_long, aes(x = date, y = cum_abs_error, colour = model)) +
  geom_line(size = 0.8) +
  theme_light() +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y") +
  labs(
    y = "Cumulative absolute error",
    colour = "Model"
  )

cum_err_p1_h3 <- ggplot(all1_3_cum_long, 
                        aes(x = date, y = cum_abs_error, colour = model)) +
  geom_line(size = 0.9) +
  theme_light() +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y") +
  labs(
    title = "Cumulative Absolute Errors (2001–2015, horizon = 3)",
    x = "",
    y = "Cumulative absolute error",
    colour = "Model"
  ) +
  theme(
    plot.title  = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x  = element_text(size = 14, angle = 0, hjust = 0.5),
    axis.text.y  = element_text(size = 12),
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 13)
  )
cum_err_p1_h3

ggsave(
  filename = paste0("03_Output/Charts/cum_err_p1_h3", path_save, ".png"),
  plot     = cum_err_p1_h3,
  width    = 12,
  height   = 6,
  dpi      = 300
)






#### Comparison for Second Out of Sample Period ####
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





#### Calculate Cumulative Absolute Errors
#### Sample 2, Horizon 1 ####

all2_1_err <- copy(all2_1)
cols <- setdiff(names(all2_1_err), "real")
all2_1_err[, (cols) := lapply(.SD, function(x) abs(x - real)), .SDcols = cols]
all2_1_err[, real := NULL]

all2_1_cum <- copy(all2_1_err)
all2_1_cum[, (names(all2_1_cum)) := lapply(.SD, cumsum)]

dates <- seq(
  from = as.Date("2016-01-01"),
  to   = as.Date("2024-12-01"),
  by   = "month"
)

stopifnot(length(dates) == nrow(all2_1_cum))
all2_1_cum[, date := dates]
setcolorder(all2_1_cum, c("date", intersect(c("RW", "RSM", "AR"), names(all2_1_cum))))

# wide -> long
all2_1_cum_long <- melt(
  all2_1_cum,
  id.vars = "date",
  variable.name = "model",
  value.name = "cum_abs_error"
)
all2_1_cum_long <- all2_1_cum_long[model %in% models]

# plot

cum_err_p2_h1 <- ggplot(all2_1_cum_long, 
                        aes(x = date, y = cum_abs_error, colour = model)) +
  geom_line(size = 0.9) +
  theme_light() +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y") +
  labs(
    title = "Cumulative Absolute Errors (2016–2024, horizon = 1)",
    x = "",
    y = "Cumulative absolute error",
    colour = "Model"
  ) +
  theme(
    plot.title  = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x  = element_text(size = 14, angle = 0, hjust = 0.5),
    axis.text.y  = element_text(size = 12),
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 13)
  )
cum_err_p2_h1

ggsave(
  filename = paste0("03_Output/Charts/cum_err_p2_h1", path_save, ".png"),
  plot     = cum_err_p2_h1,
  width    = 12,
  height   = 6,
  dpi      = 300
)


#### Sample 2, Horizon 3 ####


all2_3_err <- copy(all2_3)
cols <- setdiff(names(all2_3_err), "real")
all2_3_err[, (cols) := lapply(.SD, function(x) abs(x - real)), .SDcols = cols]
all2_3_err[, real := NULL]

all2_3_cum <- copy(all2_3_err)
all2_3_cum[, (names(all2_3_cum)) := lapply(.SD, cumsum)]

dates <- seq(
  from = as.Date("2016-01-01"),
  to   = as.Date("2024-12-01"),
  by   = "month"
)

stopifnot(length(dates) == nrow(all2_3_cum))
all2_3_cum[, date := dates]
setcolorder(all2_3_cum, c("date", intersect(c("RW", "RSM", "AR"), names(all2_3_cum))))

# wide -> long
all2_3_cum_long <- melt(
  all2_3_cum,
  id.vars = "date",
  variable.name = "model",
  value.name = "cum_abs_error"
)
all2_3_cum_long <- all2_3_cum_long[model %in% models]

# plot

cum_err_p2_h3 <- ggplot(all2_3_cum_long, 
                        aes(x = date, y = cum_abs_error, colour = model)) +
  geom_line(size = 0.9) +
  theme_light() +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y") +
  labs(
    title = "Cumulative Absolute Errors (2016–2024, horizon = 3)",
    x = "",
    y = "Cumulative absolute error",
    colour = "Model"
  ) +
  theme(
    plot.title  = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x  = element_text(size = 14, angle = 0, hjust = 0.5),
    axis.text.y  = element_text(size = 12),
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 13)
  )
cum_err_p2_h3

ggsave(
  filename = paste0("03_Output/Charts/cum_err_p2_h3", path_save, ".png"),
  plot     = cum_err_p2_h3,
  width    = 12,
  height   = 6,
  dpi      = 300
)






#### Sample All, Horizon 1 -----------------------------------------------------
sample1_end_err <- all1_1_cum_long[date == max(date), .(model, last=cum_abs_error)]
all2_1_cum_continuing <- merge(all2_1_cum_long, sample1_end_err, by="model", all.x=T)
all2_1_cum_continuing[, cum_abs_error := cum_abs_error + last]
all2_1_cum_continuing[, last := NULL]
sam_all_1_cum <- rbind(all1_1_cum_long, all2_1_cum_continuing)
sam_all_1_cum[, model := factor(model,
                                levels = c("RSM",          "AR",          "ElNet_L",            "ElNet",          "LLF_L",          "LLF"),
                                labels = c("Bench. (RSM)", "Bench. (AR)", "Phil. C. (ElNet_L)", "LinVar (ElNet)", "NonLin (LLF_L)", "NonLin and Var (LLF)")
)]
cum_all_step1 <- ggplot(sam_all_1_cum, aes(x=date, y=cum_abs_error, colour=model)) +
  geom_line(size = 0.9) + theme_light() +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y") +
  labs(title="(2001–2025, Step 1)", x="", y="Cum. Abs. Error", colour=NULL) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",
    legend.direction = "horizontal"
  ) +
  guides(colour = guide_legend(nrow = 2))
cum_all_step1
ggsave(paste0("03_Output/Paper/Cum_Error/cum_err_all_step1", path_save, ".png"), width=7, height=4)


#### Sample All, Horizon 3 -----------------------------------------------------
sample1_end_err <- all1_3_cum_long[date == max(date), .(model, last=cum_abs_error)]
all2_3_cum_continuing <- merge(all2_3_cum_long, sample1_end_err, by="model", all.x=T)
all2_3_cum_continuing[, cum_abs_error := cum_abs_error + last]
all2_3_cum_continuing[, last := NULL]
sam_all_3_cum <- rbind(all1_3_cum_long, all2_3_cum_continuing)
cum_all_step3 <- ggplot(sam_all_3_cum, aes(x=date, y=cum_abs_error, colour=model)) +
  geom_line(size = 0.9) +  theme_light() +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y") +
  labs(title="(2001–2025, Step 3)", x="", y="Cum. Abs. Error", colour="Model")
cum_all_step3

sam_all_3_cum <- rbind(all1_3_cum_long, all2_3_cum_continuing)
sam_all_3_cum[, model := factor(model,
                                levels = c("RSM",          "AR",          "ElNet_L",            "ElNet",          "LLF_L",          "LLF"),
                                labels = c("Bench. (RSM)", "Bench. (AR)", "Phil. C. (ElNet_L)", "LinVar (ElNet)", "NonLin (LLF_L)", "NonLin and Var (LLF)")
)]
cum_all_step3 <- ggplot(sam_all_3_cum, aes(x=date, y=cum_abs_error, colour=model)) +
  geom_line(size = 0.9) + theme_light() +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y") +
  labs(title="(2001–2025, Step 3)", x="", y="Cum. Abs. Error", colour=NULL) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",
    legend.direction = "horizontal"
  ) +
  guides(colour = guide_legend(nrow = 2))
cum_all_step3
ggsave(paste0("03_Output/Paper/Cum_Error/cum_err_all_step3", path_save,".png"), width=7, height= 4)



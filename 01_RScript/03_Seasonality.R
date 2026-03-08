# Author: Orhun Ozel
# Date  : 26/11/2025
# Scope : Check Seasonality
rm(list = ls())
source("01_RScript/00_Functions_Lasso.R")
library(data.table)
library(glmnet)
library(ggplot2)
library(gt)
library(scales)
library(RJDemetra)
options(print.max = 300, scipen = 50, digits = 3)

### Load & prepare
fred <- readRDS("02_Input/data_cleaned.rds")
setDT(fred)
setnames(fred, "CPIAUCSL", "inf")
setcolorder(fred, c("date", "inf"))

inf <- ts(fred$inf, start=c(1960, 1), frequency = 12)
monthplot(inf)



#' Plots a comparison between hourly and daily values for the same test data the C code is using
source("test_hFWI.r")
source("NG_FWI.r")
library(ggplot2)
library(data.table)
library(lubridate)
df_wx <- as.data.table(read.csv("./data/wx_hourly.csv"))
setnames(df_wx, c("year", "hour", "wind", "rain"), c("yr", "hr", "ws", "prec"))
result <- test_hfwi(df_wx, timezone = -6)

plot_comparison(result)

wx_diurnal <- as.data.table(read.csv("./data/wx_diurnal.csv"))
setnames(wx_diurnal, c("year", "hour", "wind", "rain"), c("yr", "hr", "ws", "prec"))
result3 <- test_hfwi(wx_diurnal, timezone = -6)

plot_comparison(result3)

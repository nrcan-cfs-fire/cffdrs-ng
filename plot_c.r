#' Plots a comparison between hourly and daily values for the same test data the C code is using
source("test_hFWI.r")
source("NG_FWI.r")
library(ggplot2)
library(data.table)
library(lubridate)
bak <- as.data.table(read.csv("./bak_hourly.csv"))
setnames(bak, c("year", "hour", "wind", "rain"), c("yr", "hr", "ws", "prec"))
result <- test_hfwi(bak, timezone = -6)

plot_comparison(result)

bak_diurnal <- as.data.table(read.csv("./bak_diurnal.csv"))
setnames(bak_diurnal, c("year", "hour", "wind", "rain"), c("yr", "hr", "ws", "prec"))
result3 <- test_hfwi(bak_diurnal, timezone = -6)

plot_comparison(result3)

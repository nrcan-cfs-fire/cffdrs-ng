source("test_hFWI.r")
source("hFWI.r")
library(ggplot2)
library(data.table)
library(lubridate)
bak <- as.data.table(read.csv("./bak_hourly.csv"))
setnames(bak, c("year", "hour", "wind", "rain"), c("yr", "hr", "ws", "prec"))
result <- test_hfwi(bak, timezone=-6)

plot_comparison(result)

bak_diurnal <- as.data.table(read.csv("./bak_diurnal.csv"))
setnames(bak_diurnal, c("year", "hour", "wind", "rain"), c("yr", "hr", "ws", "prec"))
result3 <- test_hfwi(bak_diurnal, timezone=-6)

plot_comparison(result3)


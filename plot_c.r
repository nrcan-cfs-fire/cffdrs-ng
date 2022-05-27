source("test_hFWI.r")
source("hFWI.r")
library(ggplot2)
library(data.table)
library(lubridate)
DIR_DATA <- normalizePath("../data")

csv_bak_hourly <- paste0(DIR_DATA, "/BAK2018_hourly.csv")
bak <- as.data.table(read.csv(csv_bak_hourly, header=FALSE, col.names=c("lat", "long", "yr", "mon", "day", "hr", "temp", "rh", "ws", "prec")))
result <- test_hfwi(bak)

plot_comparison(result)

#' Runs through the functions and outputs results so they can be compared to the C code outputs
source("NG_FWI.r")
library(ggplot2)
library(data.table)

library(lubridate)
df_wx <- as.data.table(read.csv("./data/wx_hourly.csv"))
result <- hFWI(df_wx, timezone = -6)
save_csv(result, "./result.csv")

# # create data file from cffdrs R package file
# test_hffmc <- read.csv("test_hffmc.csv")
# df <- data.table(test_hffmc)
# df[, lat := df_wx$lat[[1]]]
# df[, long := df_wx$long[[1]]]
# write.table(df[, c("lat", "long", "yr", "mon", "day", "hr", "temp", "rh", "ws", "rh")], "./data/test_hffmc.csv", quote = FALSE, row.names = FALSE, sep = ",")


df <- as.data.table(read.csv("./data/test_hffmc.csv"))

result_hffmc <- hFWI(df, timezone = -6)
save_csv(result_hffmc, "./result_hffmc.csv")

source("make_daily.r")
df_wx <- as.data.table(read.csv("./data/wx_hourly.csv"))
df <- hourly_to_daily(df_wx)
save_csv(df, "./data/wx_daily.csv")


source("make_minmax.r")
df_wx <- as.data.table(read.csv("./data/wx_daily.csv"))
df <- daily_to_minmax(df_wx)
save_csv(df, "./data/wx_minmax.csv")



source("make_hourly.r")
wx_minmax <- as.data.table(read.csv("./data/wx_minmax.csv"))
df <- minmax_to_hourly(wx_minmax, timezone = -6)
df_hourly <- copy(df)
save_csv(df, "./data/wx_diurnal.csv")


wx_diurnal <- as.data.table(read.csv("./data/wx_diurnal.csv"))
result3 <- hFWI(wx_diurnal, timezone = -6)
save_csv(result3, "./result3.csv")


wx_windy <- as.data.table(read.csv("./data/wx_windy.csv"))
result4 <- hFWI(wx_windy, timezone = -6)
save_csv(result4, "./result4.csv")

save_csv(hFWI(as.data.table(read.csv("./data/wx_rh100.csv")), timezone = -6), "./result5.csv")
save_csv(hFWI(as.data.table(read.csv("./data/wx_rh0.csv")), timezone = -6), "./result6.csv")

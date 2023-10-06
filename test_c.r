#' Runs through the functions and outputs results so they can be compared to the C code outputs
source("NG_FWI.r")
library(ggplot2)
library(data.table)
library(lubridate)
df_wx <- as.data.table(read.csv("./wx_hourly.csv"))

save_csv <- function(df, file) {
  result <- copy(df)
  colnames(result) <- tolower(colnames(result))
  result[, mon := sprintf("%2d", mon)]
  result[, day := sprintf("%2d", day)]
  result[, hour := sprintf("%2d", hour)]
  result[, temp := sprintf("%5.1f", round(temp, 1))]
  result[, rh := sprintf("%3.0f", round(rh, 0))]
  result[, wind := sprintf("%5.1f", round(wind, 1))]
  result[, rain := sprintf("%5.1f", round(rain, 2))]
  result[, ffmc := sprintf("%6.1f", round(ffmc, 1))]
  result[, dmc := sprintf("%6.1f", round(dmc, 1))]
  result[, dc := sprintf("%6.1f", round(dc, 1))]
  result[, isi := sprintf("%6.1f", round(isi, 1))]
  result[, bui := sprintf("%6.1f", round(bui, 1))]
  result[, fwi := sprintf("%6.1f", round(fwi, 1))]
  result[, gfmc := sprintf("%6.1f", round(gfmc, 1))]
  result[, gsi := sprintf("%6.1f", round(gsi, 1))]
  result[, gfwi := sprintf("%6.1f", round(gfwi, 1))]
  write.csv(result[, c("year", "mon", "day", "hour", "temp", "rh", "wind", "rain", "ffmc", "dmc", "dc", "isi", "bui", "fwi", "gfmc", "gsi", "gfwi")], file, row.names = FALSE, quote = FALSE)
}

result <- hFWI(df_wx, timezone = -6)
save_csv(result, "./result.csv")

test_hffmc <- read.csv("test_hffmc.csv")
df <- data.table(test_hffmc)
df[, lat := df_wx$lat[[1]]]
df[, long := df_wx$long[[1]]]
setnames(df, c("yr", "hr", "ws", "prec"), c("year", "hour", "wind", "rain"))
write.table(df[, c("lat", "long", "year", "mon", "day", "hour", "temp", "rh", "wind", "rain")], "./input_hffmc.csv", quote = FALSE, row.names = FALSE, sep = ",")
df <- as.data.table(read.csv("./input_hffmc.csv"))

result_hffmc <- hFWI(df, timezone = -6)
save_csv(result_hffmc, "./result_hffmc.csv")

source("make_daily.r")
df_wx <- as.data.table(read.csv("./wx_hourly.csv"))
df <- hourly_to_daily(df_wx)
df[, mon := sprintf("%02d", mon)]
df[, day := sprintf("%02d", day)]
df[, hour := sprintf("%02d", hour)]
df[, temp := sprintf("%.1f", round(temp, 1))]
df[, rh := sprintf("%.0f", round(rh, 0))]
df[, wind := sprintf("%.1f", round(wind, 1))]
df[, rain := sprintf("%.1f", round(rain, 1))]
write.table(df, "wx_daily.csv", quote = FALSE, sep = ",", row.names = FALSE)


source("make_minmax.r")
df_wx <- as.data.table(read.csv("./wx_daily.csv"))
df <- daily_to_minmax(df_wx)
df[, mon := sprintf("%02d", mon)]
df[, day := sprintf("%02d", day)]
df[, hour := sprintf("%02d", hour)]
df[, temp_max := sprintf("%.1f", temp_max)]
df[, temp_min := sprintf("%.1f", temp_min)]
df[, rh_max := sprintf("%.0f", rh_max)]
df[, rh_min := sprintf("%.0f", rh_min)]
df[, wind_max := sprintf("%.1f", wind_max)]
df[, wind_min := sprintf("%.1f", wind_min)]
df[, rain := sprintf("%.1f", rain)]
write.table(df, "wx_minmax.csv", quote = FALSE, sep = ",", row.names = FALSE)



source("make_hourly.r")
wx_minmax <- as.data.table(read.csv("./wx_minmax.csv"))
df <- minmax_to_hourly(wx_minmax, timezone = -6)
df_hourly <- copy(df)
df[, year := sprintf("%02d", year)]
df[, mon := sprintf("%02d", mon)]
df[, day := sprintf("%02d", day)]
df[, hour := sprintf("%02d", hour)]
df[, temp := sprintf("%.1f", round(temp, 1))]
df[, rh := sprintf("%.0f", round(rh, 0))]
df[, wind := sprintf("%.1f", round(wind, 1))]
df[, rain := sprintf("%.1f", round(rain, 1))]
write.table(df, "wx_diurnal.csv", quote = FALSE, sep = ",", row.names = FALSE)


wx_diurnal <- as.data.table(read.csv("./wx_diurnal.csv"))
result3 <- hFWI(wx_diurnal, timezone = -6)
save_csv(result3, "./result3.csv")


wx_windy <- as.data.table(read.csv("./wx_windy.csv"))
result4 <- hFWI(wx_windy, timezone = -6)
save_csv(result4, "./result4.csv")

save_csv(hFWI(as.data.table(read.csv("./wx_rh100.csv")), timezone = -6), "./result5.csv")
save_csv(hFWI(as.data.table(read.csv("./wx_rh0.csv")), timezone = -6), "./result6.csv")

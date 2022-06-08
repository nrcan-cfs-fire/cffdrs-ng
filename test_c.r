source("hFWI.r")
library(ggplot2)
library(data.table)
library(lubridate)
bak <- as.data.table(read.csv("./bak_hourly.csv"))

save_csv <- function(df, file)
{
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
  write.csv(result[, c("year", "mon", "day", "hour", "temp", "rh", "wind", "rain", "ffmc", "dmc", "dc", "isi", "bui", "fwi", "gfmc", "gsi", "gfwi")], file, row.names=FALSE, quote=FALSE)
}

result <- hFWI(bak, timezone=-6)
save_csv(result, "./result.csv")

df <- data.table(cffdrs::test_hffmc)
df[, lat := bak$lat[[1]]]
df[, long := bak$long[[1]]]
setnames(df, c("yr", "hr", "ws", "prec"), c("year", "hour", "wind", "rain"))
write.table(df[, c("lat", "long", "year", "mon", "day", "hour", "temp", "rh", "wind", "rain")], "./input_hffmc.csv", quote=FALSE, row.names=FALSE, sep=",")
df <- as.data.table(read.csv("./input_hffmc.csv"))

result_hffmc <- hFWI(df, timezone=-6)
save_csv(result_hffmc, "./result_hffmc.csv")

source("make_daily.r")
bak <- as.data.table(read.csv("./bak_hourly.csv"))
df <- hourly_to_daily(bak)
df[, mon := sprintf("%02d", mon)]
df[, day := sprintf("%02d", day)]
df[, hour := sprintf("%02d", hour)]
write.table(df, "bak_daily.csv", quote=FALSE, sep=",", row.names=FALSE)


source("make_minmax.r")
bak <- as.data.table(read.csv("./bak_daily.csv"))
df <- daily_to_minmax(bak)
df[, mon := sprintf("%02d", mon)]
df[, day := sprintf("%02d", day)]
df[, hour := sprintf("%02d", hour)]
df[, temp_max := sprintf("%.2f", temp_max)]
df[, temp_min := sprintf("%.2f", temp_min)]
df[, rh_max := sprintf("%.2f", rh_max)]
df[, rh_min := sprintf("%.2f", rh_min)]
df[, wind_max := sprintf("%.2f", wind_max)]
df[, wind_min := sprintf("%.2f", wind_min)]
df[, rain := sprintf("%.2f", rain)]
write.table(df, "bak_minmax.csv", quote=FALSE, sep=",", row.names=FALSE)



source("make_hourly.r")
bak_minmax <- as.data.table(read.csv("./bak_minmax.csv"))
df <- minmax_to_hourly(bak_minmax, timezone=-6)
df_hourly <- copy(df)
df[, year := sprintf("%02d", year)]
df[, mon := sprintf("%02d", mon)]
df[, day := sprintf("%02d", day)]
df[, hour := sprintf("%02d", hour)]
df[, temp := sprintf("%.1f", round(temp, 1))]
df[, rh := sprintf("%.0f", round(rh, 0))]
df[, wind := sprintf("%.1f", round(wind, 1))]
df[, rain := sprintf("%.1f", round(rain, 1))]
write.table(df, "bak_diurnal.csv", quote=FALSE, sep=",", row.names=FALSE)


bak_diurnal <- as.data.table(read.csv("./bak_diurnal.csv"))
result3 <- hFWI(bak_diurnal, timezone=-6)
save_csv(result3, "./result3.csv")


bak_windy <- as.data.table(read.csv("./bak_windy.csv"))
result4 <- hFWI(bak_windy, timezone=-6)
save_csv(result4, "./result4.csv")

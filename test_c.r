source("hFWI.r")
library(ggplot2)
library(data.table)
library(lubridate)
DIR_DATA <- normalizePath("../data")

csv_bak_hourly <- paste0(DIR_DATA, "/BAK2018_hourly.csv")
bak <- as.data.table(read.csv(csv_bak_hourly, header=FALSE, col.names=c("lat", "long", "yr", "mon", "day", "hr", "temp", "rh", "ws", "prec")))


save_csv <- function(df, file)
{
  result <- copy(df)
  setnames(result, c("yr", "hr", "ws", "prec"), c("year", "hour", "wind", "rain"))
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

df <- data.table(test_hffmc)
df[, lat := bak$lat[[1]]]
df[, long := bak$long[[1]]]
setnames(df, c("yr", "hr", "prec"), c("year", "hour", "rain"))
write.table(df[, c("lat", "long", "year", "mon", "day", "hour", "temp", "rh", "ws", "rain")], "./input_hffmc.csv", row.names=FALSE, col.names=FALSE, sep=",")
df <- as.data.table(read.csv("./input_hffmc.csv", header=FALSE, col.names=c("lat", "long", "yr", "mon", "day", "hr", "temp", "rh", "ws", "prec")))

result_hffmc <- hFWI(df, timezone=-6)
save_csv(result_hffmc, "./result_hffmc.csv")

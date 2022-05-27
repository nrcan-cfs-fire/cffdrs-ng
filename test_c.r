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
  result[, MIN_RH := sprintf("%0.1f", round(MIN_RH, 1))]
  result[, temp := sprintf("%0.1f", round(temp, 1))]
  result[, rh := sprintf("%0.0f", round(rh, 0))]
  result[, ws := sprintf("%0.1f", round(ws, 1))]
  result[, prec := sprintf("%0.2f", round(prec, 2))]
  result[, SOLPROP := sprintf("%0.5f", round(SOLPROP, 5))]
  result[, SOLRAD := sprintf("%0.5f", round(SOLRAD, 5))]
  result[, SUNRISE := sprintf("%0.3f", round(SUNRISE, 5))]
  result[, SUNSET := sprintf("%0.3f", round(SUNSET, 5))]
  result[, FFMC := sprintf("%0.1f", round(FFMC, 1))]
  result[, DMC := sprintf("%0.1f", round(DMC, 1))]
  result[, DC := sprintf("%0.1f", round(DC, 1))]
  result[, ISI := sprintf("%0.1f", round(ISI, 1))]
  result[, BUI := sprintf("%0.1f", round(BUI, 1))]
  result[, FWI := sprintf("%0.1f", round(FWI, 1))]
  result[, DSR := sprintf("%0.1f", round(DSR, 1))]
  result[, MCGMC := sprintf("%0.1f", round(MCGMC, 1))]
  result[, GFMC := sprintf("%0.1f", round(GFMC, 1))]
  result[, GSI := sprintf("%0.1f", round(GSI, 1))]
  result[, GFWI := sprintf("%0.1f", round(GFWI, 1))]
  # write.csv(result[, -c("lat", "long", "DSR")], "./result.csv", row.names=FALSE, quote=FALSE)
  # write.csv(result[, c("yr", "mon", "day", "hr", "SOLPROP", "SOLRAD", "SUNRISE", "SUNSET", "MIN_RH", "temp", "rh", "ws", "prec", "FFMC", "DMC", "DC", "ISI", "BUI", "FWI", "MCGMC", "GFMC", "GSI", "GFWI")], "./result.csv", row.names=FALSE, quote=FALSE)
  write.csv(result[, c("yr", "mon", "day", "hr", "MIN_RH", "temp", "rh", "ws", "prec", "FFMC", "DMC", "DC", "ISI", "BUI", "FWI", "MCGMC", "GFMC", "GSI", "GFWI")], file, row.names=FALSE, quote=FALSE)
}

result <- hFWI(bak)
save_csv(result, "./result.csv")

df <- data.table(test_hffmc)
df[, lat := bak$lat[[1]]]
df[, long := bak$long[[1]]]
setnames(df, c("yr", "hr", "prec"), c("year", "hour", "rain"))
write.csv(df[, c("lat", "long", "year", "mon", "day", "hour", "temp", "rh", "ws", "rain")], "./input_hffmc.csv", row.names=FALSE, col.names=FALSE)
df <- as.data.table(read.csv("./input_hffmc.csv", header=FALSE, col.names=c("lat", "long", "yr", "mon", "day", "hr", "temp", "rh", "ws", "prec")))

result_hffmc <- hFWI(df)
save_csv(result_hffmc, "./result_hffmc.csv")

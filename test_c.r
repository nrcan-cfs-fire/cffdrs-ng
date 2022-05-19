source("hFWI.r")
library(ggplot2)

DIR_DATA <- normalizePath("../data")

csv_bak_hourly <- paste0(DIR_DATA, "/BAK2018_hourly.csv")
bak <- as.data.table(read.csv(csv_bak_hourly, header=FALSE, col.names=c("lat", "long", "yr", "mon", "day", "hr", "temp", "rh", "ws", "prec")))
result <- hFWI(bak)
result[, temp := sprintf("%0.1f", round(temp, 1))]
result[, rh := sprintf("%0.0f", round(rh, 0))]
result[, ws := sprintf("%0.1f", round(ws, 1))]
result[, prec := sprintf("%0.1f", round(prec, 1))]
result[, FFMC := sprintf("%0.1f", round(FFMC, 1))]
result[, DMC := sprintf("%0.1f", round(DMC, 1))]
result[, DC := sprintf("%0.1f", round(DC, 1))]
result[, ISI := sprintf("%0.1f", round(ISI, 1))]
result[, BUI := sprintf("%0.1f", round(BUI, 1))]
result[, FWI := sprintf("%0.1f", round(FWI, 1))]
result[, DSR := sprintf("%0.1f", round(DSR, 1))]
write.csv(result[, -c("lat", "long", "DSR")], "./result.csv", row.names=FALSE, quote=FALSE)

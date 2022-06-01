DIR_DATA <- normalizePath("../data")

csv_bak_hourly <- paste0(DIR_DATA, "/BAK2018_hourly.csv")
bak <- as.data.table(read.csv(csv_bak_hourly, header=FALSE, col.names=c("lat", "long", "yr", "mon", "day", "hr", "temp", "rh", "ws", "prec")))

df <- data.table(bak)
df[, DATE := as_date(sprintf("%4d-%02d-%02d", yr, mon, day))]
df[, FOR_DATE := as_date(ifelse(hr <= 12, DATE, DATE + days(1)))]
precip <- df[, list(prec = sum(prec, na.rm=TRUE)), by=c("FOR_DATE")]
df <- df[hr == 12, -c("prec")]
df <- merge(df, precip, by=c("FOR_DATE"))[, -c("DATE", "FOR_DATE")]
df[, mon := sprintf("%02d", mon)]
df[, day := sprintf("%02d", day)]
df[, hr := sprintf("%02d", hr)]
write.table(df, "bak_daily.csv", quote=FALSE, sep=",", row.names=FALSE, col.names=FALSE)

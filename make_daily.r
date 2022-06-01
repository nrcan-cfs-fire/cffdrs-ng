bak <- as.data.table(read.csv("./bak_hourly.csv"))

df <- data.table(bak)
df[, DATE := as_date(sprintf("%4d-%02d-%02d", year, mon, day))]
df[, FOR_DATE := as_date(ifelse(hour <= 12, DATE, DATE + days(1)))]
rain <- df[, list(rain = sum(rain, na.rm=TRUE)), by=c("FOR_DATE")]
df <- df[hour == 12, -c("rain")]
df <- merge(df, rain, by=c("FOR_DATE"))[, -c("DATE", "FOR_DATE")]
df[, mon := sprintf("%02d", mon)]
df[, day := sprintf("%02d", day)]
df[, hour := sprintf("%02d", hour)]
write.table(df, "bak_daily.csv", quote=FALSE, sep=",", row.names=FALSE)

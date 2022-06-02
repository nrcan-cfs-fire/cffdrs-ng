# convert daily weather into min/max weather format

findQ <- function(temp, rh)
{
  # find absolute humidity
  svp <- 6.108 * exp(17.27 * temp / (temp + 237.3))
  vp <- svp * rh / 100.0
  return(217 * vp / (273.17 + temp))
}

findrh <- function(q, temp)
{
  cur_vp <- (273.17 + temp) * q / 217
  return(100 * cur_vp / (6.108 * exp(17.27 * temp / (temp + 237.3))))
}

bak <- as.data.table(read.csv("./bak_daily.csv"))

df <- data.table(bak)

df[, temp_min := temp - 15]
df[, temp_max := temp + 2]
df[, q := findQ(temp, rh)]
df[, rh_min := findrh(q, temp_max)]
df[, rh_min := ifelse(rh_min < 0, 0, rh_min)]
df[, rh_max := findrh(q, temp_min)]
df[, rh_max := ifelse(rh_max > 100, 100, rh_max)]
df[, wind_min := 0.15 * wind]
df[, wind_max := 1.25 * wind]
df[, mon := sprintf("%02d", mon)]
df[, day := sprintf("%02d", day)]
df[, hour := sprintf("%02d", hour)]
df <- df[, c("lat", "long", "year", "mon", "day", "hour", "temp_min", "temp_max", "rh_min", "rh_max", "wind_min", "wind_max", "rain")]
write.table(df, "bak_minmax.csv", quote=FALSE, sep=",", row.names=FALSE)

# convert daily weather into min/max weather format
library(data.table)
source("util.r")

#' Convert daily noon values stream to daily min/max values stream.
#' Uses values from statistics to do the conversion.
#'
#' @param df        daily noon values weather stream [lat, long, year, mon, day, hour, temp, rh, wind, rain]
#' @return          daily min/max values weather stream [lat, long, year, mon, day, hour, temp_min, temp_max, rh_min, rh_max, wind_min, wind_max, rain]
#' @export daily_to_minmax
daily_to_minmax <- function(df)
{
  df <- data.table(df)
  df[, temp_min := temp - 15]
  df[, temp_max := temp + 2]
  df[, q := findQ(temp, rh)]
  df[, rh_min := findrh(q, temp_max)]
  df[, rh_min := ifelse(rh_min < 0, 0, rh_min)]
  df[, rh_max := findrh(q, temp_min)]
  df[, rh_max := ifelse(rh_max > 100, 100, rh_max)]
  df[, wind_min := 0.15 * wind]
  df[, wind_max := 1.25 * wind]
  df <- df[, c("lat", "long", "year", "mon", "day", "hour", "temp_min", "temp_max", "rh_min", "rh_max", "wind_min", "wind_max", "rain")]
  return(df)
}

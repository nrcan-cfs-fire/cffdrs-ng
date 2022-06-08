# convert hourly weather into daily format
library(data.table)
library(lubridate)

#' Convert hourly values stream to daily noon values stream.
#'
#' @param   df    hourly values weather stream [lat, long, year, mon, day, hour, temp, rh, wind, rain]
#' @return        daily noon values weather stream [lat, long, year, mon, day, hour, temp, rh, wind, rain]
#' @export hourly_to_daily
hourly_to_daily <- function(df)
{
  df <- data.table(df)
  df[, DATE := as_date(sprintf("%4d-%02d-%02d", year, mon, day))]
  df[, FOR_DATE := as_date(ifelse(hour <= 12, DATE, DATE + days(1)))]
  rain <- df[, list(rain = sum(rain, na.rm=TRUE)), by=c("FOR_DATE")]
  df <- df[hour == 12, -c("rain")]
  df <- merge(df, rain, by=c("FOR_DATE"))[, -c("DATE", "FOR_DATE")]
  return(df)
}

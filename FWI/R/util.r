#' Various utility functions used by the other files
library(data.table)
library(lubridate)


#' Determine if data is sequential at intervals of 1 unit
#'
#' @param data          data to check
#' @return              whether each entry in data is 1 unit from the next entry
is_sequential <- function(data) {
  v <- na.omit(unique(data - data.table::shift(data, 1)))
  return(length(data) == 1 || (1 == v[[1]] && length(v) == 1))
}

#' Determine if data is sequential days
#'
#' @param df            data to check
#' @return              whether each entry is 1 day from the next entry
is_sequential_days <- function(df) {
  return(is_sequential(as.Date(df$date)))
}

#' Determine if data is sequential hours
#'
#' @param df            data to check
#' @return              whether each entry is 1 hour from the next entry
is_sequential_hours <- function(df) {
  return(is_sequential(as.POSIXct(df$timestamp)))
}

#' Find specific humidity
#'
#' @param temp        Temperature (Celcius)
#' @param rh          Relative humidity (percent, 0-100)
#' @return            Specific humidity (g/kg)
find_q <- function(temp, rh) {
  # find absolute humidity
  svp <- 6.108 * exp(17.27 * temp / (temp + 237.3))
  vp <- svp * rh / 100.0
  return(217 * vp / (273.17 + temp))
}

#' Find relative humidity
#'
#'  @param q           Specific humidity (g/kg)
#'  @param temp        Temperature (Celcius)
#'  @return            Relative humidity (percent, 0-100)
find_rh <- function(q, temp) {
  cur_vp <- (273.17 + temp) * q / 217
  return(100 * cur_vp / (6.108 * exp(17.27 * temp / (temp + 237.3))))
}

#' Find day of year. Does not properly deal with leap years.
#'
#' @param mon         Month
#' @param day         Day of month
#' @return            Day of year
julian <- function(mon, day) {
  month <- c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365)
  return(month[mon] + day)
}

#' Calculate sunrise, sunset, (solar radiation) for one station (location) for one year
#' (does not take leap years into account)
#'
#' @param dt                data.table to add columns to
#' @param get_solrad        Whether to calculate solar radiation
#' @return                  Sunrise, sunset, sunlight hours, and solar radiation (kW/m^2)
get_sunlight <- function(dt, get_solrad = FALSE) {
  # columns to split along unique days
  cols_day <- c("lat", "long", "date", "timezone")
  # required columns
  cols_req <- c("lat", "long", "timezone", "timestamp")
  if (get_solrad) {
    cols_req <- c(cols_req, "temp")
  }
  for (n in cols_req) {
    stopifnot(n %in% colnames(dt))
  }
  df_copy <- copy(dt)
  # (re)make date column
  df_copy[, date := as_date(timestamp)]

  # calculate sunrise and sunset
  # drop duplicate days
  df_stn_dates <- unique(df_copy[, ..cols_day])
  df_dates <- unique(df_stn_dates[, list(date)])
  df_dates[, jd := julian(month(date), day(date))]
  dechour <- 12.0
  df_dates[, fracyear := 2.0 * pi / 365.0 * (jd - 1.0 + (dechour - 12.0) / 24.0)]
  df_dates[, eqtime := 229.18 * (0.000075 +
    0.001868 * cos(fracyear) - 0.032077 * sin(fracyear) -
    0.014615 * cos(2.0 * fracyear) - 0.040849 * sin(2.0 * fracyear))]
  df_dates[, decl := 0.006918 -
    0.399912 * cos(fracyear) + 0.070257 * sin(fracyear) -
    0.006758 * cos(fracyear * 2.0) + 0.000907 * sin(2.0 * fracyear) -
    0.002697 * cos(3.0 * fracyear) + 0.00148 * sin(3.0 * fracyear)]
  df_dates[, zenith := 90.833 * pi / 180.0]
  # at this point we actually need the LAT/LONG/TIMEZONE
  df_dates <- merge(df_stn_dates, df_dates, by = c("date"))
  df_dates[, timeoffset := eqtime + 4 * long - 60 * timezone]
  df_dates[, x_tmp := cos(zenith) / (cos(lat * pi / 180.0) * cos(decl)) -
    tan(lat * pi / 180.0) * tan(decl)]
  # keep in range
  df_dates[, x_tmp := pmax(-1, pmin(1, x_tmp))]
  df_dates[, halfday := 180.0 / pi * acos(x_tmp)]
  df_dates[, sunrise := (720.0 - 4.0 * (long + halfday) - eqtime) / 60 + timezone]
  df_dates[, sunset := (720.0 - 4.0 * (long - halfday) - eqtime) / 60 + timezone]
  df_all <- merge(df_copy, df_dates, by = cols_day)

  # calculate solar radiation
  if (get_solrad) {
    df_all[, hr := hour(timestamp)]
    df_all[, tst := as.numeric(hr) * 60.0 + timeoffset]
    df_all[, hourangle := tst / 4 - 180]
    df_all[, zenith := acos(sin(lat * pi / 180) * sin(decl) +
      cos(lat * pi / 180) * cos(decl) * cos(hourangle * pi / 180))]
    df_all[, zenith := pmin(pi / 2, zenith)]
    df_all[, cos_zenith := cos(zenith)]
    df_all[, vpd := 6.11 * (1.0 - rh / 100.0) * exp(17.29 * temp / (temp + 237.3))]
    df_all[, solrad := 0.0]
    df_all[(hour(timestamp) >= sunrise) & (hour(timestamp) <= sunset),
      solrad := cos_zenith * 0.92 * (1.0 - exp(-0.22 * vpd))]

    cols_sun <- c("solrad", "sunrise", "sunset")
  } else {
    cols_sun <- c("sunrise", "sunset")
  }

  # don't output intermediate calculations/variables
  cols <- c(names(dt), cols_sun)
  df_result <- df_all[, ..cols]
  df_result[, sunlight_hours := sunset - sunrise]
  return(df_result)
 }

seasonal_curing <- function(julian_date) {
  PERCENT_CURED <- c(
    96.0,
    96.0,
    96.0,
    96.0,
    96.0,
    96.0,
    96.0,
    96.0,
    95.0,
    93.0,
    92.0,
    90.5,
    88.4,
    84.4,
    78.1,
    68.7,
    50.3,
    32.9,
    23.0,
    22.0,
    21.0,
    20.0,
    25.7,
    35.0,
    43.0,
    49.8,
    60.0,
    68.0,
    72.0,
    75.0,
    78.9,
    86.0,
    96.0,
    96.0,
    96.0,
    96.0,
    96.0,
    96.0
  )
  jd_class <- (julian_date %/% 10) + 1
  first <- PERCENT_CURED[jd_class]
  last <- PERCENT_CURED[jd_class + 1]
  period_frac <- (julian_date %% 10) / 10.0
  return(first + (last - first) * period_frac)
}

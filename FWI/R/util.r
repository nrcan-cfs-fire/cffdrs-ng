#' Various utility functions used by the other files
library(data.table)
library(lubridate)



#' Determine if data is sequential days
#'
#' @param df            data to check (requires timestamp column)
#' @return              whether each entry is 1 day from the next entry
is_sequential_days <- function(df) {
  data <- copy(df)
  colnames(data) <- tolower(colnames(data))
  if (!"timestamp" %in% names(data)) {
    stop("timestamp column (using make_datetime()) required to check sequential days")
  }
  l <- nrow(data)
  diff <- data$timestamp[2:l] - data$timestamp[1:l - 1]
  return(l == 1 || (all(diff == 1) && all(attr(diff, "units") == "days")))
}

#' Determine if data is sequential hours
#'
#' @param df            data to check (requires timestamp column)
#' @return              whether each entry is 1 hour from the next entry
is_sequential_hours <- function(df) {
  data <- copy(df)
  colnames(data) <- tolower(colnames(data))
  if (!"timestamp" %in% names(data)) {
    stop("timestamp column (using make_datetime()) required to check sequential hours")
  }
  l <- nrow(data)
  diff <- data$timestamp[2:l] - data$timestamp[1:l - 1]
  return(l == 1 || (all(diff == 1) && all(attr(diff, "units") == "hours")))
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

# #' Find day of year. Does not properly deal with leap years.
# #'
# #' @param mon         Month
# #' @param day         Day of month
# #' @return            Day of year
# julian <- function(mon, day) {
#   month <- c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365)
#   return(month[mon] + day)
# }

#' Calculate sunrise, sunset, (solar radiation) for one station (location) for one year
#' (does not take leap years into account)
#'
#' @param dt                data.table to add columns to
#' @param get_solrad        Whether to calculate solar radiation
#' @return                  Sunrise, sunset, sunlight hours, and solar radiation (kW/m^2)
get_sunlight <- function(dt, get_solrad = FALSE) {
  colnames(dt) <- tolower(colnames(dt))
  # columns to split along unique days
  cols_day <- c("lat", "long", "timezone", "date")
  # required columns
  cols_req <- c("lat", "long", "timezone", "timestamp")
  if (get_solrad) {
    cols_req <- c(cols_req, "temp", "rh")
  }
  for (n in cols_req) {
    stopifnot(n %in% colnames(dt))
  }
  df_copy <- copy(dt)
  if (!"date" %in% colnames(df_copy)) {
    df_copy[, date := as_date(timestamp)]
  }

  # calculate sunrise and sunset
  # drop duplicate days
  df_stn_dates <- unique(df_copy[, ..cols_day])
  df_dates <- unique(df_stn_dates[, list(date)])
  df_dates[, jd := yday(date)]
  # calculate fraction of the year
  dechour <- 12.0
  df_dates[, fracyear := 2.0 * pi * (jd - 1.0 + (dechour - 12.0) / 24.0)]
  df_dates[, fracyear := ifelse(leap_year(year(date)),
    fracyear / 366.0, fracyear / 365.0)]
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
    df_all[, solrad := cos_zenith * 0.92 * (1.0 - exp(-0.22 * vpd))]
    df_all[solrad < 1e-4, solrad := 0.0]  # always set low values to 0

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

#' Set default percent_cured values based off annual variation in Boreal Plains region
#'
#' @param yr             Year
#' @param mon            Month of year
#' @param day            Day of month
#' @param start_mon      Month of grassland fuel green up start (Boreal Plains Mar 12)
#' @param start_day      Day of grassland fuel green up start (Boreal Plains Mar 12)
#' @return               percent_cured [%], percent of grassland fuel that is cured

seasonal_curing <- function(yr, mon, day, start_mon = 3, start_day = 12) {
  # store default values of percent_cured every 10 days of the year
  PERCENT_CURED <- c(
    96.0,  # "winter" cured value
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
    96.0  # "winter" cured value for rest of year
  )
  # find previous green up start date (year - 1 or year)
  shift <- make_date(yr, mon, day) - make_date(yr, start_mon, start_day)
  if (shift < 0) {
    shift <- make_date(yr, mon, day) - make_date(yr - 1, start_mon, start_day)
  }
  days_in <- as.integer(shift) + 1  # green up start date is first value (not 0th)
  # check if date is in green phase or winter (cured) phase
  if (days_in < (length(PERCENT_CURED) - 1) * 10) {
    # linear interpolation between every 10-day value
    per_cur0 <- PERCENT_CURED[days_in %/% 10 + 1]
    per_cur1 <- PERCENT_CURED[days_in %/% 10 + 2]
    period_frac <- (days_in %% 10) / 10.0
    return(per_cur0 + (per_cur1 - per_cur0) * period_frac)
  } else {
    return(PERCENT_CURED[length(PERCENT_CURED)])
  }
}

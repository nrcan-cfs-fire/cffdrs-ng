# convert min/max weather format into hourly format
library(data.table)
library(lubridate)
source("util.r")

C_TEMP <- list(c_alpha=0.2, c_beta=2.0, c_gamma=-2.9)
C_RH <- list(c_alpha=0.4, c_beta=1.9, c_gamma=-2.9)
C_WIND <- list(c_alpha=1.2, c_beta=1.7, c_gamma=-1.5)

makePrediction <- function(fcsts, c_alpha, c_beta, c_gamma, v='TEMP', change_at='SUNSET', min_value=-Inf, max_value=Inf, intervals=1, verbose=FALSE)
{
  if (verbose) {
    print(paste0('Predicting ', v, ' changing at ', change_at))
  }
  fcsts <- copy(fcsts)
  var_min <- paste0(v, '_MIN')
  var_max <- paste0(v, '_MAX')
  fcsts[, `:=`(TIME_MIN = SUNRISE + c_alpha,
               TIME_MAX = (SOLARNOON + c_beta),
               VAR_MIN_TOM = data.table::shift(get(var_min), -1),
               TIME_MIN_TOM = data.table::shift(SUNRISE, -1) + c_alpha)]
  #~ fcsts[, `:=`(r2_time_min = data.table::shift(TIME_MIN, -1),
  #~ r2_var_min = data.table::shift(get(var_min), -1))]
  stopifnot(1 == nrow(unique(fcsts$ID)))
  hours <- data.table(HOUR = 0:23)
  cross <- as.data.table(merge(as.data.frame(hours),as.data.frame(fcsts[, c('DATE')]), all=TRUE))
  minutes <- data.table(MINUTE = seq(0, 59, by=60 / intervals))
  cross <- as.data.table(merge(as.data.frame(cross), as.data.frame(minutes), all=TRUE))
  cross[, ID := unique(fcsts$ID)]
  setorder(cross, cols='DATE', 'HOUR', 'MINUTE')
  cross[, TIME := HOUR + MINUTE / 60.0]
  #cross <- merge(cross, fcsts[, -c('HOUR', 'MINUTE', 'TIME')], by=c('ID', 'DATE'))
  for (column in c('HOUR', 'MINUTE', 'TIME', 'MON', 'YEAR'))
  {
    if (column %in% colnames(fcsts))
    {
      set(fcsts, , column, NULL)
    }
  }
  cross <- merge(cross, fcsts, by=c('ID', 'DATE'))
  cross[,`:=`(START_DATE = ifelse(TIME <= TIME_MIN, as.character(as.Date(DATE) - 1), DATE),
              HOUR_CURVE = ifelse(TIME <= TIME_MIN, TIME + 24, TIME),
              IS_RISING = ifelse(TIME <= TIME_MIN, FALSE, TIME < get(change_at)),
              TIME_G_MIN = ifelse(TIME <= TIME_MIN, TIME_MIN, TIME_MIN_TOM),
              VAR_G_MIN = ifelse(TIME <= TIME_MIN, get(var_min), VAR_MIN_TOM)),]
  rising <- cross[IS_RISING == TRUE,]
  falling <- cross[IS_RISING == FALSE,]
  # figure out values before change_at
  rising[, F_OR_G := (HOUR_CURVE - TIME_MIN) / (TIME_MAX - TIME_MIN)]
  rising[, eval(v) := get(var_min) + (get(var_max) - get(var_min)) * sin((pi / 2) * F_OR_G)]
  rising[, VAR_CHANGE := get(var_min) + (get(var_max) - get(var_min)) * sin((pi / 2) * ((get(change_at) - TIME_MIN) / (TIME_MAX - TIME_MIN)))]
  # need to figure out var value at change_at
  tmp <- unique(rising[,c('START_DATE', 'VAR_CHANGE')])
  # now merge change_at values into falling
  falling <- merge(falling, tmp, by=c('START_DATE'))
  falling[, F_OR_G := (HOUR_CURVE - get(change_at)) / (24 - get(change_at) + TIME_G_MIN)]
  falling[, eval(v) := VAR_G_MIN + (VAR_CHANGE - VAR_G_MIN) * exp(c_gamma * F_OR_G)]
  # combine and sort everything
  out <- rbind(rising, falling)
  setorder(out, cols='DATE', 'HOUR')
  out[, TIMESTAMP := as_datetime(paste0(as.character(DATE), ' ', HOUR, ':', MINUTE, ':00'))]
  out[, eval(v) := pmin(pmax(get(v), min_value), max_value)]
  # HACK: somehow this isn't returning unless we do something to it first
  out <- out[!is.na(get(v))]
  return(copy(out))
}


doPrediction <- function(fcsts, row_temp, row_wind, row_RH, intervals=1, verbose=FALSE)
{
  if (verbose) {
    print('Doing prediction')
  }
  v_temp <- makePrediction(fcsts, row_temp$c_alpha, row_temp$c_beta, row_temp$c_gamma, 'TEMP', 'SUNSET', intervals=intervals, verbose=verbose)
  v_wind <- makePrediction(fcsts, row_wind$c_alpha, row_wind$c_beta, row_wind$c_gamma, 'WIND', 'SUNSET', min_value=0, intervals=intervals, verbose=verbose)
  t <- v_temp[,c('ID', 'TIMESTAMP', 'DATE', 'HOUR', 'LAT', 'LONG', 'TEMP')]
  w <- v_wind[,c('ID', 'TIMESTAMP', 'DATE', 'HOUR', 'WIND')]
  out <- merge(t, w)
  RH <- makePrediction(fcsts, row_RH$c_alpha, row_RH$c_beta, row_RH$c_gamma, 'RH_OPP', intervals=intervals, min_value=0, max_value=1, verbose=verbose)
  RH[, `:=`(RH = 100 * (1 - RH_OPP))]
  RH <- RH[,c('ID', 'TIMESTAMP', 'RH')]
  out <- merge(RH, out, by=c('ID', 'TIMESTAMP'))
  output <- out[,c('ID', 'LAT', 'LONG', 'TIMESTAMP', 'TEMP', 'WIND', 'RH')]
  #~ output <- fwi(output)
  if (verbose) {
    print('Assigning times')
  }
  output[, HOUR := hour(TIMESTAMP)]
  output[, MINUTE := minute(TIMESTAMP)]
  if (verbose) {
    print('Converting date')
  }
  output[, DATE := as.character(as_date(TIMESTAMP))]
  if (verbose) {
    print('Allocating rain')
  }
  rain <- fcsts[, c('DATE', 'RAIN')]
  rain[, HOUR := 7]
  rain[, MINUTE := 0]
  if (verbose) {
    print('Merging')
  }
  cmp <- merge(output, rain, by=c('DATE', 'HOUR', 'MINUTE'), all=TRUE)[!is.na(TIMESTAMP)]
  cmp$RAIN <- nafill(cmp$RAIN, fill=0)
  if (verbose) {
    print('Calculating times')
  }
  cmp[, YEAR := year(TIMESTAMP)]
  cmp[, MON := month(TIMESTAMP)]
  cmp[, DAY := day(TIMESTAMP)]
  cmp[, TIME := HOUR + MINUTE / 60.0]
  if (verbose) {
    print("Done prediction")
  }
  return(cmp)
}


#' Convert daily min/max values stream to hourly values stream.
#' Uses Beck & Trevitt method with default A/B/G values.
#'
#' @param     w         daily min/max values weather stream [lat, long, year, mon, day, hour, temp_min, temp_max, rh_min, rh_max, wind_min, wind_max, rain]
#' @param     timezone  integer offset from GMT to use for sun calculations
#' @return              hourly values weather stream [lat, long, year, mon, day, hour, temp, rh, wind, rain]
minmax_to_hourly_single <- function(w, timezone, skipInvalid=FALSE, verbose=FALSE)
{
  r <- copy(w)
  colnames(r) <- toupper(colnames(r))
  if (length(na.omit(unique(r$LAT))) != 1)
  {
    stop('Expected a single LAT value for input weather')
  }
  if (length(na.omit(unique(r$LONG))) != 1)
  {
    stop('Expected a single LONG value for input weather')
  }
  hadId <- TRUE
  if (!("ID" %in% colnames(r))) {
    r$ID <- 1
    hadId <- FALSE
  } else if (length(na.omit(unique(r$ID))) != 1) {
    stop('Expected a single ID value for input weather')
  }
  if (length(na.omit(unique(r$YEAR))) != 1) {
    stop('Expected a single YEAR value for input weather')
  }
  r[, TIMESTAMP := as_datetime(sprintf('%04d-%02d-%02d %02d:%02d:00', YEAR, MON, DAY, HOUR, 0),
                               tz=paste0('Etc/GMT', ifelse(timezone > 0, '-', '+'), abs(timezone)))]
  if (!(nrow(r) > 1 && isSequential(as.Date(r$TIMESTAMP)))) {
    if (skipInvalid)
    {
      warning(paste0(r$ID[[1]], " for ", r$YEAR[[1]], " - Expected input to be sequential daily weather"))
      return(NULL)
    }
    stop('Expected input to be sequential daily weather')
  }
  orig_dates <- data.table(date=as.character(unique(as_date(r$TIMESTAMP))))
  # duplicate start and end dates so we can use their values for yesterday and tomorrow in predictions
  yest <- r[1,]
  tom <- r[nrow(r),]
  yest[, TIMESTAMP := TIMESTAMP - days(1)]
  tom[, TIMESTAMP := TIMESTAMP + days(1)]
  r <- rbind(yest, r, tom)
  r[, DATE := as_date(TIMESTAMP)]
  r[, YEAR := year(TIMESTAMP)]
  r[, MON := month(TIMESTAMP)]
  r[, DAY := day(TIMESTAMP)]
  r[, HOUR := hour(TIMESTAMP)]
  dates <- as_datetime(unique(r$TIMESTAMP))
  latitude <- r$LAT[[1]]
  longitude <- r$LONG[[1]]
  sunlight <- getSunlight(dates, timezone, latitude, longitude)
  setnames(sunlight, c("DATE"), c("TIMESTAMP"))
  sunlight$TIMESTAMP <- as_datetime(sunlight$TIMESTAMP)
  r <- merge(r, sunlight, by=c("TIMESTAMP", "LAT", "LONG"))
  # # FIX: is solar noon just midpoint between sunrise and sunset?
  r[, SOLARNOON := (SUNSET - SUNRISE) / 2 + SUNRISE]
  r[, RH_OPP_MIN := 1 - RH_MAX/100]
  r[, RH_OPP_MAX := 1 - RH_MIN/100]
  r[, DATE := as.character(DATE)]
  df <- doPrediction(r, row_temp=C_TEMP, row_wind=C_WIND, row_RH=C_RH, verbose=verbose)
  colnames(df) <- tolower(colnames(df))
  df <- merge(orig_dates, df, by="date")
  if (hadId)
  {
    df <- df[, c("id", "lat", "long", "year", "mon", "day", "hour", "temp", "rh", "wind", "rain")]
  } else {
    df <- df[, c("lat", "long", "year", "mon", "day", "hour", "temp", "rh", "wind", "rain")]
  }
  return(df)
}

#' Convert daily min/max values stream to hourly values stream.
#' Uses Beck & Trevitt method with default A/B/G values.
#'
#' @param     w         daily min/max values weather stream [id, lat, long, year, mon, day, hour, temp_min, temp_max, rh_min, rh_max, wind_min, wind_max, rain]
#' @param     timezone  integer offset from GMT to use for sun calculations
#' @return              hourly values weather stream [id, lat, long, year, mon, day, hour, temp, rh, wind, rain]
#' @export minmax_to_hourly
minmax_to_hourly <- function(w, timezone, skipInvalid=FALSE, verbose=FALSE)
{
  r <- copy(w)
  colnames(r) <- toupper(colnames(r))
  hadId <- TRUE
  if (!("ID" %in% colnames(r))) {
    r$ID <- 1
    hadId <- FALSE
  }
  result <- NULL
  for (stn in unique(r$ID))
  {
    by_stn <- r[ID == stn,]
    for (yr in unique(by_stn$YEAR))
    {
      by_year <- by_stn[YEAR == yr,]
      print(paste0("Running ", stn, " for ", yr))
      df <- minmax_to_hourly_single(by_year, timezone, skipInvalid, verbose)
      result <- rbind(result, df)
    }
  }
  if (!hadId) {
    result <- result[, c("lat", "long", "year", "mon", "day", "hour", "temp", "rh", "wind", "rain")]
  }
  return(result)
}

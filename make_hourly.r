# convert min/max weather format into hourly format
library(data.table)
library(lubridate)
source("util.r")

source("hFWI.r")
C_TEMP <- list(c_alpha=0.3, c_beta=2.0, c_gamma=-3.3)
C_RH <- list(c_alpha=0.4, c_beta=2.0, c_gamma=-3.4)
C_WIND <- list(c_alpha=0.3, c_beta=3.5, c_gamma=-3.3)

makePrediction <- function(fcsts, c_alpha, c_beta, c_gamma, v='TEMP', change_at='SUNSET', min_value=-Inf, max_value=Inf, intervals=1)
{
  print(paste0('Predicting ', v, ' changing at ', change_at))
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
  for (column in c('HOUR', 'MINUTE', 'TIME', 'MON', 'YR'))
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


doPrediction <- function(fcsts, row_temp, row_WS, row_RH, intervals=1)
{
  print('Doing prediction')
  v_temp <- makePrediction(fcsts, row_temp$c_alpha, row_temp$c_beta, row_temp$c_gamma, 'TEMP', 'SUNSET', intervals=intervals)
  v_WS <- makePrediction(fcsts, row_WS$c_alpha, row_WS$c_beta, row_WS$c_gamma, 'WS', 'SUNSET', min_value=0, intervals=intervals)
  t <- v_temp[,c('ID', 'TIMESTAMP', 'DATE', 'HOUR', 'LAT', 'LONG', 'TEMP')]
  w <- v_WS[,c('ID', 'TIMESTAMP', 'DATE', 'HOUR', 'WS')]
  out <- merge(t, w)
  RH <- makePrediction(fcsts, row_RH$c_alpha, row_RH$c_beta, row_RH$c_gamma, 'RH_OPP', intervals=intervals, min_value=0, max_value=1)
  RH[, `:=`(RH = 100 * (1 - RH_OPP))]
  RH <- RH[,c('ID', 'TIMESTAMP', 'RH')]
  out <- merge(RH, out, by=c('ID', 'TIMESTAMP'))
  output <- out[,c('ID', 'TIMESTAMP', 'TEMP', 'WS', 'RH')]
  #~ output <- fwi(output)
  print('Assigning times')
  output[, HOUR := hour(TIMESTAMP)]
  output[, MINUTE := minute(TIMESTAMP)]
  print('Converting date')
  output[, DATE := as.character(as_date(TIMESTAMP))]
  print('Allocating rain')
  rain <- fcsts[, c('DATE', 'APCP')]
  rain[, HOUR := 7]
  setnames(rain, 'APCP', 'PREC')
  rain[, MINUTE := 0]
  print('Merging')
  cmp <- merge(output, rain, by=c('DATE', 'HOUR', 'MINUTE'), all=TRUE)[!is.na(TIMESTAMP)]
  cmp$PREC <- nafill(cmp$PREC, fill=0)
  print('Calculating times')
  cmp[, YR := year(TIMESTAMP)]
  cmp[, MON := month(TIMESTAMP)]
  cmp[, TIME := HOUR + MINUTE / 60.0]
  print("Done prediction")
  return(cmp)
}


#' Convert daily min/max values stream to hourly values stream.
#' Uses Beck & Trevitt method with default A/B/G values.
#' 
#' @param     w         daily min/max values weather stream [lat, long, year, mon, day, hour, temp_min, temp_max, rh_min, rh_max, wind_min, wind_max, rain]
#' @param     timezone  integer offset from GMT to use for sun calculations
#' @return              hourly values weather stream [lat, long, year, mon, day, hour, temp, rh, wind, rain]
#' @export minmax_to_hourly
minmax_to_hourly <- function(w, timezone)
{
  r <- copy(w)
  setnames(r, c("year", "hour"), c("yr", "hr"))
  colnames(r) <- toupper(colnames(r))
  r[, TIMESTAMP := as_datetime(sprintf('%04d-%02d-%02d %02d:%02d:00', YR, MON, DAY, HR, 0))]
  orig_dates <- data.table(date=as.character(unique(as_date(r$TIMESTAMP))))
  # duplicate start and end dates so we can use their values for yesterday and tomorrow in predictions
  yest <- r[1,]
  tom <- r[nrow(r),]
  yest[, TIMESTAMP := TIMESTAMP - days(1)]
  tom[, TIMESTAMP := TIMESTAMP + days(1)]
  r <- rbind(yest, r, tom)
  r[, DATE := as_date(TIMESTAMP)]
  r[, YR := year(TIMESTAMP)]
  r[, MON := month(TIMESTAMP)]
  r[, DAY := day(TIMESTAMP)]
  r[, HR := hour(TIMESTAMP)]
  dates <- as_datetime(unique(r$TIMESTAMP))
  latitude <- r$LAT[[1]]
  longitude <- r$LONG[[1]]
  sunlight <- getSunlight(dates, timezone, latitude, longitude, TRUE)
  setnames(sunlight, c("DATE"), c("TIMESTAMP"))
  sunlight$TIMESTAMP <- as_datetime(sunlight$TIMESTAMP)
  r <- merge(r, sunlight, by=c("TIMESTAMP", "LAT", "LONG"))
  # # FIX: is solar noon just midpoint between sunrise and sunset?
  r[, SOLARNOON := (SUNSET - SUNRISE) / 2 + SUNRISE]
  r$ID <- 1
  setnames(r, c("WIND_MIN", "WIND_MAX", "RAIN"), c("WS_MIN", "WS_MAX", "APCP"))
  r[, RH_OPP_MIN := 1 - RH_MAX/100]
  r[, RH_OPP_MAX := 1 - RH_MIN/100]
  r[, DATE := as.character(DATE)]
  pred <- doPrediction(r, row_temp=C_TEMP, row_WS=C_WIND, row_RH=C_RH)
  setnames(pred, c("WS", "PREC", "YR"), c("WIND", "RAIN", "YEAR"))
  colnames(pred) <- tolower(colnames(pred))
  # FIX: fill in start and end so they have 24 hours for every day
  dates <- data.table(date=unique(pred$date))
  hours <- data.table(hour = 0:23)
  cross <- as.data.table(merge(as.data.frame(hours),as.data.frame(dates), all=TRUE))
  df <- merge(cross, pred, by=c("date", "hour"), all=TRUE)
  df <- merge(orig_dates, df, by="date")
  df[, year := year(date)]
  df[, mon := month(date)]
  df[, day := day(date)]
  df[, hour := hour(timestamp)]
  df[, lat := latitude]
  df[, long := longitude]
  df <- df[, c("lat", "long", "year", "mon", "day", "hour", "temp", "rh", "wind", "rain")]
  return(df)
}

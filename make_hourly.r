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
  tz <- fcsts$TIMEZONE[[1]]
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
  out[, TIMESTAMP := as.POSIXct(paste0(as.character(DATE), ' ', HOUR, ':', MINUTE, ':00'), tz=tz)]
  out[, eval(v) := pmin(pmax(get(v), min_value), max_value)]
  # HACK: somehow this isn't returning unless we do something to it first
  out <- out[!is.na(get(v))]
  return(copy(out))
}


doPrediction <- function(fcsts, row_temp, row_WS, intervals=1, row_RH=NULL)
{
  tz <- fcsts$TIMEZONE[[1]]
  print('Doing prediction')
  v_temp <- makePrediction(fcsts, row_temp$c_alpha, row_temp$c_beta, row_temp$c_gamma, 'TEMP', 'SUNSET', intervals=intervals)
  v_WS <- makePrediction(fcsts, row_WS$c_alpha, row_WS$c_beta, row_WS$c_gamma, 'WS', 'SUNSET', min_value=0, intervals=intervals)
  t <- v_temp[,c('ID', 'TIMESTAMP', 'DATE', 'HOUR', 'LAT', 'LONG', 'TIME_MAX', 'HOUR_CURVE', 'TIME_G_MIN', 'APCP', 'TEMP_MAX', 'TEMP_MIN', 'RH_MIN', 'RH_MAX', 'WS_MIN', 'WS_MAX', 'SUNRISE', 'SUNSET', 'TEMP', 'START_DATE', 'VAR_CHANGE')]
  w <- v_WS[,c('ID', 'TIMESTAMP', 'DATE', 'HOUR', 'WS')]
  out <- merge(t, w)
  if (is.null(row_RH))
  {
    # add in absolute humidity calculation based on max TEMP and min RH
    out[, Q_MIN := findQ(TEMP_MAX, RH_MIN)]
    out[, Q_MAX := findQ(TEMP_MIN, RH_MAX)]
    q_day <- unique(out[,c('DATE', 'Q_MIN', 'Q_MAX')])
    q_day[, `:=`(Q_MIN_TOM = data.table::shift(Q_MIN, -1),
                 Q_MIN_YEST = data.table::shift(Q_MIN, 1),
                 Q_MAX_TOM = data.table::shift(Q_MAX, -1),
                 Q_MAX_YEST = data.table::shift(Q_MAX, 1))]
    q_day$Q_MIN_TOM <- nafill(q_day$Q_MIN_TOM, 'locf')
    q_day$Q_MIN_YEST <- nafill(q_day$Q_MIN_YEST, 'nocb')
    q_day$Q_MAX_TOM <- nafill(q_day$Q_MAX_TOM, 'locf')
    q_day$Q_MAX_YEST <- nafill(q_day$Q_MAX_YEST, 'nocb')
    out <- merge(out, q_day, by=c('DATE', 'Q_MIN', 'Q_MAX'))
    out[, MINUTE := minute(TIMESTAMP)]
    out[, TIME := HOUR + MINUTE / 60.0]
    out[,HOUR_CURVE := ifelse(TIME > TIME_MAX,
                              0,
                              24) + TIME]
    out[, TIME_SINCE_MAX := HOUR_CURVE - TIME_MAX]
    out[, TIME_SINCE_MIN := HOUR_CURVE - (24 + TIME_G_MIN)]
    out[, RISING_PM := TIME_G_MIN + 24.0 - TIME_MAX]
    out[, FALLING_AM := TIME_MAX - TIME_G_MIN]
    out[, P_Q := ifelse(TIME > TIME_MAX,
                        Q_MIN + (Q_MAX_TOM - Q_MIN) / RISING_PM * TIME_SINCE_MAX,
                        ifelse(HOUR_CURVE >= 24 & TIME < TIME_G_MIN,
                               Q_MIN_YEST + (Q_MAX - Q_MIN_YEST) / RISING_PM * TIME_SINCE_MAX,
                               ifelse(HOUR_CURVE >= 24 & TIME >= TIME_G_MIN,
                                      Q_MAX + (Q_MIN - Q_MAX) / FALLING_AM * TIME_SINCE_MIN,
                                      NA)))]
    #~ lines(P_Q ~ TIMESTAMP, out, col=4)
    out[, CUR_RH := findRH(P_Q, TEMP)]
    out[, MAX_Q := findQ(TEMP, 100)]
    out[, P_Q := pmin(P_Q, MAX_Q)]
    out$P_Q <- nafill(out$P_Q, 'locf')
    #~ out[,CUR_RH := findRH(Q, TEMP)]
    # HACK: use pmin so it doesn't do min of whole CUR_RH column
    out[,CUR_RH := pmax(0.0, pmin(100.0, CUR_RH))]
    out$CUR_RH <- nafill(out$CUR_RH, 'locf')
  } else {
    if (3 == length(row_RH))
    {
      RH <- makePrediction(fcsts, row_RH$c_alpha, row_RH$c_beta, row_RH$c_gamma, 'RH_OPP', intervals=intervals, min_value=0, max_value=1)
    } else {
      RH <- makePredictionSinusoidal(fcsts, row_RH$c_alpha, row_RH$c_beta, 'RH_OPP', intervals=intervals, min_value=0, max_value=1)
    }
    RH[, `:=`(CUR_RH = 100 * (1 - RH_OPP))]
    RH <- RH[,c('ID', 'TIMESTAMP', 'CUR_RH')]
    out <- merge(RH, out, by=c('ID', 'TIMESTAMP'))
    out[, P_Q := findQ(TEMP, CUR_RH)]
  }
  output <- out[,c('ID', 'TIMESTAMP', 'TEMP', 'WS', 'CUR_RH', 'P_Q', 'TIME_MAX', 'HOUR_CURVE', 'START_DATE')]
  #~ output <- fwi(output)
  print('Setting names')
  setnames(output, 'TEMP', 'P_TEMP')
  setnames(output, 'WS', 'P_WS')
  setnames(output, 'CUR_RH', 'P_RH')
  print('Assigning times')
  output[, HOUR := hour(TIMESTAMP)]
  output[, MINUTE := minute(TIMESTAMP)]
  print('Converting date')
  output[, DATE := as.character(as.Date(TIMESTAMP, tz=tz))]
  print('Allocating rain')
  rain1900 <- fcsts[, c('DATE', 'RAIN0000')]
  rain1900[, `:=`(DATE = as.character(as.Date(DATE) - 1),
                  HOUR = 19)]
  setnames(rain1900, 'RAIN0000', 'P_PREC')
  rain0100 <- fcsts[, c('DATE', 'RAIN0600')]
  rain0100[, HOUR := 1]
  setnames(rain0100, 'RAIN0600', 'P_PREC')
  rain <- rbind(rain1900, rain0100)
  rain0700 <- fcsts[, c('DATE', 'RAIN1200')]
  rain0700[, HOUR := 7]
  setnames(rain0700, 'RAIN1200', 'P_PREC')
  rain <- rbind(rain, rain0700)
  rain1300 <- fcsts[, c('DATE', 'RAIN1800')]
  rain1300[, HOUR := 13]
  setnames(rain1300, 'RAIN1800', 'P_PREC')
  rain <- rbind(rain, rain1300)
  rain[, MINUTE := 0]
  print('Merging')
  cmp <- merge(output, rain, by=c('DATE', 'HOUR', 'MINUTE'), all=TRUE)[!is.na(TIMESTAMP)]
  cmp$P_PREC <- nafill(cmp$P_PREC, fill=0)
  print('Calculating times')
  cmp[, YR := year(TIMESTAMP)]
  cmp[, MON := month(TIMESTAMP)]
  cmp[, TIME := HOUR + MINUTE / 60.0]
  print("Done prediction")
  return(cmp)
}


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
  r$TIMEZONE <- "Etc/GMT+6"
  for_temp <- makePrediction(r, C_TEMP$c_alpha, C_TEMP$c_beta, C_TEMP$c_gamma, v="TEMP")
  setnames(r, c("WIND_MIN", "WIND_MAX", "RAIN"), c("WS_MIN", "WS_MAX", "APCP"))
  r[, RH_OPP_MIN := 1 - RH_MAX/100]
  r[, RH_OPP_MAX := 1 - RH_MIN/100]
  r[, RAIN1200 := APCP]
  r[, RAIN0000 := 0]
  r[, RAIN0600 := 0]
  r[, RAIN1800 := 0]
  r[, DATE := as.character(DATE)]
  pred <- doPrediction(r, row_temp=C_TEMP, row_WS=C_WIND, row_RH=C_RH)
  setnames(pred, c("P_TEMP", "P_WS", "P_RH", "P_PREC", "YR"), c("TEMP", "WIND", "RH", "RAIN", "YEAR"))
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

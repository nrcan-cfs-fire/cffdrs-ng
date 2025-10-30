# convert min/max weather format into hourly format
library(data.table)
library(lubridate)
source("util.r")

C_TEMP <- list(c_alpha = 0.0, c_beta = 2.75, c_gamma = -1.9)
C_RH <- list(c_alpha = 0.25, c_beta = 2.75, c_gamma = -2.0)
C_WIND <- list(c_alpha = 1.0, c_beta = 1.5, c_gamma = -1.3)

make_prediction <- function(fcsts, c_alpha, c_beta, c_gamma, v = "TEMP", change_at = "SUNSET",
  min_value = -Inf, max_value = Inf, intervals = 1, verbose = FALSE) {
  if (verbose) {
    print(paste0("Predicting ", v, " changing at ", change_at))
  }
  fcsts <- copy(fcsts)
  var_min <- paste0(v, "_MIN")
  var_max <- paste0(v, "_MAX")
  fcsts[, `:=`(
    TIME_MIN = SUNRISE + c_alpha,
    TIME_MAX = (SOLARNOON + c_beta),
    VAR_MIN_TOM = data.table::shift(get(var_min), -1),
    TIME_MIN_TOM = data.table::shift(SUNRISE, -1) + c_alpha
  )]
  # ~ fcsts[, `:=`(r2_time_min = data.table::shift(TIME_MIN, -1),
  # ~ r2_var_min = data.table::shift(get(var_min), -1))]
  stopifnot(1 == nrow(unique(fcsts$ID)))
  hours <- data.table(HR = 0:23)
  cross <- as.data.table(merge(as.data.frame(hours), as.data.frame(fcsts[, c("DATE")]),
    all = TRUE))
  minutes <- data.table(MINUTE = seq(0, 59, by = 60 / intervals))
  cross <- as.data.table(merge(as.data.frame(cross), as.data.frame(minutes), all = TRUE))
  cross[, ID := unique(fcsts$ID)]
  setorder(cross, cols = "DATE", "HR", "MINUTE")
  cross[, TIME := HR + MINUTE / 60.0]
  # cross <- merge(cross, fcsts[, -c('HR', 'MINUTE', 'TIME')], by=c('ID', 'DATE'))
  for (column in c("HR", "MINUTE", "TIME", "MON", "YR")) {
    if (column %in% colnames(fcsts)) {
      set(fcsts, , column, NULL)
    }
  }
  cross <- merge(cross, fcsts, by = c("ID", "DATE"))
  cross[, `:=`(
    START_DATE = ifelse(TIME <= TIME_MIN, as.character(as.Date(DATE) - 1), DATE),
    HOUR_CURVE = ifelse(TIME <= TIME_MIN, TIME + 24, TIME),
    IS_RISING = ifelse(TIME <= TIME_MIN, FALSE, TIME < get(change_at)),
    TIME_G_MIN = ifelse(TIME <= TIME_MIN, TIME_MIN, TIME_MIN_TOM),
    VAR_G_MIN = ifelse(TIME <= TIME_MIN, get(var_min), VAR_MIN_TOM)
  ), ]
  rising <- cross[IS_RISING == TRUE, ]
  falling <- cross[IS_RISING == FALSE, ]
  # figure out values before change_at
  rising[, F_OR_G := (HOUR_CURVE - TIME_MIN) / (TIME_MAX - TIME_MIN)]
  rising[, eval(v) := get(var_min) + (get(var_max) - get(var_min)) * sin((pi / 2) * F_OR_G)]
  rising[, VAR_CHANGE := get(var_min) + (get(var_max) - get(var_min)) *
    sin((pi / 2) * ((get(change_at) - TIME_MIN) / (TIME_MAX - TIME_MIN)))]
  # need to figure out var value at change_at
  tmp <- unique(rising[, c("START_DATE", "VAR_CHANGE")])
  # now merge change_at values into falling
  falling <- merge(falling, tmp, by = c("START_DATE"))
  falling[, F_OR_G := (HOUR_CURVE - get(change_at)) / (24 - get(change_at) + TIME_G_MIN)]
  falling[, eval(v) := VAR_G_MIN + (VAR_CHANGE - VAR_G_MIN) * exp(c_gamma * F_OR_G)]
  # combine and sort everything
  out <- rbind(rising, falling)
  setorder(out, cols = "DATE", "HR")
  out[, TIMESTAMP := as_datetime(paste0(as.character(DATE), " ", HR, ":", MINUTE, ":00"))]
  out[, eval(v) := pmin(pmax(get(v), min_value), max_value)]
  # HACK: somehow this isn't returning unless we do something to it first
  out <- out[!is.na(get(v))]
  return(copy(out))
}


do_prediction <- function(fcsts, row_temp, row_wind, row_RH, intervals = 1, verbose = FALSE) {
  if (verbose) {
    print("Doing prediction")
  }
  v_temp <- make_prediction(fcsts, row_temp$c_alpha, row_temp$c_beta, row_temp$c_gamma,
    "TEMP", "SUNSET", intervals = intervals, verbose = verbose)
  v_wind <- make_prediction(fcsts, row_wind$c_alpha, row_wind$c_beta, row_wind$c_gamma,
    "WS", "SUNSET", min_value = 0, intervals = intervals, verbose = verbose)
  t <- v_temp[, c("ID", "TIMESTAMP", "DATE", "HR", "LAT", "LONG", "TIMEZONE", "TEMP")]
  w <- v_wind[, c("ID", "TIMESTAMP", "DATE", "HR", "WS")]
  out <- merge(t, w)
  RH <- make_prediction(fcsts, row_RH$c_alpha, row_RH$c_beta, row_RH$c_gamma,
    "RH_OPP", intervals = intervals, min_value = 0, max_value = 1, verbose = verbose)
  RH[, `:=`(RH = 100 * (1 - RH_OPP))]
  RH <- RH[, c("ID", "TIMESTAMP", "RH")]
  out <- merge(RH, out, by = c("ID", "TIMESTAMP"))
  output <- out[, c("ID", "LAT", "LONG", "TIMEZONE", "TIMESTAMP", "TEMP", "WS", "RH")]
  # ~ output <- fwi(output)
  if (verbose) {
    print("Assigning times")
  }
  output[, HR := hour(TIMESTAMP)]
  output[, MINUTE := minute(TIMESTAMP)]
  if (verbose) {
    print("Converting date")
  }
  output[, DATE := as.character(as_date(TIMESTAMP))]
  if (verbose) {
    print("Allocating rain")
  }
  prec <- fcsts[, c("DATE", "PREC")]
  prec[, HR := 7]
  prec[, MINUTE := 0]
  if (verbose) {
    print("Merging")
  }
  cmp <- merge(output, prec, by = c("DATE", "HR", "MINUTE"), all = TRUE)[!is.na(TIMESTAMP)]
  cmp$PREC <- nafill(cmp$PREC, fill = 0)
  if (verbose) {
    print("Calculating times")
  }
  cmp[, YR := year(TIMESTAMP)]
  cmp[, MON := month(TIMESTAMP)]
  cmp[, DAY := day(TIMESTAMP)]
  cmp[, TIME := HR + MINUTE / 60.0]
  if (verbose) {
    print("Done prediction")
  }
  return(cmp)
}


#' Convert daily min/max values stream to hourly values stream.
#' Uses Beck & Trevitt method with default A/B/G values.
#'
#' @param     w         daily min/max values weather stream [lat, long, yr, mon, day, temp_min, temp_max, rh_min, rh_max, ws_min, ws_max, prec]
#' @return              hourly values weather stream [lat, long, timezone, yr, mon, day, hr, temp, rh, ws, prec]
minmax_to_hourly_single <- function(w, skip_invalid = FALSE, verbose = FALSE) {
  r <- copy(w)
  colnames(r) <- toupper(colnames(r))
  if (length(na.omit(unique(r$LAT))) != 1) {
    stop("Expected a single LAT value for input weather")
  }
  if (length(na.omit(unique(r$LONG))) != 1) {
    stop("Expected a single LONG value for input weather")
  }
  if (length(na.omit(unique(r$TIMEZONE))) != 1) {
    stop("Expected a single UTC offset (timezone) each station year")
  }
  timezone <- unique(r$TIMEZONE)
  hadId <- TRUE
  if (!("ID" %in% colnames(r))) {
    r$ID <- "STN"
    hadId <- FALSE
  } else if (length(na.omit(unique(r$ID))) != 1) {
    stop("Expected a single ID value for input weather")
  }
  if (length(na.omit(unique(r$YR))) != 1) {
    stop("Expected a single YR value for input weather")
  }
  r$HR <- 12
  # as_datetime() defaults to UTC, but we only use TIMESTAMP for it's combined yr, mon, day, hr
  r[, TIMESTAMP := make_datetime(YR, MON, DAY, HR)]
  if (!(nrow(r) == 1 || is_sequential(as.Date(r$TIMESTAMP)))) {
    if (skip_invalid) {
      warning(paste0(r$ID[[1]], " for ", r$YR[[1]],
        " - Expected input to be sequential daily weather"))
      return(NULL)
    }
    stop("Expected input to be sequential daily weather")
  }
  orig_dates <- data.table(date = as.character(unique(as_date(r$TIMESTAMP))))
  # duplicate start and end days to assume their values for one day before and after dataset
  yest <- r[1, ]
  tom <- r[nrow(r), ]
  yest[, TIMESTAMP := TIMESTAMP - days(1)]
  tom[, TIMESTAMP := TIMESTAMP + days(1)]
  r <- rbind(yest, r, tom)
  r[, DATE := as_date(TIMESTAMP)]
  r[, YR := year(TIMESTAMP)]
  r[, MON := month(TIMESTAMP)]
  r[, DAY := day(TIMESTAMP)]
  r[, HR := hour(TIMESTAMP)]
  r <- get_sunlight(r, get_solrad = FALSE)
  colnames(r) <- toupper(colnames(r))  # get_sunlight outputs lowercase columns
  # FIX: is solar noon just midpoint between sunrise and sunset?
  r[, SOLARNOON := (SUNSET - SUNRISE) / 2 + SUNRISE]
  r[, RH_OPP_MIN := 1 - RH_MAX / 100]
  r[, RH_OPP_MAX := 1 - RH_MIN / 100]
  r[, DATE := as.character(DATE)]
  df <- do_prediction(r, row_temp = C_TEMP, row_wind = C_WIND, row_RH = C_RH, verbose = verbose)
  colnames(df) <- tolower(colnames(df))
  df <- merge(orig_dates, df, by = "date")
  cols <- c("lat", "long", "timezone", "yr", "mon", "day", "hr", "temp", "rh", "ws", "prec")
  if (hadId) {
    df <- df[, c("id", ..cols)]
  } else {
    df <- df[, ..cols]
  }
  return(df)
}

#' Convert daily min/max values stream to hourly values stream.
#' Uses Beck & Trevitt method with default A/B/G values.
#'
#' @param   w           daily min/max values weather stream [lat, long, yr, mon, day, temp_min, temp_max, rh_min, rh_max, ws_min, ws_max, prec]
#' @param   timezone    UTC offset (default NA for column provided in w)
#' @param   skipInvalid if station year data non-sequential, skip and raise warning
#' @param   verbose     whether to output progress messages
#' @param   round_out   decimals to truncate output to, NA for none (default 4)
#' @return              hourly values weather stream [lat, long, timezone, yr, mon, day, hr, temp, rh, ws, prec]
#' @export minmax_to_hourly
minmax_to_hourly <- function(w, timezone = NA, skip_invalid = FALSE,
  verbose = FALSE, round_out = 4) {
  # check df_wx class for data.frame or data.table
  wasDT <- is.data.table(w)
  if (wasDT) {
    r <- copy(w)
  } else if (is.data.frame(w)) {
    r <- copy(w)
    setDT(r)
  } else {
    stop("Input weather stream w needs to be a data.frame or data.table!")
  }

  # check for required columns
  colnames(r) <- toupper(colnames(r))
  req_cols <- c("LAT", "LONG", "YR", "MON", "DAY", "TEMP_MIN", "TEMP_MAX",
                "RH_MIN", "RH_MAX", "WS_MIN", "WS_MAX", "PREC")
  for (col in req_cols) {
    if (!col %in% names(r)) {
      stop(paste("Missing required input column:", col))
    }
  }
  # check for ID column
  hadId <- TRUE
  if (!("ID" %in% colnames(r))) {
    r$ID <- "STN"
    hadId <- FALSE
  }
  # check timezone
  if (is.na(timezone) || timezone == "NA") {
    if (!"TIMEZONE" %in% names(r)) {
      stop("Either provide a timezone column or specify argument in minmax_to_hourly")
    }
  } else {
    r[, TIMEZONE := as.numeric(..timezone)]
  }
  # loop over every station year
  result <- NULL
  for (stn in unique(r$ID)) {
    by_stn <- r[ID == stn, ]
    for (yr in unique(by_stn$YR)) {
      by_year <- by_stn[YR == yr, ]
      print(paste0("Running ", stn, " for ", yr))
      df <- minmax_to_hourly_single(by_year, skip_invalid, verbose)
      result <- rbind(result, df)
    }
  }

  # remove ID column if it wasn't provided
  if (!hadId) {
    result[, id := NULL]
  }

  # format decimal places of output columns
  if (!(is.na(round_out) || round_out == "NA")) {
    outcols <- c("temp", "rh", "ws")
    set(result, j = outcols, value = round(result[, ..outcols], as.integer(round_out)))
  }

  if (!wasDT) {
    setDF(result)
  }
  return(result)
}

# run minmax_to_hourly by command line via Rscript, requires 2 args: input csv and output csv
# optional args: timezone, skip_invalid, verbose, round_out
if ("--args" %in% commandArgs() && sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 2) {
    stop("at least 2 arguments required: input csv and output csv")
  }
  input <- args[1]
  output <- args[2]
  # load optional arguments if provided, or set to default
  if (length(args) >= 3) timezone <- as.numeric(args[3])
  else timezone <- NA
  if (length(args) >= 4) skip_invalid <- as.numeric(args[3])
  else skip_invalid <- FALSE
  if (length(args) >= 5) verbose <- as.logical(args[4])
  else verbose <- FALSE
  if (length(args) >= 6) round_out <- args[5]
  else round_out <- 4
  if (length(args) >= 7) warning("Too many input arguments provided, some unused")

  df_in <- read.csv(input)
  df_out <- minmax_to_hourly(df_in, timezone, skip_invalid, verbose, round_out)
  write.csv(df_out, output, row.names = FALSE)
}

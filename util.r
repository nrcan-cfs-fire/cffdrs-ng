#' Various utility functions used by the other files
library(data.table)
library(lubridate)

#' Determine if data is sequential at intervals of 1 unit
#'
#' @param data          data to check
#' @return              whether each entry in data is 1 unit from the next entry
isSequential <- function(data) {
  v <- na.omit(unique(data - data.table::shift(data, 1)))
  return(1 == v[[1]] && length(v) == 1)
}

#' Determine if data is sequential days
#'
#' @param df            data to check
#' @return              whether each entry is 1 day from the next entry
isSequentialDays <- function(df) {
  return(isSequential(as.Date(df$DATE)))
}

#' Determine if data is sequential hours
#'
#' @param df            data to check
#' @return              whether each entry is 1 hour from the next entry
isSequentialHours <- function(df) {
  return(isSequential(as.POSIXct(df$TIMESTAMP)))
}

#' Find specific humidity
#'
#' @param temp        Temperature (Celcius)
#' @param rh          Relative humidity (percent, 0-100)
#' @return            Specific humidity (g/kg)
findQ <- function(temp, rh) {
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
findrh <- function(q, temp) {
  cur_vp <- (273.17 + temp) * q / 217
  return(100 * cur_vp / (6.108 * exp(17.27 * temp / (temp + 237.3))))
}


#' Find day of year. Does not properly deal with leap years.
#'
#' @param mon         Month
#' @param day         Day of month
#' @return            Day of year
julian <- function(mon, day) {
  month <- c(0, 31, 59, 90, 120, 151, 181, 212, 242, 273, 304, 334, 365)
  return(month[mon] + day)
}


getSunlight <- function(df, with_solrad = FALSE) {
  df_copy <- copy(df)
  COLS_ID <- c("LAT", "LONG", "DATE", "TIMEZONE")
  cols_req <- c(COLS_ID, "TIMESTAMP")
  if (with_solrad) {
    cols_req <- c(cols_req, "TEMP")
  }
  for (n in cols_req) {
    stopifnot(n %in% colnames(df))
  }
  # just make date column so we know what type it is
  df_copy[, DATE := as_date(TIMESTAMP)]
  df_stn_dates <- unique(df_copy[, ..COLS_ID])
  dechour <- 12.0
  # calculate common values once
  df_dates <- unique(df_stn_dates[, list(DATE)])
  df_dates[, JD := julian(month(DATE), day(DATE))]
  df_dates[, FRACYEAR := 2.0 * pi / 365.0 * (JD - 1.0 + (dechour - 12.0) / 24.0)]
  df_dates[, EQTIME := 229.18 * (0.000075 + 0.001868 * cos(FRACYEAR) - 0.032077 * sin(FRACYEAR) - 0.014615 * cos(2.0 * FRACYEAR) - 0.040849 * sin(2.0 * FRACYEAR))]
  df_dates[, DECL := 0.006918 - 0.399912 * cos(FRACYEAR) + 0.070257 * sin(FRACYEAR) - 0.006758 * cos(FRACYEAR * 2.0) + 0.000907 * sin(2.0 * FRACYEAR) - 0.002697 * cos(3.0 * FRACYEAR) + 0.00148 * sin(3.0 * FRACYEAR)]
  df_dates[, ZENITH := 90.833 * pi / 180.0]
  # at this point we actually need the LAT/LONG/TIMEZONE
  df_dates <- merge(df_stn_dates, df_dates, by = c("DATE"))
  df_dates[, TIMEOFFSET := EQTIME + 4 * LONG - 60 * TIMEZONE]
  # FIX: is this some kind of approximation that can be wrong?
  #       breaks with (67.1520291504819, -132.37538245496188)
  df_dates[, X_TMP := cos(ZENITH) / (cos(LAT * pi / 180.0) * cos(DECL)) - tan(LAT * pi / 180.0) * tan(DECL)]
  # HACK: keep in range
  df_dates[, X_TMP := pmax(-1, pmin(1, X_TMP))]
  df_dates[, HALFDAY := 180.0 / pi * acos(X_TMP)]
  df_dates[, SUNRISE := (720.0 - 4.0 * (LONG + HALFDAY) - EQTIME) / 60 + TIMEZONE]
  df_dates[, SUNSET := (720.0 - 4.0 * (LONG - HALFDAY) - EQTIME) / 60 + TIMEZONE]
  df_all <- merge(df_copy, df_dates, by = COLS_ID)
  if (with_solrad) {
    df_all[, HR := hour(TIMESTAMP)]
    df_all[, TST := as.numeric(HR) * 60.0 + TIMEOFFSET]
    df_all[, HOURANGLE := TST / 4 - 180]
    df_all[, ZENITH := acos(sin(LAT * pi / 180) * sin(DECL) + cos(LAT * pi / 180) * cos(DECL) * cos(HOURANGLE * pi / 180))]
    ###########################################################################################
    ##################################### DMC-UPDATE ##########################################
    ## calculateing solar radiation using Hargraeves model suggested at:
    ## (https://github.com/derekvanderkampcfs/open_solar_model/tree/V1#conclusions)
    df_all[, ZENITH := pmin(pi / 2, ZENITH)]
    # need later so keep column
    df_all[, COS_ZENITH := cos(ZENITH)]
    # Extraterrestrial solar radiation in kW/m^2
    df_all[, SOLRAD_EXT := 1.367 * COS_ZENITH]
    # Daily total of Extra. Solar Rad in kJ/m^2/day
    df_solrad <- df_all[, list(
      SOLRAD_EXT_SUM = sum(SOLRAD_EXT) * 3600,
      SUM_COS_ZENITH = sum(COS_ZENITH),
      TEMP_RANGE = max(TEMP) - min(TEMP)
    ), by = COLS_ID]
    # Daily surface Solar Rad in kJ/m^2/day
    df_solrad[, SOLRAD_DAY_SUM := 0.11 * SOLRAD_EXT_SUM * (TEMP_RANGE^0.59)]
    df_all <- merge(df_all, df_solrad, by = COLS_ID)
    # Hargreaves hourly surface solar rad in kW/m^2
    df_all[, SOLRAD := COS_ZENITH / SUM_COS_ZENITH * SOLRAD_DAY_SUM / 3600]
    # this was a reduction so it wasn't the full amount for the grass calculation?
    # df_all[, SOLRAD := 0.95 * cos(ZENITH)]
    df_all[, SOLRAD := pmax(0, SOLRAD)]
    df_all <- merge(df_all, df_solrad, by = COLS_ID)
  }
  # colnames(df_all) <- toupper(colnames(df_all))
  cols_sun <- intersect(c("SOLRAD", "SUNRISE", "SUNSET"), colnames(df_all))
  # don't include temporary calculations
  cols <- c(names(df), cols_sun)
  df_result <- df_all[, ..cols]
  df_result[, SUNLIGHT_HOURS := SUNSET - SUNRISE]
  return(df_result)
}

toDecimal <- function(t) {
  return(hour(t) + (minute(t) + (second(t) / 60.0)) / 60.0)
}

toDaily <- function(w, all = FALSE) {
  # split into morning and afternoon so we can assign rain to the proper fwi 'day'
  # NOTE: actually need to figure out what makes the most sense
  # - if we split at 12 then that means we're in LST not LDT
  # - the rain at 12 is from 1100-1200, so that should be part of today's calculation, not tomorrow's
  wx <- copy(w)
  # set DATE field in case there's only a TIMESTAMP
  wx[, DATE := as.character(as.Date(TIMESTAMP))]
  # use toDecimal() so we only need TIMESTAMP field and we can deal with minutes or even seconds
  wx[, FOR_DATE := ifelse(toDecimal(TIMESTAMP) <= 12, as.character(DATE), as.character(as.Date(DATE) + 1))]
  # wx[, FOR_DATE := DATE]
  precip <- wx[, list(PREC = sum(PREC, na.rm = TRUE)), by = c("FOR_DATE")]
  setnames(precip, "FOR_DATE", "DATE")
  merged <- merge(wx[toDecimal(TIMESTAMP) == 12, -c("FOR_DATE", "PREC")], precip, by = c("DATE"), all = all)
  merged$PREC <- nafill(merged$PREC, fill = 0.0)
  if (all) {
    # fix up columns that would be missing values if no noon value for a day
    merged[, TIMESTAMP := as_datetime(sprintf("%s 12:00:00", as.character(DATE)))]
    merged[, YR := year(TIMESTAMP)]
    merged[, MON := month(TIMESTAMP)]
    merged[, DAY := day(TIMESTAMP)]
    merged[, HR := hour(TIMESTAMP)]
    merged[, MINUTE := minute(TIMESTAMP)]
    merged[, ID := na.omit(unique(merged$ID)[[1]])]
    merged[, LAT := na.omit(unique(merged$LAT)[[1]])]
    merged[, LONG := na.omit(unique(merged$LONG)[[1]])]
    # use default drying day indices from weather guide
    merged$TEMP <- nafill(merged$TEMP, fill = 21.1)
    merged$RH <- nafill(merged$RH, fill = 45)
    merged$WS <- nafill(merged$WS, fill = 13)
  }
  return(merged)
}

seasonal_curing <- function(julian_date) {
  PERCENT_CURED <- c(96.0, 96.0, 96.0, 96.0, 96.0, 96.0, 96.0, 96.0, 95.0, 93.0, 92.0, 90.5, 88.4, 84.4, 78.1, 68.7, 50.3, 32.9, 23.0, 22.0, 21.0, 20.0, 25.7, 35.0, 43.0, 49.8, 60.0, 68.0, 72.0, 75.0, 78.9, 86.0, 96.0, 96.0, 96.0, 96.0, 96.0, 96.0)
  jd_class <- (julian_date %/% 10) + 1
  first <- PERCENT_CURED[jd_class]
  last <- PERCENT_CURED[jd_class + 1]
  period_frac <- (julian_date %% 10) / 10.0
  return(first + (last - first) * period_frac)
}

save_csv <- function(df, file) {
  COLS_LOC <- c("lat", "long")
  COLS_DATE <- c("yr", "mon", "day", "hr")
  COLS_RH <- c("rh")
  COLS_WX <- c("temp", "ws")
  COLS_PREC <- c("prec")
  COLS_SOLRAD <- c("solrad")
  COLS_INDICES <- c(
    "ffmc",
    "dmc",
    "dc",
    "isi",
    "bui",
    "fwi",
    "dsr",
    "gfmc",
    "gsi",
    "gfwi"
  )
  COLS_EXTRA <- c("mcffmc", "mcgfmc")
  COLS_GFL <- c("grass_fuel_load")
  COLS_PC <- c("percent_cured")
  cols_used <- c()
  result <- copy(df)
  colnames(result) <- tolower(colnames(result))
  apply_format <- function(cols, fmt, as_int = FALSE) {
    fix_col <- Vectorize(function(x) {
      if (as_int) {
        x <- as.integer(x)
      }
      # HACK: deal with negative 0
      return(gsub("^-0\\.0*$", "0.0", sprintf(fmt, x)))
    })

    for (col in names(result)) {
      # HACK: deal with min/max columns
      col_root <- gsub("_max", "", gsub("_min", "", col))
      if (col_root %in% cols) {
        cols_used <<- append(cols_used, col)
        result[[col]] <<- fix_col(result[[col]])
      }
    }
  }
  apply_format(COLS_LOC, "%.4f")
  apply_format(COLS_DATE, "%02d", TRUE)
  apply_format(COLS_RH, "%.0f")
  apply_format(COLS_WX, "%.1f")
  apply_format(COLS_PREC, "%.2f")
  apply_format(COLS_SOLRAD, "%.4f")
  apply_format(COLS_INDICES, "%.1f")
  apply_format(COLS_EXTRA, "%.4f")
  apply_format(COLS_GFL, "%.2f")
  apply_format(COLS_PC, "%.1f")
  # order used columns based on original ordering
  cols <- intersect(names(result), cols_used)
  result <- result[, ..cols]
  write.csv(result, file, row.names = FALSE, quote = FALSE)
}

dmc_to_moisture_percent <- function(dmc) {
  MC <- 20 + exp(dmc - 244.72) / 43.43
  return(MC)
}

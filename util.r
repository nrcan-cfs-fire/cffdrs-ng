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

getSunlightDT <- function(df) {
  df_copy <- copy(df)
  df_copy[, D := as_date(TIMESTAMP)]
  df_dates <- unique(df_copy[, c("LAT", "LONG", "D", "TIMEZONE")])
  dechour <- 12.0
  df_dates[, JD := julian(month(D), day(D))]
  df_dates[, FRACYEAR := 2.0 * pi / 365.0 * (JD - 1.0 + (dechour - 12.0) / 24.0)]
  df_dates[, EQTIME := 229.18 * (0.000075 + 0.001868 * cos(FRACYEAR) - 0.032077 * sin(FRACYEAR) - 0.014615 * cos(2.0 * FRACYEAR) - 0.040849 * sin(2.0 * FRACYEAR))]
  df_dates[, DECL := 0.006918 - 0.399912 * cos(FRACYEAR) + 0.070257 * sin(FRACYEAR) - 0.006758 * cos(FRACYEAR * 2.0) + 0.000907 * sin(2.0 * FRACYEAR) - 0.002697 * cos(3.0 * FRACYEAR) + 0.00148 * sin(3.0 * FRACYEAR)]
  df_dates[, TIMEOFFSET := EQTIME + 4 * LONG - 60 * TIMEZONE]
  df_dates[, ZENITH := 90.833 * pi / 180.0]
  # FIX: is this some kind of approximation that can be wrong?
  #       breaks with (67.1520291504819, -132.37538245496188)
  df_dates[, X_TMP := cos(ZENITH) / (cos(LAT * pi / 180.0) * cos(DECL)) - tan(LAT * pi / 180.0) * tan(DECL)]
  # HACK: keep in range
  df_dates[, X_TMP := pmax(-1, pmin(1, X_TMP))]
  df_dates[, HALFDAY := 180.0 / pi * acos(X_TMP)]
  df_dates[, SUNRISE := (720.0 - 4.0 * (LONG + HALFDAY) - EQTIME) / 60 + TIMEZONE]
  df_dates[, SUNSET := (720.0 - 4.0 * (LONG - HALFDAY) - EQTIME) / 60 + TIMEZONE]
  df_all <- merge(df_copy, df_dates, by = c("LAT", "LONG", "D", "TIMEZONE"))
  df_all[, HR := hour(TIMESTAMP)]
  df_all[, TST := as.numeric(HR) * 60.0 + TIMEOFFSET]
  df_all[, HOURANGLE := TST / 4 - 180]
  df_all[, ZENITH := acos(sin(LAT * pi / 180) * sin(DECL) + cos(LAT * pi / 180) * cos(DECL) * cos(HOURANGLE * pi / 180))]
  df_all[, SOLRAD := 0.95 * cos(ZENITH)]
  df_all[, SOLRAD := ifelse(SOLRAD <= 0, 0, SOLRAD)]
  colnames(df_all) <- toupper(colnames(df_all))
  # remove temporary calculations
  cols <- c(names(df), "SOLRAD", "SUNRISE", "SUNSET")
  df_result <- df_all[, ..cols]
  df_result[, SUNLIGHT_HOURS := SUNSET - SUNRISE]
  return(df_result)
}

#' Find solar radiation at a give time and place
#'
#' @param dates             Datetimes to find solar radiation for
#' @param timezone          Offset from GMT in hours
#' @param latitude          Latitude (degrees)
#' @param longitude         Longitude (degrees)
#' @return                  Solar radiation (kW/m^2), sunrise, sunset, sunlight hours
getSunlight <- function(dates, timezone, latitude, longitude) {
  # replicate old function for now
  df <- data.table(TIMESTAMP = dates)
  df$TIMEZONE <- timezone
  df$LAT <- latitude
  df$LONG <- longitude
  df_sunlight <- getSunlightDT(df)
  setnames(df_sunlight, c("TIMESTAMP"), c("DATE"))
  result <- df_sunlight[, c("DATE", "LAT", "LONG", "SOLRAD", "SUNRISE", "SUNSET", "SUNLIGHT_HOURS")]
  return(result)
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
  COLS_WX <- c("temp", "ws", "prec")
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
  apply_format <- function(cols, fmt, digits = 0) {
    fix_col <- Vectorize(function(x) {
      y <- round(x, digits)
      # HACK: deal with negative 0
      y <- ifelse(0 == y, 0.0, y)
      return(sprintf(fmt, y))
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
  apply_format(COLS_LOC, "%.4f", 4)
  apply_format(COLS_DATE, "%02d")
  apply_format(COLS_RH, "%.0f")
  apply_format(COLS_WX, "%.1f", 1)
  apply_format(COLS_SOLRAD, "%.4f", 4)
  apply_format(COLS_INDICES, "%.1f", 1)
  apply_format(COLS_EXTRA, "%.4f", 4)
  apply_format(COLS_GFL, "%.2f", 2)
  apply_format(COLS_PC, "%.1f", 1)
  # order used columns based on original ordering
  cols <- intersect(names(result), cols_used)
  result <- result[, ..cols]
  write.csv(result, file, row.names = FALSE, quote = FALSE)
}

dmc_to_moisture_percent <- function(dmc) {
  MC <- 20 + exp(dmc - 244.72) / 43.43
  return(MC)
}

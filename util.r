#' Various utility functions used by the other files
library(data.table)
library(lubridate)
#source("NG_FWI.r")



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

solar_reduction <- function(DTR){
  #simple hardgraves model based on estimate of DTR
  #this uses the numbers DVK found
  #this is a reduction factor due to atmosphere  sol_surf/Sol_top_of_atm
  reduction <- 0.108*pow(DTR,0.59)
  return(reduction)
}

getSunlight <- function(df, with_solrad = FALSE, DST = FALSE) {
  dst_adjust <- 0
  if (DST){
    dst_adjust <- 1
  }
  
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
  df_dates[, SUNRISE := (720.0 - 4.0 * (LONG + HALFDAY) - EQTIME) / 60 + TIMEZONE + dst_adjust]
  df_dates[, SUNSET := (720.0 - 4.0 * (LONG - HALFDAY) - EQTIME) / 60 + TIMEZONE + dst_adjust]
  df_all <- merge(df_copy, df_dates, by = COLS_ID)
  if (with_solrad) {
    df_all[, HR := hour(TIMESTAMP)]
    df_all[, TST := as.numeric(HR - dst_adjust) * 60.0 + TIMEOFFSET]
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##################################################################################### old code
  #df_copy <- copy(df)
  #COLS_ID <- c("LAT", "LONG", "DATE", "TIMEZONE")
  #cols_req <- c(COLS_ID, "TIMESTAMP")
  #if (with_solrad) {
  #  cols_req <- c(cols_req, "TEMP")
  #}
  #for (n in cols_req) {
  #  stopifnot(n %in% colnames(df))
  #}
  ## just make date column so we know what type it is
  #df_copy[, DATE := as_date(TIMESTAMP)]
  #df_stn_dates <- unique(df_copy[, ..COLS_ID])
  #dechour <- 12.0
  ## calculate common values once
  #df_dates <- unique(df_stn_dates[, list(DATE)])
  #df_dates[, JD := julian(month(DATE), day(DATE))]
  #df_dates[, FRACYEAR := 2.0 * pi / 365.0 * (JD - 1.0 + (dechour - 12.0) / 24.0)]
  #df_dates[, EQTIME := 229.18 * (0.000075 + 0.001868 * cos(FRACYEAR) - 0.032077 * sin(FRACYEAR) - 0.014615 * cos(2.0 * FRACYEAR) - 0.040849 * sin(2.0 * FRACYEAR))]
  #df_dates[, DECL := 0.006918 - 0.399912 * cos(FRACYEAR) + 0.070257 * sin(FRACYEAR) - 0.006758 * cos(FRACYEAR * 2.0) + 0.000907 * sin(2.0 * FRACYEAR) - 0.002697 * cos(3.0 * FRACYEAR) + 0.00148 * sin(3.0 * FRACYEAR)]
  #df_dates[, ZENITH := 90.833 * pi / 180.0]
  ## at this point we actually need the LAT/LONG/TIMEZONE
  #df_dates <- merge(df_stn_dates, df_dates, by = c("DATE"))
  #df_dates[, TIMEOFFSET := EQTIME + 4 * LONG - 60 * TIMEZONE]
  ## FIX: is this some kind of approximation that can be wrong?
  ##       breaks with (67.1520291504819, -132.37538245496188)
  #df_dates[, X_TMP := cos(ZENITH) / (cos(LAT * pi / 180.0) * cos(DECL)) - tan(LAT * pi / 180.0) * tan(DECL)]
  ## HACK: keep in range
  #df_dates[, X_TMP := pmax(-1, pmin(1, X_TMP))]
  #df_dates[, HALFDAY := 180.0 / pi * acos(X_TMP)]
  #df_dates[, SUNRISE := (720.0 - 4.0 * (LONG + HALFDAY) - EQTIME) / 60 + TIMEZONE]
  #df_dates[, SUNSET := (720.0 - 4.0 * (LONG - HALFDAY) - EQTIME) / 60 + TIMEZONE]
  #df_all <- merge(df_copy, df_dates, by = COLS_ID)
  #if (with_solrad) {
  #  df_all[, HR := hour(TIMESTAMP)]
  #  df_all[, TST := as.numeric(HR) * 60.0 + TIMEOFFSET]
  #  df_all[, HOURANGLE := TST / 4 - 180]
  #  df_all[, ZENITH := acos(sin(LAT * pi / 180) * sin(DECL) + cos(LAT * pi / 180) * cos(DECL) * cos(HOURANGLE * pi / 180))]
  #  ###########################################################################################
  #  ##################################### DMC-UPDATE ##########################################
  ### calculateing solar radiation using Hargraeves model suggested at:
  # ## (https://github.com/derekvanderkampcfs/open_solar_model/tree/V1#conclusions)
  # df_all[, ZENITH := pmin(pi / 2, ZENITH)]
  # # need later so keep column
  # df_all[, COS_ZENITH := cos(ZENITH)]
  # # Extraterrestrial solar radiation in kW/m^2
  # df_all[, SOLRAD_EXT := 1.367 * COS_ZENITH]
  # # Daily total of Extra. Solar Rad in kJ/m^2/day
  # df_solrad <- df_all[, list(
  #   SOLRAD_EXT_SUM = sum(SOLRAD_EXT) * 3600,
  #   SUM_COS_ZENITH = sum(COS_ZENITH),
  #   TEMP_RANGE = max(TEMP) - min(TEMP)
  # ), by = COLS_ID]
  # # Daily surface Solar Rad in kJ/m^2/day
  # df_solrad[, SOLRAD_DAY_SUM := 0.11 * SOLRAD_EXT_SUM * (TEMP_RANGE^0.59)]
  # df_all <- merge(df_all, df_solrad, by = COLS_ID)
  # # Hargreaves hourly surface solar rad in kW/m^2
  # df_all[, SOLRAD := COS_ZENITH / SUM_COS_ZENITH * SOLRAD_DAY_SUM / 3600]
  # # this was a reduction so it wasn't the full amount for the grass calculation?
  # # df_all[, SOLRAD := 0.95 * cos(ZENITH)]
  # df_all[, SOLRAD := pmax(0, SOLRAD)]
  # df_all <- merge(df_all, df_solrad, by = COLS_ID)
  #}
  #  # colnames(df_all) <- toupper(colnames(df_all))
  #cols_sun <- intersect(c("SOLRAD", "SUNRISE", "SUNSET"), colnames(df_all))
  ## don't include temporary calculations
  #cols <- c(names(df), cols_sun)
  #df_result <- df_all[, ..cols]
  #df_result[, SUNLIGHT_HOURS := SUNSET - SUNRISE]
  #return(df_result)
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

smooth_5pt <- function(source) {
  #binomial smoother  ... specifically for the 24 hour day
  #1pt = 1
  #3pt = (1 2 1) = 4
  #5pt = (1 4 6 4 1) = 16
  #7pt = (1 6 15 20 15 6 1) = 64
  
  cap <- length(source) #normally 24 edge cases on data set input though
  
  dest <- numeric(cap)
  
  dest[1] <- source[1]
  dest[cap] <- source[cap]
  
  miss <- 0
  for (i in 1:3){
    if (source[i] < -90.0){
      miss <- miss + 1
    }
  }
  if (miss == 0){
    dest[2] <- (0.25 * source[1]) + (0.5 * source[2]) + (0.25 * source[3])
  }
  else {
    dest[2] <- source[2]
  }
  
  
  for (i in 3:(cap-2)){
    miss <- 0
    for (j in (i-2):(i+2)){
      
      if (source[j] < -90.0){
        miss <- miss + 1
      }
    }
    if (miss == 0){
      dest[i] <- (1.0/16.0 * source[i - 2]) + (4.0/16.0 * source[i - 1]) + (6.0/16.0 * source[i]) + (4.0/16.0 * source[i + 1]) + (1.0/16.0 * source[i + 2])
    }
    else {
      dest[i] <- source[i]
    }
  }
  
  
  miss <- 0 
  for (i in (cap-2):cap){
    if (source[i] < -90.0){
      miss <- miss + 1
    }
  }
  if (miss == 0){
    dest[cap - 1] <- (0.25 * source[cap - 2]) + (0.5 * source[cap - 1]) + (0.25 * source[cap])
  }
  else {
    dest[cap - 1] <- source[cap - 1]
  }
  
  return(dest)
}


#smooth_7pt <- function(source) {
  #binomial smoother  ... specifically for the 24 hour day
  #1pt = 1
  #3pt = (1 2 1) = 4
  #5pt = (1 4 6 4 1) = 16
  #7pt = (1 6 15 20 15 6 1) = 64
  
  
#  dest <- numeric(24)
  
#  dest[1] <- source[1]
#  dest[24] <- source[24]
  
#  miss <- 0
#  for (i in 1:3){
#    if (source[i] < -90.0){
#      miss <- miss + 1
#    }
#  }
#  if (miss == 0){
#    dest[2] <- (0.25 * source[1]) + (0.5 * source[2]) + (0.25 * source[3])
#  }
#  else {
#    dest[2] <- source[2]
#  }
  
  
#  miss <- 0
#  for (i in 1:5){
#    if (source[i] < -90.0){
#      miss <- miss + 1
#    }
#  }
#  if (miss == 0){
#    dest[3] <- (1.0/16.0 * source[1]) + (4.0/16.0 * source[2]) + (6.0/16.0 * source[3]) + (4.0/16.0 * source[4]) + (1.0/16.0 * source[5])
#  }
#  else {
#    dest[3] <- source[3]
#  }
  
  
#  for (i in 4:21){
#    miss <- 0
#    for (j in (i-3):(i+3)){
#      if (source[j] < -90.0){
#        miss <- miss + 1
#      }
#    }
#    if (miss == 0){
#      dest[i] <- (1.0/64.0 * source[i - 3]) + (6.0/64.0 * source[i - 2]) + (15.0/64.0 * source[i - 1]) + (20.0/64.0 * source[i]) + (15.0/64.0 * source[i + 1]) + (6.0/64.0 * source[i + 2]) + (1.0/64.0 * source[i + 3])
#    }
#    else {
#      dest[i] <- source[i]
#    }
#  }
  
  
#  miss <- 0
#  for (i in 20:24){
#    if (source[i] < -90.0){
#      miss <- miss + 1
#    }
#  }
#  if (miss == 0){
#    dest[22] <- (1.0/16.0 * source[20]) + (4.0/16.0 * source[21]) + (6.0/16.0 * source[22]) + (4.0/16.0 * source[23]) + (1.0/16.0 * source[24])
#  }
#  else {
#    dest[3] <- source[3]
#  }
  
  
#  miss <- 0 
#  for (i in 22:24){
#    if (source[i] < -90.0){
#      miss <- miss + 1
#    }
#  }
#  if (miss == 0){
#    dest[23] <- (0.25 * source[22]) + (0.5 * source[23]) + (0.25 * source[24])
#  }
#  else {
#    dest[23] <- source[23]
#  }
  
  
#  return(dest)
#}

#this function calculates a fake date based on 5am-5am instead of midnight to midnight
#used for generating daily summaries to make data look at 5am-5am instead
#output form is "year-julian_day", with the julian day rounded back if the time is before 5am
#for Jan 1st where the julian day rollback would make it zero it gets bumped to the last day of the previous year
pseudo_date <- function(year, month, day, hour) {
  
  
  
  adjusted_jd <- ifelse(hour >= 5, 
                          julian(month,day),
                          julian(month,day) - 1)
  adjusted_year <- ifelse(adjusted_jd == 0, 
                          year - 1,
                          year)
  adjusted_jd <- ifelse(adjusted_jd == 0,
                        julian(12,31),
                        adjusted_jd)
  
  out <- sprintf("%d-%d",adjusted_year,adjusted_jd)
  return(out)
  
}


generate_daily_summaries <- function(hourly_data){
  #note: need to account for spill over inlast day after pseudo_date calc where there is not 24 hours in the data
  
  Spread_Threshold_ISI <- 5.0
  
  results <- NULL
  for (stn in unique(hourly_data$id)) {
    
    by_stn <- hourly_data[id == stn]
    by_stn[,pseudo_DATE := pseudo_date(yr,mon,day,hr)]
    
    for (p_date in unique(by_stn$pseudo_DATE)) {
      
      by_date <- by_stn[pseudo_DATE == p_date, ]
      
      peak_time_traditional_spot <- which(by_date$hr == 17)
      if(length(peak_time_traditional_spot) == 0){
        next
      }
      
      peak_time <- -1
      duration <- 0
      wind_smooth <- smooth_5pt(by_date$ws)
      peak_isi_smooth <- -1
      max_ffmc <- 0
      ffmc <- 0
      dmc <- 0
      dc <- 0
      isi <- 0
      bui <- 0
      fwi <- 0
      dsr <- 0
    
      for (i in 1:nrow(by_date)){
        
        smooth_isi <- 0
        if (wind_smooth[i] > -90.0 && by_date[i,ffmc] > -90.0) {
          smooth_isi <- initial_spread_index(wind_smooth[i],by_date[i,ffmc])
        }
        else{
          smooth_isi <- -98.9
        }
        
        if (smooth_isi > peak_isi_smooth){
          peak_time <- i
          peak_isi_smooth <- smooth_isi
        }
        if (by_date[i,ffmc] > max_ffmc){
          max_ffmc <- by_date[i,ffmc]
        }
        if (smooth_isi > Spread_Threshold_ISI){
          duration = duration + 1
        }
      }
      
      
      if (smooth_isi < 5 && duration == 24){
        duration <- 0
      } 
      
      if (max_ffmc < 85.0) {
        peak_time <- peak_time_traditional_spot
      }
      
      ffmc <- by_date[peak_time, ffmc]
      dmc <- by_date[peak_time, dmc]
      dc <- by_date[peak_time, dc]
      isi <- by_date[peak_time, isi]
      bui <- by_date[peak_time, bui]
      fwi <- by_date[peak_time, fwi]
      dsr <- by_date[peak_time, dsr]
      smooth_ws_peak <- wind_smooth[peak_time]
      
      pick_year <- unique(by_date$yr)
      if(length(pick_year) > 1){
        pick_year <- pick_year[1]
      }
      pick_month <- unique(by_date$mon)
      if(length(pick_month) > 1){
        pick_month <- pick_month[1]
      }
      pick_day <- unique(by_date$day)
      if(length(pick_day) > 1){
        pick_day <- pick_day[1]
      }
      
     
      sunrise_val <- by_date[peak_time, sunrise]
      sunset_val <- by_date[peak_time, sunset]
      
      peak_time <- by_date[peak_time,hr]
      
      sunrise <- sprintf("%d:%d", as.integer(sunrise_val), as.integer(60*(sunrise_val - as.integer(sunrise_val))))
      sunset <- sprintf("%d:%d", as.integer(sunset_val), as.integer(60*(sunset_val - as.integer(sunset_val))))
      
      daily_report <- c(unique(by_date$id), pick_year, pick_month, pick_day, peak_time, duration, smooth_ws_peak, peak_isi_smooth, ffmc, dmc, dc, isi, bui, fwi, dsr, max_ffmc, sunrise, sunset)
      

      
      results <- rbind(results, daily_report)
      
      
      
    }
  }

  colnames(results) <- c("wstind","year","mon","day","peak_time","duration","wind_speed_smoothed","peak_isi_smoothed","ffmc","dmc","dc","isi","bui","fwi","dsr", "max_ffmc", "sunrise", "sunset")
  
  results <- results[,c("wstind","year","mon","day","peak_time","duration","wind_speed_smoothed","peak_isi_smoothed","ffmc","dmc","dc","isi","bui","fwi","dsr", "sunrise", "sunset")]
  
  return(results)
}


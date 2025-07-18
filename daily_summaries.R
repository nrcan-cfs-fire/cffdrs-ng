library(data.table)
library(lubridate)

source("NG_FWI.r")
source("util.r")


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

#' Calculate hourly FWI indices from hourly weather stream.
#'
#' @param     hourly_data     hourly FWI dataframe (output of hFWI())
#' @param     silent          suppresses informative print statements (default False)
#' @param     round_out       decimals to truncate output to, None for none (default 4)
#' @return                    daily summary of peak FWI conditions
generate_daily_summaries <- function(hourly_data, silent = FALSE, round_out = 4) {
  wasDf <- is.data.frame(hourly_data)
  if (wasDf) {
    setDT(hourly_data)
  } else if (!is.data.table(hourly_data)) {
    stop("Input hourly FWI needs to be a data.frame or data.table!")
  }

  Spread_Threshold_ISI <- 5.0

  # check for "id" column
  if ("id" %in% names(hourly_data)) {
    had_stn <- TRUE
  } else {
    if (uniqueN(hourly_data, by = c("yr", "lat", "long")) == 1) {
      hourly_data[, id := "stn"]
      had_stn <- FALSE
    } else {
      stop("Missing 'id' column with multiple years and locations in data")
    }
  }

  results <- NULL

  for (stn in unique(hourly_data[, id])) {
    if (!silent) {
      print(paste("Summarizing", stn, "to daily"))
    }
    by_stn <- hourly_data[id == stn]
    by_stn[, pseudo_DATE := pseudo_date(yr, mon, day, hr)]

    for (p_date in unique(by_stn[, pseudo_DATE])) {
      by_date <- by_stn[pseudo_DATE == p_date]

      # if this day doesn't go up to hour 17, skip
      if (sum(by_date[, hr] == 17, na.rm = TRUE) == 0) {
        next
      }

      # find daily peak burn times
      by_date <- by_date[, ws_smooth := smooth_5pt(ws)]
      by_date <- by_date[, isi_smooth := initial_spread_index(ws_smooth, ffmc)]

      max_ffmc <- by_date[, max(ffmc)]
      if (max_ffmc < 85.0) {
        peak_time <- by_date[, which(hr == 17)][1]
      } else {
        peak_time <- by_date[, which.max(isi_smooth)]
      }

      # calculate some extra variables before creating datatable all at once
      if (by_date[1, julian(mon, day)] < DATE_GRASS) {
        standing <- FALSE
      } else {
        standing <- TRUE
      }
      # load to format sunrise and sunset as hh:mm from decimal hours later
      sr <- by_date[peak_time, sunrise]
      ss <- by_date[peak_time, sunset]

      # find the rest of the values at peak
      daily_report <- data.table(id = by_date[1, id],
        yr = by_date[1, yr],
        mon = by_date[1, mon],
        day = by_date[1, day],
        sunrise = sprintf("%02d:%02d", trunc(sr), trunc(60 * (sr - trunc(sr)))),
        sunset = sprintf("%02d:%02d", trunc(ss), trunc(60 * (ss - trunc(ss)))),
        peak_hr = by_date[peak_time, hr],
        duration = by_date[, sum(isi_smooth > Spread_Threshold_ISI)],
        ffmc = by_date[peak_time, ffmc],
        dmc = by_date[peak_time, dmc],
        dc = by_date[peak_time, dc],
        isi = by_date[peak_time, isi],
        bui = by_date[peak_time, bui],
        fwi = by_date[peak_time, fwi],
        dsr = by_date[peak_time, dsr],
        gfmc = by_date[peak_time, gfmc],
        gsi = by_date[peak_time, gsi],
        gfwi = by_date[peak_time, gfwi],
        ws_smooth = by_date[peak_time, ws_smooth],
        isi_smooth = by_date[peak_time, isi_smooth],
        gsi_smooth = by_date[peak_time,
          grass_spread_index(ws_smooth, gfmc, percent_cured, standing)])

      results <- rbind(results, daily_report)
    }
  }

  if (!had_stn) {
    results <- results[, -"id"]
  }

  # format decimal places of output columns
  if (!is.na(round_out)) {
    outcols <- c("ffmc", "dmc", "dc", "isi", "bui", "fwi", "dsr",
      "gfmc", "gsi", "gfwi", "ws_smooth", "isi_smooth", "gsi_smooth")
    set(results, j = outcols, value = round(results[, ..outcols], round_out))
  }

  if (wasDf) {
    setDF(results)
  }

  results
}

# run generate_daily_summaries by command line via Rscript, requires input and output csv
# optional args: silent, round_out
if ("--args" %in% commandArgs() && sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 2) {
    stop("at least 2 arguments required: input csv, output csv")
  }
  input <- args[1]
  output <- args[2]
  # load optional arguments if provided, or set to default
  if (length(args) >= 3) silent <- as.logical(args[3])
  else silent <- FALSE
  if (length(args) >= 4) round_out <- args[4]
  else round_out <- 4
  if (length(args) >= 5) warning("Too many input arguments provided, some unused")

  df_in <- read.csv(input)
  df_out <- generate_daily_summaries(df_in, silent, round_out)
  write.csv(df_out, output, row.names = FALSE)
}

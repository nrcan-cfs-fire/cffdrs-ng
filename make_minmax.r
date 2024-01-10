# convert daily weather into min/max weather format
library(data.table)
source("util.r")

# This function is for the method that takes old traditional 1pm weather (labelled here as noon)
# and estimates a diurnal temperature range to fit into the scheme that flows into the max/min method
# Temp input in Celsius   RH input in Percent.   These should be that traditional 1pm values
# Written as a function to enable upgrading later if needs be
temp_min_max <- function(temp_noon, rh_noon) {
  # FIX: verify what this should be if temp_noon is negative
  temp_range <- 17 - 0.16 * rh_noon + 0.22 * temp_noon
  temp_max <- ifelse(((temp_noon < 3) & (rh_noon == 100)) | (temp_range < 2),
    temp_noon + (temp_range / 2.0),
    temp_noon + 2
  )
  temp_min <- ifelse(((temp_noon < 3) & (rh_noon == 100)) | (temp_range < 2),
    temp_noon - (temp_range / 2.0),
    temp_max - temp_range
  )
  # HACK: for now just sort so we know it's min, max
  return(list(pmin(temp_min, temp_max), pmax(temp_min, temp_max)))
}
#' Convert daily noon values stream to daily min/max values stream.
#' Uses values from statistics to do the conversion.
#'
#' @param df        daily noon values weather stream [lat, long, yr, mon, day, temp, rh, ws, prec]
#' @return          daily min/max values weather stream [lat, long, yr, mon, day, temp_min, temp_max, rh_min, rh_max, ws_min, ws_max, prec]
#' @export daily_to_minmax
daily_to_minmax <- function(df) {
  df <- data.table(df)
  hadId <- FALSE
  if ("id" %in% tolower(colnames(df))) {
    hadId <- TRUE
  }
  df[, c("temp_min", "temp_max") := temp_min_max(temp, rh)]
  df[, q := findQ(temp, rh)]
  df[, rh_min := pmin(100, pmax(0, findrh(q, temp_max)))]
  df[, rh_max := pmin(100, pmax(0, findrh(q, temp_min)))]
  df[, ws_min := 0.15 * ws]
  df[, ws_max := 1.25 * ws]
  if (hadId) {
    df <- df[, c("id", "lat", "long", "yr", "mon", "day", "temp_min", "temp_max", "rh_min", "rh_max", "ws_min", "ws_max", "prec")]
  } else {
    df <- df[, c("lat", "long", "yr", "mon", "day", "temp_min", "temp_max", "rh_min", "rh_max", "ws_min", "ws_max", "prec")]
  }
  return(df)
}

# so this can be run via Rscript
if ("--args" %in% commandArgs()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (2 == length(args)) {
    inp <- args[1]
    out <- args[2]
    df <- as.data.table(read.csv(inp))
    df_minmax <- daily_to_minmax(df)
    save_csv(df_minmax, out)
  } else {
    message("Wrong number of arguments")
  }
}

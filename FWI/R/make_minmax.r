# convert daily weather into min/max weather format
library(data.table)
source("util.r")

#' Convert daily temperature at 1pm (noon standard time) to daily min/max
#'
#' @param    temp_noon   traditional temperature measurement [°C]
#' @param    rh_noon     traditional relative humidity measurement [%]
#' @return               list of two values [min temperature, max temperature]
temp_min_max <- function(temp_noon, rh_noon) {
  temp_range <- 17 - 0.16 * rh_noon + 0.22 * temp_noon
  temp_max <- ifelse(temp_range <= 2,
    temp_noon + 1,
    temp_noon + 2
  )
  temp_min <- ifelse(temp_range <= 2,
    temp_noon - 1,
    temp_max - temp_range
  )
  return(list(temp_min, temp_max))
}


#' Convert daily noon weather to daily min/max weather using statistical values
#'
#' @param   df          daily noon values weather stream [lat, long, yr, mon, day, temp, rh, ws, prec]
#' @param   round_out   decimals to truncate output to, NA for none (default 4)
#' @return              daily min/max values weather stream [lat, long, yr, mon, day, temp_min, temp_max, rh_min, rh_max, ws_min, ws_max, prec]
#' @export  daily_to_minmax
daily_to_minmax <- function(df, round_out = 4) {
  # check df_wx class for data.frame or data.table
  wasDT <- is.data.table(df)
  if (wasDT) {
    df <- copy(df)
  } else if (is.data.frame(df)) {
    df <- copy(df)
    setDT(df)
  } else {
    stop("Input weather stream w needs to be a data.frame or data.table!")
  }

  # check for required columns
  colnames(df) <- tolower(colnames(df))
  req_cols <- c("yr", "mon", "day", "temp", "rh", "ws", "prec")
  for (col in req_cols) {
    if (!col %in% names(df)) {
      stop(paste("Missing required input column:", col))
    }
  }

  df[, c("temp_min", "temp_max") := temp_min_max(temp, rh)]
  df[, q := find_q(temp, rh)]
  # ideally maximum temperature lines up with minimum relative humidity and vice versa
  df[, rh_min := pmin(100, pmax(0, find_rh(q, temp_max)))]
  df[, rh_max := pmin(100, pmax(0, find_rh(q, temp_min)))]
  df[, ws_min := 0.15 * ws]
  df[, ws_max := 1.25 * ws]

  # drop calculation columns and move prec column to the end (right-most column)
  df[, c("temp", "rh", "ws", "q") := NULL]
  setcolorder(df, "prec", after = ncol(df))

  # format decimal places of output columns
  if (!(is.na(round_out) || round_out == "NA")) {
    outcols <- c("temp_min", "temp_max", "rh_min", "rh_max", "ws_min", "ws_max")
    set(df, j = outcols, value = round(df[, ..outcols], as.integer(round_out)))
  }

  if (!wasDT) {
    setDF(df)
  }
  return(df)
}

# run daily_to_minmax by command line via Rscript, requires 2 args: input csv and output csv
# optional arg: round_out
if ("--args" %in% commandArgs() && sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 2) {
    stop("At least 2 arguments required: input csv and output csv")
  }
  input <- args[1]
  output <- args[2]
  if (length(args) >= 3) round_out <- args[3]
  else round_out <- 4
  if (length(args) >= 4) warning("Too many input arguments provided, some unused")

  df_in <- read.csv(input)
  df_out <- daily_to_minmax(df_in, round_out)
  write.csv(df_out, output, row.names = FALSE)
}

# convert hourly weather into daily format
library(data.table)
library(lubridate)
source("util.r")

#' Convert hourly values stream to daily noon values stream.
#'
#' @param   df    hourly values weather stream [lat, long, yr, mon, day, hr, temp, rh, ws, prec]
#' @return        daily noon values weather stream [lat, long, yr, mon, day, hr, temp, rh, ws, prec]
#' @export hourly_to_daily
hourly_to_daily <- function(df) {
  df <- data.table(df)
  df[, DATE := as_date(sprintf("%4d-%02d-%02d", yr, mon, day))]
  df[, FOR_DATE := as_date(ifelse(hr <= 12, DATE, DATE + days(1)))]
  prec <- df[, list(prec = sum(prec, na.rm = TRUE)), by = c("FOR_DATE")]
  df <- df[hr == 12, -c("prec")]
  df <- merge(df, prec, by = c("FOR_DATE"))[, -c("hr", "DATE", "FOR_DATE")]
  names(df) <- tolower(names(df))
  return(df)
}

# so this can be run via Rscript
if ("--args" %in% commandArgs()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (2 == length(args)) {
    inp <- args[1]
    out <- args[2]
    df <- as.data.table(read.csv(inp))
    df_daily <- hourly_to_daily(df)
    save_csv(df_daily, out)
  } else {
    message("Wrong number of arguments")
  }
}

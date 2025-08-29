#' Computes hourly FWI indices for an input hourly weather stream
library(lubridate)
library(data.table)
source("util.r")

# Fuel Load (kg/m^2)
DEFAULT_GRASS_FUEL_LOAD <- 0.35

# FIX: figure out what this should be
DEFAULT_LATITUDE <- 55.0
DEFAULT_LONGITUDE <- -120.0


#' Calculate hourly FWI indices from hourly weather stream.
#'
#' @param     df_wx           hourly values weather stream
#' @param     timezone        integer offset from GMT to use for sun calculations
#' @return                    hourly inputs for FWI calculation
#' @export make_inputs
make_inputs <- function(df_wx, timezone) {
    wx <- as.data.table(copy(df_wx))
    # only return specific columns - don't worry about other columns right now
    header <- "lat,long,yr,mon,day,hr,temp,rh,ws,prec"
    header_out <- "lat,long,yr,mon,day,hr,temp,rh,ws,prec,solrad,percent_cured,grass_fuel_load"
    cols_req <- unlist(strsplit(header, ","))
    stopifnot(colnames(wx) == cols_req)
    colnames(wx) <- toupper(colnames(wx))
    wx[, MINUTE := 0]
    wx[, TIMESTAMP := as_datetime(sprintf("%04d-%02d-%02d %02d:%02d:00", YR, MON, DAY, HR, MINUTE))]
    wx[, DATE := as.Date(TIMESTAMP)]
    wx$JULIAN <- julian(wx$MON, wx$DAY)
    wx$PERCENT_CURED <- seasonal_curing(wx$JULIAN)
    wx$GRASS_FUEL_LOAD <- DEFAULT_GRASS_FUEL_LOAD
    wx$TIMEZONE <- timezone
    wx <- get_sunlight(wx, with_solrad = TRUE)
    cols_out <- unlist(strsplit(header_out, ","))
    colnames(wx) <- tolower(colnames(wx))
    wx <- wx[, ..cols_out]
    return(wx)
}

# so this can be run via Rscript
if ("--args" %in% commandArgs()) {
    args <- commandArgs(trailingOnly = TRUE)
    if (3 == length(args)) {
        # args <- c("-6", "./out/wx_diurnal_r.csv", "./out/inputs_diurnal_r.csv")
        timezone <- as.double(args[1])
        file_in <- args[2]
        file_out <- args[3]
        df_wx <- as.data.table(read.csv(file_in))
        df_inputs <- make_inputs(df_wx, timezone = timezone)
        save_csv(df_inputs, file_out)
    } else {
        message("Wrong number of arguments")
    }
}

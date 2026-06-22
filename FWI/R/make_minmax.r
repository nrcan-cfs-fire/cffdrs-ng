# Compute daily minimum and maximum (minmax) weather from traditional
# daily values (13:00 Local Daylight Time or 12:00 Local Standard Time)


### Import packages ###########################################################

library(data.table)
source("util.r")


### Functions #################################################################

#' Convert daily temperature at 13:00 LDT or 12:00 LST to daily minmax
#'
#' @param    temp_day   Daily temperature at 13:00 LDT or 12:00 LST [°C]
#' @param    rh_day     Daily relative humidity at 13:00 LDT or 12:00 LST [%]
#' @return              List of: [minimum temperature, maximum temperature]
temp_min_max <- function(temp_day, rh_day) {
    temp_range <- 0.22*temp_day - 0.16*rh_day + 17
    if (temp_range <= 2) {
        temp_max <- temp_day + 1
        temp_min <- temp_day - 1
    } else {
        temp_max <- temp_day + 2
        temp_min <- temp_max - temp_range
    }
    return(c(temp_min, temp_max))
}

#' Find specific humidity, the mass ratio of water vapour (g) to all air (kg)
#'
#' @param temp        Temperature (°C)
#' @param rh          Relative humidity (%)
#' @return            Specific humidity (g/kg)
find_q <- function(temp, rh) {
    svp <- 6.108 * exp(17.27*temp/(temp+237.3))
    vp <- svp * rh / 100
    q <- 217 * vp / (273.17+temp)
    return(q)
}

#' Find relative humidity
#'
#'  @param q           Specific humidity (g/kg)
#'  @param temp        Temperature (°C)
#'  @return            Relative humidity (%)
find_rh <- function(q, temp) {
    cur_vp <- (273.17+temp) * q / 217
    rh <- 100 * cur_vp / (6.108*exp(17.27*temp/(temp+237.3)))
    return(rh)
}

#' Convert daily weather at 13:00 LDT / 12:00 LST to daily minmax statistically
#'
#' @param   df_wx_day   Daily weather at 13:00 LDT or 12:00 LST df, columns:
#'                          yr, mon, day, temp, rh, ws, prec
#' @param   silent      Suppresses informative print statements (default False)
#' @param   round_out   Decimals to truncate output to, or NA (default 4)
#' @return              Daily minmax weather, columns:
#'                          yr, mon, day, temp_min, temp_max, rh_min, rh_max,
#'                          ws_min, ws_max, prec
#' @export  daily_to_minmax
daily_to_minmax <- function(df_wx_day, silent = FALSE, round_out = 4) {
    if (!silent) {
        writeLines("\n########")
        writeLines(paste0("FWI2025: Make Min/Max Inputs (", version(), ")\n"))
        writeLines("Predicting daily min/max weather")
    }
    ### Check if class of df_wx_day is data.frame or data.table ###
    wasDT <- is.data.table(df_wx_day)
    if (wasDT) {
        df <- copy(df_wx_day)
    } else if (is.data.frame(df_wx_day)) {
        df <- copy(df_wx_day)
        setDT(df)
    } else {
        stop("Input weather df_wx_day needs to be a data.frame or data.table!")
    }
    ### Check for required columns ###
    colnames(df) <- tolower(colnames(df))
    req_cols <- c("yr", "mon", "day", "temp", "rh", "ws", "prec")
    for (col in req_cols) {
        if (!col %in% names(df)) {
            stop(paste("Missing required input column:", col))
        }
    }
    ## Calculate minmax temperature ###
    temp_calc <- t(Vectorize(temp_min_max)(df[, temp], df[, rh]))
    df[, c("temp_min", "temp_max") := list(temp_calc[, 1], temp_calc[, 2])]
    ### Calculate minmax relative humidity ###
    # Calculate specific humidity, assume it is the same at minmax temperature.
    df[, q := find_q(temp, rh)]
    # Assume min relative humidity happens at max temperature and vice versa.
    df[, rh_min := pmin(100, find_rh(q, temp_max))]
    df[, rh_max := pmin(100, find_rh(q, temp_min))]
    ### Calculate minmax wind speed ###
    df[, ws_min := 0.15 * ws]
    df[, ws_max := 1.25 * ws]
    ### Prepare for output ###
    df[, c("temp", "rh", "ws", "q") := NULL]
    setcolorder(df, "prec", after = ncol(df))
    if (!(is.na(round_out) || round_out == "NA")) {
        outcols <- c("temp_min", "temp_max", "rh_min",
                     "rh_max", "ws_min", "ws_max")
        set(df, j = outcols, value = round(df[, ..outcols],
            as.integer(round_out)))
    }
    if (!wasDT) {
        setDF(df)
    }
    if (!silent) {
       writeLines("########\n")
    }
    return(df)
}


### Command line file execution ###############################################

# Run daily_to_minmax by command line via Rscript.
# Required arguments: input csv, output csv
# Optional arguments: silent, round_out
if ("--args" %in% commandArgs() && sys.nframe() == 0) {
    ### Parse arguments ###
    args <- commandArgs(trailingOnly = TRUE)
    if (length(args) < 2) {
        stop("At least 2 arguments required: input csv and output csv")
    }
    input <- args[1]
    output <- args[2]
    if (length(args) >= 3) silent <- as.logical(args[3])
    else silent <- FALSE
    if (length(args) >= 4) round_out <- args[4]
    else round_out <- 4
    if (length(args) >= 5) {
        warning("Too many input arguments provided, some unused")
    }
    ### Run daily_to_minmax() ###
    df_in <- read.csv(input)
    df_out <- daily_to_minmax(df_in, silent, round_out)
    write.csv(df_out, output, row.names = FALSE)
}

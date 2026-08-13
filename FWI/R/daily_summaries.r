# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.


# Summarize hourly FWI outputs into daily peak burn metrics.


### Import packages ###########################################################

library(data.table)
library(lubridate)
source("NG_FWI.r")
source("util.r")


### Functions #################################################################

#' Calculate a smoothed vector weighted by binomial coefficients
#' @param    ser    Vector of values
#' @return          Vector of smoothed values
smooth_binomial_5pt <- function(vec) {
    # Calculate interior (cells with 2 neighbours either side) with binomial
    # coefficient weighting: [1, 4, 6, 4, 1].
    smoothed <- (shift(vec, -2)
                 +4*shift(vec, -1)
                 +6*vec
                 +4*shift(vec, 1)
                 +shift(vec, 2)) / 16
    # Calculate inner edges (only 1 neighbour either side) weighted: [1, 2, 1].
    n <- length(smoothed)
    if (n >= 3) {
        smoothed[2] <- (vec[1]+2*vec[2]+vec[3]) / 4
        if (n > 3) {
            smoothed[n - 1] <- (vec[n - 2]+2*vec[n - 1]+vec[n]) / 4
        }
    }
    # Copy values into any remaining NA, including outer edges.
    smoothed[is.na(smoothed)] <- vec[is.na(smoothed)]
    return(smoothed)
}

#' Calculate a pseudo date that changes at another hour past midnight
#'
#' @param    yr          Year
#' @param    mon         Month number
#' @param    day         Day of month
#' @param    hr          Hour of day
#' @param    reset_hr    The new boundary hour instead of midnight (default 5)
#' @return               Pseudo ordinal date as year and ordinal day: "YYYY-D"
pseudo_date <- function(yr, mon, day, hr, reset_hr = 5) {
    d <- make_date(yr, mon, day)
    if (hr < reset_hr) {
        # yday() function accounts for leap years.
        pseudo_ord_day <- as.integer(yday(d)) - 1
    }
    else {
        pseudo_ord_day <- as.integer(yday(d))
    }
    # When ordinal day 1 (Jan 1) shifts to 0, bump it to end of previous year.
    if (pseudo_ord_day == 0) {
        pseudo_ord_day <- as.integer(yday(make_date(yr - 1, 12, 31)))
        pseudo_yr <- yr - 1
    }
    else {
        pseudo_yr <- yr
    }
    return(sprintf("%d-%d", pseudo_yr, pseudo_ord_day))
}

#' Calculate Daily Summaries from hourly FWI indices
#'
#' @param    df_hfwi         Hourly FWI dataframe (output of hFWI())
#' @param    reset_hr        Hour defining new day to summarize (default 5)
#' @param    bw_threshold    isi_smooth threshold for active burning
#'                               (default 5)
#' @param    silent          Suppress print statements (default False)
#' @param    round_out       Number of decimal places to truncate outputs to,
#'                               or NA for none (default 4)
#' @return                   Daily summary of peak FWI conditions
generate_daily_summaries <- function(
    df_hfwi,
    reset_hr = 5,
    bw_threshold = 5,
    silent = FALSE,
    round_out = 4
) {
    if (!silent) {
        writeLines(paste0("\n########\nFWI2025: Daily Summaries (",
                          version(), ")\n"))
    }
    was_dt <- is.data.table(df_hfwi)
    if (was_dt) {
        df <- copy(df_hfwi)
    }
    else if (is.data.frame(df_hfwi)) {
        df <- copy(df_hfwi)
        setDT(df)
    }
    else {
        stop("Input hourly FWI needs to be a data.frame or data.table!")
    }
    ### Check for required and optional columns ###
    req_cols <- c("yr", "mon", "day", "hr", "ws", "sunrise", "sunset", "ffmc",
                  "dmc", "dc", "isi", "bui", "fwi", "dsr", "mcgfmc_matted",
                  "mcgfmc_standing", "gfmc", "gsi", "gfwi", "percent_cured")
    for (col in req_cols) {
        if (!col %in% names(df)) {
            stop(paste("Missing required input column:", col))
        }
    }
    if ("id" %in% names(df)) {
        had_id <- TRUE
    }
    else {
        if (uniqueN(df, by = c("yr", "lat", "long")) == 1) {
            df[, id := "stn"]
            had_id <- FALSE
        }
        else {
            stop(paste("Missing 'id' column with multiple years and locations",
                       "in data"))
        }
    }
    ### Split by station ID and pseudo date ###
    daily <- NULL
    for (stnid in unique(df[, id])) {
        if (!silent) {
            writeLines(paste("Summarizing", stnid, "to daily"))
        }
        df_id <- df[id == stnid]
        df_id[, date_pseudo := Vectorize(pseudo_date)(yr, mon, day, hr,
                                                      reset_hr)]
        # Use first year available for transition between matted and standing
        # grassland (considering southern hemisphere fire season timing).
        DATE_GRASS_STANDING <- make_date(
            df_id[1, yr],
            MON_STANDING,
            DAY_STANDING
        )
        for (p_date in unique(df_id[, date_pseudo])) {
            df_date <- df_id[date_pseudo == p_date]
            # If this pseudo-date doesn't have more than 12 hours, skip.
            if (nrow(df_date) <= 12) next
            ### Find daily peak active burning hour ###
            df_date <- df_date[, ws_smooth := smooth_binomial_5pt(ws)]
            df_date <- df_date[, isi_smooth := initial_spread_index(ws_smooth,
                                                                    ffmc)]
            max_ffmc <- df_date[, max(ffmc)]
            if (max_ffmc < 85.0) {
                peak_time <- 13  # 12 hours into pseudo date.
            }
            else {
                peak_time <- df_date[, which.max(isi_smooth)]
            }
            ### Calculate duration of active burning window ###
            if (any(df_date[, isi_smooth] >= bw_threshold)) {
                # Difference between first and last hours of active burning.
                active_burning <- df_date[isi_smooth >= bw_threshold]
                t_ab0 <- make_datetime(
                    first(active_burning)[, yr],
                    first(active_burning)[, mon],
                    first(active_burning)[, day],
                    first(active_burning)[, hr]
                )
                t_ab1 <- make_datetime(
                    last(active_burning)[, yr],
                    last(active_burning)[, mon],
                    last(active_burning)[, day],
                    last(active_burning)[, hr]
                )
                dt_ab <- as.integer(
                    difftime(t_ab1, t_ab0, units = "hours") + 1
                )
            }
            else {
                dt_ab <- 0L
            }
            ### Find the values at peak active burning all at once ###
            sr <- df_date[1, sunrise]
            ss <- df_date[1, sunset]
            d <- make_date(df_date[1, yr], df_date[1, mon], df_date[1, day])
            if (GRASS_TRANSITION && d < DATE_GRASS_STANDING) {
                standing <- FALSE
                mcgfmc <- df_date[peak_time, mcgfmc_matted]
            }
            else {
                standing <- TRUE
                mcgfmc <- df_date[peak_time, mcgfmc_standing]
            }
            day_report <- data.table(
                id = df_date[1, id],
                yr = df_date[1, yr],
                mon = df_date[1, mon],
                day = df_date[1, day],
                # Format sunrise and sunset as hh:mm instead of decimal hours.
                sunrise = sprintf(
                    "%02d:%02d",
                    trunc(sr),
                    trunc(60 * (sr - trunc(sr)))
                ),
                sunset = sprintf(
                    "%02d:%02d",
                    trunc(ss),
                    trunc(60 * (ss - trunc(ss)))
                ),
                peak_hr = df_date[peak_time, hr],
                duration = dt_ab,
                ffmc = df_date[peak_time, ffmc],
                dmc = df_date[peak_time, dmc],
                dc = df_date[peak_time, dc],
                isi = df_date[peak_time, isi],
                bui = df_date[peak_time, bui],
                fwi = df_date[peak_time, fwi],
                dsr = df_date[peak_time, dsr],
                gfmc = df_date[peak_time, gfmc],
                gsi = df_date[peak_time, gsi],
                gfwi = df_date[peak_time, gfwi],
                ws_smooth = df_date[peak_time, ws_smooth],
                isi_smooth = df_date[peak_time, isi_smooth],
                gsi_smooth = df_date[
                    peak_time,
                    grass_spread_index(
                        ws_smooth,
                        mcgfmc,
                        percent_cured,
                        standing
                    )
                ]
            )
            daily <- rbind(daily, day_report)
        }
    }
    ### Prepare for output ###
    if (!had_id) daily <- daily[, -"id"]
    if (!(is.na(round_out) || round_out == "NA")) {
        outcols <- c("ffmc", "dmc", "dc", "isi", "bui", "fwi", "dsr", "gfmc",
                     "gsi", "gfwi", "ws_smooth", "isi_smooth", "gsi_smooth")
        set(
            daily,
            j = outcols,
            value = round(daily[, ..outcols], as.integer(round_out))
        )
    }
    if (!was_dt) setDF(daily)
    if (!silent) writeLines("########\n")
    return(daily)
}


### Command line file execution (Rscript) #####################################
# Required arguments: input csv, output csv.
# Optional arguments: reset_hr, silent, round_out.
if ("--args" %in% commandArgs() && sys.nframe() == 0) {
    ### Parse arguments ###
    args <- commandArgs(trailingOnly = TRUE)
    if (length(args) < 2) {
        stop("At least 2 arguments required: input csv, output csv.")
    }
    input <- args[1]
    output <- args[2]
    if (length(args) >= 3) reset_hr <- as.integer(args[3])
    else reset_hr <- 5
    if (length(args) >= 4) silent <- as.logical(args[4])
    else silent <- FALSE
    if (length(args) >= 5) round_out <- args[5]
    else round_out <- 4
    if (length(args) >= 6) {
        warning("Too many input arguments provided, some unused.")
    }
    ### Run generate_daily_summaries() ###
    df_in <- read.csv(input)
    df_out <- generate_daily_summaries(df_in, reset_hr, silent, round_out)
    write.csv(df_out, output, row.names = FALSE)
}

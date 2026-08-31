# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.


# Utility functions for FWI2025.


### Import packages ###########################################################

library(data.table)
library(lubridate)


### Functions #################################################################

#' FWI2025 version (date) following changelog defined for print statements
#'
#' @return       String of date (YYYY-MM-DD)
version <- function() {
    # update this and CHANGELOG.md before merging to GitHub main branch
    return("2026-08-13 + DEV")
}

#' Determine if datatable rows are sequential in days
#'
#' @param    dt    Datatable to check (requires timestamp column)
#' @return         Whether each row is 1 day from the next row
is_sequential_days <- function(dt) {
    cols <- tolower(colnames(dt))
    if ("timestamp" %in% cols) {
        ts <- dt[[which(cols == "timestamp")[1]]]
    }
    else {
        stop("'timestamp' column required for is_sequential_days()")
    }
    return((length(ts) == 1) || (
        all((ts - shift(ts) == as.difftime(1, units = "days"))[2:length(ts)])
    ))
}

#' Determine if datatable rows are sequential in hours
#'
#' @param    dt    Datatable to check (requires timestamp column)
#' @return         Whether each rpw is 1 hour from the next row
is_sequential_hours <- function(dt) {
    cols <- tolower(colnames(dt))
    if ("timestamp" %in% cols) {
        ts <- dt[[which(cols == "timestamp")[1]]]
    }
    else {
        stop("'timestamp' column required for is_sequential_hours()")
    }
    return((length(ts) == 1) || (
        all((ts - shift(ts) == as.difftime(1, units = "hours"))[2:length(ts)])
    ))
}

#' Calculate start and end times of the drying day based on location and date.
#' Can also calculate hourly solar radiation using vapour pressure deficit
#'
#' @param    dt            Datatable of weather data to add columns to
#' @param    get_solrad    Whether to calculate solar radiation (default FALSE)
#' @return                 Original datatable with new columns:
#'                             sunrise, sunset, sunlight_hours, [solrad]
sun_times <- function(dt, get_solrad = FALSE) {
    dt2 <- copy(dt)
    cols <- tolower(colnames(dt2))
    colnames(dt2) <- cols
    # Columns that define unique locations and days to split along.
    cols_day <- c("lat", "long", "timezone", "date")
    ### Check for required columns ###
    cols_req <- c("lat", "long", "timezone", "timestamp")
    if (get_solrad) {
        cols_req <- c(cols_req, "temp", "rh")
    }
    for (c in cols_req) {
        stopifnot(c %in% cols)
    }
    if (!"date" %in% cols) {
        dt2[, date := as_date(timestamp)]
    }
    ### Calculate drying day start and end times ###
    # Based on generalized solar position equations.
    dt_loc_dt <- unique(dt2[, ..cols_day])
    dt_dt <- unique(dt_loc_dt[, "date"])
    # t_od is the ordinal date (julian day).
    dt_dt[, t_od := yday(date)]
    # t_yr is the day of the year as a fraction, in radians. Accounts for leap
    # years.
    dt_dt[, t_yr := ifelse(
        leap_year(year(date)),
        2 * pi * (t_od-1) / 366,
        2 * pi * (t_od-1) / 365
    )]
    # eot is the equation of time correction, in minutes.
    dt_dt[, eot := 229.18 * (7.5e-5+1.868e-3*cos(t_yr)-3.2077e-2*sin(t_yr)
                             -1.4615e-2*cos(2*t_yr)-4.0849e-2*sin(2*t_yr))]
    # decl is the solar declination angle, in radians.
    dt_dt[, decl := (6.918e-3 - 0.399912*cos(t_yr) + 7.0257e-2*sin(t_yr)
                     - 6.758e-3*cos(2*t_yr) + 9.07e-4*sin(2*t_yr)
                     - 2.697e-3*cos(3*t_yr) + 1.48e-3*sin(3*t_yr))]
    # z_max is the solar zenith angle at the start and end of the drying day,
    # in radians (90.833° for standard sunrise and sunset).
    z_max <- 90.833 * pi / 180
    dt_dt <- merge(dt_loc_dt, dt_dt, by = c("date"))
    # cos_ha_z is the cosine of the solar hour angle at z_max.
    dt_dt[, cos_ha_z := (cos(z_max)/(cos(pi*lat/180)*cos(decl))
                         - tan(pi*lat/180)*tan(decl))]
    # Keep cos_ha_z between -1 and 1.
    dt_dt[, cos_ha_z := pmax(-1, pmin(1, cos_ha_z))]
    # ha_z is the solar hour angle at z_max, in DD.
    dt_dt[, ha_z := 180 * acos(cos_ha_z) / pi]
    dt_dt[, sunrise := (720-4*(long+ha_z)-eot)/60 + timezone]
    dt_dt[, sunset := (720-4*(long-ha_z)-eot)/60 + timezone]
    dt_all <- merge(dt2, dt_dt, by = cols_day)
    ### Calculate solar radiation ###
    if (get_solrad) {
        # ha is the solar hour angle, in radians.
        dt_all[, ha := pi/180*(15*(hour(timestamp)-timezone)+long+eot/4) - pi]
        # cos_z is the cosine of the solar zenith angle.
        dt_all[, cos_z := (sin(pi*lat/180)*sin(decl)
                           + cos(pi*lat/180)*cos(decl)*cos(ha))]
        # vpd is vapour pressure deficit.
        dt_all[, vpd := 6.11 * (1-rh/100) * exp(17.29*temp/(temp+237.3))]
        # Coefficients for cos_z and vpd from a regression analysis using data
        # from the 2007 season at the Petawawa Research Forest.
        dt_all[, solrad := 0.92 * cos_z * (1-exp(-0.22*vpd))]
        # Set negative and really small solar radiation values to 0.
        dt_all[solrad < 1e-4, solrad := 0]
        # Columns to keep in output dataframe.
        cols_sun <- c("solrad", "sunrise", "sunset")
    } else {
        cols_sun <- c("sunrise", "sunset")
    }
    ### Prepare for output ###
    df_result <- dt_all  # cbind(dt, dt_all[, ..cols_sun])
    df_result[, sunlight_hours := sunset - sunrise]
    return(df_result)
}

# Alias for sun_times().
get_sunlight <- sun_times

#' Calculate default grassland curing values. Based on typical daily variations
#' of grassland curing in the Boreal Plains region.
#'
#' @param    yr           Year
#' @param    mon          Month of year
#' @param    day          Day of month
#' @param    start_mon    Start month of grassland fuel green-up (Default 3)
#' @param    start_day    Start day of grassland fuel green-up (Default 12)
#' @return                Grassland curing percentage [%]
grassland_curing <- function(yr, mon, day, start_mon = 3, start_day = 12) {
    # Default grassland curing values for every 10 days in a growing season.
    # Start and end with identical "winter" cured value.
    PERCENT_CURED <- c(96.0, 95.0, 93.0, 92.0, 90.5, 88.4, 84.4, 78.1, 68.7,
                       50.3, 32.9, 23.0, 22.0, 21.0, 20.0, 25.7, 35.0, 43.0,
                       49.8, 60.0, 68.0, 72.0, 75.0, 78.9, 86.0, 96.0)
    # Find previous green-up start date (either current or prior year).
    delta_t <- make_date(yr, mon, day) - make_date(yr, start_mon, start_day)
    if (delta_t < 0) {
        delta_t <- (make_date(yr, mon, day)
                    - make_date(yr - 1, start_mon, start_day))
    }
    # Green-up start date is the first non-winter value (not 0th).
    t_days <- as.integer(delta_t) + 1
    # Check if date is in growing season or winter (cured) season.
    if (t_days < (length(PERCENT_CURED) - 1) * 10) {
        # Linearly interpolate between every 10-day value.
        pc_0 <- PERCENT_CURED[t_days%/%10 + 1]
        pc_1 <- PERCENT_CURED[t_days%/%10 + 2]
        t_10day <- (t_days%%10) / 10
        return(pc_0 + (pc_1-pc_0)*t_10day)
    } else {
        return(PERCENT_CURED[length(PERCENT_CURED)])
    }
}

# Aliases for grassland_curing().
seasonal_curing <- grassland_curing
grass_curing <- grassland_curing

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.


# Utility functions for FWI2025.


### Import packages ###########################################################

from calendar import isleap
from datetime import date, timedelta
from math import acos, cos, exp, pi, sin, tan
import pandas as pd


### Functions #################################################################

##
# FWI2025 version (date) following changelog defined for print statements
#
# @return       String of date (YYYY-MM-DD)
def version():
    # update this and CHANGELOG.md before merging to GitHub main branch
    return "2026-08-13 + DEV"

##
# Determine if dataframe rows are sequential in days
#
# @param    df    Dataframe to check (requires timestamp column)
# @return         Whether each row is 1 day from the next row
def is_sequential_days(df):
    cols = [x.lower() for x in df.columns]
    if "timestamp" in cols:
        ts = df.iloc[:, cols.index("timestamp")]
    else:
        raise RuntimeError("'timestamp' column required for "
                           "is_sequential_days()")
    return (ts.shape[0] == 1) or (
        (ts - ts.shift(1) == timedelta(days = 1)).iloc[1:].all()
    )

##
# Determine if dataframe rows are sequential in hours
#
# @param    df    Dataframe to check (requires timestamp column)
# @return         Whether each row is 1 hour from the next row
def is_sequential_hours(df):
    cols = [x.lower() for x in df.columns]
    if "timestamp" in cols:
        ts = df.iloc[:, cols.index("timestamp")]
    else:
        raise RuntimeError("'timestamp' column required for "
                           "is_sequential_hours()")
    return (ts.shape[0] == 1) or (
        (ts - ts.shift(1) == timedelta(hours = 1)).iloc[1:].all()
    )

##
# Calculate start and end times of the drying day based on location and date.
# Can also calculate hourly solar radiation using vapour pressure deficit
#
# @param    df            Dataframe of weather data to add columns to
# @param    get_solrad    Whether to calculate solar radiation (default False)
# @return                 Original dataframe with new columns:
#                             sunrise, sunset, sunlight_hours, [solrad]
def sun_times(df, get_solrad = False):
    df2 = df.copy()
    cols = [x.lower() for x in df2.columns]
    df2.columns = cols
    # Columns that define unique locations and days to split along.
    cols_day = ["lat", "long", "timezone", "date"]
    ### Check for required columns ###
    cols_req = ["lat", "long", "timezone", "timestamp"]
    if get_solrad:
        cols_req += ["temp", "rh"]
    for c in cols_req:
        if c not in cols:
            raise RuntimeError(c + " column required for get_sunrise()")
    if "date" not in cols:
        df2["date"] = df2["timestamp"].apply(lambda ts: ts.date())
    ### Calculate drying day start and end times ###
    # Based on generalized solar position equations.
    df_loc_dt = df2[cols_day].drop_duplicates()
    df_dt = df_loc_dt[["date"]].drop_duplicates()
    # t_od is the ordinal date (julian day).
    df_dt["t_od"] = df_dt["date"].apply(lambda d: int(d.strftime("%j")))
    # t_yr is the day of the year as a fraction, in radians. Accounts for leap
    # years.
    df_dt["t_yr"] = df_dt.apply(
        lambda x: (2 * pi * (x["t_od"]-1) / 366) if isleap(x["date"].year)
        else (2 * pi * (x["t_od"]-1) / 365), axis = 1
    )
    # eot is the equation of time correction, in minutes.
    df_dt["eot"] = df_dt["t_yr"].apply(lambda t: 229.18 * (
        7.5e-5 + 1.868e-3*cos(t) - 3.2077e-2*sin(t)
        - 1.4615e-2*cos(2*t) - 4.0849e-2*sin(2*t)
    ))
    # decl is the solar declination angle, in radians.
    df_dt["decl"] = df_dt["t_yr"].apply(lambda t: (
        6.918e-3 - 0.399912*cos(t) + 7.0257e-2*sin(t)
        - 6.758e-3*cos(2*t) + 9.07e-4*sin(2*t)
        - 2.697e-3*cos(3*t) + 1.48e-3*sin(3*t)
    ))
    # z_max is the solar zenith angle at the start and end of the drying day,
    # in radians (90.833° for standard sunrise and sunset).
    z_max = 90.833 * pi / 180
    df_dt = pd.merge(df_loc_dt, df_dt, on = ["date"])
    # cos_ha_z is the cosine of the solar hour angle at z_max.
    df_dt["cos_ha_z"] = df_dt.apply(lambda x: (
        cos(z_max)/(cos(pi*x["lat"]/180)*cos(x["decl"]))
        - tan(pi*x["lat"]/180)*tan(x["decl"])
    ), axis = 1)
    # Keep cos_ha_z between -1 and 1.
    df_dt["cos_ha_z"] = df_dt["cos_ha_z"].clip(-1, 1)
    # ha_z is the solar hour angle at z_max, in DD.
    df_dt["ha_z"] = df_dt["cos_ha_z"].apply(lambda cha: 180 * acos(cha) / pi)
    df_dt["sunrise"] = df_dt.apply(
        lambda x: (720-4*(x["long"]+x["ha_z"])-x["eot"])/60 + x["timezone"],
        axis = 1
    )
    df_dt["sunset"] = df_dt.apply(
        lambda x: (720-4*(x["long"]-x["ha_z"])-x["eot"])/60 + x["timezone"],
        axis = 1
    )
    df_all = pd.merge(df2, df_dt, on = cols_day)
    ### Calculate solar radiation ###
    if get_solrad:
        # ha is the solar hour angle, in radians.
        df_all["ha"] = df_all.apply(lambda x: (
            pi/180*(
                15*(x["timestamp"].hour-x["timezone"]) + x["long"] + x["eot"]/4
            ) - pi
        ), axis = 1)
        # cos_z is the cosine of the solar zenith angle.
        df_all["cos_z"] = df_all.apply(lambda x: (
            sin(pi*x["lat"]/180)*sin(x["decl"])
            + cos(pi*x["lat"]/180)*cos(x["decl"])*cos(x["ha"])
        ), axis = 1)
        # vpd is vapour pressure deficit.
        df_all["vpd"] = df_all.apply(lambda x: (
            6.11 * (1-x["rh"]/100) * exp(17.29*x["temp"]/(x["temp"]+237.3))
        ), axis = 1)
        # Coefficients for cos_z and vpd from a regression analysis using data
        # from the 2007 season at the Petawawa Research Forest.
        df_all["solrad"] = df_all.apply(
            lambda x: 0.92 * x["cos_z"] * (1-exp(-0.22*x["vpd"])), axis = 1
        )
        # Set negative and really small solar radiation values to 0.
        df_all.loc[df_all["solrad"] < 1e-4, "solrad"] = 0
        # Columns to keep in output dataframe.
        cols_sun = ["solrad", "sunrise", "sunset"]
    else:
        cols_sun = ["sunrise", "sunset"]
    ### Prepare for output ###
    df_out = pd.concat([df, df_all[cols_sun]], axis = 1)
    # Calculate the number of drying hours in the day.
    df_out["sunlight_hours"] = df_out.apply(
        lambda x: x["sunset"] - x["sunrise"], axis = 1
    )
    return df_out

# Alias for sun_times().
get_sunlight = sun_times

##
# Calculate default grassland curing values. Based on typical 10 day variations
# of grassland curing based on NDVI in the Boreal Plains region.
#
# @param    yr           Year
# @param    mon          Month of year
# @param    day          Day of month
# @param    start_mon    Start month of grassland fuel green-up (Default 3)
# @param    start_day    Start day of grassland fuel green-up (Default 12)
# @return                Grassland curing percentage [%]
def grassland_curing(yr, mon, day, start_mon = 3, start_day = 12):
    # Default grassland curing values for every 10 days in a growing season.
    # Start and end with identical "winter" cured value.
    PERCENT_CURED = [96.0, 95.0, 93.0, 92.0, 90.5, 88.4, 84.4, 78.1, 68.7,
                     50.3, 32.9, 23.0, 22.0, 21.0, 20.0, 25.7, 35.0, 43.0,
                     49.8, 60.0, 68.0, 72.0, 75.0, 78.9, 86.0, 96.0]
    # Find previous green-up start date (either current or prior year).
    delta_t = date(yr, mon, day) - date(yr, start_mon, start_day)
    if delta_t.days < 0:
        delta_t = date(yr, mon, day) - date(yr - 1, start_mon, start_day)
    # Green-up start date is the first non-winter value (not 0th).
    t_days = delta_t.days + 1
    # Check if date is in growing season or winter (cured) season.
    if t_days < (len(PERCENT_CURED)-1) * 10:
        # Linearly interpolate between every 10-day value.
        pc_0 = PERCENT_CURED[t_days // 10]
        pc_1 = PERCENT_CURED[t_days//10 + 1]
        t_10day = (t_days%10) / 10
        return pc_0 + (pc_1-pc_0)*t_10day
    else:
        return PERCENT_CURED[-1]

# Aliases for grassland_curing().
seasonal_curing = grassland_curing
grass_curing = grassland_curing

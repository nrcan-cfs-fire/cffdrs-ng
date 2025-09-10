# Various utility functions used by the other files
import datetime
from math import acos, cos, exp, pi, sin, tan
import numpy as np
import pandas as pd


##
# Determine if data is sequential days
#
# @param data          data to check
# @return              whether each entry is 1 day from the next entry
def is_sequential_days(data):
    return np.all(
        datetime.timedelta(days=1)
        == (data["timestamp"] - data["timestamp"].shift(1)).iloc[1:]
    )

##
# Determine if data is sequential hours
#
# @param data          data to check
# @return              whether each entry is 1 hour from the next entry
def is_sequential_hours(data):
    return np.all(
        datetime.timedelta(hours=1)
        == (data["timestamp"] - data["timestamp"].shift(1)).iloc[1:]
    )

##
# Find specific humidity
#
# @param temp        Temperature (Celcius)
# @param rh          Relative humidity (percent, 0-100)
# @return            Specific humidity (g/kg)
def find_q(temp, rh):
    # find absolute humidity
    svp = 6.108 * exp(17.27 * temp / (temp + 237.3))
    vp = svp * rh / 100.0
    q = 217 * vp / (273.17 + temp)
    return q

##
# Find relative humidity
#
#  @param q           Specific humidity (g/kg)
#  @param temp        Temperature (Celcius)
#  @return            Relative humidity (percent, 0-100)
def find_rh(q, temp):
    cur_vp = (273.17 + temp) * q / 217
    rh = 100 * cur_vp / (6.108 * exp(17.27 * temp / (temp + 237.3)))
    return rh

##
# Find day of year. Does not properly deal with leap years.
#
# @param mon         Month
# @param day         Day of month
# @return            Day of year
def julian(mon, day):
    month = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365]
    return month[int(mon) - 1] + int(day)

##
# Calculate sunrise, sunset, (solar radiation) for one station (location) for one year
# (does not take leap years into account)
#
# @param df                Dataframe to add columns to
# @param get_solrad        Whether to calculate solar radiation
# @return                  Sunrise, sunset, sunlight hours, and solar radiation (kW/m^2)
def get_sunlight(df, get_solrad = False):
    # columns to split along unique days
    cols_day = ["lat", "long", "date", "timezone"]
    # required columns
    cols_req = ["lat", "long", "timezone", "timestamp"]
    if get_solrad:
        cols_req.append("temp")
    for n in cols_req:
        if n not in df.columns:
            raise RuntimeError(f"Expected column '{n}' not found")
    df_copy = df.loc[:]
    # (re)make date column
    df_copy.loc[:, "date"] = df_copy["timestamp"].apply(lambda ts: ts.date())
    
    # calculate sunrise and sunset
    # drop duplicate days
    df_stn_dates = df_copy[cols_day].drop_duplicates()
    df_dates = df_stn_dates[["date"]].drop_duplicates()
    df_dates["jd"] = df_dates["date"].apply(lambda d: julian(d.month, d.day))
    dec_hour = 12.0
    df_dates["fracyear"] = df_dates["jd"].apply(
        lambda jd: 2.0 * pi / 365.0 * (jd - 1.0 + (dec_hour - 12.0) / 24.0))
    df_dates["eqtime"] = df_dates["fracyear"].apply(
        lambda fracyear: 229.18 * (0.000075 +
        0.001868 * cos(fracyear) - 0.032077 * sin(fracyear) -
        0.014615 * cos(2.0 * fracyear) - 0.040849 * sin(2.0 * fracyear)))
    df_dates["decl"] = df_dates["fracyear"].apply(
        lambda fracyear: 0.006918 -
        0.399912 * cos(fracyear) + 0.070257 * sin(fracyear) -
        0.006758 * cos(fracyear * 2.0) + 0.000907 * sin(2.0 * fracyear) -
        0.002697 * cos(3.0 * fracyear) + 0.00148 * sin(3.0 * fracyear))
    df_dates["zenith"] = 90.833 * pi / 180.0
    # at this point we actually need the LAT/LONG/TIMEZONE
    df_dates = pd.merge(df_stn_dates, df_dates, on = ["date"])
    df_dates["timeoffset"] = df_dates.apply(
        lambda x: x["eqtime"] + 4 * x["long"] - 60 * x["timezone"], axis = 1)
    df_dates["x_tmp"] = df_dates.apply(
        lambda x: cos(x["zenith"]) / (cos(x["lat"] * pi / 180.0) * cos(x["decl"])) -
        tan(x["lat"] * pi / 180.0) * tan(x["decl"]), axis = 1)
    # keep in range
    df_dates["x_tmp"] = df_dates["x_tmp"].apply(lambda x_tmp: max(-1, min(1, x_tmp)))
    df_dates["halfday"] = df_dates["x_tmp"].apply(lambda x_tmp: 180.0 / pi * acos(x_tmp))
    df_dates["sunrise"] = df_dates.apply(
        lambda x: (720.0 - 4.0 * (x["long"] + x["halfday"]) - x["eqtime"]) / 60 +
        x["timezone"], axis = 1)
    df_dates["sunset"] = df_dates.apply(
        lambda x: (720.0 - 4.0 * (x["long"] - x["halfday"]) - x["eqtime"]) / 60 +
        x["timezone"], axis = 1)
    df_all = pd.merge(df_copy, df_dates, on = cols_day)

    # calculate solar radiation
    if get_solrad:
        df_all["tst"] = df_all.apply(
            lambda x: (x["timestamp"].hour) * 60.0 + x["timeoffset"], axis = 1)
        df_all["hourangle"] = df_all.apply(lambda x: x["tst"] / 4 - 180, axis = 1)
        df_all["zenith"] = df_all.apply(
            lambda x: acos(sin(x["lat"] * pi / 180) * sin(x["decl"]) +
            cos(x["lat"] * pi / 180) * cos(x["decl"]) *
            cos(x["hourangle"] * pi / 180)), axis = 1)
        df_all["zenith"] = df_all["zenith"].apply(lambda zenith: min(pi / 2, zenith))
        # need later so keep column
        df_all["cos_zenith"] = df_all["zenith"].apply(cos)
        df_all["vpd"] = df_all.apply(lambda x:
            6.11 * (1.0 - x["rh"] / 100.0) * exp(17.29 * x["temp"] / (x["temp"] + 237.3)),
            axis = 1)
        df_all["solrad"] = df_all.apply(lambda x:
            x["cos_zenith"] * 0.92 * (1.0 - exp(-0.22 * x["vpd"]))
            if (x["sunrise"] <= x["hr"] <= x["sunset"]) else 0.0, axis = 1)
        
        cols_sun = ["solrad", "sunrise", "sunset"]
    else:
        cols_sun = ["sunrise", "sunset"]

    # don't output intermediate calculations/variables
    cols = list(df.columns) + cols_sun
    df_result = df_all.loc[:, cols]
    df_result["sunlight_hours"] = df_result.apply(
        lambda x: x["sunset"] - x["sunrise"], axis = 1)
    return df_result

def seasonal_curing(julian_date):
    PERCENT_CURED = [
        96.0,
        96.0,
        96.0,
        96.0,
        96.0,
        96.0,
        96.0,
        96.0,
        95.0,
        93.0,
        92.0,
        90.5,
        88.4,
        84.4,
        78.1,
        68.7,
        50.3,
        32.9,
        23.0,
        22.0,
        21.0,
        20.0,
        25.7,
        35.0,
        43.0,
        49.8,
        60.0,
        68.0,
        72.0,
        75.0,
        78.9,
        86.0,
        96.0,
        96.0,
        96.0,
        96.0,
        96.0,
        96.0
    ]
    jd_class = julian_date // 10
    first = PERCENT_CURED[jd_class]
    last = PERCENT_CURED[jd_class + 1]
    period_frac = (julian_date % 10) / 10.0
    return first + (last - first) * period_frac

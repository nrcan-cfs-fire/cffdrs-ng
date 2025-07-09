import datetime
import logging
from math import acos, cos, exp, log, pi, sin, tan
import re
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
        == (data["TIMESTAMP"] - data["TIMESTAMP"].shift(1)).iloc[1:]
    )


##
# Determine if data is sequential hours
#
# @param data          data to check
# @return              whether each entry is 1 hour from the next entry
def is_sequential_hours(data):
    return np.all(
        datetime.timedelta(hours=1)
        == (data["TIMESTAMP"] - data["TIMESTAMP"].shift(1)).iloc[1:]
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
# Find solar radiation for a data frame's times and places
#
# @param df                DataFrame to add columns to
# @param with_solrad       Whether to include solar radiation
# @return                  Solar radiation (kW/m^2), sunrise, sunset
def getSunlight(df, get_solrad=False, DST = False):
    dst_adjust = 0
    if (DST):
      dst_adjust = 1
  
  
    # columns to use as unique ID
    COLS_ID = ["LAT", "LONG", "DATE", "TIMEZONE"]
    cols_req = COLS_ID + ["TIMESTAMP"]
    # just make date column so we know what type it is
    df_copy = df.loc[:]
    df_copy.loc[:, "DATE"] = df_copy["TIMESTAMP"].apply(lambda x: x.date())
    if get_solrad:
        cols_req += ["TEMP"]
    for n in cols_req:
        if n not in df_copy.columns:
            raise RuntimeError(f"Expected column '{n}' not found")
    # this routine approximately calcualtes sunrise and sunset and daylength
    # REally any routine like this could be used,  some are more precise than others.
    #
    # It takes in:
    # latitude:   in degrees north -  poistive number
    # longtitude: in degress EAST(standard)  - WEST hemisphere is a negative
    # month:
    # day:
    # adjust:  hours off of Greenich mean time (for EST = -5  (EDT=-4)   CST=-6 MST=-7  PST=-8)
    #
    # It returns (as pass by reference from the funciton call line)
    # SUNRISE in decimal hours  (in the local time zone specified)
    # SUNSET in decimal hours  (in the local time zone specified)
    #
    # and the function itself returns
    # DAYLENGTH (in hours)
    #
    #
    # bmw
    # FIX: need to work on data frame because we need all 24 hours?
    df_stn_dates = df_copy[COLS_ID].drop_duplicates()
    dec_hour = 12.0
    df_dates = df_stn_dates[["DATE"]].drop_duplicates()
    df_dates["JD"] = df_dates["DATE"].apply(lambda x: julian(x.month, x.day))
    df_dates["FRACYEAR"] = df_dates["JD"].apply(
        lambda jd: 2.0 * pi / 365.0 * (jd - 1.0 + (dec_hour - 12.0) / 24.0)
    )
    df_dates["EQTIME"] = df_dates["FRACYEAR"].apply(
        lambda fracyear: 229.18
        * (
            0.000075
            + 0.001868 * cos(fracyear)
            - 0.032077 * sin(fracyear)
            - 0.014615 * cos(2.0 * fracyear)
            - 0.040849 * sin(2.0 * fracyear)
        )
    )
    df_dates["DECL"] = df_dates["FRACYEAR"].apply(
        lambda fracyear: (
            0.006918
            - 0.399912 * cos(fracyear)
            + 0.070257 * sin(fracyear)
            - 0.006758 * cos(fracyear * 2.0)
            + 0.000907 * sin(2.0 * fracyear)
            - 0.002697 * cos(3.0 * fracyear)
            + 0.00148 * sin(3.0 * fracyear)
        )
    )
    df_dates["ZENITH"] = 90.833 * pi / 180.0
    # at this point we actually need the LAT/LONG/TIMEZONE
    df_dates = pd.merge(df_stn_dates, df_dates, on=["DATE"])
    df_dates["TIMEOFFSET"] = df_dates.apply(
        lambda x: x["EQTIME"] + 4 * x["LONG"] - 60 * x["TIMEZONE"], axis=1
    )
    # FIX: is this some kind of approximation that can be wrong?
    #       breaks with (67.1520291504819, -132.37538245496188)
    df_dates["X_TMP"] = df_dates.apply(
        lambda x: cos(x["ZENITH"]) / (cos(x["LAT"] * pi / 180.0) * cos(x["DECL"]))
        - tan(x["LAT"] * pi / 180.0) * tan(x["DECL"]),
        axis=1,
    )
    # HACK: keep in range
    df_dates["X_TMP"] = df_dates["X_TMP"].apply(lambda x_tmp: max(-1, min(1, x_tmp)))
    df_dates["HALFDAY"] = df_dates["X_TMP"].apply(
        lambda x_tmp: 180.0 / pi * acos(x_tmp)
    )
    df_dates["SUNRISE"] = df_dates.apply(
        lambda x: (720.0 - 4.0 * (x["LONG"] + x["HALFDAY"]) - x["EQTIME"]) / 60
        + x["TIMEZONE"] + dst_adjust,
        axis=1,
    )
    df_dates["SUNSET"] = df_dates.apply(
        lambda x: (720.0 - 4.0 * (x["LONG"] - x["HALFDAY"]) - x["EQTIME"]) / 60
        + x["TIMEZONE"] + dst_adjust,
        axis=1,
    )
    df_all = pd.merge(df_copy, df_dates, on=COLS_ID)
    if get_solrad:
        df_all["TST"] = df_all.apply(
            lambda x: (x["TIMESTAMP"].hour - dst_adjust) * 60.0 + x["TIMEOFFSET"], axis=1
        )
        df_all["HOURANGLE"] = df_all.apply(lambda x: x["TST"] / 4 - 180, axis=1)
        df_all["ZENITH"] = df_all.apply(
            lambda x: acos(
                sin(x["LAT"] * pi / 180) * sin(x["DECL"])
                + cos(x["LAT"] * pi / 180)
                * cos(x["DECL"])
                * cos(x["HOURANGLE"] * pi / 180)
            ),
            axis=1,
        )
        ###########################################################################################
        ##################################### DMC-UPDATE ##########################################
        ## calculateing solar radiation using Hargraeves model suggested at:
        ## (https://github.com/derekvanderkampcfs/open_solar_model/tree/V1#conclusions)
        df_all["ZENITH"] = df_all["ZENITH"].apply(lambda zenith: min(pi / 2, zenith))
        # need later so keep column
        df_all["COS_ZENITH"] = df_all["ZENITH"].apply(cos)
        # Extraterrestrial solar radiation in kW/m^2
        df_all["SOLRAD_EXT"] = df_all["COS_ZENITH"] * 1.367
        # Daily total of Extra. Solar Rad in kJ/m^2/day
        df_solrad = df_all.groupby(COLS_ID)[["SOLRAD_EXT", "COS_ZENITH"]].agg("sum")
        df_solrad["SOLRAD_EXT"] *= 3600
        df_temp_range = df_all.groupby(COLS_ID)["TEMP"].agg(lambda x: max(x) - min(x))
        # not sure why it won't merge on groups
        df_solrad = pd.merge(df_solrad.reset_index(), df_temp_range.reset_index())
        df_solrad = df_solrad.rename(
            columns={
                "SOLRAD_EXT": "SOLRAD_EXT_SUM",
                "COS_ZENITH": "SUM_COS_ZENITH",
                "TEMP": "TEMP_RANGE",
            }
        )
        # Daily surface Solar Rad in kJ/m^2/day
        df_solrad["SOLRAD_DAY_SUM"] = df_solrad.apply(
            lambda x: 0.11 * x["SOLRAD_EXT_SUM"] * (x["TEMP_RANGE"] ** 0.59), axis=1
        )
        df_all = pd.merge(df_all, df_solrad, on=COLS_ID)
        # Hargreaves hourly surface solar rad in kW/m^2
        df_all["SOLRAD"] = df_all.apply(
            lambda x: x["COS_ZENITH"]
            / x["SUM_COS_ZENITH"]
            * x["SOLRAD_DAY_SUM"]
            / 3600,
            axis=1,
        )
        df_all["SOLRAD"] = df_all["SOLRAD"].apply(lambda x: max(x, 0))
        cols_sun = ["SOLRAD", "SUNRISE", "SUNSET"]
    else:
        cols_sun = ["SUNRISE", "SUNSET"]
    cols = list(df.columns) + cols_sun
    df_result = df_all.loc[:, cols]
    df_result["SUNLIGHT_HOURS"] = df_result.apply(
        lambda x: x["SUNSET"] - x["SUNRISE"], axis=1
    )
    # df_result = df_result.sort_values(COLS_ID + ["TIMESTAMP"])
    return df_result
  
  
  
    """
    old code
    
    # columns to use as unique ID
    COLS_ID = ["LAT", "LONG", "DATE", "TIMEZONE"]
    cols_req = COLS_ID + ["TIMESTAMP"]
    # just make date column so we know what type it is
    df_copy = df.loc[:]
    df_copy.loc[:, "DATE"] = df_copy["TIMESTAMP"].apply(lambda x: x.date())
    if get_solrad:
        cols_req += ["TEMP"]
    for n in cols_req:
        if n not in df_copy.columns:
            raise RuntimeError(f"Expected column '{n}' not found")
    # this routine approximately calcualtes sunrise and sunset and daylength
    # REally any routine like this could be used,  some are more precise than others.
    #
    # It takes in:
    # latitude:   in degrees north -  poistive number
    # longtitude: in degress EAST(standard)  - WEST hemisphere is a negative
    # month:
    # day:
    # adjust:  hours off of Greenich mean time (for EST = -5  (EDT=-4)   CST=-6 MST=-7  PST=-8)
    #
    # It returns (as pass by reference from the funciton call line)
    # SUNRISE in decimal hours  (in the local time zone specified)
    # SUNSET in decimal hours  (in the local time zone specified)
    #
    # and the function itself returns
    # DAYLENGTH (in hours)
    #
    #
    # bmw
    # FIX: need to work on data frame because we need all 24 hours?
    df_stn_dates = df_copy[COLS_ID].drop_duplicates()
    dec_hour = 12.0
    df_dates = df_stn_dates[["DATE"]].drop_duplicates()
    df_dates["JD"] = df_dates["DATE"].apply(lambda x: julian(x.month, x.day))
    df_dates["FRACYEAR"] = df_dates["JD"].apply(
        lambda jd: 2.0 * pi / 365.0 * (jd - 1.0 + (dec_hour - 12.0) / 24.0)
    )
    df_dates["EQTIME"] = df_dates["FRACYEAR"].apply(
        lambda fracyear: 229.18
        * (
            0.000075
            + 0.001868 * cos(fracyear)
            - 0.032077 * sin(fracyear)
            - 0.014615 * cos(2.0 * fracyear)
            - 0.040849 * sin(2.0 * fracyear)
        )
    )
    df_dates["DECL"] = df_dates["FRACYEAR"].apply(
        lambda fracyear: (
            0.006918
            - 0.399912 * cos(fracyear)
            + 0.070257 * sin(fracyear)
            - 0.006758 * cos(fracyear * 2.0)
            + 0.000907 * sin(2.0 * fracyear)
            - 0.002697 * cos(3.0 * fracyear)
            + 0.00148 * sin(3.0 * fracyear)
        )
    )
    df_dates["ZENITH"] = 90.833 * pi / 180.0
    # at this point we actually need the LAT/LONG/TIMEZONE
    df_dates = pd.merge(df_stn_dates, df_dates, on=["DATE"])
    df_dates["TIMEOFFSET"] = df_dates.apply(
        lambda x: x["EQTIME"] + 4 * x["LONG"] - 60 * x["TIMEZONE"], axis=1
    )
    # FIX: is this some kind of approximation that can be wrong?
    #       breaks with (67.1520291504819, -132.37538245496188)
    df_dates["X_TMP"] = df_dates.apply(
        lambda x: cos(x["ZENITH"]) / (cos(x["LAT"] * pi / 180.0) * cos(x["DECL"]))
        - tan(x["LAT"] * pi / 180.0) * tan(x["DECL"]),
        axis=1,
    )
    # HACK: keep in range
    df_dates["X_TMP"] = df_dates["X_TMP"].apply(lambda x_tmp: max(-1, min(1, x_tmp)))
    df_dates["HALFDAY"] = df_dates["X_TMP"].apply(
        lambda x_tmp: 180.0 / pi * acos(x_tmp)
    )
    df_dates["SUNRISE"] = df_dates.apply(
        lambda x: (720.0 - 4.0 * (x["LONG"] + x["HALFDAY"]) - x["EQTIME"]) / 60
        + x["TIMEZONE"],
        axis=1,
    )
    df_dates["SUNSET"] = df_dates.apply(
        lambda x: (720.0 - 4.0 * (x["LONG"] - x["HALFDAY"]) - x["EQTIME"]) / 60
        + x["TIMEZONE"],
        axis=1,
    )
    df_all = pd.merge(df_copy, df_dates, on=COLS_ID)
    if get_solrad:
        df_all["TST"] = df_all.apply(
            lambda x: x["TIMESTAMP"].hour * 60.0 + x["TIMEOFFSET"], axis=1
        )
        df_all["HOURANGLE"] = df_all.apply(lambda x: x["TST"] / 4 - 180, axis=1)
        df_all["ZENITH"] = df_all.apply(
            lambda x: acos(
                sin(x["LAT"] * pi / 180) * sin(x["DECL"])
                + cos(x["LAT"] * pi / 180)
                * cos(x["DECL"])
                * cos(x["HOURANGLE"] * pi / 180)
            ),
            axis=1,
        )
        ###########################################################################################
        ##################################### DMC-UPDATE ##########################################
        ## calculateing solar radiation using Hargraeves model suggested at:
        ## (https://github.com/derekvanderkampcfs/open_solar_model/tree/V1#conclusions)
        df_all["ZENITH"] = df_all["ZENITH"].apply(lambda zenith: min(pi / 2, zenith))
        # need later so keep column
        df_all["COS_ZENITH"] = df_all["ZENITH"].apply(cos)
        # Extraterrestrial solar radiation in kW/m^2
        df_all["SOLRAD_EXT"] = df_all["COS_ZENITH"] * 1.367
        # Daily total of Extra. Solar Rad in kJ/m^2/day
        df_solrad = df_all.groupby(COLS_ID)[["SOLRAD_EXT", "COS_ZENITH"]].agg("sum")
        df_solrad["SOLRAD_EXT"] *= 3600
        df_temp_range = df_all.groupby(COLS_ID)["TEMP"].agg(lambda x: max(x) - min(x))
        # not sure why it won't merge on groups
        df_solrad = pd.merge(df_solrad.reset_index(), df_temp_range.reset_index())
        df_solrad = df_solrad.rename(
            columns={
                "SOLRAD_EXT": "SOLRAD_EXT_SUM",
                "COS_ZENITH": "SUM_COS_ZENITH",
                "TEMP": "TEMP_RANGE",
            }
        )
        # Daily surface Solar Rad in kJ/m^2/day
        df_solrad["SOLRAD_DAY_SUM"] = df_solrad.apply(
            lambda x: 0.11 * x["SOLRAD_EXT_SUM"] * (x["TEMP_RANGE"] ** 0.59), axis=1
        )
        df_all = pd.merge(df_all, df_solrad, on=COLS_ID)
        # Hargreaves hourly surface solar rad in kW/m^2
        df_all["SOLRAD"] = df_all.apply(
            lambda x: x["COS_ZENITH"]
            / x["SUM_COS_ZENITH"]
            * x["SOLRAD_DAY_SUM"]
            / 3600,
            axis=1,
        )
        df_all["SOLRAD"] = df_all["SOLRAD"].apply(lambda x: max(x, 0))
    cols_sun = [x for x in ["SOLRAD", "SUNRISE", "SUNSET"] if x in df_all.columns]
    cols = list(df.columns) + cols_sun
    df_result = df_all.loc[:, cols]
    df_result["SUNLIGHT_HOURS"] = df_result.apply(
        lambda x: x["SUNSET"] - x["SUNRISE"], axis=1
    )
    # df_result = df_result.sort_values(COLS_ID + ["TIMESTAMP"])
    return df_result
    """


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
        96.0,
    ]
    jd_class = julian_date // 10
    first = PERCENT_CURED[jd_class]
    last = PERCENT_CURED[jd_class + 1]
    period_frac = (julian_date % 10) / 10.0
    return first + (last - first) * period_frac


def save_csv(df, file):
    COLS_ID = ["id","wstind"]
    COLS_LOC = ["lat", "long"]
    COLS_DATE = ["yr", "mon", "day", "hr", "peak_time", "duration"]
    COLS_RH = ["rh"]
    COLS_WX = ["temp", "ws", "wind_speed_smoothed"]
    COLS_PREC = ["prec"]
    COLS_SOLRAD = ["solrad"]
    COLS_INDICES = [
        "ffmc",
        "dmc",
        "dc",
        "isi",
        "bui",
        "fwi",
        "dsr",
        "gfmc",
        "gsi",
        "gfwi",
        "peak_isi_smoothed",
        "peak_gsi_smoothed"
    ]
    COLS_SUN_TIMES = ["sunrise", "sunset"]
    COLS_EXTRA = ["mcffmc", "mcgfmc"]
    COLS_GFL = ["grass_fuel_load"]
    COLS_PC = ["percent_cured"]
    cols_used = []
    result = df.copy()
    result.columns = map(str.lower, result.columns)

    def apply_format(cols, fmt, as_int=False):
        def fix_col(x):
            if as_int:
                x = int(x)
            # HACK: deal with negative 0
            return(re.sub("^-0\\.0*$", "0.0", fmt.format(x)))

        for col in result.columns:
            # HACK: deal with min/max columns
            col_root = col.replace("_min", "").replace("_max", "")
            if col_root in cols:
                cols_used.append(col)
                result[col] = result[col].apply(fix_col)

    apply_format(COLS_ID, "{}")
    apply_format(COLS_LOC, "{:.4f}")
    apply_format(COLS_DATE, "{:02d}", True)
    apply_format(COLS_RH, "{:.0f}")
    apply_format(COLS_WX, "{:.1f}")
    apply_format(COLS_PREC, "{:.2f}")
    apply_format(COLS_SOLRAD, "{:.4f}")
    apply_format(COLS_INDICES, "{:.1f}")
    apply_format(COLS_SUN_TIMES, "{}")
    apply_format(COLS_EXTRA, "{:.4f}")
    apply_format(COLS_GFL, "{:.2f}")
    apply_format(COLS_PC, "{:.1f}")
    result = result[[col for col in result.columns if col in cols_used]]
    result.to_csv(file, index=False)
    

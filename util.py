import datetime
import logging
from math import acos, cos, exp, log, pi, sin, tan

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
    month = [0, 31, 59, 90, 120, 151, 181, 212, 242, 273, 304, 334, 365]
    return month[int(mon) - 1] + int(day)


##
# Find solar radiation at a give time and place
#
# @param lat               Latitude (degrees)
# @param long              Longitude (degrees)
# @param mon               Month (1-12)
# @param day               Day of month
# @param hour              Hour of day
# @param timezone          Offset from GMT in hours
# @return                  Solar radiation (kW/m^2), sunrise, sunset
def sun(lat, lon, mon, day, hour, timezone):
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
    dechour = 12.0
    jd = julian(mon, day)
    fracyear = 2.0 * pi / 365.0 * (jd - 1.0 + (dechour - 12.0) / 24.0)
    eqtime = 229.18 * (
        0.000075
        + 0.001868 * cos(fracyear)
        - 0.032077 * sin(fracyear)
        - 0.014615 * cos(2.0 * fracyear)
        - 0.040849 * sin(2.0 * fracyear)
    )
    decl = (
        0.006918
        - 0.399912 * cos(fracyear)
        + 0.070257 * sin(fracyear)
        - 0.006758 * cos(fracyear * 2.0)
        + 0.000907 * sin(2.0 * fracyear)
        - 0.002697 * cos(3.0 * fracyear)
        + 0.00148 * sin(3.0 * fracyear)
    )
    timeoffset = eqtime + 4 * lon - 60 * timezone
    tst = hour * 60.0 + timeoffset
    hourangle = tst / 4 - 180
    zenith = acos(
        sin(lat * pi / 180) * sin(decl)
        + cos(lat * pi / 180) * cos(decl) * cos(hourangle * pi / 180)
    )
    solrad = 0.95 * cos(zenith)
    if solrad < 0:
        solrad = 0.0
    # print(" SOLAR: %d  %d fracyear=%f dec=%f  toff=%f  tst=%fha=%f zen=%f  solrad=%f".format(jd, hour, fracyear, decl,
    #                                                                                          timeoffset, tst,
    #                                                                                          hourangle, zenith,
    #                                                                                          solrad))
    zenith = 90.833 * pi / 180.0
    # FIX: is this some kind of approximation that can be wrong?
    #       breaks with (67.1520291504819, -132.37538245496188)
    x_tmp = cos(zenith) / (cos(lat * pi / 180.0) * cos(decl)) - tan(
        lat * pi / 180.0
    ) * tan(decl)
    # HACK: keep in range
    x_tmp = max(-1, min(1, x_tmp))
    halfday = 180.0 / pi * acos(x_tmp)
    sunrise = (720.0 - 4.0 * (lon + halfday) - eqtime) / 60 + timezone
    sunset = (720.0 - 4.0 * (lon - halfday) - eqtime) / 60 + timezone
    return solrad, sunrise, sunset


def save_csv(df, file):
    COLS_LOC = ["lat", "long"]
    COLS_DATE = ["yr", "mon", "day", "hr"]
    COLS_RH = ["rh"]
    COLS_WX = ["temp", "ws", "prec"]
    COLS_SOLRAD = ["solrad"]
    COLS_INDICES = ["ffmc", "dmc", "dc", "isi", "bui", "fwi", "gfmc", "gsi", "gfwi"]
    COLS_EXTRA = ["mcffmc", "mcgfmc"]
    COLS_GFL = ["grass_fuel_load"]
    COLS_PC = ["percent_cured"]
    cols_used = []
    result = df.copy()
    result.columns = map(str.lower, result.columns)
    print(df.columns)
    print(result.columns)

    def apply_format(cols, fmt, digits=0):
        def fix_col(x):
            return fmt.format(round(x, digits))

        for col in result.columns:
            # HACK: deal with min/max columns
            col_root = col.replace("_min", "").replace("_max", "")
            if col_root in cols:
                cols_used.append(col)
                result[col] = result[col].apply(fix_col)

    apply_format(COLS_LOC, "{:.4f}", 4)
    apply_format(COLS_DATE, "{:02d}")
    apply_format(COLS_RH, "{:.0f}")
    apply_format(COLS_WX, "{:.1f}", 1)
    apply_format(COLS_SOLRAD, "{:.4f}", 4)
    apply_format(COLS_INDICES, "{:.1f}", 1)
    apply_format(COLS_EXTRA, "{:.4f}", 4)
    apply_format(COLS_GFL, "{:.2f}", 2)
    apply_format(COLS_PC, "{:.1f}")
    result = result[[col for col in result.columns if col in cols_used]]
    result.to_csv(file, index=False)

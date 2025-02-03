import datetime
import logging
from math import acos, cos, exp, log, pi, sin, tan
import re
import numpy as np
import pandas as pd
import NG_FWI



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
# Find solar radiation for a data frame's times and places
#
# @param df                DataFrame to add columns to
# @param with_solrad       Whether to include solar radiation
# @return                  Solar radiation (kW/m^2), sunrise, sunset
def getSunlight(df, with_solrad=False, DST = False):
    dst_adjust = 0
    if (DST):
      dst_adjust = 1
  
  
    # columns to use as unique ID
    COLS_ID = ["LAT", "LONG", "DATE", "TIMEZONE"]
    cols_req = COLS_ID + ["TIMESTAMP"]
    # just make date column so we know what type it is
    df_copy = df.loc[:]
    df_copy.loc[:, "DATE"] = df_copy["TIMESTAMP"].apply(lambda x: x.date())
    if with_solrad:
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
    if with_solrad:
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
    cols_sun = [x for x in ["SOLRAD", "SUNRISE", "SUNSET"] if x in df_all.columns]
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
    if with_solrad:
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
    if with_solrad:
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
    COLS_LOC = ["lat", "long"]
    COLS_DATE = ["yr", "mon", "day", "hr"]
    COLS_RH = ["rh"]
    COLS_WX = ["temp", "ws"]
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
    ]
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

    apply_format(COLS_LOC, "{:.4f}")
    apply_format(COLS_DATE, "{:02d}", True)
    apply_format(COLS_RH, "{:.0f}")
    apply_format(COLS_WX, "{:.1f}")
    apply_format(COLS_PREC, "{:.2f}")
    apply_format(COLS_SOLRAD, "{:.4f}")
    apply_format(COLS_INDICES, "{:.1f}")
    apply_format(COLS_EXTRA, "{:.4f}")
    apply_format(COLS_GFL, "{:.2f}")
    apply_format(COLS_PC, "{:.1f}")
    result = result[[col for col in result.columns if col in cols_used]]
    result.to_csv(file, index=False)
    
    
def smooth_5pt(source):
  #binomial smoother  ... specifically for the 24 hour day
  #1pt = 1
  #3pt = (1 2 1) = 4
  #5pt = (1 4 6 4 1) = 16
  #7pt = (1 6 15 20 15 6 1) = 64


  cap = source.__len__()
  dest = [None]*cap
  

  dest[0] = source.iloc[0]
  dest[cap -1] = source.iloc[cap -1]
  
  miss = 0
  for i in range(0,2 +1):
    if source.iloc[i] < -90.0:
      miss = miss + 1
  if miss == 0:
    dest[1] = (0.25 * source.iloc[0]) + (0.5 * source.iloc[1]) + (0.25 * source.iloc[2])
  else:
    dest[1] = source.iloc[1]
  
  for i in range(2,(cap-3)+1):
    miss = 0
    for j in range((i-2),(i+2)+1):
      if source.iloc[j] < -90.0:
        miss = miss + 1
    if miss == 0:
      dest[i] = (1.0/16.0 * source.iloc[i - 2]) + (4.0/16.0 * source.iloc[i - 1]) + (6.0/16.0 * source.iloc[i]) + (4.0/16.0 * source.iloc[i + 1]) + (1.0/16.0 * source.iloc[i + 2])
    else:
      dest[i] = source.iloc[i]
  
  miss = 0
  for i in range((cap-3),(cap-1)+1):
    if source.iloc[i] < -90.0:
       miss - miss + 1
  if miss == 0:
    dest[cap-2] = (0.25 * source.iloc[cap - 3]) + (0.5 * source.iloc[cap - 2]) + (0.25 * source.iloc[cap-1])
  else:
    dest[cap-2] = source.iloc[cap-2]
  return dest
  

#this function calculates a fake date based on 5am-5am instead of midnight to midnight
#used for generating daily summaries to make data look at 5am-5am instead
#output form is "year-julian_day", with the julian day rounded back if the time is before 5am
#for Jan 1st where the julian day rollback would make it zero it gets bumped to the last day of the previous year
def pseudo_date(year, month, day, hour):
  
  adjusted_jd = None
  adjusted_year = None
  if (hour >= 5):
    adjusted_jd = julian(month, day)
  else:
    adjusted_jd = julian(month, day) - 1
  
  if adjusted_jd == 0:
    adjusted_year = year - 1
    adjusted_jd = julian(12,31)
  else:
    adjusted_year = year
    
  out = "{}-{}".format(adjusted_year, adjusted_jd)
  return out


def generate_daily_summaries(hourly_data):
  #note: need to account for spill over inlast day after pseudo_date calc where there is not 24 hours in the data
  
  Spread_Threshold_ISI = 5.0
  
  
  cols = ["wstind","year","mon","day","peak_time","duration","wind_speed_smoothed","peak_isi_smoothed","ffmc","dmc","dc","isi","bui","fwi","dsr","sunrise","sunset"]
  results = pd.DataFrame(columns=cols)
  
  for stn in hourly_data["id"].unique():
    #print(stn)
    by_stn = hourly_data.loc[hourly_data["id"] == stn]
    by_stn["pseudo_DATE"] =  by_stn.apply(
            lambda row: pseudo_date(
                row["yr"], row["mon"], row["day"], row["hr"]
            ),
            axis=1,
        )
    
    
    for p_date in by_stn["pseudo_DATE"].unique():
      by_date = by_stn.loc[by_stn["pseudo_DATE"] == p_date]
      peak_time_traditional_spot = (by_date.reset_index()).index[by_date["hr"] == 17].tolist()[0]
      peak_time = -1
      duration = 0
      wind_smooth = smooth_5pt(by_date["ws"])
      peak_isi_smooth = -1
      max_ffmc = 0
      dmc = 0
      dc = 0
      isi = 0
      bui = 0
      fwi = 0
      dsr = 0

      
      

      for i in range(0,by_date.shape[0]):
        smooth_isi = 0
        if (wind_smooth[i] > -90.0) and (by_date.iloc[i]["ffmc"] > -90.0):
          smooth_isi = NG_FWI.initial_spread_index(wind_smooth[i], by_date.iloc[i]["ffmc"])
        else:
          smooth_isi = -98.9
      
        if smooth_isi > peak_isi_smooth:
          peak_time = i
          peak_isi_smooth = smooth_isi
        if by_date.iloc[i]["ffmc"] > max_ffmc:
          max_ffmc = by_date.iloc[i]["ffmc"]
        if smooth_isi > Spread_Threshold_ISI:
          duration = duration + 1
       
      if (smooth_isi < 5) and (duration == 24):
         duration = 0
      
      if max_ffmc < 85.0:
        peak_time = peak_time_traditional_spot
    
      max_ffmc = by_date.iloc[peak_time]["ffmc"]
      dmc = by_date.iloc[peak_time]["dmc"]
      dc = by_date.iloc[peak_time]["dc"]
      isi = by_date.iloc[peak_time]["isi"]
      bui = by_date.iloc[peak_time]["bui"]
      fwi = by_date.iloc[peak_time]["fwi"]
      dsr = by_date.iloc[peak_time]["dsr"]
      smooth_ws_peak = wind_smooth[peak_time]
      
      pick_year = by_date["yr"].unique()
      if pick_year.shape[0] > 1:
        pick_year = pick_year[0]
      else:
         pick_year = pick_year[0]
      pick_month = by_date["mon"].unique()
      if pick_month.shape[0] > 1:
        pick_month = pick_month[0]
      else:
         pick_month = pick_month[0]
      pick_day = by_date["day"].unique()
      if pick_day.shape[0] > 1:
        pick_day = pick_day[0]
      else:
        pick_day = pick_day[0]

      sunrise_val = by_date.iloc[peak_time]["sunrise"]
      sunset_val = by_date.iloc[peak_time]["sunset"]
    
      peak_time = by_date.iloc[peak_time]["hr"]

      sunrise_formated = "{}:{}".format(int(sunrise_val),int(60*(sunrise_val-int(sunrise_val))))
      sunset_formated = "{}:{}".format(int(sunset_val),int(60*(sunset_val-int(sunset_val))))


      daily_report_line = [by_date["id"].unique()[0], pick_year, pick_month, pick_day, peak_time, duration, smooth_ws_peak, peak_isi_smooth, max_ffmc, dmc,  dc, isi, bui, fwi, dsr, sunrise_formated, sunset_formated]
      
      daily_report = pd.DataFrame(columns=cols)
      daily_report.loc[0] = daily_report_line

      
      results = pd.concat([results,daily_report])

  
  results = results[["wstind","year","mon","day","peak_time","duration","wind_speed_smoothed","peak_isi_smoothed","ffmc","dmc","dc","isi","bui","fwi","dsr","sunrise","sunset"]]

  return results
  
  


































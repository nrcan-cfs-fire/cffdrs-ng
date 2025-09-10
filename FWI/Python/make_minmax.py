import pandas as pd

import util
from util import save_csv


# This function is for the method that takes old traditional 1pm weather (labelled here as noon)
# and estimates a diurnal temperature range to fit into the scheme that flows into the max/min method
# Temp input in Celsius   RH input in Percent.   These should be that traditional 1pm values
# Written as a function to enable upgrading later if needs be
def temp_min_max(temp_noon, rh_noon):
    temp_range = 17 - 0.16 * rh_noon + 0.22 * temp_noon
    if temp_range <= 2:
        temp_max = temp_noon + 1
        temp_min = temp_noon - 1
    else:
        temp_max = temp_noon + 2
        temp_min = temp_max - temp_range
    return [temp_min, temp_max]


##
# Convert daily noon values stream to daily min/max values stream.
# Uses values from statistics to do the conversion.
#
# @param df        daily noon values weather stream [lat, long, year, mon, day, hour, temp, rh, wind, rain]
# @return          daily min/max values weather stream [lat, long, year, mon, day, hour, temp_min, temp_max, rh_min, rh_max, ws_min, ws_max, rain]
def daily_to_minmax(df):
    df = df.copy()
    had_id = "id" in map(str.lower, df.columns)
    df.loc[:, ["temp_min", "temp_max"]] = df.apply(
        lambda row: pd.Series(
            data=temp_min_max(row["temp"], row["rh"]), index=["temp_min", "temp_max"]
        ),
        axis=1,
    )
    df["q"] = df.apply(lambda row: util.find_q(row["temp"], row["rh"]), axis=1)
    df["rh_min"] = df.apply(lambda row: util.find_rh(row["q"], row["temp_max"]), axis=1)
    df["rh_min"] = df["rh_min"].apply(lambda rh: min(100, max(0, rh)))
    df["rh_max"] = df.apply(lambda row: util.find_rh(row["q"], row["temp_min"]), axis=1)
    df["rh_max"] = df["rh_max"].apply(lambda rh: min(100, max(0, rh)))
    df["ws_min"] = 0.15 * df["ws"]
    df["ws_max"] = 1.25 * df["ws"]
    columns = [
        "lat",
        "long",
        "yr",
        "mon",
        "day",
        "temp_min",
        "temp_max",
        "rh_min",
        "rh_max",
        "ws_min",
        "ws_max",
        "prec",
    ]
    if had_id:
        df = df[["id"] + columns]
    else:
        df = df[columns]
    return df


if "__main__" == __name__:
    import os
    import sys

    if 3 != len(sys.argv):
        print(f"Command line:   {sys.argv[0]} <input file> <output file>\n")
        print(
            "INPUT FILE format must be DAILY weather data, comma seperated and take the form"
        )
        print(
            "Latitude,Longitude,YEAR,MONTH,DAY,HOUR,Temperature(C),Relative_humidity(%%),Wind_speed(km/h),Rainfall(mm)\n"
        )
        print("All times should be local standard time")
        sys.exit(1)
    inp = sys.argv[1]
    out = sys.argv[2]
    print(f"Opening input file >> {inp}")
    if not os.path.exists(inp):
        print(f"\n\n ***** FILE  {inp}  does not exist")
        sys.exit(1)
    df = pd.read_csv(inp)
    COLUMNS = [
        "lat",
        "long",
        "yr",
        "mon",
        "day",
        "temp",
        "rh",
        "ws",
        "prec",
    ]
    try:
        df = df[COLUMNS]
    except:
        print(f"Expected columns to be {COLUMNS}")
        sys.exit(1)
    minmax = daily_to_minmax(df)
    save_csv(minmax, out)

import pandas as pd
import datetime


##
# Convert hourly values stream to daily noon values stream.
#
# @param   df    hourly values weather stream [lat, long, year, mon, day, hour, temp, rh, wind, rain]
# @return        daily noon values weather stream [lat, long, year, mon, day, hour, temp, rh, wind, rain]
def hourly_to_daily(df):
    df = df.copy()
    df["DATE"] = df.apply(
        lambda row: f'{int(row["year"]):4d}-{int(row["mon"]):02d}-{int(row["day"]):02d}',
        axis=1,
    )
    df["FOR_DATE"] = df.apply(
        lambda row: (
            pd.to_datetime(row["DATE"])
            if row["hour"] <= 12
            else (pd.to_datetime(row["DATE"]) + datetime.timedelta(days=1))
        ).date(),
        axis=1,
    )
    rain = df.groupby("FOR_DATE")["rain"].sum()
    del df["rain"]
    df = df.loc[df["hour"] == 12]
    df = pd.merge(df, pd.DataFrame({"rain": rain}), on=["FOR_DATE"])
    return df


if "__main__" == __file__:
    import sys
    import os

    if 3 != len(sys.argv):
        print(f"Command line:   {sys.argv[0]} <input file> <output file>\n")
        print(
            "INPUT FILE format must be HOURLY weather data, comma seperated and take the form"
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
        "year",
        "mon",
        "day",
        "hour",
        "temp",
        "rh",
        "wind",
        "rain",
    ]
    try:
        df = df[COLUMNS]
    except:
        print(f"Expected columns to be {COLUMNS}")
        sys.exit(1)
    daily = hourly_to_daily(df)
    daily.to_csv(out, index=False)

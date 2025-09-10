import datetime
from math import acos, cos, exp, log, pi, sin, tan

import pandas as pd

import util
from util import save_csv

C_TEMP = {"c_alpha": 0.2, "c_beta": 2.0, "c_gamma": -2.9}
C_RH = {"c_alpha": 0.4, "c_beta": 1.9, "c_gamma": -2.9}
C_WIND = {"c_alpha": 1.2, "c_beta": 1.7, "c_gamma": -1.5}


def make_prediction(
    fcsts,
    c_alpha,
    c_beta,
    c_gamma,
    v="TEMP",
    change_at="SUNSET",
    min_value=float("-inf"),
    max_value=float("inf"),
    intervals=1,
    verbose=False,
):
    if verbose:
        print(f"Predicting {v} changing at {change_at}")
    fcsts = fcsts.copy()
    var_min = f"{v}_MIN"
    var_max = f"{v}_MAX"
    fcsts["TIME_MIN"] = fcsts["SUNRISE"] + c_alpha
    fcsts["TIME_MAX"] = fcsts["SOLARNOON"] + c_beta
    fcsts["VAR_MIN_TOM"] = pd.concat(
        [fcsts[var_min][1:], pd.Series([fcsts[var_min].iloc[-1]])]
    ).values
    # FIX: simplify in R code too
    # FIX: R code seems to use NA instead of a value at the end?
    fcsts["TIME_MIN_TOM"] = pd.concat(
        [fcsts["TIME_MIN"][1:], pd.Series([fcsts["TIME_MIN"].iloc[-1]])]
    ).values
    if 1 != len(fcsts["ID"].unique()):
        raise RuntimeError("Expected a single unique ID")
    hours = pd.DataFrame({"HR": range(24)})
    cross = pd.merge(fcsts[["DATE"]], hours, how="cross")
    minutes = pd.DataFrame({"MINUTE": range(0, 60, 60 // intervals)})
    cross = pd.merge(cross, minutes, how="cross")
    cross["ID"] = fcsts["ID"].iloc[0]
    cross["TIME"] = cross.apply(lambda row: row["HR"] + row["MINUTE"] / 60.0, axis=1)
    for column in ["HR", "MINUTE", "TIME", "MON", "YR"]:
        if column in fcsts.columns:
            del fcsts[column]
    cross = pd.merge(cross, fcsts, on=["ID", "DATE"])
    cross["START_DATE"] = cross.apply(
        lambda row: (pd.to_datetime(row["DATE"]) - datetime.timedelta(days=1)).strftime(
            "%Y-%m-%d"
        )
        if row["TIME"] <= row["TIME_MIN"]
        else row["DATE"],
        axis=1,
    )
    cross["HOUR_CURVE"] = cross.apply(
        lambda row: (row["TIME"] + 24)
        if row["TIME"] <= row["TIME_MIN"]
        else row["TIME"],
        axis=1,
    )
    cross["IS_RISING"] = cross.apply(
        lambda row: False
        if row["TIME"] <= row["TIME_MIN"]
        else row["TIME"] < row[change_at],
        axis=1,
    )
    cross["TIME_G_MIN"] = cross.apply(
        lambda row: row["TIME_MIN"]
        if row["TIME"] <= row["TIME_MIN"]
        else row["TIME_MIN_TOM"],
        axis=1,
    )
    cross["VAR_G_MIN"] = cross.apply(
        lambda row: row[var_min]
        if row["TIME"] <= row["TIME_MIN"]
        else row["VAR_MIN_TOM"],
        axis=1,
    )
    rising = cross[cross["IS_RISING"] == True][:]
    falling = cross[cross["IS_RISING"] == False][:]
    # figure out values before change_at
    rising["F_OR_G"] = rising.apply(
        lambda row: (row["HOUR_CURVE"] - row["TIME_MIN"])
        / (row["TIME_MAX"] - row["TIME_MIN"]),
        axis=1,
    )
    rising[v] = rising.apply(
        lambda row: (
            row[var_min] + (row[var_max] - row[var_min]) * sin((pi / 2) * row["F_OR_G"])
        ),
        axis=1,
    )
    rising["VAR_CHANGE"] = rising.apply(
        lambda row: (
            row[var_min]
            + (row[var_max] - row[var_min])
            * sin(
                (pi / 2)
                * (
                    (row[change_at] - row["TIME_MIN"])
                    / (row["TIME_MAX"] - row["TIME_MIN"])
                )
            )
        ),
        axis=1,
    )
    # need to figure out var value at change_at
    tmp = rising[["START_DATE", "VAR_CHANGE"]].drop_duplicates()
    # now merge change_at values into falling
    falling = pd.merge(falling, tmp, on=["START_DATE"])
    falling["F_OR_G"] = falling.apply(
        lambda row: (row["HOUR_CURVE"] - row[change_at])
        / (24 - row[change_at] + row["TIME_G_MIN"]),
        axis=1,
    )
    falling[v] = falling.apply(
        lambda row: row["VAR_G_MIN"]
        + (row["VAR_CHANGE"] - row["VAR_G_MIN"]) * exp(c_gamma * row["F_OR_G"]),
        axis=1,
    )
    # combine and sort everything
    out = pd.concat([rising, falling])
    out = out.sort_values(["DATE", "HR"])
    out["TIMESTAMP"] = out.apply(
        lambda row: pd.to_datetime(f'{row["DATE"]} {row["HR"]}:{row["MINUTE"]}:00'),
        axis=1,
    )
    out[v] = out[v].apply(lambda x: min(max(x, min_value), max_value))
    return out


def do_prediction(fcsts, row_temp, row_wind, row_rh, intervals=1, verbose=False):
    if verbose:
        print("Doing prediction")
    v_temp = make_prediction(
        fcsts,
        row_temp["c_alpha"],
        row_temp["c_beta"],
        row_temp["c_gamma"],
        "TEMP",
        "SUNSET",
        intervals=intervals,
        verbose=verbose,
    )
    v_wind = make_prediction(
        fcsts,
        row_wind["c_alpha"],
        row_wind["c_beta"],
        row_wind["c_gamma"],
        "WS",
        "SUNSET",
        min_value=0,
        intervals=intervals,
        verbose=verbose,
    )
    # FIX: R version only looks at HOUR so wouldn't work with intervals != 1
    t = v_temp[["ID", "TIMESTAMP", "DATE", "HR", "MINUTE", "LAT", "LONG", "TEMP"]]
    w = v_wind[["ID", "TIMESTAMP", "WS"]]
    out = pd.merge(t, w)
    rh = make_prediction(
        fcsts,
        row_rh["c_alpha"],
        row_rh["c_beta"],
        row_rh["c_gamma"],
        "RH_OPP",
        intervals=intervals,
        min_value=0,
        max_value=1,
        verbose=verbose,
    )
    rh["RH"] = rh["RH_OPP"].apply(lambda rh_opp: 100 * (1 - rh_opp))
    rh = rh[["ID", "TIMESTAMP", "RH"]]
    out = pd.merge(out, rh)
    output = out[["ID", "LAT", "LONG", "TIMESTAMP", "TEMP", "WS", "RH"]][:]
    if verbose:
        print("Assigning times")
    output["HR"] = output["TIMESTAMP"].apply(lambda x: x.hour)
    output["MINUTE"] = output["TIMESTAMP"].apply(lambda x: x.minute)
    if verbose:
        print("Converting date")
    output["DATE"] = output["TIMESTAMP"].apply(lambda x: x.date().strftime("%Y-%m-%d"))
    if verbose:
        print("Allocating rain")
    rain = fcsts[["DATE", "PREC"]][:]
    rain["HR"] = 7
    rain["MINUTE"] = 0
    if verbose:
        print("Merging")
    cmp = pd.merge(output, rain, how="outer")
    cmp["PREC"] = cmp["PREC"].fillna(0)
    if verbose:
        print("Calculating times")
    cmp["YR"] = cmp["TIMESTAMP"].apply(lambda x: x.year)
    cmp["MON"] = cmp["TIMESTAMP"].apply(lambda x: x.month)
    cmp["DAY"] = cmp["TIMESTAMP"].apply(lambda x: x.day)
    cmp["TIME"] = cmp.apply(lambda row: row["HR"] + row["MINUTE"] / 60.0, axis=1)
    if verbose:
        print("Done prediction")
    return cmp


##
# Convert daily min/max values stream to hourly values stream.
# Uses Beck & Trevitt method with default A/B/G values.
#
# @param     w              daily min/max values weather stream [lat, long, year, mon, day, hour, temp_min, temp_max, rh_min, rh_max, ws_min, ws_max, rain]
# @param     timezone       integer offset from GMT to use for sun calculations
# @param     skip_invalid   whether to continue if invalid data is found or throw an exception
# @param     verbose        whether to output progress messages
# @return                   hourly values weather stream [lat, long, year, mon, day, hour, temp, rh, wind, rain]
def minmax_to_hourly_single(w, timezone, skip_invalid=False, verbose=False):
    r = w.copy()
    r.columns = map(str.upper, r.columns)
    if 1 != len(r["LAT"].unique()):
        raise RuntimeError("Expected a single LAT value for input weather")
    if 1 != len(r["LONG"].unique()):
        raise RuntimeError("Expected a single LONG value for input weather")
    had_id = "ID" in r.columns
    if not had_id:
        r["ID"] = 1
    elif 1 != len(r["ID"].unique()):
        raise RuntimeError("Expected a single ID value for input weather")
    if 1 != len(r["YR"].unique()):
        raise RuntimeError("Expected a single YEAR value for input weather")
    r["HR"] = 12
    r["TIMESTAMP"] = r.apply(
        lambda row: datetime.datetime(
            int(row["YR"]), int(row["MON"]), int(row["DAY"]), int(row["HR"])
        ),
        axis=1,
    )
    if not (len(r) > 1 and util.is_sequential_days(r)):
        if skip_invalid:
            raise RuntimeWarning(
                f'{r["ID"].iloc[0]} for {r["YR"].iloc[0]} - Expected input to be sequential daily weather'
            )
        return None
    orig_dates = pd.DataFrame(
        {
            "date": map(
                lambda x: x.strftime("%Y-%m-%d"),
                r["TIMESTAMP"].apply(lambda x: x.date()).unique(),
            )
        }
    )
    # duplicate start and end dates so we can use their values for yesterday and tomorrow in predictions
    yest = r.iloc[0][:]
    tom = r.iloc[-1][:]
    yest["TIMESTAMP"] = yest["TIMESTAMP"] - datetime.timedelta(days=1)
    tom["TIMESTAMP"] = tom["TIMESTAMP"] + datetime.timedelta(days=1)
    r = pd.concat([pd.DataFrame(data=[yest]), r, pd.DataFrame(data=[tom])])
    r["DATE"] = r["TIMESTAMP"].apply(lambda x: x.date())
    r["YR"] = r["TIMESTAMP"].apply(lambda x: x.year)
    r["MON"] = r["TIMESTAMP"].apply(lambda x: x.month)
    r["DAY"] = r["TIMESTAMP"].apply(lambda x: x.day)
    r["HR"] = r["TIMESTAMP"].apply(lambda x: x.hour)
    dates = r["TIMESTAMP"].unique()
    latitude = r["LAT"].iloc[0]
    longitude = r["LONG"].iloc[0]
    r["TIMEZONE"] = timezone
    r = util.get_sunlight(r, with_solrad=False)
    # FIX: is solar noon just midpoint between sunrise and sunset?
    r["SOLARNOON"] = r.apply(
        lambda row: (row["SUNSET"] - row["SUNRISE"]) / 2 + row["SUNRISE"], axis=1
    )
    r["RH_OPP_MIN"] = 1 - r["RH_MAX"] / 100
    r["RH_OPP_MAX"] = 1 - r["RH_MIN"] / 100
    r["DATE"] = r["DATE"].apply(lambda x: x.strftime("%Y-%m-%d"))
    df = do_prediction(
        r, row_temp=C_TEMP, row_wind=C_WIND, row_rh=C_RH, verbose=verbose
    )
    df.columns = map(str.lower, df.columns)
    df = pd.merge(orig_dates, df, on=["date"])
    columns = [
        "lat",
        "long",
        "yr",
        "mon",
        "day",
        "hr",
        "temp",
        "rh",
        "ws",
        "prec",
    ]
    if had_id:
        df = df[["id"] + columns]
    else:
        df = df[columns]
    return df


# Convert daily min/max values stream to hourly values stream.
# Uses Beck & Trevitt method with default A/B/G values.
#
# @param     w              daily min/max values weather stream [lat, long, year, mon, day, hour, temp_min, temp_max, rh_min, rh_max, ws_min, ws_max, rain]
# @param     timezone       integer offset from GMT to use for sun calculations
# @param     skip_invalid   whether to continue if invalid data is found or throw an exception
# @param     verbose        whether to output progress messages
# @return                   hourly values weather stream [lat, long, year, mon, day, hour, temp, rh, wind, rain]
def minmax_to_hourly(w, timezone, skip_invalid=False, verbose=False):
    r = w.copy()
    r.columns = map(str.upper, r.columns)
    had_id = "ID" in r.columns
    if not had_id:
        r["ID"] = 1
    result = None
    for stn in r["ID"].unique():
        by_stn = r[r["ID"] == stn]
        for yr in by_stn["YR"].unique():
            by_year = by_stn[by_stn["YR"] == yr]
            print(f"Running {stn} for {yr}")
            df = minmax_to_hourly_single(by_year, timezone, skip_invalid, verbose)
            result = pd.concat([result, df])
    if not had_id:
        del result["id"]
    return result


if "__main__" == __name__:
    import os
    import sys

    COLUMNS = [
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
    if 4 != len(sys.argv):
        print(
            f"Command line:   {sys.argv[0]} <local GMToffset> <input file> <output file>\n"
        )
        print(
            "<local GMToffset> is the off of Greenich mean time (for Eastern = -5  Central=-6   MT=-7  PT=-8 )"
        )
        print(
            "INPUT FILE format must be DAILY min/max weather data, comma seperated and take the form"
        )
        print(f"{COLUMNS}\n")
        sys.exit(1)
    inp = sys.argv[2]
    out = sys.argv[3]
    print(f"Opening input file >> {inp}")
    if not os.path.exists(inp):
        print(f"\n\n ***** FILE  {inp}  does not exist")
        sys.exit(1)
    TZadjust = int(sys.argv[1])
    if TZadjust < -9 or TZadjust > -2:
        print(
            "/n *****   Local time zone adjustment must be vaguely in Canada so between -9 and -2"
        )
        sys.exit(1)
    df = pd.read_csv(inp)
    try:
        df = df[COLUMNS]
    except:
        print(f"Expected columns to be {COLUMNS}")
        sys.exit(1)
    hourly = minmax_to_hourly(df, TZadjust)
    save_csv(hourly, out)

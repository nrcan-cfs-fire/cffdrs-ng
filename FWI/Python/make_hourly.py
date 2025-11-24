import datetime
import argparse
from math import exp, pi, sin

import pandas as pd

import util

C_TEMP = {"c_alpha": 0.0, "c_beta": 2.75, "c_gamma": -1.9}
C_RH = {"c_alpha": 0.25, "c_beta": 2.75, "c_gamma": -2.0}
C_WIND = {"c_alpha": 1.0, "c_beta": 1.5, "c_gamma": -1.3}


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
    t = v_temp[["ID", "TIMESTAMP", "DATE", "HR", "MINUTE", "LAT", "LONG", "TIMEZONE", "TEMP"]]
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
    output = out[["ID", "LAT", "LONG", "TIMEZONE", "TIMESTAMP", "TEMP", "WS", "RH"]][:]
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
# @param     w              daily min/max values weather stream [lat, long, timezone, yr, mon, day, temp_min, temp_max, rh_min, rh_max, ws_min, ws_max, prec]
# @param     skip_invalid   if station year data non-sequential, skip and raise warning
# @param     verbose        whether to output progress messages
# @return                   hourly values weather stream [lat, long, timezone, yr, mon, day, hr, temp, rh, wind, prec]
def minmax_to_hourly_single(w, skip_invalid=False, verbose=False):
    r = w.copy()
    r.columns = map(str.upper, r.columns)
    if 1 != len(r["LAT"].unique()):
        raise RuntimeError("Expected a single LAT value for input weather")
    if 1 != len(r["LONG"].unique()):
        raise RuntimeError("Expected a single LONG value for input weather")
    if len(w["TIMEZONE"].unique()) != 1:
        raise RuntimeError("Expected a single UTC offset (timezone) each station year")
    had_id = "ID" in r.columns
    if not had_id:
        r["ID"] = "STN"
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
    if not (util.is_sequential_days(r)):
        if skip_invalid:
            raise RuntimeWarning(
                f'{r["ID"].iloc[0]} for {r["YR"].iloc[0]}' +
                    ' - Expected input to be sequential daily weather'
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
    # duplicate start and end days to assume their values for one day before and after dataset
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
    r = util.get_sunlight(r, get_solrad = False)
    r.columns = map(str.upper, r.columns)  # get_sunlight outputs lowercase columns
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
    cols = ["lat", "long", "timezone", "yr", "mon", "day", "hr", "temp", "rh", "ws", "prec"]
    if had_id:
        df = df[["id"] + cols]
    else:
        df = df[cols]
    return df


# Convert daily min/max values stream to hourly values stream.
# Uses Beck & Trevitt method with default A/B/G values.
#
# @param    w               daily min/max values weather stream [lat, long, yr, mon, day temp_min, temp_max, rh_min, rh_max, ws_min, ws_max, prec]
# @param    timezone        UTC offset (default None for column provided in w)
# @param    skip_invalid    if station year data non-sequential, skip and raise warning
# @param    verbose         whether to output progress messages
# @param    round_out       decimals to truncate output to, None for none (default 4)
# @return                   hourly values weather stream [lat, long, timezone, yr, mon, day, hr, temp, rh, wind, prec]
def minmax_to_hourly(w, timezone = None, skip_invalid = False,
                     verbose = False, round_out = 4):
    r = w.copy()
    # check for required columns
    r.columns = map(str.upper, r.columns)
    req_cols = ["LAT", "LONG", "YR", "MON", "DAY", "TEMP_MIN", "TEMP_MAX",
                "RH_MIN", "RH_MAX", "WS_MIN", "WS_MAX", "PREC"]
    for col in req_cols:
        if not col in r.columns:
            raise RuntimeError("Missing required input column: " + col)
    # check for ID column
    had_id = "ID" in r.columns
    if not had_id:
        r["ID"] = "STN"
    # check timezone
    if timezone == None:
        if not "TIMEZONE" in r.columns:
            raise RuntimeError("Either provide a timezone column or specify argument in minmax_to_hourly")
    else:
        r["TIMEZONE"] = float(timezone)
    # loop over every station year
    result = None
    for stn in r["ID"].unique():
        by_stn = r[r["ID"] == stn]
        for yr in by_stn["YR"].unique():
            by_year = by_stn[by_stn["YR"] == yr]
            print(f"Running {stn} for {yr}")
            df = minmax_to_hourly_single(by_year, skip_invalid, verbose)
            result = pd.concat([result, df])

    # delete ID column if it wasn't provided
    if not had_id:
        del result["id"]

    # round decimal places of output columns
    if not (round_out == None or round_out == "None"):
        outcols = ["temp", "rh", "ws"]
        result[outcols] = result[outcols].map(round, ndigits = int(round_out))
    
    return result

if __name__ == "__main__":
    # run minmax_to_hourly by command line. run with option -h or --help to see usage
    parser = argparse.ArgumentParser(prog = "make_hourly")

    parser.add_argument("input", help = "Input csv data file")
    parser.add_argument("output", help = "Output csv file name and location")
    parser.add_argument("timezone", nargs = "?", default = None,
        help = "UTC offset (default None for column provided in input)")
    parser.add_argument("skip_invalid", nargs = "?", default = False,
        help = "If station year data non-sequential, skip and raise warning")
    parser.add_argument("-v", "--verbose", action = "store_true")
    parser.add_argument("-r", "--round_out", default = 4, nargs = "?",
        help = "Decimal places to truncate outputs to, None for no rounding (default 4)")

    args = parser.parse_args()
    df_in = pd.read_csv(args.input)
    df_out = minmax_to_hourly(df_in, args.timezone, args.skip_invalid,
                              args.verbose, args.round_out)
    df_out.to_csv(args.output, index = False)

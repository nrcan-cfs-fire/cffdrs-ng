import argparse
import pandas as pd

import util

##
# Convert daily temperature at 1pm (noon standard time) to daily min/max
#
# @param    temp_noon   traditional temperature measurement [°C]
# @param    rh_noon     traditional relative humidity measurement [%]
# @return               list of two values [min temperature, max temperature]
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
# Convert daily noon weather to daily min/max weather using statistical values
#
# @param    df              daily noon weather stream [lat, long, yr, mon, day, temp, rh, ws, prec]
# @param    round_out       decimals to truncate output to, None for none (default 4)
# @return                   daily min/max weather stream [lat, long, yr, mon, day, temp_min, temp_max, rh_min, rh_max, ws_min, ws_max, prec]
def daily_to_minmax(df, round_out = 4):
    df = df.copy()
    # check for required columns
    df.columns = map(str.lower, df.columns)
    req_cols = ["yr", "mon", "day", "temp", "rh", "ws", "prec"]
    for col in req_cols:
        if not col in df.columns:
            raise RuntimeError("Missing required input column: " + col)
    
    df.loc[:, ["temp_min", "temp_max"]] = df.apply(
        lambda row: pd.Series(
            data = temp_min_max(row["temp"], row["rh"]), index = ["temp_min", "temp_max"]
        ), axis = 1
    )
    df["q"] = df.apply(lambda row: util.find_q(row["temp"], row["rh"]), axis = 1)
    # ideally maximum temperature lines up with minimum relative humidity and vice versa
    df["rh_min"] = df.apply(lambda row: util.find_rh(row["q"], row["temp_max"]), axis = 1)
    df["rh_min"] = df["rh_min"].apply(lambda rh: min(100, max(0, rh)))
    df["rh_max"] = df.apply(lambda row: util.find_rh(row["q"], row["temp_min"]), axis = 1)
    df["rh_max"] = df["rh_max"].apply(lambda rh: min(100, max(0, rh)))
    df["ws_min"] = 0.15 * df["ws"]
    df["ws_max"] = 1.25 * df["ws"]
    
    # drop calculation columns and move prec column to the end (right-most column)
    df = df.drop(columns = ["temp", "rh", "ws", "q"])
    df.insert(df.shape[1] - 1, "prec", df.pop("prec"))

    # round decimal places of output columns
    if not (round_out == None or round_out == "None"):
        outcols = ["temp_min", "temp_max", "rh_min", "rh_max", "ws_min", "ws_max"]
        df[outcols] = df[outcols].map(round, ndigits = int(round_out))

    return df

if __name__ == "__main__":
    # run daily_to_minmax by command line. run with option -h or --help to see usage
    parser = argparse.ArgumentParser(prog = "make_minmax")

    parser.add_argument("input", help = "Input csv data file")
    parser.add_argument("output", help = "Output csv file name and location")
    parser.add_argument("-r", "--round_out", default = 4, nargs = "?",
        help = "Decimal places to truncate outputs to, None for no rounding (default 4)")

    args = parser.parse_args()
    df_in = pd.read_csv(args.input)
    df_out = daily_to_minmax(df_in, args.round_out)
    df_out.to_csv(args.output, index = False)

# Compute daily minimum and maximum (minmax) weather from traditional
# daily values (13:00 Local Daylight Time or 12:00 Local Standard Time)


### Import packages ###########################################################

import argparse
from math import exp
import pandas as pd
import util


### Functions #################################################################

##
# Convert daily temperature at 13:00 LDT or 12:00 LST to daily minmax
#
# @param    temp_day    Daily temperature at 13:00 LDT or 12:00 LST [°C]
# @param    rh_day      Daily relative humidity at 13:00 LDT or 12:00 LST [%]
# @return               List of: [minimum temperature, maximum temperature]
def temp_min_max(temp_day, rh_day):
    temp_range = 0.22*temp_day - 0.16*rh_day + 17
    if temp_range <= 2:
        temp_max = temp_day + 1
        temp_min = temp_day - 1
    else:
        temp_max = temp_day + 2
        temp_min = temp_max - temp_range
    return [temp_min, temp_max]

##
# Find specific humidity, the mass ratio of water vapour (g) to all air (kg)
#
# @param temp        Temperature (°C)
# @param rh          Relative humidity (%)
# @return            Specific humidity (g/kg)
def find_q(temp, rh):
    svp = 6.108 * exp(17.27*temp/(temp+237.3))
    vp = svp * rh / 100
    q = 217 * vp / (273.17+temp)
    return q

##
# Find relative humidity
#
#  @param q           Specific humidity (g/kg)
#  @param temp        Temperature (°C)
#  @return            Relative humidity (%)
def find_rh(q, temp):
    cur_vp = (273.17+temp) * q / 217
    rh = 100 * cur_vp / (6.108*exp(17.27*temp/(temp+237.3)))
    return rh

##
# Convert daily weather at 13:00 LDT or 12:00 LST to daily minmax statistically
#
# @param    df_wx_day   Daily weather at 13:00 LDT or 12:00 LST df, columns:
#                           yr, mon, day, temp, rh, ws, prec
# @param    silent      Suppresses informative print statements (default False)
# @param    round_out   Decimals to truncate output to, or None (default 4)
# @return               Daily minmax weather, columns:
#                           yr, mon, day, temp_min, temp_max, rh_min, rh_max,
#                           ws_min, ws_max, prec
def daily_to_minmax(df_wx_day, silent = False, round_out = 4):
    if not silent:
        print("\n########")
        print("FWI2025: Make Min/Max Inputs (" + util.version() + ")\n")
        print("Predicting daily min/max weather")
    df = df_wx_day.copy()
    ### Check for required columns ###
    df.columns = map(str.lower, df.columns)
    req_cols = ["yr", "mon", "day", "temp", "rh", "ws", "prec"]
    for col in req_cols:
        if not col in df.columns:
            raise RuntimeError("Missing required input column: " + col)
    ### Calculate minmax temperature ###
    df.loc[:, ["temp_min", "temp_max"]] = df.apply(
        lambda x: pd.Series(
            data = temp_min_max(x["temp"], x["rh"]),
            index = ["temp_min", "temp_max"]
        ),
        axis = 1
    )
    ### Calculate minmax relative humidity ###
    # Calculate specific humidity, assume it is the same at minmax temperature.
    df["q"] = df.apply(lambda x: find_q(x["temp"], x["rh"]), axis = 1)
    # Assume min relative humidity happens at max temperature and vice versa.
    df["rh_min"] = df.apply(
        lambda x: min(100, find_rh(x["q"], x["temp_max"])),
        axis = 1
    )
    df["rh_max"] = df.apply(
        lambda x: min(100, find_rh(x["q"], x["temp_min"])),
        axis = 1
    )
    ### Calculate minmax wind speed ###
    df["ws_min"] = 0.15 * df["ws"]
    df["ws_max"] = 1.25 * df["ws"]
    ### Prepare for output ###
    df = df.drop(columns = ["temp", "rh", "ws", "q"])
    df.insert(df.shape[1] - 1, "prec", df.pop("prec"))
    if not (round_out == None or round_out == "None"):
        outcols = ["temp_min", "temp_max", "rh_min", "rh_max",
                   "ws_min", "ws_max"]
        df[outcols] = df[outcols].map(round, ndigits = int(round_out))
    if not silent:
        print("########\n")
    return df


### Command line file execution ###############################################

# Run with option -h or --help to see usage.
if __name__ == "__main__":
    ### Add arguments ###
    parser = argparse.ArgumentParser(prog = "make_minmax")
    parser.add_argument(
        "input",
        help = "Input csv data file, columns: yr, mon, day, temp, rh, ws, prec"
    )
    parser.add_argument(
        "output",
        help = ("Output csv file name and location, columns: yr, mon, day, "
                "temp_min, temp_max, rh_min, rh_max, ws_min, ws_max, prec")
    )
    parser.add_argument("-s", "--silent", action = "store_true")
    parser.add_argument(
        "-r",
        "--round_out",
        default = 4,
        nargs = "?",
        help = ("Decimals to truncate outputs to, "
                "None for no rounding (default 4)")
    )
    ### Parse arguments and run daily_to_minmax() ###
    args = parser.parse_args()
    df_in = pd.read_csv(args.input)
    df_out = daily_to_minmax(df_in, args.silent, args.round_out)
    df_out.to_csv(args.output, index = False)

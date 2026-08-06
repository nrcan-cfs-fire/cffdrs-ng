# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.


# Summarize hourly FWI outputs into daily peak burn metrics.


### Import packages ###########################################################

import logging
import argparse
import datetime
import pandas as pd
import NG_FWI
import util

logger = logging.getLogger("cffdrs")
logger.setLevel(logging.WARNING)


### Functions #################################################################

##
# Calculate a smoothed vector weighted by binomial coefficients
# @param    ser    Vector of values
# @return          Vector of smoothed values
def smooth_binomial_5pt(ser):
    # Use pd.Series methods, so convert to a pd.Series first.
    if not isinstance(ser, pd.Series):
        ser = pd.Series(ser)
    # Calculate interior (cells with 2 neighbours either side) with binomial
    # coefficient weighting: [1, 4, 6, 4, 1].
    smoothed = (ser.shift(-2)
                +4*ser.shift(-1)
                +6*ser
                +4*ser.shift(1)
                +ser.shift(2)) / 16
    # Calculate inner edges (only 1 neighbour either side) weighted: [1, 2, 1].
    if ser.shape[0] >= 3:
        smoothed.iloc[1] = (ser.iloc[0]+2*ser.iloc[1]+ser.iloc[2]) / 4
        if ser.shape[0] > 3:
            smoothed.iloc[-2] = (ser.iloc[-3]+2*ser.iloc[-2]+ser.iloc[-1]) / 4
    # Copy values into any remaining NaN, including outer edges.
    smoothed.loc[smoothed.isna()] = ser.loc[smoothed.isna()]
    return smoothed.tolist()

##
# Calculate a pseudo date that changes at another hour past midnight
# @param    yr          Year
# @param    mon         Month number
# @param    day         Day of month
# @param    hr          Hour of day
# @param    reset_hr    The new boundary hour instead of midnight (default 5)
# @return               Pseudo ordinal date as year and ordinal day: "YYYY-D"
def pseudo_date(yr, mon, day, hr, reset_hr = 5):
    d = datetime.date(yr, mon, day)
    if hr < reset_hr:
        # Datetime "%j" format accounts for leap years.
        pseudo_ord_day = int(d.strftime("%j")) - 1
    else:
        pseudo_ord_day = int(d.strftime("%j"))
    # When ordinal day 1 (Jan 1) shifts to 0, bump it to end of previous year.
    if pseudo_ord_day == 0:
        pseudo_ord_day = int(datetime.date(yr - 1, 12, 31).strftime("%j"))
        pseudo_yr = yr - 1
    else:
        pseudo_yr = yr
    return "{}-{}".format(pseudo_yr, pseudo_ord_day)

##
# Calculate Daily Summaries from hourly FWI indices
# @param    df_hfwi         Hourly FWI dataframe (output of hFWI())
# @param    reset_hr        Hour defining new day to summarize (default 5)
# @param    bw_threshold    isi_smooth threshold for active burning (default 5)
# @param    silent          Suppress print statements (default False)
# @param    round_out       Number of decimal places to truncate outputs to,
#                               or None for none (default 4)
# @return                   Daily summary of peak FWI conditions
def generate_daily_summaries(
    df_hfwi,
    reset_hr = 5,
    bw_threshold = 5,
    silent = False,
    round_out = 4
):
    if not silent:
        print("\n########\nFWI2025: Daily Summaries ("
              + util.version() + ")\n")
    df = df_hfwi.copy()
    ### Check for required and optional columns ###
    req_cols = ["yr", "mon", "day", "hr", "ws", "sunrise", "sunset", "ffmc",
                "dmc", "dc", "isi", "bui", "fwi", "dsr", "mcgfmc_matted",
                "mcgfmc_standing", "gfmc", "gsi", "gfwi", "percent_cured"]
    for col in req_cols:
        if not col in df.columns:
            raise RuntimeError("Missing required input column: " + col)
    if "id" in df.columns:
        had_id = True
    elif (
        len(df["yr"].unique()) == 1 and
        len(df["lat"].unique()) == 1 and
        len(df["long"].unique()) == 1
    ):
        df["id"] = "stn"
        had_id = False
    else:
        raise RuntimeError('Missing "id" column with multiple years and '
                           'locations in data')
    ### Split by station ID and pseudo date ###
    # Initialize dictionary of lists to store outputs (convert to df at end).
    outcols = ["id", "yr", "mon", "day", "sunrise", "sunset", "peak_hr",
               "duration", "ffmc", "dmc", "dc", "isi", "bui", "fwi", "dsr",
               "gfmc", "gsi", "gfwi", "ws_smooth", "isi_smooth", "gsi_smooth"]
    daily = {k: [] for k in outcols}
    for stnid, df_id in df.groupby("id", sort = False):
        if not silent:
            print("Summarizing " + str(stnid) + " to daily")
        df_id["date_pseudo"] = df_id.apply(lambda row: pseudo_date(
            row["yr"], row["mon"], row["day"], row["hr"], reset_hr
        ), axis = 1)
        # Use first year available for transition between matted and standing
        # grassland (considering southern hemisphere fire season timing).
        DATE_GRASS_STANDING = datetime.date(
            df_id.reset_index().at[0, "yr"],
            NG_FWI.MON_STANDING,
            NG_FWI.DAY_STANDING
        )
        for _, df_date in df_id.groupby("date_pseudo", sort = False):
            df_date = df_date.reset_index(drop = True)
            # If this pseudo date doesn't have more than 12 hours, skip.
            if df_date.shape[0] <= 12:
                continue
            ### Copy date to output ###
            daily["id"].append(stnid)
            daily["yr"].append(df_date.at[0, "yr"])
            daily["mon"].append(df_date.at[0, "mon"])
            daily["day"].append(df_date.at[0, "day"])
            # Format sunrise and sunset as hh:mm instead of decimal hours.
            sr = df_date.at[0, "sunrise"]
            ss = df_date.at[0, "sunset"]
            daily["sunrise"].append(
                "{:02d}:{:02d}".format(int(sr), int(60 * (sr-int(sr))))
            )
            daily["sunset"].append(
                "{:02d}:{:02d}".format(int(ss), int(60 * (ss-int(ss))))
            )
            ### Find daily peak active burning hour ###
            df_date["ws_smooth"] = smooth_binomial_5pt(df_date["ws"])
            df_date["isi_smooth"] = df_date.apply(
                lambda row: NG_FWI.initial_spread_index(
                    row["ws_smooth"],
                    row["ffmc"]
                ),
                axis = 1
            )
            max_ffmc = df_date["ffmc"].max()
            if max_ffmc < 85.0:
                # 12 hours into pseudo date.
                peak_time = 12
            else:
                peak_time = df_date["isi_smooth"].idxmax()
            daily["peak_hr"].append(df_date.at[peak_time, "hr"])
            ### Calculate duration of active burning window ###
            if any(df_date["isi_smooth"] >= bw_threshold):
                # Difference between first and last hours of active burning.
                active_burning = df_date[df_date["isi_smooth"] >= bw_threshold]
                t_ab0 = datetime.datetime(
                    active_burning.iloc[0].yr,
                    active_burning.iloc[0].mon,
                    active_burning.iloc[0].day,
                    active_burning.iloc[0].hr
                )
                t_ab1 = datetime.datetime(
                    active_burning.iloc[-1].yr,
                    active_burning.iloc[-1].mon,
                    active_burning.iloc[-1].day,
                    active_burning.iloc[-1].hr
                )
                daily["duration"].append((t_ab1-t_ab0).seconds//3600 + 1)
            else:
                daily["duration"].append(0)
            ### Add FWI components at peak burning hour ###
            daily["ffmc"].append(df_date.at[peak_time, "ffmc"])
            daily["dmc"].append(df_date.at[peak_time, "dmc"])
            daily["dc"].append(df_date.at[peak_time, "dc"])
            daily["isi"].append(df_date.at[peak_time, "isi"])
            daily["bui"].append(df_date.at[peak_time, "bui"])
            daily["fwi"].append(df_date.at[peak_time, "fwi"])
            daily["dsr"].append(df_date.at[peak_time, "dsr"])
            daily["gfmc"].append(df_date.at[peak_time, "gfmc"])
            daily["gsi"].append(df_date.at[peak_time, "gsi"])
            daily["gfwi"].append(df_date.at[peak_time, "gfwi"])
            daily["ws_smooth"].append(df_date.at[peak_time, "ws_smooth"])
            daily["isi_smooth"].append(df_date.at[peak_time, "isi_smooth"])
            d = datetime.date(
                df_date.at[0, "yr"],
                df_date.at[0, "mon"],
                df_date.at[0, "day"]
            )
            if NG_FWI.GRASS_TRANSITION and d < DATE_GRASS_STANDING:
                standing = False
                mcgfmc = df_date.at[peak_time, "mcgfmc_matted"]
            else:
                standing = True
                mcgfmc = df_date.at[peak_time, "mcgfmc_standing"]
            daily["gsi_smooth"].append(NG_FWI.grass_spread_index(
                df_date.at[peak_time, "ws_smooth"],
                mcgfmc,
                df_date.at[peak_time, "percent_cured"],
                standing
            ))
    ### Prepare for output ###
    if not had_id:
        daily.pop("id")
    daily = pd.DataFrame(daily)
    if not (round_out == None or round_out == "None"):
        rcols = ["ffmc", "dmc", "dc", "isi", "bui", "fwi", "dsr", "gfmc",
                 "gsi", "gfwi", "ws_smooth", "isi_smooth", "gsi_smooth"]
        daily[rcols] = daily[rcols].map(round, ndigits = int(round_out))
    if not silent:
        print("########\n")
    return daily


### Command line file execution ###############################################

# Run with option -h or --help to see usage.
if __name__ == "__main__":
    ### Add arguments ###
    parser = argparse.ArgumentParser(prog = "daily_summaries")
    parser.add_argument("input", help = "Input csv data file")
    parser.add_argument("output",
                        help = "Output csv file name and/or location")
    parser.add_argument(
        "reset_hr",
        nargs = "?",
        default = 5,
        type = int,
        help = ("Hour defining new day to summarize instead of midnight "
                "(default 5)")
    )
    parser.add_argument("-s", "--silent", action = "store_true")
    parser.add_argument(
        "-r",
        "--round_out",
        default = 4,
        nargs = "?",
        help = ("Number of decimal places to truncate outputs to, "
                "None for no rounding (default 4)")
    )
    ### Parse arguments and run generate_daily_summaries() ###
    args = parser.parse_args()
    df_in = pd.read_csv(args.input)
    df_out = generate_daily_summaries(
        df_in,
        args.reset_hr,
        args.silent,
        args.round_out
    )
    df_out.to_csv(args.output, index = False)

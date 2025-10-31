import logging
import argparse
import pandas as pd
import NG_FWI
import util
import datetime

logger = logging.getLogger("cffdrs")
logger.setLevel(logging.WARNING)


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

## 
# Calculate a pseudo-date that changes not at midnight but forward at another hour
# @param    yr        year
# @param    mon       month number
# @param    day       day of month
# @param    hr        hour of day
# @param    reset_hr  the new boundary hour instead of midnight (default 5)
# @return             pseudo-date including year and ordinal day of the form "YYYY-D"
def pseudo_date(yr, mon, day, hr, reset_hr = 5):
  d = datetime.date(yr, mon, day)
  if hr < reset_hr:
    adjusted_jd = int(d.strftime("%j")) - 1
  else:
    adjusted_jd = int(d.strftime("%j"))
  
  if adjusted_jd == 0:  # where Jan 1 shifts to 0, bump it to end of previous year
    adjusted_jd = int(datetime.date(yr - 1, 12, 31).strftime("%j"))  # for leap yr
    adjusted_yr = yr - 1
  else:
    adjusted_yr = yr
  
  return "{}-{}".format(adjusted_yr, adjusted_jd)

##
# Calculate Daily Summaries from hourly FWI indices
# @param    hourly_FWI      hourly FWI dataframe (output of hFWI())
# @param    reset_hr        new boundary to define day to summarize (default 5)
# @param    silent          suppresses informative print statements (default False)
# @param    round_out       decimals to truncate output to, None for none (default 4)
# @return                   daily summary of peak FWI conditions
def generate_daily_summaries(hourly_FWI, reset_hr = 5,
  silent = False, round_out = 4):
  hourly_data = hourly_FWI.copy()
  Spread_Threshold_ISI = 5.0

  # check for "id" column
  if "id" in hourly_data.columns:
    had_stn = True
  else:
    if (len(hourly_data["yr"].unique()) == 1 and
      len(hourly_data["lat"].unique()) == 1 and
      len(hourly_data["long"].unique()) == 1):
      hourly_data["id"] = "stn"
      had_stn = False
    else:
      logger.error('Missing "id" column with multiple years and locations in data')
  
  # initialize dictionary of lists
  outcols = ["id", "yr", "mon", "day", "sunrise", "sunset", "peak_hr", "duration",
    "ffmc", "dmc", "dc", "isi", "bui", "fwi", "dsr", "gfmc", "gsi", "gfwi",
    "ws_smooth", "isi_smooth", "gsi_smooth"]
  results = {k: [] for k in outcols}
  
  for stn, by_stn in hourly_data.groupby("id", sort = False):
    if not silent:
      print("Summarizing " + str(stn) + " to daily")
    by_stn["pseudo_DATE"] = by_stn.apply(lambda row:
      pseudo_date(row["yr"], row["mon"], row["day"], row["hr"], reset_hr), axis = 1)
    # first year for transition btwn matted and standing (esp if southern hemisphere)
    DATE_GRASS_STANDING = datetime.date(by_stn.reset_index().at[0, "yr"],
      NG_FWI.MON_STANDING, NG_FWI.DAY_STANDING)

    for _, by_date in by_stn.groupby("pseudo_DATE", sort = False):
      by_date = by_date.reset_index(drop = True)

      # if this pseudo-date doesn't have more than 12 hours, skip
      if by_date.shape[0] <= 12:
        continue
      
      # find daily peak burn times
      by_date["ws_smooth"] = smooth_5pt(by_date["ws"])
      by_date["isi_smooth"] = by_date.apply(lambda row:
        NG_FWI.initial_spread_index(row["ws_smooth"], row["ffmc"]), axis = 1)
      
      max_ffmc = by_date["ffmc"].max()
      if max_ffmc < 85.0:
        peak_time = 12  # 12 hours into pseudo-date
      else:
        peak_time = by_date["isi_smooth"].idxmax()
      
      # find the rest of the values at peak
      results["id"].append(stn)
      results["yr"].append(by_date.at[0, "yr"])
      results["mon"].append(by_date.at[0, "mon"])
      results["day"].append(by_date.at[0, "day"])
      
      # format sunrise and sunset as hh:mm from decimal hours
      sr = by_date.at[peak_time, "sunrise"]
      ss = by_date.at[peak_time, "sunset"]
      results["sunrise"].append("{:02d}:{:02d}".format(int(sr), int(60 * (sr - int(sr)))))
      results["sunset"].append("{:02d}:{:02d}".format(int(ss), int(60 * (ss - int(ss)))))
      
      results["peak_hr"].append(by_date.at[peak_time, "hr"])
      results["duration"].append(sum(by_date["isi_smooth"] > Spread_Threshold_ISI))

      results["ffmc"].append(by_date.at[peak_time, "ffmc"])
      results["dmc"].append(by_date.at[peak_time, "dmc"])
      results["dc"].append(by_date.at[peak_time, "dc"])
      results["isi"].append(by_date.at[peak_time, "isi"])
      results["bui"].append(by_date.at[peak_time, "bui"])
      results["fwi"].append(by_date.at[peak_time, "fwi"])
      results["dsr"].append(by_date.at[peak_time, "dsr"])
      results["gfmc"].append(by_date.at[peak_time, "gfmc"])
      results["gsi"].append(by_date.at[peak_time, "gsi"])
      results["gfwi"].append(by_date.at[peak_time, "gfwi"])
      
      results["ws_smooth"].append(by_date.at[peak_time, "ws_smooth"])
      results["isi_smooth"].append(by_date.at[peak_time, "isi_smooth"])

      d = datetime.date(by_date.at[0, "yr"], by_date.at[0, "mon"], by_date.at[0, "day"])
      if NG_FWI.GRASS_TRANSITION and d < DATE_GRASS_STANDING:
        standing = False
        mcgfmc = by_date.at[peak_time, "mcgfmc_matted"]
      else:
        standing = True
        mcgfmc = by_date.at[peak_time, "mcgfmc_standing"]
      results["gsi_smooth"].append(NG_FWI.grass_spread_index(
        by_date.at[peak_time, "ws_smooth"], mcgfmc,
        by_date.at[peak_time, "percent_cured"], standing))

  if not had_stn:
    results.pop("id")
  
  results = pd.DataFrame(results)

  # round decimal places of output columns
  if not (round_out == None or round_out == "None"):
    outcols = ["ffmc", "dmc", "dc", "isi", "bui", "fwi", "dsr",
      "gfmc", "gsi", "gfwi", "ws_smooth", "isi_smooth", "gsi_smooth"]
    results[outcols] = results[outcols].map(round, ndigits = int(round_out))
    
  return results


if __name__ == "__main__":
  # run generate_daily_summaries() by command line
  # run with option -h or --help to see usage
  parser = argparse.ArgumentParser(prog = "daily_summaries")
  parser.add_argument("input", help = "Input csv data file")
  parser.add_argument("output", help = "Output csv file name and/or location")
  parser.add_argument("reset_hr", nargs = "?", default = 5, type = int,
    help = "New boundary to define day to summarize instead of midnight (default 5)")
  parser.add_argument("-s", "--silent", action = "store_true")
  parser.add_argument("-r", "--round_out", default = 4, nargs = "?",
    help = "Decimal places to truncate outputs to, None for no rounding (default 4)")
  
  args = parser.parse_args()
  df_in = pd.read_csv(args.input)
  df_out = generate_daily_summaries(df_in, args.reset_hr, args.silent, args.round_out)
  df_out.to_csv(args.output, index = False)

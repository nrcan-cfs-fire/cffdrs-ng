import logging
import os.path
import sys
import pandas as pd
import NG_FWI
import util

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
  

#this function calculates a fake date based on 5am-5am instead of midnight to midnight
#used for generating daily summaries to make data look at 5am-5am instead
#output form is "year-julian_day", with the julian day rounded back if the time is before 5am
#for Jan 1st where the julian day rollback would make it zero it gets bumped to the last day of the previous year
def pseudo_date(year, month, day, hour):
  
  adjusted_jd = None
  adjusted_year = None
  if (hour >= 5):
    adjusted_jd = util.julian(month, day)
  else:
    adjusted_jd = util.julian(month, day) - 1
  
  if adjusted_jd == 0:
    adjusted_year = year - 1
    adjusted_jd = util.julian(12,31)
  else:
    adjusted_year = year
    
  out = "{}-{}".format(adjusted_year, adjusted_jd)
  return out


def generate_daily_summaries(hourly_data):
  #note: need to account for spill over inlast day after pseudo_date calc where there is not 24 hours in the data
  
  Spread_Threshold_ISI = 5.0
  
  
  cols = ["wstind","yr","mon","day","peak_time","duration","wind_speed_smoothed","peak_isi_smoothed","peak_gsi_smoothed","ffmc","dmc","dc","isi","bui","fwi","dsr","gfmc","gsi","gfwi","sunrise","sunset"]
  results = pd.DataFrame(columns=cols)
  
  for stn in hourly_data["id"].unique():
    print("Summarizing " + stn + " to daily")
    by_stn = hourly_data.loc[hourly_data["id"] == stn]
    by_stn["pseudo_DATE"] =  by_stn.apply(
            lambda row: pseudo_date(
                row["yr"], row["mon"], row["day"], row["hr"]
            ),
            axis=1,
        )
    
    
    for p_date in by_stn["pseudo_DATE"].unique():
      by_date = by_stn.loc[by_stn["pseudo_DATE"] == p_date]

      # if this day doesn't go up to hour 17, skip
      if ((by_date.reset_index()).index[by_date["hr"] == 17].tolist().__len__() == 0):
        continue

      peak_time_traditional_spot = (by_date.reset_index()).index[by_date["hr"] == 17].tolist()[0]
      
      
      
      peak_time = -1
      duration = 0
      wind_smooth = smooth_5pt(by_date["ws"])
      peak_isi_smooth = -1
      peak_gsi_smooth = -1
      max_ffmc = 0
      ffmc = 0
      gfmc = 0
      dmc = 0
      dc = 0
      isi = 0
      gsi = 0
      bui = 0
      fwi = 0
      gfwi = 0
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
    
      ffmc = by_date.iloc[peak_time]["ffmc"]
      dmc = by_date.iloc[peak_time]["dmc"]
      dc = by_date.iloc[peak_time]["dc"]
      isi = by_date.iloc[peak_time]["isi"]
      bui = by_date.iloc[peak_time]["bui"]
      fwi = by_date.iloc[peak_time]["fwi"]
      dsr = by_date.iloc[peak_time]["dsr"]
      smooth_ws_peak = wind_smooth[peak_time]
      
      gfmc = by_date.iloc[peak_time]["gfmc"]
      gsi = by_date.iloc[peak_time]["gsi"]
      gfwi = by_date.iloc[peak_time]["gfwi"]

    


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


      standing = True
      if (util.julian(pick_month, pick_day) < NG_FWI.DATE_GRASS):
        standing = False
      peak_gsi_smooth = NG_FWI.grass_spread_index(smooth_ws_peak, gfmc, by_date.iloc[peak_time]["percent_cured"], standing)

      sunrise_val = by_date.iloc[peak_time]["sunrise"]
      sunset_val = by_date.iloc[peak_time]["sunset"]
    
      peak_time = by_date.iloc[peak_time]["hr"]

      sunrise_formated = "{}:{}".format(int(sunrise_val),int(60*(sunrise_val-int(sunrise_val))))
      sunset_formated = "{}:{}".format(int(sunset_val),int(60*(sunset_val-int(sunset_val))))


      daily_report_line = [by_date["id"].unique()[0], pick_year, pick_month, pick_day,
        peak_time, duration, smooth_ws_peak, peak_isi_smooth, peak_gsi_smooth,
        ffmc, dmc, dc, isi, bui, fwi, dsr, gfmc, gsi, gfwi,
        sunrise_formated, sunset_formated]
      
      daily_report = pd.DataFrame(columns=cols)
      daily_report.loc[0] = daily_report_line

      
      results = pd.concat([results, daily_report])

  
  results = results[["wstind","yr","mon","day","peak_time","duration","wind_speed_smoothed","peak_isi_smoothed","peak_gsi_smoothed","ffmc","dmc","dc","isi","bui","fwi","dsr","gfmc","gsi","gfwi","sunrise","sunset"]]

  return results


if "__main__" == __name__:
    args = sys.argv[1:]
    if len(args) != 2:
        logger.fatal(f"{sys.argv[0]} arguments must be: <input file> <output file>\n")
        sys.exit(1)
    infile = args[0]
    outfile = args[1]
    hourly_data = pd.read_csv(infile)
    summaries = generate_daily_summaries(hourly_data)
    util.save_csv(summaries, outfile)

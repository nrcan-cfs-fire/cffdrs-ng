#### Hourly Fire Weather Index (FWI) Tutorial ####
# April 2025 (last updated February 2026)
#
# This script was designed to go with a tutorial on the NG-CFFDRS website to inform
# users how to use scripts associated with FWI2025. Follow along with the
# 'Hourly FWI Tutorial - Python' page on the NG-CFFDRS website:
# https://cffdrs.github.io/website_en/tutorials/Hourly_FWI_Python
# This tutorial will demonstrate how to generate FWI2025 outputs based on an
# input from a CSV file. The method will differ if using another source file type or
# if integrating code into existing fire management systems. This tutorial
# assumes the user has a working level knowledge of Python.
##############################################################################

### Load packages ###
# Run `pip install` to install any you are missing.
import pandas as pd
import sys

### Load functions and data ###
# This tutorial will refer to file locations as structured in the GitHub repository.
# Specify the local path to the cffdrs-ng folder. It can accept absolute paths (e.g.
# starting with "C:/") or relative paths (e.g. "./" if the working directory is
# cffdrs-ng). Change the path strings to match your file layout if it is different.
path_prefix = "CHANGE/TO/PATH/TO/cffdrs-ng/"
sys.path.append(path_prefix + "FWI/Python")

# Load the files containing the variables and functions to calculate FWI2025.
from NG_FWI import hFWI
from daily_summaries import generate_daily_summaries

# Load the input weather station data file.
data = pd.read_csv(path_prefix + "data/PRF2007_hourly_wx.csv")

# Print the column names, data should contain 12 columns
print(data.columns)
# Index(['id', 'lat', 'long', 'timezone', 'yr', 'mon', 'day', 'hr', 'temp', 'rh', 
#        'ws', 'prec'],
#       dtype='object')

### Run FWI2025 ###
# hFWI() is the function that calculates hourly FWI codes in FWI2025. It can
# handle multiple stations and years/fire seasons (not shown in this tutorial).
# For details you can run the help.
help(hFWI)
# Help on function hFWI in module NG_FWI:

# hFWI(
#     df_wx,
#     timezone=None,
#     ffmc_old=85.0,
#     mcffmc_old=None,
#     dmc_old=6.0,
#     dc_old=15.0,
#     mcgfmc_matted_old=16.3075131042516,
#     mcgfmc_standing_old=16.3075131042516,
#     prec_cumulative=0.0,
#     canopy_drying=0,
#     silent=False,
#     round_out=4
# )
#     ##
#     # Calculate hourly FWI indices from hourly weather stream.
#     #
#     # @param    df_wx               hourly values weather stream
# ...

# For this tutorial, we will leave all the optional parameters to default.
data_fwi = hFWI(data)
# ########
# FWI2025 (YYYY-MM-DD)

# Startup values used:
# FFMC = 85.0 % and mcffmc = None
# DMC = 6.0 and DC = 15.0
# mcgfmc matted = 16.3075 % and standing = 16.3075 %
# cumulative precipitation = 0.0 mm and canopy drying = 0

# Running PRF for 2007
# ########

# Output is a DataFrame, with FWI calculations appended after the input columns.
# Save the output as a CSV file (overrides any preexisting file).
data_fwi.to_csv(path_prefix + "PRF2007_hourly_FWI.csv", index = False)

# Print the last two rows of the standard moisture codes and fire behaviour indices.
standard_components = ['ffmc', 'dmc', 'dc', 'isi', 'bui', 'fwi']
print(data_fwi.loc[:, standard_components].tail(2))
#          ffmc     dmc        dc     isi     bui     fwi
# 2621  80.2669  3.0924  219.5169  1.6423  5.9745  0.7650
# 2622  82.1759  3.3503  220.0533  2.1462  6.4550  1.2023

# Print a simple summary of the standard FWI components.
print(data_fwi.loc[:, standard_components].describe())
#               ffmc          dmc           dc          isi          bui          fwi
# count  2623.000000  2623.000000  2623.000000  2623.000000  2623.000000  2623.000000
# mean     70.622770    17.487231   136.122169     2.158163    23.971546     3.877128
# std      22.973326    13.331302    76.454242     2.233486    17.499332     4.438263
# min       0.000000     0.000000    15.409200     0.000000     0.000000     0.000000
# 25%      67.866500     5.924600    72.276400     0.787700     8.407750     0.422050
# 50%      79.377900    16.460900   133.062700     1.395800    22.623400     2.407500
# 75%      85.177050    26.511200   188.638500     2.978500    34.605350     5.929900
# max      94.397300    50.174000   292.950100    17.595800    66.148100    25.135200

# Compare your outputs with our standard outputs in PRF2007_standard_hourly_FWI.csv

### Calculate daily summaries ###
# Calculate outputs like peak burn time and number of hours of spread potential.
report = generate_daily_summaries(data_fwi)
# ########
# FWI2025: Daily Summaries (YYYY-MM-DD)

# Summarizing PRF to daily
# ########

# Print a simple summary of the daily report.
daily_components = ["peak_hr", "duration", "isi_smooth", "dsr"]
print(report[daily_components].describe())
#           peak_hr    duration  isi_smooth         dsr
# count  109.000000  109.000000  109.000000  109.000000
# mean    17.293578    2.697248    4.324269    1.553256
# std      1.271502    3.818824    3.220367    1.762854
# min     13.000000    0.000000    0.000000    0.000000
# 25%     17.000000    0.000000    1.234300    0.033800
# 50%     17.000000    0.000000    4.031900    0.991300
# 75%     18.000000    5.000000    6.495300    2.594000
# max     23.000000   14.000000   15.770700    6.948600

# View a distribution of the hour of daily peak burn, which looks like this:
print(report['peak_hr'].value_counts().sort_index())
# peak_hr
# 13     1
# 14     2
# 15     4
# 16    10
# 17    53
# 18    23
# 19    14
# 20     1
# 23     1
# Name: count, dtype: int64

# From here, the outputs can be converted to any datatype for further analysis or
# plotted for visualization.

### Appendix: Timezone ###
# This section will cover how to determine the UTC offset of a Local Standard Time
# (LST) based on a date and location (latitude and longitude). This only applies to
# datasets that are known to be recorded using LST, and is not a substitute in cases
# where a dataset's UTC offset is unknown. See the website FAQ for more details.

# The 'timezonefinder' and 'pytz' packages have functions that can get the timezone
# based on latitude, longitude, and date.
from datetime import datetime
from timezonefinder import TimezoneFinder
from pytz import timezone

# First, make a dataframe of stations with unique ID, latitude, and longitude.
stations = data.loc[:, ['id', 'lat', 'long']].drop_duplicates()
# Print the unique station IDs and coordinates. For this dataset the only station
# is at Petawawa Research Forest (PRF).
print(stations)
#     id     lat    long
# 0  PRF  45.996 -77.427

# Find the timezone based on latitude and longitude.
tf = TimezoneFinder()
tz_loc = tf.timezone_at(lat = stations.at[0, 'lat'], lng = stations.at[0, 'long'])
# Print timezone location. PRF is equivalent to "America/Toronto".
print(tz_loc)
# 'America/Toronto'

# The UTC offset can then be determined from the timezone location and a date. To
# guarantee the UTC offset for LST, use a date in winter (e.g. January 1st in the
# Northern hemisphere or July 1st in the Southern hemisphere).
utc = timezone(tz_loc).localize(datetime(2007, 1, 1)).strftime('%z')
# Print UTC offset, PRF in winter is in Eastern Standard Time.
print(utc)
# '-0500'

# The provided PRF dataset is actually not recorded in LST, but Local Daylight Time
# (Eastern Daylight Time). This is why the timezone parameter is set to -4, and
# why this process is not a substitute for actual information about a dataset.

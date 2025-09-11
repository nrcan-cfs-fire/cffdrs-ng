#### Hourly Fire Weather Index (FWI) Tutorial ####
# April 2025 (Last updated September 2025)
#
# This script was designed to go with a tutorial on the NG-CFFDRS website to inform
# users how to use scripts associated with FWI2025. Follow along with the
# 'Hourly FWI Tutorial - Python' page on the NG-CFFDRS website:
# https://cffdrs.github.io/website_en/tutorials/Hourly_FWI_Python
# This tutorial will demonstrate how to generate FWI2025 outputs based on an
# input from a .csv. The method will differ if using another source file type or
# if integrating code into existing fire management systems. This tutorial
# assumes the user has a working level knowledge of Python.
##############################################################################

### Load packages ###
# Run `pip install` to install any you are missing.
import pandas as pd
from datetime import datetime

### Load functions and data ###
# If the working directory is different from where you saved the FWI2025 scripts,
# you can add the path to the scripts with the sys package and sys.path.append()

# Load the files containing the variables and functions to calculate FWI2025.
from NG_FWI import hFWI
from daily_summaries import generate_daily_summaries

# Load the input weather station data file
# Specify the file path if PRF2007_hourly_wx.csv is not in the working directory
data = pd.read_csv("PRF2007_hourly_wx.csv")

# Print the column names, data should contain 12 columns
data.columns
# Index(['id', 'lat', 'long', 'timezone', 'yr', 'mon', 'day', 'hr', 'temp', 'rh', 
#        'ws', 'prec'],
#       dtype='object')

# Previously, the timezone (UTC offset) was a function parameter and calculated from
# latitude and longitude. Now it is a data frame column and provided. See the
# appendix of this tutorial for extra information on how to calculate the timezone,
# along with the extra information required about the dataset.

### Run FWI2025 ###
# hFWI() is the function that calculates hourly FWI codes in FWI2025. It can
# handle multiple stations and years/fire seasons (not shown in this tutorial).
# Default FWI start-up code values are: ffmc_old = 85, dmc_old = 6, dc_old = 15
data_fwi = hFWI(data)

# Output is a DataFrame, with FWI calculations appended after the input columns.
# Save the output as a .csv file (overrides any preexisting file).
data_fwi.to_csv("PRF2007_hourly_FWI.csv", index = False)

# Print the last two rows of the standard moisture codes and fire behaviour indices.
standard_components = ['ffmc', 'dmc', 'dc', 'isi', 'bui', 'fwi']
data_fwi.loc[:, standard_components].tail(2)
#          ffmc     dmc        dc     isi     bui     fwi
# 2621  80.2669  3.0924  219.5169  1.6423  5.9745  0.7650
# 2622  82.1759  3.3503  220.0533  2.1462  6.4550  1.2023

# Print a simple summary of the standard FWI components.
data_fwi.loc[:, standard_components].describe()
#              ffmc          dmc           dc          isi          bui          fwi
#count  2623.000000  2623.000000  2623.000000  2623.000000  2623.000000  2623.000000
#mean     70.622770    17.487231   136.122169     2.158163    23.971546     3.877128
#std      22.973326    13.331302    76.454242     2.233486    17.499332     4.438263
#min       0.000000     0.000000    15.409200     0.000000     0.000000     0.000000
#25%      67.866500     5.924600    72.276400     0.787700     8.407750     0.422050
#50%      79.377900    16.460900   133.062700     1.395800    22.623400     2.407500
#75%      85.177050    26.511200   188.638500     2.978500    34.605350     5.929900
#max      94.397300    50.174000   292.950100    17.595800    66.148100    25.135200

### Calculate daily summaries ###
# Calculate outputs like peak burn time and number of hours of spread potential.
report = generate_daily_summaries(data_fwi)

# Print a simple summary of the daily report.
daily_components = ["peak_hr", "duration", "isi_smooth", "dsr"]
report[daily_components].describe()
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
report['peak_hr'].value_counts().sort_index()
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

### Appendix: Timezones ###
# This section will cover how to calculate a timezone based on a location (latitude
# and longitude) and date. Note that this is not necessarily a substitute for
# actual information about the time used in a dataset. The CFFDRS Weather Guide
# specifies to collect data by local standard time, which is different in many
# places due to the shift to daylight time in summer months (daylight savings).

# The 'timezonefinder' and 'pytz' packages have functions that can get the timezone
# based on latitude, longitude, and date.
from timezonefinder import TimezoneFinder
from pytz import timezone

# First, make a dataframe of stations with unique ID, latitude, and longitude.
stations = data.loc[:, ['id', 'lat', 'long']].drop_duplicates()
# Print the unique station IDs and coordinates. For this dataset the only station
# is at Petawawa Research Forest (PRF).
stations
#     id     lat    long
# 0  PRF  45.996 -77.427

# Find the timezone based on latitude and longitude.
tf = TimezoneFinder()
tz_loc = tf.timezone_at(lat = stations.at[0, 'lat'], lng = stations.at[0, 'long'])
# Print timezone location. PRF is equivalent to "America/Toronto".
tz_loc
# 'America/Toronto'

# The UTC offset can then be determined from the timezone location.
utc = timezone(tz_loc).localize(datetime(2007, 5, 10)).strftime('%z')
# Print UTC offset, PRF in May is in Eastern Daylight Time (EDT)
utc
# '-0400'

# The UTC offset is expected as integer hours, so we can set it to -4.
utc = -4

# Since May 10, 2007 is during daylight savings the UTC offset calculated above
# corresponds to Eastern Daylight Time (EDT), UTC-4. This matches the timezone
# column provided since this data was collected using EDT. For Eastern Standard Time
# (EST), the UTC offset would be UTC-5.

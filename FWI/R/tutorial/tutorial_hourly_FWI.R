#### Hourly Fire Weather Index (FWI) Tutorial ####
# February 2025 (Last updated December 2025)
#
# This script was designed to go with a tutorial on the NG-CFFDRS website to inform
# users how to use scripts associated with FWI2025. Follow along with the
# 'Hourly FWI Tutorial - R' page on the NG-CFFDRS website:
# https://cffdrs.github.io/website_en/tutorials/Hourly_FWI_R
# This tutorial will demonstrate how to generate FWI2025 outputs based on an
# input from a CSV file. The method will differ if using another source file type or
# if integrating code into existing fire management systems. This tutorial
# assumes the user has a working level knowledge of R.
##############################################################################

### Load libraries ###
# Run install.packages() to install any you are missing
library(lubridate)
library(data.table)

### Load functions and data ###
# If the working directory is different from where you saved the FWI2025 scripts,
# change the working directory with setwd() to that folder.

# Load the files containing the variables and functions to calculate FWI2025.
source("NG_FWI.r")
source("util.r")
source("daily_summaries.r")

# Load the input weather station data file
# The path below is specified based on the layout in the GitHub repository
# Change the file path for PRF2007_hourly_wx.csv if your structure is different
data <- read.csv("../../data/PRF2007_hourly_wx.csv")

# Print the column names, data should contain 12 columns
print(names(data))
# [1] "id"       "lat"      "long"     "timezone" "yr"       "mon"
# [7] "day"      "hr"       "temp"     "rh"       "ws"       "prec"

# Previously, the UTC offset (`timezone` column) was a function parameter and could
# be calculated from latitude and longitude. Now it is a data frame column and
# provided. See the appendix of this tutorial for extra information on how to
# calculate the timezone, along with the extra information required about the
# dataset.

### Run FWI2025 ###
# hFWI() is the function that calculates hourly FWI codes in FWI2025. It can
# handle multiple stations and years/fire seasons (not shown in this tutorial). For the arguments you can run `args()`.
args(hFWI)

# For this tutorial, we will leave all the optional parameters to default.
data_fwi <- hFWI(data)
# ########
# Startup values used:
# FFMC = 85.0 or mcffmc = NA %
# DMC = 6.0 and DC = 15.0
# mcgfmc matted = 16.3075 % and standing = 16.3075 %
# cumulative precipitation = 0.0 mm and canopy drying = 0

# Running PRF for 2007
# ########

# Output is a data.frame (matching input class), with FWI calculations appended
# after the input columns. Save the output as a CSV file (overrides any preexisting
# file).
write.csv(data_fwi, "PRF2007_hourly_FWI.csv", row.names = FALSE)

# Print the last two rows of the standard moisture codes and fire behaviour indices.
standard_components <- c("ffmc", "dmc", "dc", "isi", "bui", "fwi")
tail(data_fwi[standard_components], 2)
#         ffmc    dmc       dc    isi    bui    fwi
# 2622 80.2669 3.0924 219.5169 1.6423 5.9745 0.7650
# 2623 82.1759 3.3503 220.0533 2.1462 6.4550 1.2023

# Print a simple summary of the standard FWI components.
summary(data_fwi[standard_components])
#      ffmc            dmc               dc              isi
# Min.   : 0.00   Min.   : 0.000   Min.   : 15.41   Min.   : 0.0000
# 1st Qu.:67.87   1st Qu.: 5.925   1st Qu.: 72.28   1st Qu.: 0.7877
# Median :79.38   Median :16.461   Median :133.06   Median : 1.3958
# Mean   :70.62   Mean   :17.487   Mean   :136.12   Mean   : 2.1582
# 3rd Qu.:85.18   3rd Qu.:26.511   3rd Qu.:188.64   3rd Qu.: 2.9785
# Max.   :94.40   Max.   :50.174   Max.   :292.95   Max.   :17.5958
#      bui              fwi
# Min.   : 0.000   Min.   : 0.000
# 1st Qu.: 8.408   1st Qu.: 0.422
# Median :22.623   Median : 2.408
# Mean   :23.972   Mean   : 3.877
# 3rd Qu.:34.605   3rd Qu.: 5.930
# Max.   :66.148   Max.   :25.135

# Compare your outputs with our standard outputs in PRF2007_standard_hourly_FWI.csv

### Calculate daily summaries ###
# Calculate outputs like peak burn time and number of hours of spread potential.
report <- generate_daily_summaries(data_fwi)

# Print a simple summary of the daily report.
daily_components <- c("peak_hr", "duration", "isi_smooth", "dsr")
summary(report[daily_components])
#    peak_hr         duration        isi_smooth          dsr
# Min.   :13.00   Min.   : 0.000   Min.   : 0.000   Min.   :0.0000
# 1st Qu.:17.00   1st Qu.: 0.000   1st Qu.: 1.234   1st Qu.:0.0338
# Median :17.00   Median : 0.000   Median : 4.032   Median :0.9913
# Mean   :17.29   Mean   : 2.697   Mean   : 4.324   Mean   :1.5533
# 3rd Qu.:18.00   3rd Qu.: 5.000   3rd Qu.: 6.495   3rd Qu.:2.5940
# Max.   :23.00   Max.   :14.000   Max.   :15.771   Max.   :6.9486

# From here, the outputs can be converted to any datatype for further analysis or
# plotted for visualization.

### Appendix: timezone ###
# This section will cover how to calculate a timezone based on a location (latitude
# and longitude) and date. Note that this is not necessarily a substitute for
# actual information about the time used in a dataset. The CFFDRS Weather Guide
# specifies to collect data by local standard time, which is different in many
# places due to the shift to daylight time in summer months (daylight savings).

# The 'lutz' library has functions to get the timezone of the weather station
# based on latitude and longitude.
library(lutz)

# First, make a dataframe of stations with unique ID, latitude, and longitude.
stations <- unique(data[c("id", "lat", "long")])
# Print the unique station IDs and coordinates. For this dataset the only station
# is at Petawawa Research Forest (PRF).
stations
#    id    lat    long
# 1 PRF 45.996 -77.427

# Find the timezone based on latitude and longitude, this can take some time.
# You may need to download the package 'sf' for method = "accurate".
tz_loc <- tz_lookup_coords(stations$lat, stations$long, method = "accurate")
# Print the timezone location. PRF is equivalent to "America/Toronto".
tz_loc
# [1] "America/Toronto"

# The UTC offset can then be determined from the timezone location.
utc <- tz_offset("2007-05-10", tz_loc)[[5]]
# Print the UTC offset
utc

# Since May 10, 2007 is during daylight savings the UTC offset calculated above
# corresponds to Eastern Daylight Time (EDT), UTC-4. This matches the timezone
# column provided since this data was collected using EDT. For Eastern Standard Time
# (EST), the UTC offset would be UTC-5.

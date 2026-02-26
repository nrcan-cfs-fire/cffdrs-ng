#### Daily to Hourly Weather Tutorial ####
# February 2026
#
# This script was designed to go with a tutorial on the NG-CFFDRS website to inform
# users how to use scripts associated with FWI2025. Follow along with the
# 'Daily to Hourly Tutorial - R' page on the NG-CFFDRS website:
# https://cffdrs.github.io/website_en/tutorials/Daily_to_Hourly_R
# This tutorial will demonstrate how to generate hourly weather from a daily weather
# CSV file. The method will differ if using another source file type or
# if integrating code into existing fire management systems. This tutorial
# assumes the user has a working level knowledge of R.
##############################################################################

### Load libraries ###
# Run install.packages() to install any you are missing
library(data.table)
library(lubridate)

### Load functions and data ###
# Check your current working directory
getwd()

# This tutorial will refer to file locations as structured in the GitHub repository.
# If the working directory is different from the cffdrs-ng/FWI/R folder, you can
# change to it with setwd(). Change the path characters to match your file layout if
# it is different.

# Load the files containing the variables and functions to calculate FWI2025.
source("make_minmax.r")
source("make_hourly.r")

# Load the input daily weather data file.
daily <- read.csv("../../data/PRF2007_daily_wx.csv")

# Print the column names, data should contain 11 columns
print(names(daily))
#  [1] "id"       "lat"      "long"     "timezone" "yr"       "mon"
#  [7] "day"      "temp"     "rh"       "ws"       "prec"

### Convert Daily to Minmax ###
# `daily_to_minmax()` is the function that calculates daily minmax data from daily
# weather data. For the arguments you can run `args()`.
args(daily_to_minmax)
# function (df, silent = FALSE, round_out = 4)
# NULL

# For this tutorial, we will leave the optional `round_out` parameter to default.
minmax <- daily_to_minmax(daily)
# ########
# FWI2025: Make Min/Max Inputs (YYYY-MM-DD)

# Predicting daily min/max weather
# ########

# Output is a data.frame, with the following columns:
print(names(minmax))
#  [1] "id"       "lat"      "long"     "timezone" "yr"       "mon"
#  [7] "day"      "temp_min" "temp_max" "rh_min"   "rh_max"   "ws_min"
# [13] "ws_max"   "prec"

# Print the last two rows of the minmax weather data.
print(tail(minmax[, 5:14], 2))
#       yr mon day temp_min temp_max  rh_min rh_max ws_min ws_max prec
# 108 2007   8  26   9.4176    22.56 46.6648    100 1.8030 15.025 10.7
# 109 2007   8  27  10.2418    23.59 46.8248    100 1.0134  8.445  0.0

# You can save the daily minmax data as a CSV file (overrides any preexisting file).
write.csv(minmax, "../../PRF2007_calculated_minmax_wx.csv", row.names = FALSE)

### Convert minmax to hourly ###
# Finally, convert the daily minmax weather data to hourly weather data that can be
# used to calculate hourly FWI. Run `args()` for details about the
# `minmax_to_hourly()` function arguments.
args(minmax_to_hourly)
# function (w, timezone = NA, prec_hr = "sunrise", skip_invalid = FALSE,
#     verbose = FALSE, silent = FALSE, round_out = 4)
# NULL

# Since our minmax data includes a UTC offset (`timezone`) column, we can leave the
# timezone option as default. Daily precipitation is placed at one hour of each day,
# controlled by the `prec_hr` parameter. By default it is placed at the calculated
# hour of sunrise, but an integer hour can be specified instead.

hourly <- minmax_to_hourly(minmax)
# ########
# FWI2025: Make Hourly Inputs (YYYY-MM-DD)

# Predicting hourly weather at PRF for 2007
# ########

# The output columns should include everything required to run `hFWI()`.
print(names(hourly))
#  [1] "id"       "lat"      "long"     "timezone" "yr"       "mon"
#  [7] "day"      "hr"       "temp"     "rh"       "ws"       "prec"

# Print the first 24 hours of hourly data.
print(head(hourly[, 5:12], 24))
#      yr mon day hr    temp      rh      ws prec
# 1  2007   5  11  0  9.0497 79.2894  6.0236  0.0
# 2  2007   5  11  1  8.2408 82.7490  5.5602  0.0
# 3  2007   5  11  2  7.5821 85.5521  5.1519  0.0
# 4  2007   5  11  3  7.0456 87.8233  4.7922  0.0
# 5  2007   5  11  4  6.6087 89.6634  4.4754  0.0
# 6  2007   5  11  5  6.2529 91.1543  4.1963  0.0
# 7  2007   5  11  6  5.2016 97.3083  3.9504  0.5
# 8  2007   5  11  7  7.0357 89.1380  2.9910  0.0
# 9  2007   5  11  8  8.8135 81.1794  6.0672  0.0
# 10 2007   5  11  9 10.4922 73.6335  8.9872  0.0
# 11 2007   5  11 10 12.0316 66.6910 11.6353  0.0
# 12 2007   5  11 11 13.3946 60.5271 13.9063  0.0
# 13 2007   5  11 12 14.5485 55.2976 15.7102  0.0
# 14 2007   5  11 13 15.4655 51.1346 16.9754  0.0
# 15 2007   5  11 14 16.1237 48.1433 17.6516  0.0
# 16 2007   5  11 15 16.5071 46.3991 17.7121  0.0
# 17 2007   5  11 16 16.6067 45.9461 17.1545  0.0
# 18 2007   5  11 17 16.4199 46.7958 16.0008  0.0
# 19 2007   5  11 18 15.9513 48.9268 14.2969  0.0
# 20 2007   5  11 19 15.2122 52.2851 12.1104  0.0
# 21 2007   5  11 20 14.2203 56.7860  9.5280  0.0
# 22 2007   5  11 21 11.9971 59.6863  7.7465  0.0
# 23 2007   5  11 22  9.2897 60.3596  6.9610  0.0
# 24 2007   5  11 23  7.0858 60.9049  6.2694  0.0

# You can save the hourly weather data as a CSV file (overrides any preexisting
# file).
write.csv(hourly, "../../PRF2007_calculated_hourly_wx.csv", row.names = FALSE)

# See the website for an appendix about the minmax to hourly parameters.

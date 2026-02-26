#### Daily to Hourly Weather Tutorial ####
# February 2026
#
# This script was designed to go with a tutorial on the NG-CFFDRS website to inform
# users how to compile and execute the FWI2025 C scripts. Follow along with the
# 'Daily to Hourly Tutorial - C' page on the NG-CFFDRS website:
# https://cffdrs.github.io/website_en/tutorials/Daily_to_Hourly_C
# This tutorial will demonstrate how to generate hourly weather from a daily weather
# CSV file. The method will differ if using another source file type or
# if integrating code into existing fire management systems. This tutorial
# assumes the user has a working level knowledge of C and the command line.
##############################################################################

# If the working directory is different from where you saved the FWI2025
# scripts, navigate to it with `cd` and `dir` commands
cd FWI/C

### Compile make_minmax and make_hourly ###
# To predict minmax weather data from daily data, the code files to compile are:
# make_minmax.c and util.c
# The corresponding util.h header file needs to be in the same folder,
# but does not need to be invoked directly. One compiler option is `gcc`
gcc -o daily_to_minmax make_minmax.c util.c

# The prompt should return with no comments if the code was compiled successfully,
# and a new file should be created in the current folder named daily_to_minmax.exe.
# The name of the executable (.exe) file is specified after the -o option above.

# To predict hourly weather data from minmax data, the code files to compile are:
# make_hourly.c and util.c
gcc -o minmax_to_hourly make_hourly.c util.c

### Convert daily to minmax ###
# Details about the *daily_to_minmax.exe* file can be found by running the compiled
# executable with no arguments.
./daily_to_minmax.exe
# ########
# help/usage:
# path\to\code\folder\daily_to_minmax.exe input output [silent]

# argument descriptions:
# input        Input csv data file
# output       Output csv file name and location
# silent       Suppresses informative print statements (default false)
# ########

# Now converting daily weather data to minmax.
./daily_to_minmax.exe ../../data/PRF2007_daily_wx_C-format.csv ../../data/PRF2007_dailyminmax_out.csv
# ########
# FWI2025: Make Min/Max Inputs (YYYY-MM-DD)

# Opening input file >>> ../../data/PRF2007_daily_wx_C-format.csv
# Saving outputs to file >>> ../../data/PRF2007_dailyminmax_out.csv

# Predicting daily min/max weather
# ########

# This will place our outputs in a new CSV file called PRF2007_dailyminmax_out.csv.

### Convert minmax to hourly ###
# Details about the minmax_to_hourly.exe file can be found by running the compiled
# executable with no arguments.
./minmax_to_hourly.exe
# ########
# help/usage:
# path\to\code\folder\minmax_to_hourly.exe input output timezone [prec_hr] [silent]

# argument descriptions:
# input        Input csv data file
# output       Output csv file name and location
# timezone     UTC offset
# prec_hr      Hour when daily precipitation occurs (default sunrise)
# silent       Suppresses informative print statements (default false)
# ########

# The UTC offset for the sample PRF2007 dataset is -4 (for Eastern Daylight Time).
# Daily precipitation is placed at one hour of each day, controlled by the `prec_hr`
# parameter. By default it is placed at the calculated hour of sunrise, but an
# integer hour can be specified instead.

./minmax_to_hourly.exe ../../data/PRF2007_dailyminmax_out.csv ../../data/PRF2007_hourly_out.csv -4
# ########
# FWI2025: Make Hourly Inputs (YYYY-MM-DD)

# Opening input file >>> ../../data/PRF2007_hourly_out.csv   
# Saving outputs to file >>> ../../data/PRF2007_hourly_out.csv

# Predicting hourly weather
# ########

# This will place our outputs in a new CSV file called PRF2007_hourly_out.csv.

# See the website for an appendix about the minmax to hourly parameters.

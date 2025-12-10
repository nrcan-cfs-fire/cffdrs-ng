#### Hourly Fire Weather Index (FWI) Tutorial ####
# December 2025
#
# This script was designed to go with a tutorial on the NG-CFFDRS website to inform
# users how to compile and execute the FWI2025 C scripts. Follow along with the
# 'Hourly FWI Tutorial - C' page on the NG-CFFDRS website:
# https://cffdrs.github.io/website_en/tutorials/Hourly_FWI_C
# This tutorial will demonstrate how to generate FWI2025 outputs based on an
# input from a CSV file. The method will differ if using another source file type or
# if integrating code into existing fire management systems. This tutorial
# assumes the user has a working level knowledge of C and the command line.
##############################################################################

# If the working directory is different from where you saved the FWI2025
# scripts, navigate to it with `cd` and `dir` commands
cd FWI/C

### Compile FWI2025 ###
# To calculate hourly FWI, the code files to compile are:
# NG_FWI_main.c, NG_FWI.c, and util.c
# The corresponding NG_FWI.h and util.h header files need to be in the same folder,
# but do not need to be invoked directly. One compiler option is `gcc`
gcc -o NG_FWI NG_FWI_main.c NG_FWI.c util.c

# The prompt should return with no comments if the code was compiled successfully,
# and a new file should be created in the current folder named NG_FWI.exe. The name
# of the executable (.exe) file is specified after the -o option above.

### Run FWI2025 ###
# The NG_FWI.exe file does not have to be run with all function arguments if you
# want to use default season startup values. The only required parameters are the
# input data file, an output file location/name, and timezone (UTC offset).
# Details can be found in the code documentation page on the website or by running
# the compiled executable with no arguments.
./NG_FWI.exe
# ########
# help/usage:
# C:\Users\ksiu\Documents\FDRS\cffdrs-ng\FWI\C\NG_FWI.exe input output timezone
# [ffmc_old] [mcffmc_old] [dmc_old] [dc_old] [mcgfmc_matted_old] [mcgfmc_standing_old]
# [prec_cumulative] [canopy_drying]

# positional arguments:
# input                 Input csv data file
# ...

# For this tutorial, we will leave all the optional parameters to default. The UTC
# offset for the sample PRF2007 dataset is -4 (for Eastern Daylight Time).
./NG_FWI.exe ../../data/PRF2007_hourly_wx_C-format.csv PRF2007_hourly_FWI.csv -4
# Opening input file >>> ../../data/PRF2007_hourly_wx_C-format.csv
# Saving outputs to file >>> PRF2007_hourly_FWI.csv

# ########
# Startup values used:
# FFMC = 85.0 or mcffmc = -1.0 %
# DMC = 6.0 and DC = 15.0
# mcgfmc matted = 16.3075 % and standing = 16.3075 %
# cumulative precipitation = 0.0 mm and canopy drying = 0
# ########

# This will place our outputs in a new CSV file called **PRF2007_hourly_FWI.csv**.
# Compare your outputs with our standard outputs in PRF2007_standard_hourly_FWI.csv

### Calculate Daily Summaries ###
# Calculate outputs like peak burn time and number of hours of spread potential.
# Going through the process again, the files to compile to calculate daily summaries
# are:
# daily_summaries.c, NG_FWI.c, and util.c.
# Ensure the corresponding NG_FWI.hand util.h header files are in the same folder.
gcc -o daily_summaries daily_summaries.c NG_FWI.c util.c

# The required parameters for daily_summaries.exe are the input data file
# (output of NG_FWI.exe) and an output file location/name. Details of the optional
# `reset_hr` parameter can be found in the code documentation page on the website
./daily_summaries.exe PRF2007_hourly_FWI.csv PRF2007_daily_report.csv

# From here, the outputs can be converted to any datatype for further analysis or
# plotted for visualization.

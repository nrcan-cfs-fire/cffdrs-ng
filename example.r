#' Example of how to go from hourly to daily and back
library(data.table)

# timezone (offset from GMT)
tz <- -6
# location of station
lat <- 51.5
long <- -93

# NOTE: if input has multiple stations, would need to do them one at a time

test_hffmc <- read.csv("test_hffmc.csv")
df_orig <- data.table(test_hffmc)
# add coordinates
df_orig[, lat := lat]
df_orig[, long := long]
# rename columns to match expectations
setnames(df_orig, c("yr", "hr", "ws", "prec"), c("year", "hour", "wind", "rain"))

# calculate hourly FWI indices for original data
source("NG_FWI.r")
result_orig <- hFWI(df_orig, timezone = tz)

# calculate daily weather from hourly data
source("make_daily.r")
df_daily <- hourly_to_daily(df_orig)

# calculate daily min/max values from daily noon values
source("make_minmax.r")
df_minmax <- daily_to_minmax(df_daily)

# calculate hourly weather from daily min/max values
source("make_hourly.r")
df_hourly <- minmax_to_hourly(df_minmax, timezone = tz)

# calculate hourly FWI for hourly weather
result <- hFWI(df_hourly, timezone = tz)

# plot original vs modified output
gfmc_orig <- result_orig$GFMC
gfmc <- result$GFMC
plot(gfmc ~ gfmc_orig, main = "GFMC")
abline(0, 1)

# pull out and plot daily grass fuel moisture codes
gfmc_orig <- result_orig[hour == 12]$GFMC
gfmc <- result[hour == 12]$GFMC
plot(gfmc ~ gfmc_orig, main = "GFMC@1200LST")
abline(0, 1)

# pull out and plot peak grass fuel moisture codes
gfmc_orig <- result_orig[hour == 17]$GFMC
gfmc <- result[hour == 17]$GFMC
plot(gfmc ~ gfmc_orig, main = "GFMC@1700LST")
abline(0, 1)

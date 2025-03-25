library(progress)
library(lubridate)
library(data.table)
library(fasttime)
library(ISLR)
library(fs)
library(ggplot2)
library(snow)
source("util.r")

REQUIRED_COLUMNS <- c('ID', 'TIMESTAMP', 'DATE', 'LAT', 'LONG', 'TIMEZONE', 'YR', 'MON', 'DAY', 'HR', 'MINUTE', 'TEMP', 'RH', 'WS', 'PREC')

HOME_DIR <- './'
#~ ONTARIO_DIR <- paste0(HOME_DIR, 'coeffs_on/')
#~ ALBERTA_DIR <- paste0(HOME_DIR, 'coeffs_ab/')
#~ SASK_DIR <- paste0(HOME_DIR, 'coeffs_sk/')
#~ BC_DIR <- paste0(HOME_DIR, 'coeffs_bc/')
#~ NT_DIR <- paste0(HOME_DIR, 'coeffs_nt/')
#~ YT_DIR <- paste0(HOME_DIR, 'coeffs_yt/')

getDir <- function(prov)
{
  return(paste0(HOME_DIR, 'coeffs_', tolower(prov), '/'))
}

checkContinuity <- function(wx, filter=FALSE)
{
  for (id in unique(wx$ID))
  {
    #~ print(id)
    b <- wx[ID == id]
    for (yr in unique(b$YR))
    {
      #~ print(yr)
      v <- b[YR == yr]
      # make sure all time steps are the same
      v[, step := TIMESTAMP - data.table::shift(TIMESTAMP, 1)]
      if (1 != length(unique(v$step[2:nrow(v)])))
      {
        print(v[step != unique(v$step[2:nrow(v)])[[1]]])
        msg <- paste0(id, ' is not continuous in year ', yr)
        if (!filter)
        {
          stop(msg)
        } else {
          # remove this station/year combo
          wx <- wx[ID != id | yr != yr]
        }
      }
    }
  }
  return(wx)
}

if (!exists("ontario")) {
  tz <- unique(fread(paste0(HOME_DIR, 'hourly_wx_data.csv'))[, c('id', 'timezone', 'year')])
  names(tz) <- toupper(names(tz))
  setnames(tz, c("YEAR"), c("YR"))
  #~ tz[timezone == 'EST']$timezone <- 'America/Toronto'
  #~ tz[timezone == 'CST']$timezone <- 'America/Winnipeg'
  ontario <- fread(paste0(HOME_DIR, 'ON_hourly_wxOBS_2007-2019_20201008.csv'))
  names(ontario) <- toupper(names(ontario))
  setnames(ontario, c('STATION', "LON"), c('ID', "LONG"))
  ontario <- merge(ontario, tz, by=c('ID', 'YR'))
  timestamps <- data.table(DATETIME=unique(ontario$DATETIME))
  # HACK: convert TIMESTAMP once and then merge back in
  timestamps[, TIMESTAMP := as_datetime(DATETIME)]
  ontario <- merge(ontario, timestamps, by=c("DATETIME"))
  ontario[, DATE := as_date(TIMESTAMP)]
  ontario[, MON := month(TIMESTAMP)]
  ontario[, DAY := day(TIMESTAMP)]
  ontario[, HR := hour(TIMESTAMP)]
  ontario[, MINUTE := 0]
  ontario[, TIME := HR + MINUTE / 60.0]
  ontario <- ontario[,..REQUIRED_COLUMNS]
  ontario <- ontario[ID != 'KNG' | YR > 2009]
  ontario <- ontario[ID != 'WEB' | YR != 2007]
  ontario <- ontario[ID != 'HO1' | YR != 2014]
  # this has a bunch of bad wind readings
  ontario <- ontario[ID != 'MVA']
  # # October is just 4 hours for every station because of when we clipped it and then changed time zones
  # ontario <- ontario[MON > 3]
  # ontario <- ontario[MON < 10]
  ontario[, TIMEZONE := ifelse("EST" == TIMEZONE, -5, ifelse("CST" == TIMEZONE, -6, NA))]
  ontario <- getSunlight(ontario)
}
df_wx <- ontario
#
# if (!exists("alberta")) {
#   alberta <- fread(paste0(HOME_DIR, './AB_AAF_hourly_wxObs_2000-2017_60pTmprlCov.csv'))
#   setnames(alberta, 'datetime', 'TIMESTAMP')
#   setnames(alberta, 'lon', 'LONG')
#   setnames(alberta, 'station', 'ID')
#   names(alberta) <- toupper(names(alberta))
#   alberta[, `:=`(DATE = substr(TIMESTAMP, 1, 10),
#                  TIMESTAMP = as.POSIXct(TIMESTAMP, tz='MDT')),]
#   alberta[, YR := year(TIMESTAMP)]
#   alberta[, MON:= month(TIMESTAMP)]
#   alberta[, DAY := day(TIMESTAMP)]
#   alberta[, HOUR := hour(TIMESTAMP)]
#   alberta[, MINUTE := 0]
#   alberta[, TIMEZONE := 'MDT']
#   alberta <- alberta[,..REQUIRED_COLUMNS]
#   alberta <- alberta[ID != 'C4' | YR != 2000]
#   alberta <- alberta[ID != 'E3' | YR != 2012]
#   alberta <- alberta[ID != 'F6' | MON != 4]
#   alberta <- alberta[ID != 'FTA']
#   alberta <- alberta[ID != 'GLA']
#   alberta <- alberta[ID != 'GMA']
#   alberta <- alberta[ID != 'MQA']
#   alberta <- alberta[ID != 'RHA' | YR != 2016]
#   alberta <- alberta[ID != 'XAA']
#   alberta <- alberta[MON < 10]
# }
#
# if (!exists("sask")) {
#   sask <- fread(paste0(HOME_DIR, './SK_wxObs_2010-2019_20201008.csv'))
#   setnames(sask, 'datetime', 'TIMESTAMP')
#   setnames(sask, 'lon', 'LONG')
#   setnames(sask, 'station', 'ID')
#   names(sask) <- toupper(names(sask))
#   sask[, `:=`(DATE = substr(TIMESTAMP, 1, 10),
#               TIMESTAMP = as.POSIXct(TIMESTAMP, tz='CST')),]
#   sask[, YR := year(TIMESTAMP)]
#   sask[, MON := month(TIMESTAMP)]
#   sask[, DAY := day(TIMESTAMP)]
#   sask[, HOUR := hour(TIMESTAMP)]
#   sask[, MINUTE := 0]
#   sask[, TIMEZONE := 'CST']
#   sask <- sask[,..REQUIRED_COLUMNS]
#   #~ sask <- sask[ID != 'BEATR']
#   #~ sask <- sask[ID != 'DUCKM']
#   #~ sask <- sask[ID != 'FLCRN']
#   #~ sask <- sask[ID != 'GEIKO']
#   #~ sask <- sask[ID != 'HAULT']
#   #~ sask <- sask[ID != 'LALC2']
#   #~ sask <- sask[ID != 'LBEAR']
#   #~ sask <- sask[ID != 'LOONL']
#   #~ sask <- sask[ID != 'MISTA']
#   #~ sask <- sask[ID != 'REGNA']
#   #~ sask <- sask[ID != 'BUFFN' | YR != 2014]
#   #~ sask <- sask[ID != 'KEYLK' | YR != 2014]
#   #~ sask <- sask[ID != 'REGNA' | YR != 2014]
#   #~ sask <- sask[ID != 'SWIFT' | YR != 2014]
#   #~ sask <- sask[ID != 'LRNGE' | YR != 2015]
#   sask <- sask[MON > 3]
#   sask <- sask[MON < 10]
# }
#
# #~ if (!exists("sask_old"))
# #~ {
# #~ sask_old <- fread(paste0(HOME_DIR, './SK_wxObs_2010-2019.csv'))
# #~ setnames(sask_old, 'datetime', 'TIMESTAMP')
# #~ setnames(sask_old, 'lon', 'LONG')
# #~ setnames(sask_old, 'station', 'ID')
# #~ names(sask_old) <- toupper(names(sask_old))
# #~ sask_old[, TIMESTAMP := fastPOSIXct(TIMESTAMP, tz='GMT') - 6 * 60 * 60,]
# #~ sask_old[, DATE := substr(TIMESTAMP, 1, 10)]
# #~ sask_old[, YR := year(TIMESTAMP)]
# #~ sask_old[, MON := month(TIMESTAMP)]
# #~ sask_old[, DAY := day(TIMESTAMP)]
# #~ sask_old[, HOUR := hour(TIMESTAMP)]
# #~ sask_old[, MINUTE := 0]
# #~ sask_old[, TIMEZONE := 'CST']
# #~ sask_old <- sask_old[,..REQUIRED_COLUMNS]
# #~ sask_old <- sask_old[ID != 'BUFFN' | YR != 2014]
# #~ sask_old <- sask_old[ID != 'KEYLK' | YR != 2014]
# #~ sask_old <- sask_old[ID != 'REGNA' | YR != 2014]
# #~ sask_old <- sask_old[ID != 'SWIFT' | YR != 2014]
# #~ sask_old <- sask_old[ID != 'LRNGE' | YR != 2015]
# #~ sask_old <- sask_old[MON < 10]
# #~ }
#
#
# if (!exists("bc")) {
#   bc <- fread(paste0(HOME_DIR, 'BC_wxObs_2000-2016_20201117.csv'))
#   setnames(bc, 'datetime', 'TIMESTAMP')
#   setnames(bc, 'lon', 'LONG')
#   setnames(bc, 'station', 'ID')
#   names(bc) <- toupper(names(bc))
#   bc[, `:=`(DATE = substr(TIMESTAMP, 1, 10),
#             TIMESTAMP = as.POSIXct(TIMESTAMP, tz='PDT')),]
#   bc[, YR := year(TIMESTAMP)]
#   bc[, MON := month(TIMESTAMP)]
#   bc[, DAY := day(TIMESTAMP)]
#   bc[, HOUR := hour(TIMESTAMP)]
#   bc[, MINUTE := 0]
#   bc[, TIMEZONE := 'PDT']
#   bc <- bc[,..REQUIRED_COLUMNS]
#   bc <- bc[MON > 3]
#   bc <- bc[MON < 10]
# }
#
#
# if (!exists("nt")) {
#   nt <- fread(paste0(HOME_DIR, 'NT_ECCC_wxObs_2007-2020.csv'))
#   setnames(nt, 'datetime', 'TIMESTAMP')
#   setnames(nt, 'lon', 'LONG')
#   setnames(nt, 'station', 'ID')
#   names(nt) <- toupper(names(nt))
#   nt[, `:=`(DATE = substr(TIMESTAMP, 1, 10),
#             TIMESTAMP = as.POSIXct(TIMESTAMP, tz='MST')),]
#   nt[, YR := year(TIMESTAMP)]
#   nt[, MON := month(TIMESTAMP)]
#   nt[, DAY := day(TIMESTAMP)]
#   nt[, HOUR := hour(TIMESTAMP)]
#   nt[, MINUTE := 0]
#   nt[, TIMEZONE := 'MST']
#   nt <- nt[,..REQUIRED_COLUMNS]
#   nt <- nt[MON > 3]
#   nt <- nt[MON < 10]
#   # 00:00:00 instead of 04:00:00 because it takes it as UTC I guess?
#   nt <- nt[ID != 2200824 | !(YR == 2015 & TIMESTAMP >= '2015-09-26 00:00:00')]
#   nt <- nt[!is.na(TEMP)]
#   nt <- nt[ID != 2200824 | !(YR == 2015 & TIMESTAMP <= '2015-05-02 04:00:00')]
#   nt <- nt[ID != 2200824 | !(YR == 2016 & TIMESTAMP <= '2016-05-02 03:00:00')]
#   nt <- nt[ID != '22010H0' | !(YR == 2009 & TIMESTAMP <= '2016-04-13 02:00:00')]
#   nt <- checkContinuity(nt, TRUE)
#   nt <- nt[ID != 2201022 | YR != 2009]
# }
#
# if (!exists("yt")) {
#   yt <- fread(paste0(HOME_DIR, 'YT_ECCC_wxObs_2007-2020.csv'))
#   setnames(yt, 'datetime', 'TIMESTAMP')
#   setnames(yt, 'lon', 'LONG')
#   setnames(yt, 'station', 'ID')
#   names(yt) <- toupper(names(yt))
#   yt[, `:=`(DATE = substr(TIMESTAMP, 1, 10),
#             TIMESTAMP = as.POSIXct(TIMESTAMP, tz='PDT')),]
#   yt[, YR := year(TIMESTAMP)]
#   yt[, MON := month(TIMESTAMP)]
#   yt[, DAY := day(TIMESTAMP)]
#   yt[, HOUR := hour(TIMESTAMP)]
#   yt[, MINUTE := 0]
#   yt[, TIMEZONE := 'PDT']
#   yt <- yt[,..REQUIRED_COLUMNS]
#   yt <- yt[MON > 3]
#   yt <- yt[MON < 10]
#   yt <- checkContinuity(yt, TRUE)
# }
#

df <- read.csv("./test_hffmc.csv")
timezone <- -6
PET_DMC <- TRUE


weatherstream <- df
timezone <- timezone
ffmc_old <- FFMC_DEFAULT
dmc_old <- DMC_DEFAULT
dc_old <- DC_DEFAULT
PET_DMC <- PET_DMC



####################################################################################
wx <- data.table(copy(weatherstream))
old_names <- colnames(wx)
# add a bunch of dummy columns if they don't exist
colnames(wx) <- toupper(colnames(wx))
new_names <- colnames(wx)
hadStn <- "ID" %in% colnames(wx)
hadMinute <- "MINUTE" %in% colnames(wx)
hadDate <- "DATE" %in% colnames(wx)
hadLatitude <- "LAT" %in% colnames(wx)
hadLongitude <- "LONG" %in% colnames(wx)
hadTimestamp <- "TIMESTAMP" %in% colnames(wx)
wasWind <- "WIND" %in% colnames(wx)
wasRain <- "RAIN" %in% colnames(wx)
wasYear <- "YEAR" %in% colnames(wx)
wasHour <- "HOUR" %in% colnames(wx)
if (!hadStn) {
  wx[, ID := "STN"]
}
if (!hadMinute) {
  wx[, MINUTE := 0]
}
if (!hadLatitude) {
  warning(paste0("Using default latitude value of ", DEFAULT_LATITUDE))
  wx[, LAT := DEFAULT_LATITUDE]
}
if (!hadLongitude) {
  warning(paste0("Using default longitude value of ", DEFAULT_LONGITUDE))
  wx[, LONG := DEFAULT_LONGITUDE]
}
if (wasWind) {
  setnames(wx, c("WIND"), c("WS"))
}
if (wasRain) {
  setnames(wx, c("RAIN"), c("RAIN"))
}
if (wasYear) {
  setnames(wx, c("YEAR"), c("YEAR"))
}
if (wasHour) {
  setnames(wx, c("HOUR"), c("HR"))
}
stopifnot(all(wx$RH >= 0 & wx$RH <= 100))
stopifnot(all(wx$WS >= 0))
stopifnot(all(wx$RAIN >= 0))
stopifnot(all(wx$MON >= 1 & wx$MON <= 12))
stopifnot(all(wx$DAY >= 1 & wx$DAY <= 31))
stopifnot(ffmc_old >= 0 & ffmc_old <= 101)
stopifnot(dmc_old >= 0)
stopifnot(dc_old >= 0)
stopifnot(percent_cured >= 0 & percent_cured <= 100)
if (!hadDate) {
  wx[, DATE := as.character(as.Date(sprintf("%04d-%02d-%02d", YEAR, MON, DAY)))]
}
if (!hadTimestamp) {
  wx[, TIMESTAMP := as_datetime(sprintf("%04d-%02d-%02d %02d:%02d:00", YEAR, MON, DAY, HR, MINUTE))]
}

####################################################################################

results <- NULL
for (stn in unique(wx$ID))
{
  by_stn <- wx[ID == stn]
  for (year in unique(by_stn$YEAR))
  {
    by_year <- by_stn[YEAR == year, ]
  }
}

w <- copy(by_year)

####################################################################################
# .stnHFWI
####################################################################################

if (!isSequentialHours(w)) {
  stop("Expected input to be sequential hourly weather")
}
if (length(na.omit(unique(w$ID))) != 1) {
  stop("Expected a single ID value for input weather")
}
if (length(na.omit(unique(w$LAT))) != 1) {
  stop("Expected a single LAT value for input weather")
}
if (length(na.omit(unique(w$LONG))) != 1) {
  stop("Expected a single LONG value for input weather")
}
r <- copy(w)
# rely on this being called with a single station, so location doesn't change
dates <- as_datetime(unique(w$TIMESTAMP))
latitude <- w$LAT[[1]]
longitude <- w$LONG[[1]]

###########################################################################################
##################################### DMC-UPDATE ##########################################
## To calculate solar radiation using Hargreaves Model we need daily temperature range to
## be pasted to getSunlight function
## See: https://github.com/derekvanderkampcfs/open_solar_model#conclusions

r[, temprange := diff(range(TEMP)), by = c("DATE")]
sunlight <- getSunlight(dates, temprange = r$temprange, timezone, latitude, longitude)

###########################################################################################
###########################################################################################
setnames(sunlight, c("DATE"), c("TIMESTAMP"))
sunlight$TIMESTAMP <- as_datetime(sunlight$TIMESTAMP)
r <- merge(r, sunlight, by = c("TIMESTAMP", "LAT", "LONG"))
# print(r[(nrow(r)-10):nrow(r),])
maxsolprop <- 0.85
grassfuelload <- 0.35
# print(r[(nrow(r)-10):nrow(r),])
# print("FFMC")
r$FFMC <- rep(1, nrow(r)) # .hffmc(r, ffmc_old)
# print("DMC")


####################################################################################
# >>>> .hdmc
####################################################################################

#   r$DMC <- .hdmc(r, dmc_old,
#     ########################################################################################################
#     ############################### DMC- UPDATE ###########################################################
#     ## Pass logical dictating whether to caclulate DMC with new PET-based approach or original approach
#     PET_DMC = PET_DMC
#   )
w <- r

hourly_DMC <- function(t, rh, wind, rain, mon, hour, lastdmc, DryFrac, rain24, DELTA_MCrain, tnoon, rhnoon, DELTA_dry = NA) {
  el <- c(6.5, 7.5, 9, 12.8, 13.9, 13.9, 12.4, 10.9, 9.4, 8, 7, 6)
  # wetting FROM rain
  if (rain > 0 && DELTA_MCrain > 0.0) {
    # printf("rain=%f  change=%f lastdmc=%f\n",rain, DELTA_MCrain, lastdmc);
    mc <- 20.0 + 280.0 / exp(0.023 * lastdmc)
    #  the MC increase by the rain in this hour...  total * rain_hour/rain24
    mc <- mc + DELTA_MCrain * (rain / rain24)
    lastdmc <- 43.43 * (5.6348 - log(mc - 20))
  }
  # drying all day long too
  if (tnoon < -1.1) {
    tnoon <- -1.1
  }

  ########################################################################################################
  ############################### DMC- UPDATE ############################################################
  ## Only calculate old log drying rate "DELTA_dry" if PET-based "DELTA_dry" was not passed to "hourly_DMC

  # full day of drying in old FWI/DMC
  if (is.na(DELTA_dry)) {
    DELTA_dry <- 1.894 * (tnoon + 1.1) * (100.0 - rhnoon) * el[mon] * 0.0001
  } else {
    # # shouldn't use ratio on PET model
    # DRY_BY_HOUR <- 1.0
    # PET model represents a full day of drying, so divide by 24
    DRY_BY_HOUR <- 1.0 / 24.0
    # DryFrac <- DRY_BY_HOUR
    # if PET model should still only apply to sunlight hours
    DryFrac <- ifelse(0 == DryFrac, 0.0, DRY_BY_HOUR)
  }
  dmc <- lastdmc + (DELTA_dry * DryFrac)
  ########################################################################################################

  # printf("delta dmc, %f ,lastDMC,%f , frac,%f , fractional,%f\n",DELTA_mcrain,lastdmc, DryFrac, (DELTA_dry*DryFrac));
  if (dmc < 0) {
    dmc <- 0
  }
  return(dmc)
}
dmc <- copy(w)
stopifnot("SUNSET" %in% colnames(w))
stopifnot("SUNRISE" %in% colnames(w))
dmc[, VPD := ifelse(HR >= SUNRISE & HR <= SUNSET, vpd(TEMP, RH), 0.0)]
dmc[, RAIN24 := sum(RAIN), by = c("DATE")]
dmc[, MINRH := min(RH), by = c("DATE")]
dmc[, VPD24 := sum(VPD), by = c("DATE")]
dmc[, DRYFRAC := ifelse(HR >= SUNRISE & HR <= SUNSET, ifelse(VPD24 == 0, 1 / (SUNSET - SUNRISE), VPD / VPD24), 0.0)]

######################################################################################
########################## DMC-Update Drying rate ####################################

## Calculate understory PET and PET-Based drying rate
if (PET_DMC) {
  pet <- getPET(dmc)
  dmc <- merge(dmc, pet, by = c("TIMESTAMP", "LAT", "LONG"))
} else {
  dmc[, DELTA_dry := NA]
}
######################################################################################

# #>>>
# # FAILS when RH = 100 all day because fraction is divided weird and doesn't add up to 1 all the time
# dmc[, SUMDRY := sum(DRYFRAC), by=c("DATE")]
# stopifnot(all(dmc$SUMDRY - 1 < 0.000001))
# #<<<
lastdmc <- dmc_old
dates <- unique(w$DATE)
result <- NULL
for (n in 1:length(dates))
{
  d <- dates[[n]]
  for_date <- dmc[DATE == d]
  noon <- for_date[HR == 12]
  tnoon <- noon$TEMP
  rhnoon <- noon$RH
  # print(for_date)
  rain24 <- for_date$RAIN24[[1]]
  if (rain24 > 1.5) {
    reff <- (0.92 * rain24 - 1.27)
    if (lastdmc <= 33) {
      b <- 100.0 / (0.5 + 0.3 * lastdmc)
    } else if (lastdmc <= 65) {
      b <- 14.0 - 1.3 * log(lastdmc)
    } else {
      b <- 6.2 * log(lastdmc) - 17.2
    }
    # This is the change in MC (moisturecontent)  from FULL DAY's rain
    DELTA_mcdmcrain24 <- 1000.0 * reff / (48.77 + b * reff)
  } else {
    DELTA_mcdmcrain24 <- 0.0
  }
  hrs <- unique(for_date$HR)
  minhr <- min(hrs)
  fctDMC <- function(lastdmc, h) {
    # r <- for_date[HR == h]
    # HACK: should always be sequential and start at minhrs hrs?
    r <- for_date[h + 1 - minhr]
    # print(r)
    ########################################################################################################
    ############################### DMC- UPDATE ############################################################
    ## If A PET-based DELTA_Dry was calculated, Pass it to "hourly_DMC"

    # lastdmc <- hourly_DMC(r$TEMP, r$RH, r$WS, r$RAIN, r$MON, r$HR, lastdmc, r$DRYFRAC, r$RAIN24, DELTA_mcdmcrain24, tnoon, rhnoon, r$DELTA_dry)
    t <- r$TEMP
    rh <- r$RH
    wind <- r$WS
    rain <- r$RAIN
    mon <- r$MON
    hour <- r$HR
    lastdmc
    DryFrac <- r$DRYFRAC
    rain24 <- r$RAIN24
    DELTA_MCrain <- DELTA_mcdmcrain24
    DELTA_dry <- r$DELTA_dry

    ########################################################################################################


    return(lastdmc)
  }
  values <- Reduce(fctDMC, hrs, init = lastdmc, accumulate = TRUE)
  # get rid of init value
  values <- values[2:length(values)]
  lastdmc <- values[length(values)]
  result <- c(result, values)
  stopifnot(length(hrs) == length(values))
}

####################################################################################
# <<<< .hdmc
####################################################################################

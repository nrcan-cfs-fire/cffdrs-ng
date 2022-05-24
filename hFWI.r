library(cffdrs)
library(lubridate)
library(sf)
library(data.table)
library(fasttime)
library(rvest)
# FIX: don't want to be dependent on this but need to look up time zone for now
library(lutz)

#~ library('devtools')
#~ devtools::install_github('datastorm-open/suncalc')
library(suncalc)

# FIX: figure out what this should be
DEFAULT_LATITUDE <- 55.0
DEFAULT_LONGITUDE <- -120.0

# default startup values
FFMC_DEFAULT <- 85
DMC_DEFAULT <- 6
DC_DEFAULT <- 15


.dmcCalcPieces <- function(dmc_yda, temp, rh, prec, lat, mon, lat.adjust=TRUE) {
  #############################################################################
  # Description: Duff Moisture Code Calculation. All code
  #        is based on a C code library that was written by Canadian
  #        Forest Service Employees, which was originally based on
  #        the Fortran code listed in the reference below. All equations
  #        in this code refer to that document.
  #
  #        Equations and FORTRAN program for the Canadian Forest Fire 
  #        Weather Index System. 1985. Van Wagner, C.E.; Pickett, T.L. 
  #        Canadian Forestry Service, Petawawa National Forestry 
  #        Institute, Chalk River, Ontario. Forestry Technical Report 33. 
  #        18 p.
  #
  #        Additional reference on FWI system
  #
  #        Development and structure of the Canadian Forest Fire Weather 
  #        Index System. 1987. Van Wagner, C.E. Canadian Forestry Service,
  #        Headquarters, Ottawa. Forestry Technical Report 35. 35 p.
  #  
  #
  # Args:  dmc_yda:   The Duff Moisture Code from previous iteration
  #       temp:   Temperature (centigrade)
  #       rh:   Relative Humidity (%)
  #       prec:   Precipitation(mm)
  #      lat:   Latitude (decimal degrees)
  #      mon:   Month (1-12)
  #   lat.adjust:   Latitude adjustment (TRUE, FALSE, default=TRUE)
  #     
  #
  # Returns: list(DMC starting point after decrease,
  #         DMC increase during the day,
  #         DMC decrease from yesterday that resulted in starting point)
  #
  #############################################################################
  
  #Reference latitude for DMC day length adjustment
  #46N: Canadian standard, latitude >= 30N   (Van Wagner 1987)
  ell01 <- c(6.5, 7.5, 9, 12.8, 13.9, 13.9, 12.4, 10.9, 9.4, 8, 7, 6)
  #20N: For 30 > latitude >= 10
  ell02 <- c(7.9, 8.4, 8.9, 9.5, 9.9, 10.2, 10.1, 9.7, 9.1,8.6, 8.1, 7.8)
  #20S: For -10 > latitude >= -30  
  ell03 <- c(10.1, 9.6, 9.1, 8.5, 8.1, 7.8, 7.9, 8.3, 8.9, 9.4, 9.9, 10.2)
  #40S: For -30 > latitude
  ell04 <- c(11.5, 10.5, 9.2, 7.9, 6.8, 6.2, 6.5, 7.4, 8.7, 10, 11.2, 11.8)
  #For latitude near the equator, we simple use a factor of 9 for all months
  
  #constrain low end of temperature
  temp <- ifelse(temp < (-1.1), -1.1, temp)
  #Eq. 16 - The log drying rate
  rk <- 1.894 * (temp + 1.1) * (100 - rh) * ell01[mon] * 1e-04
  #Adjust the day length  and thus the drying r, based on latitude and month
  if (lat.adjust) {
    rk <- ifelse(lat <= 30 & lat > 10, 1.894 * (temp + 1.1) * 
                   (100 - rh) * ell02[mon] * 1e-04, rk)
    rk <- ifelse(lat <= -10 & lat > -30, 1.894 * (temp + 1.1) * 
                   (100 - rh) * ell03[mon] * 1e-04, rk)
    rk <- ifelse(lat <= -30 & lat >= -90, 1.894 * (temp + 1.1) * 
                   (100 - rh) * ell04[mon] * 1e-04, rk)
    rk <- ifelse(lat <= 10 & lat > -10, 1.894 * (temp + 1.1) * 
                   (100 - rh) * 9 * 1e-04, rk)
  }
  ra <- prec
  #Eq. 11 - Net rain amount
  rw <- 0.92 * ra - 1.27
  #Alteration to Eq. 12 to calculate more accurately
  wmi <- 20 + 280/exp(0.023 * dmc_yda)
  #Eqs. 13a, 13b, 13c
  b <- ifelse(dmc_yda <= 33, 
              100/(0.5 + 0.3 * dmc_yda), 
              ifelse(dmc_yda <= 65, 
                     14 - 1.3 * log(dmc_yda), 
                     6.2 * log(dmc_yda) - 17.2))
  #Eq. 14 - Moisture content after rain
  wmr <- wmi + 1000 * rw/(48.77 + b * rw)
  op <- options(warn = (-1))
  #Alteration to Eq. 15 to calculate more accurately
  pr0 <- 43.43 * (5.6348 - log(wmr - 20))
  options(op)
  #Constrain P
  pr <- ifelse(prec <= 1.5, dmc_yda, pr0)
  pr <- ifelse(pr < 0, 0, pr)
  #Calculate final P (DMC)
  return(list(pr, rk, dmc_yda - pr))
}

.dcCalcPieces <- function(dc_yda, temp, rh, prec, lat, mon, lat.adjust=TRUE) {
  #############################################################################
  # Description: Drought Code Calculation. All code
  #        is based on a C code library that was written by Canadian
  #        Forest Service Employees, which was originally based on
  #        the Fortran code listed in the reference below. All equations
  #        in this code refer to that document.
  #
  #        Equations and FORTRAN program for the Canadian Forest Fire 
  #        Weather Index System. 1985. Van Wagner, C.E.; Pickett, T.L. 
  #        Canadian Forestry Service, Petawawa National Forestry 
  #        Institute, Chalk River, Ontario. Forestry Technical Report 33. 
  #        18 p.
  #
  #        Additional reference on FWI system
  #
  #        Development and structure of the Canadian Forest Fire Weather 
  #        Index System. 1987. Van Wagner, C.E. Canadian Forestry Service,
  #        Headquarters, Ottawa. Forestry Technical Report 35. 35 p.
  #  
  #
  # Args:   dc_yda:   The Drought Code from previous iteration
  #       temp:   Temperature (centigrade)
  #       rh:   Relative Humidity (%)
  #       prec:   Precipitation(mm)
  #      lat:   Latitude (decimal degrees)
  #      mon:   Month (1-12)
  #   lat.adjust:   Latitude adjustment (TRUE, FALSE, default=TRUE)
  #
  # Returns: list(DC starting point after decrease,
  #         DC increase during the day,
  #         DC decrease from yesterday that resulted in starting point)
  #
  #############################################################################
  #Day length factor for DC Calculations
  #20N: North of 20 degrees N
  fl01 <- c(-1.6, -1.6, -1.6, 0.9, 3.8, 5.8, 6.4, 5, 2.4, 0.4, -1.6, -1.6)
  #20S: South of 20 degrees S
  fl02 <- c(6.4, 5, 2.4, 0.4, -1.6, -1.6, -1.6, -1.6, -1.6, 0.9, 3.8, 5.8)
  #Near the equator, we just use 1.4 for all months.
  #Constrain temperature
  temp <- ifelse(temp < (-2.8), -2.8, temp)
  
  #Eq. 22 - Potential Evapotranspiration
  pe <- (0.36 * (temp + 2.8) + fl01[mon]) / 2
  #Daylength factor adjustment by latitude for Potential Evapotranspiration
  if (lat.adjust) {
    pe <- ifelse(lat <= -20, (0.36 * (temp + 2.8) + fl02[mon]) / 2, pe)
    pe <- ifelse(lat > -20 & lat <= 20, (0.36 * (temp + 2.8) + 1.4)/2, pe)
  }
  # Cap potential evapotranspiration at 0 for negative winter DC values
  pe <- ifelse(pe < 0, 0, pe)
  ra <- prec
  #Eq. 18 - Effective Rainfall
  rw <- 0.83 * ra - 1.27
  #Eq. 19
  smi <- 800 * exp(-1 * dc_yda/400)
  #Alteration to Eq. 21
  dr0 <- dc_yda - 400 * log(1 + 3.937 * rw/smi)
  dr0 <- ifelse(dr0 < 0, 0, dr0)
  #if precip is less than 2.8 then use yesterday's DC
  dr <- ifelse(prec <= 2.8, dc_yda, dr0)
  return(list(dr, pe, dc_yda - dr))
}

vpd <- function(temperature, relative_humidity)
{
  # calculate vapour pressure deficit
  vapour_pressure_saturation <- 0.61078 * exp(17.269 * temperature / (temperature + 237.3))
  vapour_pressure_actual <- relative_humidity / 100 * vapour_pressure_saturation
  vapour_pressure_deficit <- vapour_pressure_actual - vapour_pressure_saturation
  return(vapour_pressure_deficit)
}

toDecimal <- function(t){
  return(hour(t) + (minute(t) + (second(t) / 60.0)) / 60.0)
}

toDaily <- function(w, all=FALSE)
{
  # split into morning and afternoon so we can assign rain to the proper fwi 'day'
  # NOTE: actually need to figure out what makes the most sense
  # - if we split at 12 then that means we're in LST not LDT
  # - the rain at 12 is from 1100-1200, so that should be part of today's calculation, not tomorrow's
  wx <- copy(w)
  # set DATE field in case there's only a TIMESTAMP
  wx[, DATE := as.character(as.Date(TIMESTAMP))]
  # use toDecimal() so we only need TIMESTAMP field and we can deal with minutes or even seconds
  wx[, FOR_DATE := ifelse(toDecimal(TIMESTAMP) <= 12, as.character(DATE), as.character(as.Date(DATE) + 1))]
  precip <- wx[, list(PREC = sum(PREC, na.rm=TRUE)), by=c('FOR_DATE')]
  setnames(precip, 'FOR_DATE', 'DATE')
  merged <- merge(wx[toDecimal(TIMESTAMP) == 12, -c('FOR_DATE', 'PREC')], precip, by=c('DATE'), all=all)
  merged$PREC <- nafill(merged$PREC, fill=0.0)
  if (all)
  {
    # fix up columns that would be missing values if no noon value for a day
    merged[, TIMESTAMP := as_datetime(sprintf('%s 12:00:00', as.character(DATE)))]
    merged[, YR := year(TIMESTAMP)]
    merged[, MON := month(TIMESTAMP)]
    merged[, DAY := day(TIMESTAMP)]
    merged[, HR := hour(TIMESTAMP)]
    merged[, MINUTE := minute(TIMESTAMP)]
    merged[, ID := na.omit(unique(merged$ID)[[1]])]
    merged[, LAT := na.omit(unique(merged$LAT)[[1]])]
    merged[, LONG := na.omit(unique(merged$LONG)[[1]])]
    # use default drying day indices from weather guide
    merged$TEMP <- nafill(merged$TEMP, fill=21.1)
    merged$RH <- nafill(merged$RH, fill=45)
    merged$WS <- nafill(merged$WS, fill=13)
  }
  return(merged)
}

julian <- function(mon, day)
{
  month <- c(0,31,59,90,120,151,181,212,242,273,304,334,365)
  return(month[mon]+day)
  
}
sun <- function(lat, lon, d, timezone, DST, verbose=FALSE)
{
  # print(timezone)
  dechour <- 12.0
  hr <- hour(d)
  # print(d)
  # jd <- yday(d)
  jd <- julian(month(d), day(d))
  fracyear <- 2.0*pi/365.0*( jd-1.0+(dechour-12.0)/24.0);
  
  eqtime <- 229.18*( 0.000075+ 0.001868*cos(fracyear) - 0.032077*sin (fracyear) - 0.014615*cos(2.0*fracyear) - 0.040849*sin(2.0*fracyear) )
  
  decl <- 0.006918-0.399912*cos(fracyear) + 0.070257*sin(fracyear) - 0.006758*cos(fracyear*2.0)+0.000907*sin(2.0*fracyear) - 0.002697*cos(3.0*fracyear) + 0.00148*sin(3.0*fracyear)
  timeoffset <- eqtime+4*lon-60*timezone
  
  tst <- (as.numeric(hr)-DST)*60.0+timeoffset
  hourangle <- tst/4-180
  zenith <- acos(sin(lat*pi/180)*sin(decl)+cos(lat*pi/180)*cos(decl)*cos(hourangle*pi/180) )
  # print(zenith)
  solrad <- 0.95*cos(zenith)
  # print(solrad)
  if(solrad<0)
  {
    solrad <- 0.0
  }
  if (verbose)
  {
    print(sprintf(" SOLAR: %d  %d DST=%d fracyear=%f dec=%f  toff=%f  tst=%fha=%f zen=%f  solrad=%f\n",jd,hr,DST,fracyear,decl,timeoffset,tst,hourangle,zenith,solrad))
  }
  zenith <- 90.833*pi/180.0;
  
  halfday <- 180.0/pi*acos( cos(zenith)/(cos(lat*pi/180.0)*cos(decl))-tan(lat*pi/180.0)*tan(decl) )
  sunrise <- (720.0-4.0*(lon+halfday)-eqtime)/60+timezone+DST
  sunset <- (720.0-4.0*(lon-halfday)-eqtime)/60+timezone+DST
  return(c(solrad, sunrise, sunset))
}

getSunlight <- function(dates, latitude, longitude, verbose=FALSE)
{
  # figure out sun hours so we can use them as when things would be drying
  tz <- tz_lookup_coords(latitude, longitude, method='accurate')
  # times <- suncalc::getSunlightTimes(date(dates), latitude, longitude, tz=tz)
  # print(times)
  # names(times) <- toupper(names(times))
  # times$LONG <- times$LON
  # times$DATE <- as.character(times$DATE)
  # times$SUNRISE <- toDecimal(times$SUNRISE)
  # times$SUNSET <- toDecimal(times$SUNSET)
  # times <- data.table(times[, c('DATE', 'LAT', 'LONG', 'SUNRISE', 'SUNSET')])
  # times[, SUNLIGHT_HOURS := ceiling(SUNSET) - floor(SUNRISE)]
  r <- NULL
  for (n in 1:length(dates))
  {
    d <- dates[[n]]
    # print(d)
    tzo <- tz_offset(d, tz)
    DST <- ifelse(tzo$is_dst, 1, 0)
    # timezone <- tzo$utc_offset_h
    timezone <- tzo$utc_offset_h - DST
    DST <- 0
    # print(timezone)
    r <- rbind(r, c(d, latitude, longitude,  sun(latitude, longitude, d, timezone, DST, verbose)))
  }
  r <- data.table(r)
  colnames(r) <- c("DATE", "LAT", "LONG", "SOLRAD", "SUNRISE", "SUNSET")
  # r <- merge(times[, -c("SUNLIGHT_HOURS", "SUNRISE", "SUNSET")], r, on=c("DATE"))
  # r <- r[, c("DATE", "LAT", "LONG", "solrad", "sunrise", "sunset")]
  # colnames(r) <- toupper(colnames(r))
  print(r)
  # times[, SUNLIGHT_HOURS := ceiling(SUNSET) - floor(SUNRISE)]
  r$DATE <- as_datetime(r$DATE)
  r$LAT <- as.numeric(r$LAT)
  r$LONG <- as.numeric(r$LONG)
  r$SOLRAD <- as.numeric(r$SOLRAD)
  r$SUNRISE <- as.numeric(r$SUNRISE)
  r$SUNSET <- as.numeric(r$SUNSET)
  r[, SUNLIGHT_HOURS := SUNSET - SUNRISE]
  return(r)
}

proportion <- function(df, field, by)
{
  # figure out the proportion of the field that occurs during each 'by' period 
  d <- df[, mget(c(by, 'TIMESTAMP', field))]
  daily <- d[, list(DAILY = sum(get(field))), by=c(by)]
  d <- merge(d, daily, by=c(by))
  # if daily sum is 0 then divide evenly across every hour
  d[, sprintf('%s_FRACTION', field) := ifelse(DAILY == 0, 1.0 / 24.0, get(field) / DAILY)]
  return(d)
}

proportion_sunlight <- function(df, field, by)
{
  d <- merge(proportion(df, field, by), df[, c('TIMESTAMP', 'SUNLIGHT_HOURS')], by=c('TIMESTAMP'))
  f <- sprintf('%s_FRACTION', field)
  # if no value for DAILY then just divide evenly over sunlight hours
  d[, eval(f) := ifelse(DAILY == 0, 1.0 / SUNLIGHT_HOURS, get(f))]
  return(d[, -c('SUNLIGHT_HOURS')])
}

isSequential <- function(data)
{
  v <- na.omit(unique(data - data.table::shift(data, 1)))
  return(1 == v[[1]] && length(v) == 1)
}

isSequentialDays <- function(df)
{
  return(isSequential(as.Date(df$DATE)))
}

isSequentialHours <- function(df)
{
  return(isSequential(as.POSIXct(df$TIMESTAMP)))
}

.hffmc <- function(w, ffmc_old)
{
  # get daily values but still get for days we don't have noon for because we want PREC total
  daily <- toDaily(w, all=TRUE)
  w[, FOR_DATE := ifelse(hour(TIMESTAMP) < 13, DATE, as.character(as.Date(DATE) + 1))]
  daily$FOR_DATE <- daily$DATE
  
  # FFMC uses the hourly FFMC calculation, but reduces the overall rain received by
  # 0.5mm. This is done by figuring out what fraction of the period's rain occurs in
  # each hour and then taking that fraction of 0.5mm off of that hour.
  
  # want to take 0.5 mm off of the total but proportional to amounts per hour
  # CHECK: does this make more sense than just removing the first 0.5mm?
  # figure out what fraction of the total rain to be counted 1mm is
  daily[, FFMC_MULTIPLIER := ifelse(0.5 >= PREC, 0, (PREC - 0.5) / PREC)]
  for_ffmc <- merge(w, daily[, c('FOR_DATE', 'FFMC_MULTIPLIER')], by=c('FOR_DATE'))
  for_ffmc[, PREC := PREC * FFMC_MULTIPLIER]
  
  for_ffmc$FFMC <- hffmc(for_ffmc, ffmc_old=ffmc_old, hourlyFWI=FALSE)
  for_ffmc <- for_ffmc[, c('TIMESTAMP', 'FFMC')]
  return(for_ffmc$FFMC)
}

.hdmc <- function(w, dmc_old)
{ 
  # do the old fwi calculation method on the daily values
  daily <- fwi(toDaily(w, all=TRUE), init=data.frame(ffmc=FFMC_DEFAULT, dmc=dmc_old, dc=DC_DEFAULT))
  # w[, FOR_DATE := ifelse(hour(TIMESTAMP) < 13, DATE, as.character(as.Date(DATE) + 1))]
  w$FOR_DATE <- w$DATE
  daily$FOR_DATE <- daily$DATE
  
  
  # rely on this being called with a single station, so location doesn't change
  sunlight <- getSunlight(as_datetime(unique(w$DATE)), w$LAT[[1]], w$LONG[[1]])
  sunlight$DATE <- as.character(sunlight$DATE)
  
  # divide DDMC_DEC proportionally among hours with rain from 1200 -> 1200
  # split along 1200 -> 1200 line
  # BUT rain at 12 is from 1100 to 1200, so split at 13
  # AND split the increase to the sunlight hours of the day it's for
  
  daily[, c('DDMC_START', 'DDMC_INC', 'DDMC_DEC') := 
          .dmcCalcPieces(data.table::shift(DMC, 1, dmc_old), TEMP, RH, PREC, LAT, MON)]

  # NOTE: include the 1.5mm that gets deducted because the daily dmc calculation subtracts it
  #     so if we just divide proportional to all rain that will apply the reduction proportionally too
  
  prec <- proportion(w, 'PREC', 'FOR_DATE')[, c('TIMESTAMP', 'PREC_FRACTION')]
  # do precip calculation based on FWI FOR_DATE and VPD calculation based on calendar day
  
  # Vapour Pressure Deficit is used to divide drying over the daylight hours for
  # DMC and DC. The fraction of total VPD occurring within each hour is calculated
  # for hours where the sun is up - sum(VPD_FRACTION) should equal 1.0 for every
  # day, but the VPD used to calculate that sum is based off of the drying day and
  # not the calendar day, so the VPD_FRACTION should peak at 1200 and then start
  # over.
  
  # DMC is supposed to be the same calculation as the regular DMC, but with the
  # increase and decrease spread out along the hours based on VPD for increase
  # and PRECIP for decrease.
  # Note that the PRECIP isn't decreased before passing to DMC calculation because
  # the calculation does the reduction, unlinke with the hourly FFMC calculation.
  
  # decrease is total decrease * fraction of rain for the day
  dmc <- merge(w, daily[, c('ID', 'DATE', 'DMC')], by=c('ID', 'DATE'), all=TRUE, all.y=FALSE)
  setnames(dmc, 'DMC', 'DDMC')
  dmc[, VPD := vpd(TEMP, RH)]
  dmc$DDMC <- nafill(dmc$DDMC, fill=dmc_old)
  dmc <- merge(dmc, prec, by=c('TIMESTAMP'))
  dmc <- merge(dmc, sunlight, by=c('DATE', 'LAT', 'LONG'))
  # need to figure out the drying hours and then what the fractional VPD for each of them is
  for_dmc <- merge(daily[, c('DATE', 'DDMC_INC')],
                   proportion_sunlight(dmc[HR >= SUNRISE & HR <= SUNSET, ], 'VPD', 'DATE'), by=c('DATE'))
  for_dmc[, DMC_INC := DDMC_INC * VPD_FRACTION]
  dmc <- merge(dmc, for_dmc[, c('TIMESTAMP', 'DDMC_INC', 'DMC_INC', 'VPD_FRACTION')], by=c('TIMESTAMP'), all=TRUE)
  dmc$DMC_INC <- nafill(dmc$DMC_INC, fill=0)
  dmc <- merge(dmc, daily[, c('FOR_DATE', 'DDMC_DEC')], c('FOR_DATE'))
  dmc[, DMC_DEC := PREC_FRACTION * DDMC_DEC]
  # just do sum from start of period to now for hourly values
  dmc[, DMC := dmc_old + cumsum(DMC_INC) - cumsum(DMC_DEC)]
  dmc[, DMC := ifelse(DMC < 0, 0, DMC)]
  dmc <- dmc[, c('TIMESTAMP', 'DMC')]
  return(dmc$DMC)
}

.hdc <- function(w, dc_old)
{
  # do the old fwi calculation method on the daily values
  daily <- fwi(toDaily(w, all=TRUE), init=data.frame(ffmc=FFMC_DEFAULT, dmc=DMC_DEFAULT, dc=dc_old))
  # w[, FOR_DATE := ifelse(hour(TIMESTAMP) < 13, DATE, as.character(as.Date(DATE) + 1))]
  w$FOR_DATE <- w$DATE
  daily$FOR_DATE <- daily$DATE
  
  
  # rely on this being called with a single station, so location doesn't change
  sunlight <- getSunlight(as_datetime(unique(w$DATE)), w$LAT[[1]], w$LONG[[1]])
  sunlight$DATE <- as.character(sunlight$DATE)
  
  # divide DDC_DEC proportionally among hours with rain from 1200 -> 1200
  # split along 1200 -> 1200 line
  # BUT rain at 12 is from 1100 to 1200, so split at 13
  # AND split the increase to the sunlight hours of the day it's for
  
  daily[, c('DDC_START', 'DDC_INC', 'DDC_DEC') := 
          .dcCalcPieces(data.table::shift(DC, 1, dc_old), TEMP, RH, PREC, LAT, MON)]
  
  # NOTE: include the 2.8mm that gets deducted because the daily dc calculation subtracts it
  #     so if we just divide proportional to all rain that will apply the reduction proportionally too
  
  prec <- proportion(w, 'PREC', 'FOR_DATE')[, c('TIMESTAMP', 'PREC_FRACTION')]
  # do precip calculation based on FWI FOR_DATE and VPD calculation based on calendar day
  
  # decrease is total decrease * fraction of rain for the day
  dc <- merge(w, daily[, c('ID', 'DATE', 'DC')], by=c('ID', 'DATE'), all=TRUE, all.y=FALSE)
  setnames(dc, 'DC', 'DDC')
  dc[, VPD := vpd(TEMP, RH)]
  dc$DDC <- nafill(dc$DDC, fill=dc_old)
  dc <- merge(dc, prec, by=c('TIMESTAMP'))
  dc <- merge(dc, sunlight, by=c('DATE', 'LAT', 'LONG'))
  # need to figure out the drying hours and then what the fractional VPD for each of them is
  for_dc <- merge(daily[, c('DATE', 'DDC_INC')],
                  proportion_sunlight(dc[HR >= SUNRISE & HR <= SUNSET, ], 'VPD', 'DATE'), by=c('DATE'))
  for_dc[, DC_INC := DDC_INC * VPD_FRACTION]
  dc <- merge(dc, for_dc[, c('TIMESTAMP', 'DDC_INC', 'DC_INC', 'VPD_FRACTION')], by=c('TIMESTAMP'), all=TRUE)
  dc$DC_INC <- nafill(dc$DC_INC, fill=0)
  dc <- merge(dc, daily[, c('FOR_DATE', 'DDC_DEC')], c('FOR_DATE'))
  dc[, DC_DEC := PREC_FRACTION * DDC_DEC]
  # just do sum from start of period to now for hourly values
  dc[, DC := dc_old + cumsum(DC_INC) - cumsum(DC_DEC)]
  dc[, DC := ifelse(DC < 0, 0, DC)]
  dc <- dc[, c('TIMESTAMP', 'DC')]
  return(dc$DC)
}


# MARK II of the model (2016) wth new solar rad model specific to grass
# 
# Temp is temperature in C
# RH is realtive humidty in %
# wind is average wind speed in km/h
# rain is rainfall in mm
# solrad is kW/m2  (radiaiton reaching fuel)
# mo is the old grass fuel moisture   (not as a code value...so elimates the conversion to code)
# time - time between obs in HOURS
# 
# 
# DRF of 1/16.1 comes from reducting the standard response time curve
# at 26.7C, 20%RH, 2 km/h to 0.85hr.
# 
# 
# 
# bmw
hourly_gfmc <- function(temp, rh, wind, rain, lastmc, solrad, time)
{
  drf <- 0.389633
  mo <- lastmc;
  
  # fuel temperature/humidity
  tf <- temp+17.9*solrad*exp(-0.034*wind);   #fuel temp from CEVW
  
  if(tf>temp)
  {
    rhf <- rh*6.107*10.0^(7.5*temp/(temp+237.0) )/(6.107*10.0^(7.5*tf/(tf+237.0) ))
  } else {
    rhf <- rh
  }
  if(rain!=0)
  {
    # /*     mo+=rain*rf*exp(-100.0/(251.0-mo))*(1.0-exp(-6.93/rain));*/  /* old routine*/
    # /* this new routine assumes layer is 0.3 kg/m2 so 0.3mm of rain adds +100%MC*/
    mo <- mo+ rain/0.3*100.0   #/* *100 to convert to %...  *1/.3 because of 0.3mm=100%  */
    if(mo>250.0)
    {
      mo <- 250.0
    }
  }
  ed <- 1.62*rhf^0.532+(13.7*exp( (rhf-100)/13.0))+0.27*(26.7-tf)*(1.0-1.0/exp(0.115*rhf));   #/*GRASS EMC*/
  moed <- mo-ed;
  ew <- 1.42*rhf^0.512+(12.0*exp((rhf-100)/18.0))+0.27*(26.7-tf)*(1.0-1.0/exp(0.115*rhf));     #/*GRASS EMC*/
  moew <- mo-ew;
  if (moed==0 || (moew>=0 && moed<0))
  {
    xm <- mo
    if(moed==0)
    {
      e <- ed
    }
    if(moew>=0)
    {
      e <- ew
    }
  } else {
    if( moed>0)
    {
      a1 <- rhf/100
      e <- ed
      moe <- moed
    } else {
      a1 <- (100.0-rhf)/100.0
      e <- ew
      moe <- moew
    }
    xkd <- (0.424*(1-a1^1.7)+(0.0694*sqrt(wind)*(1-a1^8)))
    xkd <- xkd*drf*exp(0.0365*tf)
    
    # //   printf("tf=%8.4f rhf=%6.2f e=%4.1f mo=%5.2f xkd=%6.4f moed=%5.1f moew=%5.1f\n",tf,rhf,e,mo,xkd,moed,moew);
    
    xm <- e+moe*exp(-1.0*log(10.0)*xkd*time)
  }
  
  return(xm)
}


grassISI <- Vectorize(function(wind, mc, cur)
{
  # print(wind)
  # print(mc)
  if(wind<5)
  {
    fw <- ( 0.054 +0.209*wind)*16.67
  } else {
    fw <- (1.1 + 0.715*(wind-5.0)*0.844)*16.67
  }
  
  if(mc<12)
  {
    fm <- exp(-0.108*mc)
  } else if(mc<20.0 && wind<10.0) {
    fm <- 0.684-0.0342*mc
  } else if(mc<23.9 && wind>=10.0) {
    fm <- 0.547-0.0228*mc
  } else {
    fm <- 0
  }
  
  if(cur>20)
  {
    cf <- 1.034/(1+104*exp(-0.1*(cur-20)))
  } else {
    cf <- 0.0
  }
  GSI <- 1.11* fw *fm * cf
  return(GSI)
})


grassFWI <- Vectorize(function(gsi, load)
{
  ros<- gsi/1.11;  #  this just converts back to ROS in m/min
  Fint <- 300.0*load * ros;
  if (Fint>100) {
    GFWI <- ( log(Fint/60.0) ) / 0.14
  } else {
    GFWI <- Fint/25.0
  }
  return(GFWI);
})

.stnHFWI <- function(w, ffmc_old, dmc_old, dc_old, percent_cured)
{
  if (!isSequentialHours(w))
  {
    stop('Expected input to be sequential hourly weather')
  }
  if (length(na.omit(unique(w$ID))) != 1)
  {
    stop('Expected a single ID value for input weather')
  }
  r <- copy(w)
  # rely on this being called with a single station, so location doesn't change
  sunlight <- getSunlight(as_datetime(unique(w$TIMESTAMP)), w$LAT[[1]], w$LONG[[1]], TRUE)
  setnames(sunlight, c("DATE"), c("TIMESTAMP"))
  sunlight$TIMESTAMP <- as_datetime(sunlight$TIMESTAMP)
  r <- merge(r, sunlight, by=c("TIMESTAMP", "LAT", "LONG"))
  print(r[(nrow(r)-10):nrow(r),])
  maxsolprop <- 0.85
  grassfuelload <- 0.35
  r[, MIN_RH := min(RH), by=c("DATE")]
  r[, SOLRAD := SOLRAD * max(0, maxsolprop * ifelse(min(99.5, MIN_RH) > 30, (1.27 - 0.0111 * RH), 1))]
  print(r[(nrow(r)-10):nrow(r),])
  
  r$FFMC <- .hffmc(w, ffmc_old)
  r$DMC <- .hdmc(w, dmc_old)
  r$DC <- .hdc(w, dc_old)
  
  # FIX: what is fbpMod doing? this is the default value for it
  r[, ISI := cffdrs:::.ISIcalc(FFMC, WS, fbpMod = FALSE)]
  r[, BUI := cffdrs:::.buiCalc(DMC, DC)]
  r[, FWI := cffdrs:::.fwiCalc(ISI, BUI)]
  # taken from package code
  r[, DSR := 0.0272 * (FWI ^ 1.77)]
  
  lastmcgmc <- 101-ffmc_old # approximation for a start up
  mcgmc <- NULL
  for (n in 1:nrow(r))
  {
    row <- r[n,]
    print(row)
    cur_mcgmc <- hourly_gfmc(row$TEMP, row$RH, row$WS, row$PREC, lastmcgmc, row$SOLRAD, 1.0)
    mcgmc <- c(mcgmc, cur_mcgmc)
    lastmcgmc <- cur_mcgmc
  }
  r$MCGMC <- mcgmc
  r[, GFMC := 59.5 * (250 - MCGMC) / (147.2772277 + MCGMC)]
  r[, GSI := grassISI(WS, MCGMC, percent_cured)]
  r[, GFWI := grassFWI(GSI, grassfuelload)]
  return(r)
}

hFWI <- function(weatherstream, ffmc_old=85, dmc_old=6, dc_old=15, percent_cured=100.0)
{
  wx <- copy(weatherstream)
  old_names <- colnames(wx)
  # add a bunch of dummy columns if they don't exist
  colnames(wx) <- toupper(colnames(wx))
  new_names <- colnames(wx)
  hadStn <- 'ID' %in% colnames(wx)
  hadMinute <- 'MINUTE' %in% colnames(wx)
  hadDate <- 'DATE' %in% colnames(wx)
  hadLatitude <- 'LAT' %in% colnames(wx)
  hadLongitude <- 'LONG' %in% colnames(wx)
  hadTimestamp <- 'TIMESTAMP' %in% colnames(wx)
  if (!hadStn)
  {
    wx[, ID := 'STN']
  }
  if (!hadMinute)
  {
    wx[, MINUTE := 0]
  }
  if (!hadDate)
  {
    wx[, DATE := as.character(as.Date(sprintf('%04d-%02d-%02d', YR, MON, DAY)))]
  }
  if (!hadLatitude)
  {
    wx[, LAT := DEFAULT_LATITUDE]
  }
  if (!hadLongitude)
  {
    wx[, LONG := DEFAULT_LONGITUDE]
  }
  if (!hadTimestamp)
  {
    wx[, TIMESTAMP := as_datetime(sprintf('%04d-%02d-%02d %02d:%02d:00', YR, MON, DAY, HR, MINUTE))]
  }
  # loop in hFWI function
  results <- NULL
  for (stn in unique(wx$ID))
  {
    w <- wx[ID == stn]
    r <- .stnHFWI(w, ffmc_old, dmc_old, dc_old, percent_cured)
    results <- rbind(results, r)
  }
  
  # this is all just to remove dummy variables that we added
  if (!is.null(results)) {
    if (!hadStn)
    {
      results <- results[, -c('ID')]
    }
    if (!hadMinute)
    {
      results <- results[, -c('MINUTE')]
    }
    if (!hadDate)
    {
      results <- results[, -c('DATE')]
    }
    if (!hadLatitude)
    {
      results <- results[, -c('LAT')]
    }
    if (!hadLongitude)
    {
      results <- results[, -c('LONG')]
    }
    if (!hadTimestamp)
    {
      results <- results[, -c('TIMESTAMP')]
    }
    setnames(results, new_names, old_names)
  }
  # should have gotten rid of all the fields we added to make the processing work
  return(results)
}

test_hfwi <- function()
{
  # set up as if we had called hFWI
  weatherstream <- data.table(test_hffmc)
  
  r <- hFWI(weatherstream, FFMC_DEFAULT, DMC_DEFAULT, DC_DEFAULT)
  
  # want to figure out what daily values would have been with old function
  w <- copy(weatherstream)
  w[, timestamp := as_datetime(sprintf('%04d-%02d-%02d %02d:%02d:00', yr, mon, day, hr, 0))]
  r[, TIMESTAMP := as_datetime(sprintf('%04d-%02d-%02d %02d:%02d:00', yr, mon, day, hr, 0))]
  colnames(w) <- toupper(colnames(w))
  colnames(r) <- toupper(colnames(r))
  d <- toDaily(w)
  d[, LAT := DEFAULT_LATITUDE]
  d[, LONG := DEFAULT_LONGITUDE]
  daily <- fwi(d, init=c(FFMC_DEFAULT, DMC_DEFAULT, DC_DEFAULT))
  setnames(daily,
           c('FFMC', 'DMC', 'DC', 'ISI', 'BUI', 'FWI', 'DSR'),
           c('DFFMC', 'DDMC', 'DDC', 'DISI', 'DBUI', 'DFWI', 'DDSR'))
  r <- merge(r,
             daily[, c('YR', 'MON', 'DAY', 'DFFMC', 'DDMC', 'DDC', 'DISI', 'DBUI', 'DFWI', 'DDSR')],
             by=c('YR', 'MON', 'DAY'))
  r[, DISI := cffdrs:::.ISIcalc(DFFMC, WS, fbpMod = FALSE)]
  
  r[, DBUI := cffdrs:::.buiCalc(DDMC, DDC)]
  r[, DFWI := cffdrs:::.fwiCalc(DISI, DBUI)]
  # taken from package code
  r[, DDSR := 0.0272 * (DFWI ^ 1.77)]
  
  
  
  
  # output input and FWI columns so git can tell us if they change
  r <- r[, c('TIMESTAMP', 'TEMP', 'WS', 'RH', 'PREC',
             'FFMC', 'DMC', 'DC', 'ISI', 'BUI', 'FWI', 'DSR',
             'DFFMC', 'DDMC', 'DDC', 'DISI', 'DBUI', 'DFWI', 'DDSR')]
  write.table(r, file='out.csv', sep=',', row.names=FALSE)
  return(r)
}

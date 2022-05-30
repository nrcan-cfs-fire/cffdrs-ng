library(cffdrs)
library(lubridate)
library(sf)
library(data.table)
library(fasttime)
library(rvest)
# FIX: don't want to be dependent on this but need to look up time zone for now
library(lutz)

# FIX: figure out what this should be
DEFAULT_LATITUDE <- 55.0
DEFAULT_LONGITUDE <- -120.0

# default startup values
FFMC_DEFAULT <- 85
DMC_DEFAULT <- 6
DC_DEFAULT <- 15

ISIcalc <- function(ws, ffmc)
{
  fm <- 147.2773 * (101.0 - ffmc)/(59.5 + ffmc)
  sf <- 19.115 * exp(-0.1386 * fm) * (1.0 + fm^5.31 / 4.93e07)
  isi <- sf * exp(0.05039 * ws)
  return(isi)
}

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
  # if (lat.adjust) {
  #   rk <- ifelse(lat <= 30 & lat > 10, 1.894 * (temp + 1.1) * 
  #                  (100 - rh) * ell02[mon] * 1e-04, rk)
  #   rk <- ifelse(lat <= -10 & lat > -30, 1.894 * (temp + 1.1) * 
  #                  (100 - rh) * ell03[mon] * 1e-04, rk)
  #   rk <- ifelse(lat <= -30 & lat >= -90, 1.894 * (temp + 1.1) * 
  #                  (100 - rh) * ell04[mon] * 1e-04, rk)
  #   rk <- ifelse(lat <= 10 & lat > -10, 1.894 * (temp + 1.1) * 
  #                  (100 - rh) * 9 * 1e-04, rk)
  # }
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
  # vapour_pressure_actual <- relative_humidity / 100 * vapour_pressure_saturation
  # vapour_pressure_deficit <- vapour_pressure_actual - vapour_pressure_saturation
  vapour_pressure_deficit <- vapour_pressure_saturation * (1.0 - relative_humidity / 100.0)
  return(vapour_pressure_deficit)
}

toDecimal <- function(t){
  return(hour(t) + (minute(t) + (second(t) / 60.0)) / 60.0)
}

julian <- function(mon, day)
{
  month <- c(0,31,59,90,120,151,181,212,242,273,304,334,365)
  return(month[mon]+day)
  
}

getSunlight <- function(dates, latitude, longitude, verbose=FALSE)
{
  # figure out sun hours so we can use them as when things would be drying
  tz <- tz_lookup_coords(latitude, longitude, method='accurate')
  r <- NULL
  by_date <- unique(date(dates))
  for (n in 1:length(by_date))
  {
    d <- by_date[[n]]
    # print(d)
    tzo <- tz_offset(d, tz)
    DST <- ifelse(tzo$is_dst, 1, 0)
    # timezone <- tzo$utc_offset_h
    timezone <- tzo$utc_offset_h - DST
    DST <- 0
    # print(timezone)
    # print(timezone)
    dechour <- 12.0
    # print(d)
    # jd <- yday(d)
    jd <- julian(month(d), day(d))
    fracyear <- 2.0*pi/365.0*( jd-1.0+(dechour-12.0)/24.0);
    
    eqtime <- 229.18*( 0.000075+ 0.001868*cos(fracyear) - 0.032077*sin (fracyear) - 0.014615*cos(2.0*fracyear) - 0.040849*sin(2.0*fracyear) )
    
    decl <- 0.006918-0.399912*cos(fracyear) + 0.070257*sin(fracyear) - 0.006758*cos(fracyear*2.0)+0.000907*sin(2.0*fracyear) - 0.002697*cos(3.0*fracyear) + 0.00148*sin(3.0*fracyear)
    timeoffset <- eqtime+4*longitude-60*timezone
    zenith <- 90.833*pi/180.0;
    
    halfday <- 180.0/pi*acos( cos(zenith)/(cos(latitude*pi/180.0)*cos(decl))-tan(latitude*pi/180.0)*tan(decl) )
    sunrise <- (720.0-4.0*(longitude+halfday)-eqtime)/60+timezone+DST
    sunset <- (720.0-4.0*(longitude-halfday)-eqtime)/60+timezone+DST
    for_datetime <- as_datetime(d)
    hrs <- unique(hour(dates[d == date(dates)]))
    for_hour <- function(hr)
    {
      # for_time <- as_datetime(for_datetime + hours(hr))
      # for_time <- for_datetime + hours(hr)
      # hours() is really slow
      for_time <- for_datetime + hr * 3600
      tst <- (as.numeric(hr)-DST)*60.0+timeoffset
      hourangle <- tst/4-180
      zenith <- acos(sin(latitude*pi/180)*sin(decl)+cos(latitude*pi/180)*cos(decl)*cos(hourangle*pi/180) )
      # print(zenith)
      solrad <- 0.95*cos(zenith)
      # print(solrad)
      if(solrad<0)
      {
        solrad <- 0.0
      }
      # if (verbose)
      # {
      #   print(sprintf(" SOLAR: %d  %d DST=%d fracyear=%f dec=%f  toff=%f  tst=%fha=%f zen=%f  solrad=%f\n",jd,hr,DST,fracyear,decl,timeoffset,tst,hourangle,zenith,solrad))
      # }
      return(c(for_time, latitude, longitude,  solrad, sunrise, sunset))
    }
    r <- rbind(r, do.call(rbind, lapply(hrs, for_hour)))
  }
  r <- data.table(r)
  colnames(r) <- c("DATE", "LAT", "LONG", "SOLRAD", "SUNRISE", "SUNSET")
  # print(r)
  r$DATE <- as_datetime(r$DATE)
  r$LAT <- as.numeric(r$LAT)
  r$LONG <- as.numeric(r$LONG)
  r$SOLRAD <- as.numeric(r$SOLRAD)
  r$SUNRISE <- as.numeric(r$SUNRISE)
  r$SUNSET <- as.numeric(r$SUNSET)
  r[, SUNLIGHT_HOURS := SUNSET - SUNRISE]
  return(r)
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
  # daily <- toDaily(w, all=TRUE)
  # w[, FOR_DATE := ifelse(hour(TIMESTAMP) < 13, DATE, as.character(as.Date(DATE) + 1))]
  # daily$FOR_DATE <- daily$DATE
  
  # FFMC uses the hourly FFMC calculation, but reduces the overall rain received by
  # 0.5mm. This is done by figuring out what fraction of the period's rain occurs in
  # each hour and then taking that fraction of 0.5mm off of that hour.
  
  # want to take 0.5 mm off of the total but proportional to amounts per hour
  # CHECK: does this make more sense than just removing the first 0.5mm?
  # figure out what fraction of the total rain to be counted 1mm is
  # daily[, FFMC_MULTIPLIER := ifelse(0.5 >= PREC, 0, (PREC - 0.5) / PREC)]
  # for_ffmc <- merge(w, daily[, c('FOR_DATE', 'FFMC_MULTIPLIER')], by=c('FOR_DATE'))
  for_ffmc <- copy(w)
  for_ffmc[, SUM_PREC := sum(PREC), by=c("DATE")]
  for_ffmc[, FFMC_MULTIPLIER := ifelse(0.5 >= SUM_PREC, 0, (SUM_PREC - 0.5) / SUM_PREC)]
  for_ffmc[, PREC := PREC * FFMC_MULTIPLIER]
  
  for_ffmc$FFMC <- hffmc(for_ffmc, ffmc_old=ffmc_old, hourlyFWI=FALSE)
  for_ffmc <- for_ffmc[, c('TIMESTAMP', 'FFMC')]
  return(for_ffmc$FFMC)
}

hourly_DMC <- function(t, rh, ws, rain, mon, lastdmc, DryFrac, rain24, DELTA_MCrain, tnoon, rhnoon)
{
  el <- c(6.5, 7.5, 9, 12.8, 13.9, 13.9, 12.4, 10.9, 9.4, 8, 7, 6)
  
  # wetting FROM rain
  if(rain>0 && DELTA_MCrain>0.0) {
    # printf("rain=%f  change=%f lastdmc=%f\n",rain, DELTA_MCrain, lastdmc);
    mc <- 20.0+280.0/exp(0.023*lastdmc)
    #  the MC increase by the rain in this hour...  total * rain_hour/rain24
    mc <- mc + DELTA_MCrain*(rain/rain24)
    lastdmc <- 43.43*(5.6348-log(mc-20))
      
  }
  
  # drying all day long too
  if(tnoon < -1.1)
  {
    tnoon <- -1.1
  }
  
  # full day of drying in old FWI/DMC
  DELTA_dry <- 1.894*(tnoon+1.1)*(100.0-rhnoon)*el[mon]*0.0001
    
  # printf("delta dmc, %f ,lastDMC,%f , frac,%f , fractional,%f\n",DELTA_mcrain,lastdmc, DryFrac, (DELTA_dry*DryFrac));
  
  dmc <- lastdmc + (DELTA_dry*DryFrac)
  
  return(dmc)
}


.hdmc <- function(w, dmc_old, DSTadjust=0)
{
    dmc <- copy(w)
    if (!("SUNSET" %in% colnames(w)) || !("SUNRISE" %in% colnames(w)))
    {
      # make sure if one column exists we remove it
      if ("SUNSET" %in% colnames(w))
      {
        dmc <- dmc[, -c("SUNSET")]
      }
      if ("SUNRISE" %in% colnames(w))
      {
        dmc <- dmc[, -c("SUNRISE")]
      }
      # rely on this being called with a single station, so location doesn't change
      sunlight <- getSunlight(as_datetime(unique(w$DATE)), w$LAT[[1]], w$LONG[[1]])
      sunlight$DATE <- as.character(sunlight$DATE)
      dmc <- merge(dmc, sunlight, by=c("DATE", "LAT", "LONG"))
    }
    dmc[, VPD := ifelse(HR >= SUNRISE & HR <= SUNSET, vpd(TEMP, RH), 0.0)]
    dmc[, RAIN24 := sum(PREC), by=c("DATE")]
    dmc[, MINRH := min(RH), by=c("DATE")]
    dmc[, VPD24 := sum(VPD), by=c("DATE")]
    dmc[, DRYFRAC := ifelse(HR >= SUNRISE & HR <= SUNSET, ifelse(VPD24 == 0,  1 / (SUNSET - SUNRISE), VPD / VPD24), 0.0)]
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
      noon <- for_date[HR == (12 + DSTadjust)]
      tnoon <- noon$TEMP
      rhnoon <- noon$RH
      # print(for_date)
      rain24 <- for_date$RAIN24[[1]]
      if(rain24>1.5)
      {
        reff <- (0.92*rain24-1.27)
        if(lastdmc<=33) {
          b <- 100.0/(0.5+0.3* lastdmc)
        } else if(lastdmc<=65) {
          b <- 14.0-1.3*log(lastdmc)
        } else {
          b <- 6.2*log(lastdmc)-17.2
        }
        # This is the change in MC (moisturecontent)  from FULL DAY's rain
        DELTA_mcdmcrain24 <- 1000.0*reff/(48.77+b*reff)
      } else {
        DELTA_mcdmcrain24 <- 0.0
      }
      hrs <- unique(for_date$HR)
      minhr <- min(hrs)
      fctDMC <- function(lastdmc, h)
      {
        # r <- for_date[HR == h]
        # HACK: should always be sequential and start at minhrs hrs?
        r <- for_date[h + 1 - minhr]
        # print(r)
        lastdmc <- hourly_DMC(r$TEMP, r$RH, r$WS, r$PREC, r$MON, lastdmc, r$DRYFRAC, r$RAIN24, DELTA_mcdmcrain24, tnoon, rhnoon)
        return(lastdmc)
      }
      values <- Reduce(fctDMC, hrs, init=lastdmc, accumulate=TRUE)
      # get rid of init value
      values <- values[2:length(values)]
      lastdmc <- values[length(values)]
      result <- c(result, values)
      stopifnot(length(hrs) == length(values))
    }
    return(result)
}

hourly_DC <- function(t, rh, ws, rain, lastdc, mon, rain24, dryfrac, DELTArain24, temp12)
{
  fl <- c(-1.6, -1.6, -1.6, 0.9, 3.8, 5.8, 6.4, 5, 2.4, 0.4, -1.6, -1.6)
  
  if(rain > 0 && DELTArain24 < 0.0) {
    # (weight it by Rainhour/rain24 )
    lastdc <- lastdc + DELTArain24 * (rain / rain24)
  }
  # total dry for the DAY
  DELTAdry24 <- (0.36 * (temp12 + 2.8) + fl[mon]) / 2.0
  if (DELTAdry24 < 0.0) {
    #;    /* the fix for winter negative DC change...shoulders*/
    DELTAdry24 = 0.0
  }
  #/* dry frac is VPD weighted value for the hour */
  dc <- lastdc + DELTAdry24*dryfrac;
  if(dc < 0) {
    dc <- 0
  }
  return(dc)
}

.hdc <- function(w, dc_old, DSTadjust=0)
{
  dc <- copy(w)
  if (!("SUNSET" %in% colnames(w)) || !("SUNRISE" %in% colnames(w)))
  {
    # make sure if one column exists we remove it
    if ("SUNSET" %in% colnames(w))
    {
      dc <- dc[, -c("SUNSET")]
    }
    if ("SUNRISE" %in% colnames(w))
    {
      dc <- dc[, -c("SUNRISE")]
    }
    # rely on this being called with a single station, so location doesn't change
    sunlight <- getSunlight(as_datetime(unique(w$DATE)), w$LAT[[1]], w$LONG[[1]])
    sunlight$DATE <- as.character(sunlight$DATE)
    dc <- merge(dc, sunlight, by=c("DATE", "LAT", "LONG"))
  }
  dc[, VPD := ifelse(HR >= SUNRISE & HR <= SUNSET, vpd(TEMP, RH), 0.0)]
  dc[, RAIN24 := sum(PREC), by=c("DATE")]
  dc[, MINRH := min(RH), by=c("DATE")]
  dc[, VPD24 := sum(VPD), by=c("DATE")]
  dc[, DRYFRAC := ifelse(HR >= SUNRISE & HR <= SUNSET, ifelse(VPD24 == 0,  1 / (SUNSET - SUNRISE), VPD / VPD24), 0.0)]
  # #>>>
  # # FAILS when RH = 100 all day because fraction is divided weird and doesn't add up to 1 all the time
  # dc[, SUMDRY := sum(DRYFRAC), by=c("DATE")]
  # stopifnot(all(dmc$SUMDRY - 1 < 0.000001))
  # #<<<
  lastdc <- dc_old
  
  dates <- unique(w$DATE)
  result <- NULL
  for (n in 1:length(dates))
  {
    d <- dates[[n]]
    for_date <- dc[DATE == d]
    noon <- for_date[HR == (12 + DSTadjust)]
    tnoon <- noon$TEMP
    # print(for_date)
    rain24 <- noon$RAIN24
    
    if(rain24 > 2.8) {
      rw <- 0.83 * rain24 - 1.27;
      smi <- 800 * exp(-lastdc / 400);
      # TOTAL change for the TOTAL 24 hour rain from FWI1970 model
      DELTA_DCrain24 <- -400.0 * log(1.0 + 3.937 * rw / smi)
    } else {
      DELTA_DCrain24 <- 0.0
    }
    hrs <- unique(for_date$HR)
    minhr <- min(hrs)
    fctDC <- function(lastdc, h)
    {
      # r <- for_date[HR == h]
      # HACK: should always be sequential and start at minhrs hrs?
      r <- for_date[h + 1 - minhr]
      # print(r)
      lastdc <- hourly_DC(r$TEMP, r$RH, r$WS, r$PREC, lastdc, r$MON, r$RAIN24, r$DRYFRAC, DELTA_DCrain24, tnoon)
      stopifnot(noon$MON == r$MON)
      return(lastdc)
    }
    values <- Reduce(fctDC, hrs, init=lastdc, accumulate=TRUE)
    # get rid of init value
    values <- values[2:length(values)]
    lastdc <- values[length(values)]
    result <- c(result, values)
    stopifnot(length(hrs) == length(values))
  }
  return(result)
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
  dates <- as_datetime(unique(w$TIMESTAMP))
  latitude <- w$LAT[[1]]
  longitude <- w$LONG[[1]]
  sunlight <- getSunlight(dates, latitude, longitude, TRUE)
  setnames(sunlight, c("DATE"), c("TIMESTAMP"))
  sunlight$TIMESTAMP <- as_datetime(sunlight$TIMESTAMP)
  r <- merge(r, sunlight, by=c("TIMESTAMP", "LAT", "LONG"))
  # print(r[(nrow(r)-10):nrow(r),])
  maxsolprop <- 0.85
  grassfuelload <- 0.35
  # print(r[(nrow(r)-10):nrow(r),])
  
  # print("FFMC")
  r$FFMC <- .hffmc(r, ffmc_old)
  # print("DMC")
  r$DMC <- .hdmc(r, dmc_old)
  # print("DC")
  r$DC <- .hdc(r, dc_old)
  
  # FIX: what is fbpMod doing? this is the default value for it
  r[, ISI := ISIcalc(WS, FFMC)]
  r[, BUI := cffdrs:::.buiCalc(DMC, DC)]
  r[, FWI := cffdrs:::.fwiCalc(ISI, BUI)]
  # taken from package code
  r[, DSR := 0.0272 * (FWI ^ 1.77)]
  
  r[, MIN_RH := min(RH), by=c("DATE")]
  # r[, SOLPROP := max(0, maxsolprop * ifelse(min(99.5, MIN_RH) > 30, (1.27 - 0.0111 * RH), 1))]
  # r[, SOLPROP := ifelse(MIN_RH > 30, (1.27 - 0.0111 * RH), 1)]
  r[, SOLPROP := ifelse(MIN_RH > 30, (1.27 - 0.0111 * MIN_RH), 1)]
  r[, SOLPROP := ifelse(SOLPROP < 0, 0, maxsolprop * SOLPROP)]
  r[, SOLRAD := SOLRAD * SOLPROP]
  
  lastmcgmc <- 101-ffmc_old # approximation for a start up
  mcgmc <- NULL
  for (n in 1:nrow(r))
  {
    row <- r[n,]
    # print(row)
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

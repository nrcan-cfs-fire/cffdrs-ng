#' Computes hourly FWI indices for an input hourly weather stream
library(lubridate)
library(data.table)
#source("util.r")

# FIX: figure out what this should be
DEFAULT_LATITUDE <- 55.0
DEFAULT_LONGITUDE <- -120.0

# default startup values
FFMC_DEFAULT <- 85
DMC_DEFAULT <- 6
DC_DEFAULT <- 15

#' Call Reduce for each row number of a dataframe. Keep outputs, aside from
#' initial input value.
#'
#' @param fct             Function to call for each row number
#' @param df              dataframe to apply function to
#' @param init            startup value for Reduce()
#' @return                list of output values, with initial input removed
reduce_by_row <- function(fct, df, init)
{
  result <- Reduce(fct, seq(1, nrow(df)), init, accumulate=TRUE)
  # get rid of init value
  return(result[2:length(result)])
}

#' Calculate Initial Spread Index (ISI)
#'
#' @param wind            Wind Speed (km/h)
#' @param ffmc            Fine Fuel Moisure Code
#' @return                Initial Spread Index
ISIcalc <- function(ws, ffmc)
{
  fm <- 147.2773 * (101.0 - ffmc)/(59.5 + ffmc)
  fw <- ifelse(ws >= 40,
               12 * (1 - exp(-0.0818 * (ws - 28))),
               exp(0.05039 * ws))
  sf <- 19.1152 * exp(-0.1386 * fm) * (1.0 + fm^5.31 / 4.93e07)
  isi <- sf * fw
  return(isi)
}

#' Calculate Build-up Index (BUI)
#'
#' @param dmc             Duff Moisture Code
#' @param dc              Drought Code
#' @return                Build-up Index
BUIcalc <- function (dmc, dc)
{
  bui1 <- ifelse(dmc == 0 & dc == 0, 0, 0.8 * dc * dmc/(dmc +
                                                          0.4 * dc))
  p <- ifelse(dmc == 0, 0, (dmc - bui1)/dmc)
  cc <- 0.92 + ((0.0114 * dmc)^1.7)
  bui0 <- dmc - cc * p
  bui0 <- ifelse(bui0 < 0, 0, bui0)
  bui1 <- ifelse(bui1 < dmc, bui0, bui1)
  return(bui1)
}

#' Calculate Fire Weather Index (FWI)
#'
#' @param isi             Initial Spread Index
#' @param bui             Build-up Index
#' @return                Fire Weather Index
FWIcalc <- function (isi, bui)
{
  bb <- ifelse(bui > 80, 0.1 * isi * (1000/(25 + 108.64/exp(0.023 *
                                                              bui))), 0.1 * isi * (0.626 * (bui^0.809) + 2))
  fwi <- ifelse(bb <= 1, bb, exp(2.72 * ((0.434 * log(bb))^0.647)))
  return(fwi)
}

#' Calculate hourly Fine Fuel Moisture Code (FFMC)
#'
#' @param weatherstream   Table of hourly weather data to use for calculations [TEMP, RH, WS, PREC]
#' @param ffmc_old        Fine Fuel Moisture Code for previous hour
#' @return                Fine Fuel Moisture Codes for given data
hourly_ffmc <- function (weatherstream, ffmc_old = 85)
{
  #' Calculate hourly Fine Fuel Moisture Code (FFMC)
  #'
  #' @param temp            Temperature (Celcius)
  #' @param rh              Relative Humidity (percent, 0-100)
  #' @param ws              Wind Speed (km/h)
  #' @param prec            Precipitation (mm)
  #' @param ffmc_old        Fine Fuel Moisture Code for previous hour
  #' @return                Fine Fuel Moisture Code for current hour
  fctHFFMC <- function(temp, rh, ws, prec, ffmc_old)
  {
    Fo <- ffmc_old
    t0 <- 1
    mo <- 147.27723 * (101 - Fo)/(59.5 + Fo)
    mr <- ifelse(mo <= 150,
                 mo + 42.5 * prec * exp(-100/(251 - mo)) * (1 - exp(-6.93/prec)),
                 mo + 42.5 * prec * exp(-100/(251 - mo)) * (1 - exp(-6.93/prec)) + 0.0015 * ((mo - 150)^2) * (prec^0.5))
    mr <- ifelse(mr > 250, 250, mr)
    mo <- ifelse(prec > 0, mr, mo)
    Ed <- 0.942 * (rh^0.679) + 11 * exp((rh - 100)/10) +
      0.18 * (21.1 - temp) * (1 - exp(-0.115 * rh))
    ko <- 0.424 * (1 - (rh/100)^1.7) + 0.0694 * (ws^0.5) *
      (1 - (rh/100)^8)
    kd <- ko * 0.0579 * exp(0.0365 * temp)
    md <- Ed + (mo - Ed) * (10^(-kd * t0))
    Ew <- 0.618 * (rh^0.753) + 10 * exp((rh - 100)/10) +
      0.18 * (21.1 - temp) * (1 - exp(-0.115 * rh))
    k1 <- 0.424 * (1 - ((100 - rh)/100)^1.7) + 0.0694 *
      (ws^0.5) * (1 - ((100 - rh)/100)^8)
    kw <- k1 * 0.0579 * exp(0.0365 * temp)
    mw <- Ew - (Ew - mo) * (10^(-kw * t0))
    m <- ifelse(mo > Ed, md, mw)
    m <- ifelse(Ed >= mo & mo >= Ew, mo, m)
    Fo <- 59.5 * (250 - m)/(147.27723 + m)
    Fo <- ifelse(Fo <= 0, 0, Fo)
    return(Fo)
  }
  f <- reduce_by_row(
    function(ffmc_old, n){
      row <- weatherstream[n,]
      return(fctHFFMC(row$TEMP, row$RH, row$WS, row$PREC, ffmc_old))
    },
    weatherstream,
    ffmc_old)
  return(f)
}

#' Calculate vapour pressure deficit
#'
#' @param temperature         Temperature (Celcius)
#' @param relative_humidity   Relative Humidity (percent, 0-100)
#' @return                    Vapour Pressure Deficit (kPa)
vpd <- function(temperature, relative_humidity)
{
  vapour_pressure_saturation <- 0.61078 * exp(17.269 * temperature / (temperature + 237.3))
  vapour_pressure_deficit <- vapour_pressure_saturation * (1.0 - relative_humidity / 100.0)
  return(vapour_pressure_deficit)
}

#' Calculate hourly Fine Fuel Moisture Code (FFMC)
#'
#' @param w               Table of hourly weather data to use for calculations [TIMESTAMP, DATE, TEMP, RH, WS, PREC]
#' @param ffmc_old        Fine Fuel Moisture Code for previous hour
#' @return                Fine Fuel Moisture Codes for given data
.hffmc <- function(w, ffmc_old)
{
  # get daily values but still get for days we don't have noon for because we want PREC total
  # daily <- toDaily(w, all=TRUE)
  # w[, FOR_DATE := ifelse(hour(TIMESTAMP) < 13, DATE, as.character(as.Date(DATE) + 1))]
  # daily$FOR_DATE <- daily$DATE
  #
  # FFMC uses the hourly FFMC calculation, but reduces the overall rain received by
  # 0.5mm. This is done by figuring out what fraction of the period's rain occurs in
  # each hour and then taking that fraction of 0.5mm off of that hour.
  #
  # want to take 0.5 mm off of the total but proportional to amounts per hour
  # CHECK: does this make more sense than just removing the first 0.5mm?
  # figure out what fraction of the total rain to be counted 1mm is
  # daily[, FFMC_MULTIPLIER := ifelse(0.5 >= PREC, 0, (PREC - 0.5) / PREC)]
  # for_ffmc <- merge(w, daily[, c('FOR_DATE', 'FFMC_MULTIPLIER')], by=c('FOR_DATE'))
  for_ffmc <- copy(w)
  for_ffmc[, SUM_PREC := sum(PREC), by=c("DATE")]
  for_ffmc[, FFMC_MULTIPLIER := ifelse(0.5 >= SUM_PREC, 0, (SUM_PREC - 0.5) / SUM_PREC)]
  for_ffmc[, PREC := PREC * FFMC_MULTIPLIER]
  for_ffmc$FFMC <- hourly_ffmc(for_ffmc, ffmc_old=ffmc_old)
  for_ffmc <- for_ffmc[, c('TIMESTAMP', 'FFMC')]
  return(for_ffmc$FFMC)
}

#' Calculate hourly Duff Moisture Code (DMC)
#'
#' @param w               Table of hourly weather data to use for calculations [TIMESTAMP, DATE, SUNRISE, SUNSET, MON, TEMP, RH, PREC]
#' @param dmc_old         Duff Moisture Code for previous hour
#' @param     PET_DMC     Logical indicating if DMC should be PET-based or Original
#' @return                Duff Moisture Codes for given data


.hdmc <- function(w, dmc_old,
########################################################################################################
############################### DMC- UPDATE ###########################################################
## Pass logical dictating whether to caclulate DMC with new PET-based approach or original approach             
                  PET_DMC = F)
  ############################### DMC- UPDATE ###########################################################
{
  hourly_DMC <- function(t, rh, ws, rain, mon, hr, lastdmc, DryFrac, rain24, DELTA_MCrain, tnoon, rhnoon,DELTA_dry = NA)
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
    
    ########################################################################################################
    ############################### DMC- UPDATE ############################################################
    ## Only calculate old log drying rate "DELTA_dry" if PET-based "DELTA_dry" was not passed to "hourly_DMC
    
    # full day of drying in old FWI/DMC
    if (is.na(DELTA_dry)) {
      DELTA_dry <- 1.894*(tnoon+1.1)*(100.0-rhnoon)*el[mon]*0.0001
    }
    
    ######################################################################################################## 
    
    # printf("delta dmc, %f ,lastDMC,%f , frac,%f , fractional,%f\n",DELTA_mcrain,lastdmc, DryFrac, (DELTA_dry*DryFrac));
    dmc <- lastdmc + (DELTA_dry*DryFrac)
    if (dmc < 0) { dmc <- 0}
    return(dmc)
  }
  dmc <- copy(w)
  stopifnot("SUNSET" %in% colnames(w))
  stopifnot("SUNRISE" %in% colnames(w))
  dmc[, VPD := ifelse(HR >= SUNRISE & HR <= SUNSET, vpd(TEMP, RH), 0.0)]
  dmc[, RAIN24 := sum(PREC), by=c("DATE")]
  dmc[, MINRH := min(RH), by=c("DATE")]
  dmc[, VPD24 := sum(VPD), by=c("DATE")]
  dmc[, DRYFRAC := ifelse(HR >= SUNRISE & HR <= SUNSET, ifelse(VPD24 == 0,  1 / (SUNSET - SUNRISE), VPD / VPD24), 0.0)]
  
  ######################################################################################
  ########################## DMC-Update Drying rate ####################################
  
  ## Calculate understory PET and PET-Based drying rate
  if(PET_DMC) {
    pet = getPET(dmc)
    dmc = merge(dmc,pet, by = c("TIMESTAMP","LAT", "LONG"))
    
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
      ########################################################################################################
      ############################### DMC- UPDATE ############################################################
      ## If A PET-based DELTA_Dry was calculated, Pass it to "hourly_DMC"
      
      lastdmc <- hourly_DMC(r$TEMP, r$RH, r$WS, r$PREC, r$MON, r$HR,lastdmc, r$DRYFRAC, r$RAIN24, DELTA_mcdmcrain24, tnoon, rhnoon,ifelse(any(grepl("DELTA_dry",colnames(r))),r$DELTA_dry,NA))
      
      ########################################################################################################
      
      
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

#' Calculate hourly Drought Code (DC)
#'
#' @param w               Table of hourly weather data to use for calculations [TIMESTAMP, DATE, SUNRISE, SUNSET, MON, TEMP, RH, PREC]
#' @param dc_old          Drought Code for previous hour
#' @return                Drought Codes for given data
.hdc <- function(w, dc_old)
{
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
  dc <- copy(w)
  stopifnot("SUNSET" %in% colnames(w))
  stopifnot("SUNRISE" %in% colnames(w))
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
    noon <- for_date[HR == 12]
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

#' Calculate Hourly Grass Fuel Moisture. Needs to be converted to get GFMC.
#'
#' @param temp            Temperature (Celcius)
#' @param rh              Relative Humidity (percent, 0-100)
#' @param wind            Wind Speed (km/h)
#' @param rain            Precipitation (mm)
#' @param lastmc          Previous grass fuel moisture (percent)
#' @param solrad          Solar radiation (kW/m^2)
#' @param time            Time since last observation (hours)
#' @return                Grass Fuel Moisture (percent)
hourly_gfmc <- function(temp, rh, wind, rain, lastmc, solrad, time)
{
  # MARK II of the model (2016) wth new solar rad model specific to grass
  # #
  # # Temp is temperature in C
  # # RH is realtive humidty in %
  # # wind is average wind speed in km/h
  # # rain is rainfall in mm
  # # solrad is kW/m2  (radiaiton reaching fuel)
  # # mo is the old grass fuel moisture   (not as a code value...so elimates the conversion to code)
  # # time - time between obs in HOURS
  # #
  # #
  # # DRF of 1/16.1 comes from reducting the standard response time curve
  # # at 26.7C, 20%RH, 2 km/h to 0.85hr.
  # #
  # #
  # #
  # # bmw
  # drf <- 0.389633
  # mo <- lastmc;
  # # fuel temperature/humidity
  # tf <- temp+17.9*solrad*exp(-0.034*wind);   #fuel temp from CEVW
  # if(tf>temp)
  # {
  #   rhf <- rh*6.107*10.0^(7.5*temp/(temp+237.0) )/(6.107*10.0^(7.5*tf/(tf+237.0) ))
  # } else {
  #   rhf <- rh
  # }
  # if(rain!=0)
  # {
  #   # /*     mo+=rain*rf*exp(-100.0/(251.0-mo))*(1.0-exp(-6.93/rain));*/  /* old routine*/
  #   # /* this new routine assumes layer is 0.3 kg/m2 so 0.3mm of rain adds +100%MC*/
  #   mo <- mo+ rain/0.3*100.0   #/* *100 to convert to %...  *1/.3 because of 0.3mm=100%  */
  #   if(mo>250.0)
  #   {
  #     mo <- 250.0
  #   }
  # }
  # ed <- 1.62*rhf^0.532+(13.7*exp( (rhf-100)/13.0))+0.27*(26.7-tf)*(1.0-1.0/exp(0.115*rhf));   #/*GRASS EMC*/
  # moed <- mo-ed;
  # ew <- 1.42*rhf^0.512+(12.0*exp((rhf-100)/18.0))+0.27*(26.7-tf)*(1.0-1.0/exp(0.115*rhf));     #/*GRASS EMC*/
  # moew <- mo-ew;
  # if (moed==0 || (moew>=0 && moed<0))
  # {
  #   xm <- mo
  #   if(moed==0)
  #   {
  #     e <- ed
  #   }
  #   if(moew>=0)
  #   {
  #     e <- ew
  #   }
  # } else {
  #   if( moed>0)
  #   {
  #     a1 <- rhf/100
  #     e <- ed
  #     moe <- moed
  #   } else {
  #     a1 <- (100.0-rhf)/100.0
  #     e <- ew
  #     moe <- moew
  #   }
  #   xkd <- (0.424*(1-a1^1.7)+(0.0694*sqrt(wind)*(1-a1^8)))
  #   xkd <- xkd*drf*exp(0.0365*tf)
  #   # //   printf("tf=%8.4f rhf=%6.2f e=%4.1f mo=%5.2f xkd=%6.4f moed=%5.1f moew=%5.1f\n",tf,rhf,e,mo,xkd,moed,moew);
  #   xm <- e+moe*exp(-1.0*log(10.0)*xkd*time)
  # }
  return(1)
}


#' Calculate Grass Spread Index (GSI)
#'
#' @param wind            Wind Speed (km/h)
#' @param mc              Grass moisture content (percent)
#' @param cur             Degree of curing (percent, 0-100)
#' @return                Grass Spread Index
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

#' Calculate Grass Fire Weather Index
#'
#' @param gsi               Grass Spread Index
#' @param load              Fuel Load (kg/m^2)
#' @return                  Grass Fire Weather Index
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

#' Calculate hourly FWI indices from hourly weather stream for a single station.
#'
#' @param     w               hourly values weather stream
#' @param     timezone        integer offset from GMT to use for sun calculations
#' @param     ffmc_old        previous value for Fine Fuel Moisture Code
#' @param     dmc_old         previous value for Duff Moisture Code
#' @param     dc_old          previous value for Drought Code
#' @param     percent_cured   Grass curing (percent, 0-100)
#' @param     PET_DMC         Logical indicating if DMC should be PET-based or Original
#' @return                    hourly values FWI and weather stream


.stnHFWI <- function(w, timezone, ffmc_old, dmc_old, dc_old, percent_cured,
########################################################################################################
############################### DMC- UPDATE ###########################################################
## Pass logical dictating whether to caclulate DMC with new PET-based approach or original approach             
                     PET_DMC = F)
############################### DMC- UPDATE ###########################################################
{
  if (!isSequentialHours(w))
  {
    stop('Expected input to be sequential hourly weather')
  }
  if (length(na.omit(unique(w$ID))) != 1)
  {
    stop('Expected a single ID value for input weather')
  }
  if (length(na.omit(unique(w$LAT))) != 1)
  {
    stop('Expected a single LAT value for input weather')
  }
  if (length(na.omit(unique(w$LONG))) != 1)
  {
    stop('Expected a single LONG value for input weather')
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
  sunlight <- getSunlight(dates, temprange = r$temprange,timezone, latitude, longitude)
  
  ###########################################################################################
  ###########################################################################################
  setnames(sunlight, c("DATE"), c("TIMESTAMP"))
  sunlight$TIMESTAMP <- as_datetime(sunlight$TIMESTAMP)
  r <- merge(r, sunlight, by=c("TIMESTAMP", "LAT", "LONG"))
  # print(r[(nrow(r)-10):nrow(r),])
  maxsolprop <- 0.85
  grassfuelload <- 0.35
  # print(r[(nrow(r)-10):nrow(r),])
  # print("FFMC")
  r$FFMC <- rep(1,nrow(r))#.hffmc(r, ffmc_old)
  # print("DMC")
  
  r$DMC <- .hdmc(r, dmc_old,
 ########################################################################################################
 ############################### DMC- UPDATE ###########################################################
## Pass logical dictating whether to caclulate DMC with new PET-based approach or original approach             
                 PET_DMC = PET_DMC)
  ############################### DMC- UPDATE ###########################################################
  # print("DC")
  r$DC <- rep(1,nrow(r))#.hdc(r, dc_old)
  r[, ISI := 1]#ISIcalc(WS, FFMC)]
  r[, BUI := 1]#BUIcalc(DMC, DC)]
  r[, FWI := 1]#FWIcalc(ISI, BUI)]
  # taken from package code
  r[, DSR := 0.0272 * (FWI ^ 1.77)]
  r[, MIN_RH := min(RH), by=c("DATE")]
  r[, MIN_RH := ifelse(100==MIN_RH, 99.5, MIN_RH)]
  # r[, SOLPROP := max(0, maxsolprop * ifelse(min(99.5, MIN_RH) > 30, (1.27 - 0.0111 * RH), 1))]
  # r[, SOLPROP := ifelse(MIN_RH > 30, (1.27 - 0.0111 * RH), 1)]
  r[, SOLPROP := ifelse(MIN_RH > 30, (1.27 - 0.0111 * MIN_RH), 1)]
  r[, SOLPROP := ifelse(SOLPROP < 0, 0, maxsolprop * SOLPROP)]
  r[, SOLRAD := SOLRAD * SOLPROP]
  # lastmcgmc <- 101-ffmc_old # approximation for a start up
  # r$MCGMC <- reduce_by_row(
  #   function(lastmcgmc, n){
  #     row <- r[n,]
  #     return(hourly_gfmc(row$TEMP, row$RH, row$WS, row$PREC, lastmcgmc, row$SOLRAD, 1.0))
  #     },
  #   r,
  #   lastmcgmc)
  # r[, GFMC := 59.5 * (250 - MCGMC) / (147.2772277 + MCGMC)]
  # r[, GSI := grassISI(WS, MCGMC, percent_cured)]
  # r[, GFWI := grassFWI(GSI, grassfuelload)]
  return(r)
}

#' Calculate hourly FWI indices from hourly weather stream.
#'
#' @param     weatherstream   hourly values weather stream
#' @param     timezone        integer offset from GMT to use for sun calculations
#' @param     ffmc_old        previous value for Fine Fuel Moisture Code
#' @param     dmc_old         previous value for Duff Moisture Code
#' @param     dc_old          previous value for Drought Code
#' @param     percent_cured   Grass curing (percent, 0-100)
#' @param     PET_DMC         Logical indicating if DMC should be PET-based or Original
#' @return                    hourly values FWI and weather stream
#' @export hFWI
hFWI <- function(weatherstream, timezone, ffmc_old=85, dmc_old=6, dc_old=15, percent_cured=100.0,
 ########################################################################################################
############################### DMC- UPDATE ###########################################################
## Pass logical dictating whether to caclulate DMC with new PET-based approach or original approach             
                 PET_DMC = F)
  ############################### DMC- UPDATE ###########################################################
{
  wx <- copy(weatherstream)
  stopifnot(all(wx$RH >= 0 & wx$RH <= 100))
  stopifnot(all(wx$WS >= 0))
  stopifnot(all(wx$PREC >= 0))
  stopifnot(all(wx$MON >= 1 & wx$MON <= 12))
  stopifnot(all(wx$DAY >= 1 & wx$DAY <= 31))
  stopifnot(ffmc_old >= 0 & ffmc_old <= 101)
  stopifnot(dmc_old >= 0)
  stopifnot(dc_old >= 0)
  stopifnot(percent_cured >= 0 & percent_cured <= 100)
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
  wasWind <- 'WIND' %in% colnames(wx)
  wasRain <- 'RAIN' %in% colnames(wx)
  wasYear <- 'YEAR' %in% colnames(wx)
  wasHour <- 'HOUR' %in% colnames(wx)
  if (!hadStn)
  {
    wx[, ID := 'STN']
  }
  if (!hadMinute)
  {
    wx[, MINUTE := 0]
  }
  if (!hadLatitude)
  {
    warning(paste0("Using default latitude value of ", DEFAULT_LATITUDE))
    wx[, LAT := DEFAULT_LATITUDE]
  }
  if (!hadLongitude)
  {
    warning(paste0("Using default longitude value of ", DEFAULT_LONGITUDE))
    wx[, LONG := DEFAULT_LONGITUDE]
  }
  if (wasWind)
  {
    setnames(wx, c("WIND"), c("WS"))
  }
  if (wasRain)
  {
    setnames(wx, c("RAIN"), c("PREC"))
  }
  if (wasYear)
  {
    setnames(wx, c("YEAR"), c("YR"))
  }
  if (wasHour)
  {
    setnames(wx, c("HOUR"), c("HR"))
  }
  if (!hadDate)
  {
    wx[, DATE := as.character(as.Date(sprintf('%04d-%02d-%02d', YR, MON, DAY)))]
  }
  if (!hadTimestamp)
  {
    wx[, TIMESTAMP := as_datetime(sprintf('%04d-%02d-%02d %02d:%02d:00', YR, MON, DAY, HR, MINUTE))]
  }
  # loop in hFWI function
  results <- NULL
  for (stn in unique(wx$ID))
  {
    by_stn <- wx[ID == stn]
    for (yr in unique(by_stn$YR))
    {
      by_year <- by_stn[YR == yr,]
      print(paste0("Running ", stn, " for ", yr))

      r <- .stnHFWI(by_year, timezone, ffmc_old, dmc_old, dc_old, percent_cured,
 ########################################################################################################
############################### DMC- UPDATE ###########################################################
## Pass logical dictating whether to caclulate DMC with new PET-based approach or original approach             
                    PET_DMC = PET_DMC)
############################### DMC- UPDATE ###########################################################)
      results <- rbind(results, r)
    }
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
    if (wasWind)
    {
      setnames(results, c("WS"), c("WIND"))
    }
    if (wasRain)
    {
      setnames(results, c("PREC"), c("RAIN"))
    }
    if (wasYear)
    {
      setnames(results, c("YR"), c("YEAR"))
    }
    if (wasHour)
    {
      setnames(results, c("HR"), c("HOUR"))
    }
    setnames(results, new_names, old_names)
  }
  # should have gotten rid of all the fields we added to make the processing work
  return(results)
}


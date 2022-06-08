library(lubridate)
library(data.table)

# FIX: figure out what this should be
DEFAULT_LATITUDE <- 55.0
DEFAULT_LONGITUDE <- -120.0

# default startup values
FFMC_DEFAULT <- 85
DMC_DEFAULT <- 6
DC_DEFAULT <- 15

reduce_by_row <- function(fct, df, init)
{
  result <- Reduce(fct, seq(1, nrow(df)), init, accumulate=TRUE)
  # get rid of init value
  return(result[2:length(result)])
}

ISIcalc <- function(ws, ffmc)
{
  fm <- 147.2773 * (101.0 - ffmc)/(59.5 + ffmc)
  sf <- 19.115 * exp(-0.1386 * fm) * (1.0 + fm^5.31 / 4.93e07)
  isi <- sf * exp(0.05039 * ws)
  return(isi)
}

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

FWIcalc <- function (isi, bui) 
{
  bb <- ifelse(bui > 80, 0.1 * isi * (1000/(25 + 108.64/exp(0.023 * 
                                                              bui))), 0.1 * isi * (0.626 * (bui^0.809) + 2))
  fwi <- ifelse(bb <= 1, bb, exp(2.72 * ((0.434 * log(bb))^0.647)))
  return(fwi)
}

hourly_ffmc <- function (weatherstream, ffmc_old = 85, time.step = 1, calc.step = FALSE, 
                   batch = TRUE) 
{
  t0 <- time.step
  names(weatherstream) <- tolower(names(weatherstream))
  if (batch) {
    if ("id" %in% names(weatherstream)) {
      n <- length(unique(weatherstream$id))
      if (length(unique(weatherstream[1:n, "id"])) != 
          n) {
        stop("Multiple stations have to start and end at the same dates/time, \n             and the data must be sorted by date/time and id")
      }
    }
    else {
      n <- 1
    }
  }
  else {
    n <- nrow(weatherstream)
  }
  if (length(ffmc_old) == 1 & n > 1) {
    Fo <- rep(ffmc_old, n)
  }
  else {
    Fo <- ffmc_old
  }
  Tp <- weatherstream$temp
  H <- weatherstream$rh
  W <- weatherstream$ws
  ro <- weatherstream$prec
  if (calc.step) {
    hr <- weatherstream$hr
    if (!exists("hr") | is.null(hr)) 
      warning("hour value is missing!")
  }
  if (!exists("Tp") | is.null(Tp)) 
    warning("temperature (temp) is missing!")
  if (!exists("ro") | is.null(ro)) 
    warning("precipitation (prec) is missing!")
  if (!exists("W") | is.null(W)) 
    warning("wind speed (ws) is missing!")
  if (!exists("H") | is.null(H)) 
    warning("relative humidity (rh) is missing!")
  if (length(H)%%n != 0) 
    warning("Weatherstream do not match with number of weather stations")
  n0 <- length(H)/n
  f <- NULL
  for (i in 1:n0) {
    k <- ((i - 1) * n + 1):(i * n)
    if (calc.step & i > 1) {
      t0 <- ifelse(n0 > 1, hr[k] - hr[k - n], t0)
      t0 <- ifelse(t0 == -23, 1, t0)
      t0 <- ifelse(t0 < 0, -1 * t0, t0)
    }
    mo <- 147.27723 * (101 - Fo)/(59.5 + Fo)
    rf <- ro[k]
    mr <- ifelse(mo <= 150, mo + 42.5 * rf * exp(-100/(251 - 
                                                         mo)) * (1 - exp(-6.93/rf)), mo + 42.5 * rf * exp(-100/(251 - 
                                                                                                                  mo)) * (1 - exp(-6.93/rf)) + 0.0015 * ((mo - 150)^2) * 
                   (rf^0.5))
    mr <- ifelse(mr > 250, 250, mr)
    mo <- ifelse(ro[k] > 0, mr, mo)
    Ed <- 0.942 * (H[k]^0.679) + 11 * exp((H[k] - 100)/10) + 
      0.18 * (21.1 - Tp[k]) * (1 - exp(-0.115 * H[k]))
    ko <- 0.424 * (1 - (H[k]/100)^1.7) + 0.0694 * (W[k]^0.5) * 
      (1 - (H[k]/100)^8)
    kd <- ko * 0.0579 * exp(0.0365 * Tp[k])
    md <- Ed + (mo - Ed) * (10^(-kd * t0))
    Ew <- 0.618 * (H[k]^0.753) + 10 * exp((H[k] - 100)/10) + 
      0.18 * (21.1 - Tp[k]) * (1 - exp(-0.115 * H[k]))
    k1 <- 0.424 * (1 - ((100 - H[k])/100)^1.7) + 0.0694 * 
      (W[k]^0.5) * (1 - ((100 - H[k])/100)^8)
    kw <- k1 * 0.0579 * exp(0.0365 * Tp[k])
    mw <- Ew - (Ew - mo) * (10^(-kw * t0))
    m <- ifelse(mo > Ed, md, mw)
    m <- ifelse(Ed >= mo & mo >= Ew, mo, m)
    Fo <- 59.5 * (250 - m)/(147.27723 + m)
    Fo <- ifelse(Fo <= 0, 0, Fo)
    f <- c(f, Fo)
  }
  return(f)
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

getSunlight <- function(dates, timezone, latitude, longitude, verbose=FALSE)
{
  df <- data.table(DATE=dates)
  df[, d := as_date(DATE)]
  dechour <- 12.0
  df[, jd := julian(month(d), day(d))]
  df[, fracyear := 2.0*pi/365.0*( jd-1.0+(dechour-12.0)/24.0)]
  df[, eqtime := 229.18*( 0.000075+ 0.001868*cos(fracyear) - 0.032077*sin (fracyear) - 0.014615*cos(2.0*fracyear) - 0.040849*sin(2.0*fracyear) )]
  df[, decl := 0.006918-0.399912*cos(fracyear) + 0.070257*sin(fracyear) - 0.006758*cos(fracyear*2.0)+0.000907*sin(2.0*fracyear) - 0.002697*cos(3.0*fracyear) + 0.00148*sin(3.0*fracyear)]
  df[, timeoffset := eqtime+4*longitude-60*timezone]
  df[, zenith := 90.833*pi/180.0]
  df[, halfday := 180.0/pi*acos( cos(zenith)/(cos(latitude*pi/180.0)*cos(decl))-tan(latitude*pi/180.0)*tan(decl) )]
  df[, sunrise := (720.0-4.0*(longitude+halfday)-eqtime)/60+timezone]
  df[, sunset := (720.0-4.0*(longitude-halfday)-eqtime)/60+timezone]
  df[, hr := hour(DATE)]
  df[, tst := as.numeric(hr)*60.0+timeoffset]
  df[, hourangle := tst/4-180]
  df[, zenith := acos(sin(latitude*pi/180)*sin(decl)+cos(latitude*pi/180)*cos(decl)*cos(hourangle*pi/180) )]
  df[, solrad := 0.95*cos(zenith)]
  df[, solrad := ifelse(solrad < 0, 0, solrad)]
  colnames(df) <- toupper(colnames(df))
  df[, LAT := latitude]
  df[, LONG := longitude]
  result <- df[, c("DATE", "LAT", "LONG", "SOLRAD", "SUNRISE", "SUNSET")]
  result[, SUNLIGHT_HOURS := SUNSET - SUNRISE]
  return(result)
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
  
  for_ffmc$FFMC <- hourly_ffmc(for_ffmc, ffmc_old=ffmc_old)
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


.hdmc <- function(w, dmc_old)
{
    dmc <- copy(w)
    stopifnot("SUNSET" %in% colnames(w))
    stopifnot("SUNRISE" %in% colnames(w))
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

.hdc <- function(w, dc_old)
{
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

#' Calculate hourly FWI indices from hourly weather stream for a single station.
#' 
#' @param     w               hourly values weather stream
#' @param     timezone        integer offset from GMT to use for sun calculations
#' @param     ffmc_old        previous value for Fine Fuel Moisture Code
#' @param     dmc_old         previous value for Duff Moisture Code
#' @param     dc_old          previous value for Drought Code
#' @param     percent_cured   Grass curing (percent, 0-100)
#' @return                    hourly values FWI and weather stream
.stnHFWI <- function(w, timezone, ffmc_old, dmc_old, dc_old, percent_cured)
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
  sunlight <- getSunlight(dates, timezone, latitude, longitude, TRUE)
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
  
  r[, ISI := ISIcalc(WS, FFMC)]
  r[, BUI := BUIcalc(DMC, DC)]
  r[, FWI := FWIcalc(ISI, BUI)]
  # taken from package code
  r[, DSR := 0.0272 * (FWI ^ 1.77)]
  
  r[, MIN_RH := min(RH), by=c("DATE")]
  # r[, SOLPROP := max(0, maxsolprop * ifelse(min(99.5, MIN_RH) > 30, (1.27 - 0.0111 * RH), 1))]
  # r[, SOLPROP := ifelse(MIN_RH > 30, (1.27 - 0.0111 * RH), 1)]
  r[, SOLPROP := ifelse(MIN_RH > 30, (1.27 - 0.0111 * MIN_RH), 1)]
  r[, SOLPROP := ifelse(SOLPROP < 0, 0, maxsolprop * SOLPROP)]
  r[, SOLRAD := SOLRAD * SOLPROP]
  
  lastmcgmc <- 101-ffmc_old # approximation for a start up
  r$MCGMC <- reduce_by_row(
    function(lastmcgmc, n){
      row <- r[n,]
      return(hourly_gfmc(row$TEMP, row$RH, row$WS, row$PREC, lastmcgmc, row$SOLRAD, 1.0))
      },
    r,
    lastmcgmc)
  r[, GFMC := 59.5 * (250 - MCGMC) / (147.2772277 + MCGMC)]
  r[, GSI := grassISI(WS, MCGMC, percent_cured)]
  r[, GFWI := grassFWI(GSI, grassfuelload)]
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
#' @return                    hourly values FWI and weather stream
#' @export minmax_to_hourly
hFWI <- function(weatherstream, timezone, ffmc_old=85, dmc_old=6, dc_old=15, percent_cured=100.0)
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
    wx[, LAT := DEFAULT_LATITUDE]
  }
  if (!hadLongitude)
  {
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
    w <- wx[ID == stn]
    r <- .stnHFWI(w, timezone, ffmc_old, dmc_old, dc_old, percent_cured)
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


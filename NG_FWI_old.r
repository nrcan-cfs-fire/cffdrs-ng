source("NG_FWI.r")

#' Calculate hourly Fine Fuel Moisture Code (FFMC)
#'
#' @param w               Table of hourly weather data to use for calculations [TIMESTAMP, DATE, TEMP, RH, WS, PREC]
#' @param ffmc_old        Fine Fuel Moisture Code for previous hour
#' @return                Fine Fuel Moisture Codes for given data
.hffmc_old <- function(w, ffmc_old)
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
#' @return                Duff Moisture Codes for given data
.hdmc_old <- function(w, dmc_old)
{
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

#' Calculate hourly Drought Code (DC)
#'
#' @param w               Table of hourly weather data to use for calculations [TIMESTAMP, DATE, SUNRISE, SUNSET, MON, TEMP, RH, PREC]
#' @param dc_old          Drought Code for previous hour
#' @return                Drought Codes for given data
.hdc_old <- function(w, dc_old)
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

#' Calculate hourly FWI indices from hourly weather stream for a single station.
#'
#' @param     w               hourly values weather stream
#' @param     timezone        integer offset from GMT to use for sun calculations
#' @param     ffmc_old        previous value for Fine Fuel Moisture Code
#' @param     dmc_old         previous value for Duff Moisture Code
#' @param     dc_old          previous value for Drought Code
#' @param     percent_cured   Grass curing (percent, 0-100)
#' @return                    hourly values FWI and weather stream
.stnHFWI_old <- function(w, timezone, ffmc_old, dmc_old, dc_old, percent_cured)
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
  sunlight <- getSunlight(dates, timezone, latitude, longitude)
  setnames(sunlight, c("DATE"), c("TIMESTAMP"))
  sunlight$TIMESTAMP <- as_datetime(sunlight$TIMESTAMP)
  r <- merge(r, sunlight, by=c("TIMESTAMP", "LAT", "LONG"))
  # print(r[(nrow(r)-10):nrow(r),])
  maxsolprop <- 0.85
  grassfuelload <- 0.35
  # print(r[(nrow(r)-10):nrow(r),])
  # print("FFMC")
  r$FFMC <- .hffmc_old(r, ffmc_old)
  # print("DMC")
  r$DMC <- .hdmc_old(r, dmc_old)
  # print("DC")
  r$DC <- .hdc_old(r, dc_old)
  r[, ISI := ISIcalc(WS, FFMC)]
  r[, BUI := BUIcalc(DMC, DC)]
  r[, FWI := FWIcalc(ISI, BUI)]
  # taken from package code
  r[, DSR := 0.0272 * (FWI ^ 1.77)]
  r[, MIN_RH := min(RH), by=c("DATE")]
  r[, MIN_RH := ifelse(100==MIN_RH, 99.5, MIN_RH)]
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
#' @export hFWI
hFWI_old <- function(weatherstream, timezone, ffmc_old=85, dmc_old=6, dc_old=15, percent_cured=100.0)
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
    w <- wx[ID == stn]
    r <- .stnHFWI_old(w, timezone, ffmc_old, dmc_old, dc_old, percent_cured)
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


#' Computes hourly FWI indices for an input hourly weather stream
library(lubridate)
library(data.table)
source("util.r")
#source("old_cffdrs.r")

DAILY_K_DMC_DRYING <- 1.894
DAILY_K_DC_DRYING <- 3.937

HOURLY_K_DMC <- 2.22
# HOURLY_K_DC <- 0.017066
# HOURLY_K_DMC <- 0.27
HOURLY_K_DC <- 0.085
DMC_OFFSET_TEMP <- 0.0
DC_OFFSET_TEMP <- 0.0

DC_DAILY_CONST <- 0.36
DC_HOURLY_CONST <- DC_DAILY_CONST / DAILY_K_DC_DRYING


OFFSET_SUNRISE <- 0 ##2.5
OFFSET_SUNSET <- 0 ##0.5

# Fuel Load (kg/m^2)
DEFAULT_GRASS_FUEL_LOAD <- 0.35

# default startup values
FFMC_DEFAULT <- 85
DMC_DEFAULT <- 6
DC_DEFAULT <- 15
GFMC_DEFAULT <- FFMC_DEFAULT 

# FIX: figure out what this should be
DEFAULT_LATITUDE <- 55.0
DEFAULT_LONGITUDE <- -120.0

# # just apply "daily" indices to noon directly
# HOUR_TO_START_FROM <- 12
# # result seemed to match better at noon so try starting from there instead
# # # start with daily indices at peak burn
# # HOUR_TO_START_FROM <- 16

MPCT_TO_MC <- 250.0 * 59.5 / 101.0
FFMC_INTERCEPT <- 0.5
DMC_INTERCEPT <- 1.5
DC_INTERCEPT <- 2.8

DATE_GRASS <- 181

# Fine Fuel Moisture Code (FFMC) from moisture %
fine_fuel_moisture_code <- function(moisture_percent) {
  return((59.5 * (250 - moisture_percent) / (MPCT_TO_MC + moisture_percent)))
}

# Fine Fuel Moisture (%) from FFMC
fine_fuel_moisture_from_code <- function(moisture_code) {
  return(MPCT_TO_MC * (101 - moisture_code) / (59.5 + moisture_code))
}

#' Calculate hourly Fine Fuel Moisture Code (FFMC)
#'
#' @param temp            Temperature (Celcius)
#' @param rh              Relative Humidity (percent, 0-100)
#' @param ws              Wind Speed (km/h)
#' @param rain            Rainfall (mm)
#' @param lastmc          Previous Fine Fuel Moisture (%)
#' @return                Hourly Fine Fuel Moisture (%)
hourly_fine_fuel_moisture <- function(temp, rh, ws, rain, lastmc) {
  # cur <- r[i + 1]
  # temp <- cur$temp
  # rh <- cur$rh
  # ws <- cur$ws
  # rain <- cur$prec
  # lastmc <- mcffmc
  # # 3.3,94.0,3.0,0.0,16.4
  # # 16.43770866,16.43770866,3.20393529,29.83789869,27.60476102,27.60476102
  # # 0.06000000,0.54065437,0.03531092,17.30973068
  rf <- 42.5
  drf <- 0.0579
  # Time since last observation (hours)
  time <- 1.0
  # use moisture directly instead of converting to/from ffmc
  # expects any rain intercept to already be applied
  mo <- lastmc
  if (rain != 0.0) {
    # duplicated in both formulas, so calculate once
    # lastmc == mo, but use lastmc since mo changes after first equation
    mo <- mo + rf * rain * exp(-100.0 / (251 - lastmc)) * (1.0 - exp(-6.93 / rain))
    if (lastmc > 150) {
      mo <- mo + 0.0015 * ((lastmc - 150)^2) * sqrt(rain)
    }
    if (mo > 250) {
      mo <- 250
    }
  }
  # duplicated in both formulas, so calculate once
  e1 <- 0.18 * (21.1 - temp) * (1.0 - (1.0 / exp(0.115 * rh)))
  ed <- 0.942 * (rh^0.679) + (11.0 * exp((rh - 100) / 10.0)) + e1
  ew <- 0.618 * (rh^0.753) + (10.0 * exp((rh - 100) / 10.0)) + e1
  # m = ed if mo >= ed else ew
  m <- ifelse(mo < ed,
    ew,
    ed
  )
  if (mo != ed) {
    # these are the same formulas with a different value for a1
    a1 <- ifelse(mo > ed,
      rh / 100.0,
      (100.0 - rh) / 100.0
    )
    k0_or_k1 <- 0.424 * (1 - (a1^1.7)) + (0.0694 * sqrt(ws) * (1 - (a1^8)))
    kd_or_kw <- (1.0/0.50)*drf * k0_or_k1 * exp(0.0365 * temp)
    m <- m + (mo - m) * (10^(-kd_or_kw * time))
  }
  return(m)
}

#' Calculate Initial Spread Index (ISI)
#'
#' @param ws              Wind Speed (km/h)
#' @param ffmc            Fine Fuel Moisure Code
#' @return                Initial Spread Index
initial_spread_index <- function(ws, ffmc) {
  fm <- fine_fuel_moisture_from_code(ffmc)
  fw <- ifelse(40 <= ws,
    12 * (1 - exp(-0.0818 * (ws - 28))),
    exp(0.05039 * ws)
  )
  ff <- 91.9 * exp(-0.1386 * fm) * (1.0 + (fm^5.31) / 4.93e07)
  isi <- 0.208 * fw * ff
  return(isi)
}

#' Calculate Build-up Index (BUI)
#'
#' @param dmc             Duff Moisture Code
#' @param dc              Drought Code
#' @return                Build-up Index
buildup_index <- function(dmc, dc) {
  bui <- ifelse(dmc == 0 & dc == 0,
    0,
    0.8 * dc * dmc / (dmc + 0.4 * dc)
  )
  # use ifelse so table works still
  bui <- ifelse(bui < dmc,
    {
      p <- (dmc - bui) / dmc
      cc <- 0.92 + ((0.0114 * dmc)^1.7)
      dmc - cc * p
    },
    bui
  )
  bui <- ifelse(bui <= 0, 0, bui)
  return(bui)
}

#' Calculate Fire Weather Index (FWI)
#'
#' @param isi             Initial Spread Index
#' @param bui             Build-up Index
#' @return                Fire Weather Index
fire_weather_index <- function(isi, bui) {
  bb <- (0.1 * isi
    * ifelse(bui > 80,
      1000 / (25 + 108.64 / exp(0.023 * bui)),
      0.626 * (bui^0.809) + 2
    )
  )
  fwi <- ifelse(bb <= 1,
    bb,
    exp(2.72 * ((0.434 * log(bb))^0.647))
  )
  return(fwi)
}

daily_severity_rating <- function(fwi) {
  return(0.0272 * (fwi^1.77))
}

#' Calculate Hourly Grass Fuel Moisture. Needs to be converted to get GFMC.
#'
#' @param temp            Temperature (Celcius)
#' @param rh              Relative Humidity (percent, 0-100)
#' @param ws              Wind Speed (km/h)
#' @param rain            Rainfall (mm)
#' @param lastmc          Previous grass fuel moisture (percent)
#' @param solrad          Solar radiation (kW/m^2)
#' @return                Grass Fuel Moisture (percent)
hourly_grass_fuel_moisture <- function(temp, rh, ws, rain, solrad, lastmc) {
  # MARK II of the model (2016) wth new solar rad model specific to grass
  #
  # Temp is temperature in C
  # RH is realtive humidty in %
  # ws is average wind speed in km/h
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

  rf <- 0.27
  drf <- 0.389633
  # Time since last observation (hours)
  time <- 1.0
  mo <- lastmc
  mo <- ifelse(rain != 0,
    {
      #     mo+=rain*rf*exp(-100.0/(251.0-mo))*(1.0-exp(-6.93/rain))*/ # old routine*/
      # this new routine assumes layer is 0.3 kg/m2 so 0.3mm of rain adds +100%MC*/
      # *100 to convert to %...  *1/.3 because of 0.3mm=100%
      mo <- mo + (rain / 0.3 * 100.0)
      mo <- ifelse(mo > 250.0, 250.0, mo)
      mo
    },
    mo
  )

  # fuel temp from CEVW*/
  tf <- temp + 17.9 * solrad * exp(-0.034 * ws)
  
  # fuel humidity
  rhf <- ifelse(tf > temp,
    (rh * 6.107 * (10.0^(7.5 * temp / (temp + 237.0)))
      / (6.107 * (10.0^(7.5 * tf / (tf + 237.0))))),
    rh
  )
  
  # 18.85749879,18.85749879,7.77659602,21.24361786,19.22479551,19.22479551
  # duplicated in both formulas, so calculate once
  e1 <- rf * (26.7 - tf) * (1.0 - (1.0 / exp(0.115 * rhf)))
  # GRASS EMC
  ed <- 1.62 * (rhf^0.532) + (13.7 * exp((rhf - 100) / 13.0)) + e1
  ew <- 1.42 * (rhf^0.512) + (12.0 * exp((rhf - 100) / 18.0)) + e1
  
  moed <- mo - ed
  moew <- mo - ew
  
  
  e <- NULL
  a1 <- NULL
  m <- NULL
  moe <- NULL
  if (moed == 0 || (moew >= 0 && moed <0)){
    m <- mo
    if(moed == 0){
      e <- ed
    }
    if (moew >= 0){
      e <- ew
    }
  }
  else{
    if(moed > 0){
      a1 <- rhf/100.0
      e <- ed
      moe <- moed
    }
    else{
      a1 <- (100.0-rhf)/100.0
      e <- ew
      moe <- moew
    }
    if (a1 < 0){
      #avoids complex number in a1^1.7 xkd calculation
      a1 <- 0
    }
    xkd <- (0.424*(1-a1^1.7)+(0.0694*sqrt(ws)*(1-a1^8)))
    xkd <- xkd*drf*exp(0.0365*tf)
    m <- e+moe*exp(-1.0*log(10.0)*xkd*time)
  }
  
  
#  m <- ifelse(mo < ed && mo < ew,
#    ew,
#    ed
#  )
#  print(temp)
#  print(solrad)
#  print(ws)
#  print(rh)
#  # use ifelse so table works
#  print(m)
  
#  m <- ifelse(mo > ed || (mo < ed && mo < ew),
#    {
#      # these are the same formulas with a different value for a1
#      a1 <- ifelse(mo > ed,
#        rhf / 100.0,
#        (100.0 - rhf) / 100.0
#      )
#      k0_or_k1 <- 0.424 * (1 - (a1^1.7)) + (0.0694 * sqrt(ws) * (1 - (a1^8)))
#      kd_or_kw <- drf * k0_or_k1 * exp(0.0365 * tf)
#      m + (mo - m) * (10^(-kd_or_kw * time))
#      m
#    },
#    m
#  )

  return(m)
}


Pign <- function(mc, wind2m, Cint, Cmc, Cws) {
  #  Thisd is the general standard form for the probability of sustained flaming models for each FF cover type
  #     here :
  #       mc is cured moisture (%) in the litter fuels being ignited
  #       wind2m (km/h)  is the estimated 2 metre standard height for wind at hte site of the fire ignition
  #       Cint, Cmc and Cws   are coefficients for the standard Pign model form for a given FF cover type

  #       return >> is the Probability of Sustained flaming from a single small flaming ember/brand
  Prob <- 1.0 / (1.0 + exp(-1.0 * (Cint + Cmc * mc + Cws * wind2m)))

  return(Prob)
}

curing_factor <- function(cur) {
  # cur is the percentage cure of the grass fuel complex.  100= fully cured
  #   ....The OPPOSITE (100-x) of greenness...

  #    This is the Cruz et al (2015) model with the original precision of the coefficent estimates
  #    and as in CSIRO code:https://research.csiro.au/spark/resources/model-library/csiro-grassland-models/
  cf <- ifelse(cur >= 20.0,
    1.036 / (1 + 103.989 * exp(-0.0996 * (cur - 20))),
    0.0
  )
  return(cf)
}




grass_moisture_code <- function(mc, cur, wind) {
  #   THIS is the way to get the CODE value from cured grassland moisture
  #     IT takes fully cured grass moisture  (from the grass moisture model (100% cured)  (from the FMS...updated version of Wotton 2009)
  #        and a estimate of the fuel complex curing (as percent cured)
  #        and estimated wind speed (necessary for a calc
  #     and calculated the probability of sustainable flaming ignition  (a funciton of MC  and wind)
  #     THEN it accounts for curing effect on probability of fire spread sustainability, using the curing factor from Cruz et al (2015) for grass
  #     THEN from this calcuates an 'effective moisture content' which is the moisture content that would be required to achieve
  #        the curing adjusted probabiltiy of sustained flaming if one were calcuating it directly through the standard Pign equation.
  #     and THEN converts this effective moisture content to a CODE value via the FF-scale the FFMC uses for consistency

  #     relies on models of:
  #        Prob of sustained flaming for grass model (PsusF(grass)
  #        and  the curing_factor  function
  #        AND and estiamte of open 10 m to 2 m wind reduction (0.75)...hardcoded in here now.....

  # MC is moisture content (%)
  # cur=percent curing of the grassland  (%)
  # wind=  10 m open wind (km/h)

  #     currently (NOv 2023) the coefficients for the PsusF(grass) models are hardcoded into the GFMC function
  wind2m_open_factor <- 0.75

  Intercept <- 1.49
  Cmoisture <- -0.11
  Cwind <- 0.075
  # GRASS: these coefficients (above) could change down the road .....explicitly coded in above*/
  # /* convert from 10 m wind in open to 2 m wind in open COULD be updated */
  wind2m <- wind2m_open_factor * wind

  probign <- Pign(mc, wind2m, Intercept, Cmoisture, Cwind)

  # /* adjust ignition diretctly with the curing function on ROS */
  newPign <- curing_factor(cur) * probign

  # /* now to back calc effective moisture - algebraically reverse the Pign equation*/
  # /* 250 is a saturation value just a check*/
  egmc <- ifelse(newPign > 0.0,
    (log(newPign / (1.0 - newPign)) - Intercept - Cwind * wind2m) / Cmoisture,
    250
  )
  # /*   convert to code with FF-scale */
  # return (59.5*(250.0-egmc)/(MPCT_TO_MC + egmc))
  if (egmc > 250.0){
    egmc <- 250.0
  }
  return(fine_fuel_moisture_code(egmc))
}


matted_grass_spread_ROS <- function(ws, mc, cur) {
  #  /*  CUT grass  Rate  of spread from cheney 1998  (and new CSIRO grassland code
  #   We use this for MATTED grass in our post-winter context
  #   --ws=10 m open wind km/h
  #   --mc = moisture content in  cured grass  (%)
  #   --cur = percentage of grassland cured  (%)
  #   output should be ROS in m/min   */
  fw <- 16.67 * ifelse(ws < 5,
    0.054 + 0.209 * ws,
    1.1 + 0.715 * (ws - 5.0)**0.844
  )
  fm <- ifelse(mc < 12,
    exp(-0.108 * mc),
    ifelse(mc < 20.0 && ws < 10.0,
      0.6838 - 0.0342 * mc,
      ifelse(mc < 23.9 && ws >= 10.0,
        0.547 - 0.0228 * mc,
        0.0
      )
    )
  )
  if (fm < 0){
    fm <- 0.0
  }
  cf <- curing_factor(cur)
  return(fw * fm * cf)
}


standing_grass_spread_ROS <- function(ws, mc, cur) {
  #  /*  standing grass  Rate  of spread from cheney 1998  (and new CSIRO grassland code)
  #   We use this for standing grass in our post-winter context
  #   ITS only the WIND function that chnges here between cut and standing
  #   --ws=10 m open wind km/h
  #   --mc = moisture content in grass  (%)
  #   --cur = percentage of grassland cured  (%)
  #   output should be ROS in m/min   */
  fw <- 16.67 * ifelse(ws < 5,
    0.054 + 0.269 * ws,
    1.4 + 0.838 * (ws - 5.0)**0.844
  )
  print_out <- c(mc,ws)
  print(print_out)
  fm <- ifelse(mc < 12,
    exp(-0.108 * mc),
    ifelse(mc < 20.0 && ws < 10.0,
      0.6838 - 0.0342 * mc,
      ifelse(mc < 23.9 && ws >= 10.0,
        0.547 - 0.0228 * mc,
        0.0
      )
    )
  )
  if (fm < 0){
    fm <- 0.0
  }
  cf <- curing_factor(cur)
  return(fw * fm * cf)
}


#' Calculate Grass Spread Index (GSI)
#'
#' @param ws              Wind Speed (km/h)
#' @param mc              Grass moisture content (percent)
#' @param cur             Degree of curing (percent, 0-100)
#' @param standing        Grass standing (True/False)
#' @return                Grass Spread Index
grass_spread_index <- function(ws, mc, cur, standing) {
  #  So we don't have to transition midseason between standing and matted grass spread rate models
  #  We will simply scale   GSI   by the average of the   matted and standing spread rates
  #ros <- (matted_grass_spread_ROS(ws, mc, cur) + standing_grass_spread_ROS(ws, mc, cur)) / 2.0
  
  
  #now allowing switch between standing and matted grass
  ros <- NULL
  if (standing){
    #standing
    ros <- standing_grass_spread_ROS(ws, mc, cur)
  }
  else {
    #matted
    ros <- matted_grass_spread_ROS(ws, mc, cur)
  }
  
  return(1.11 * ros)
}


#' Calculate Grass Fire Weather Index
#'
#' @param gsi               Grass Spread Index
#' @param load              Fuel Load (kg/m^2)
#' @return                  Grass Fire Weather Index
grass_fire_weather_index <- Vectorize(function(gsi, load) {
  #  this just converts back to ROS in m/min
  ros <- gsi / 1.11
  Fint <- 300.0 * load * ros
  return(ifelse(Fint > 100,
    log(Fint / 60.0) / 0.14,
    Fint / 25.0
  ))
})

dmc_wetting <- function(rain_total, lastdmc) {
  if (rain_total <= DMC_INTERCEPT) {
    # no wetting if below intercept threshold
    return(0.0)
  }
  b <- ifelse(lastdmc <= 33,
    100.0 / (0.5 + 0.3 * lastdmc),
    ifelse(lastdmc <= 65,
      14.0 - 1.3 * log(lastdmc),
      6.2 * log(lastdmc) - 17.2
    )
  )
  rw <- 0.92 * rain_total - 1.27
  wmi <- 20 + 280 / exp(0.023 * lastdmc)
  wmr <- wmi + 1000 * rw / (48.77 + b * rw)
  dmc <- 43.43 * (5.6348 - log(wmr - 20))
  if (dmc <= 0.0) {
    dmc <- 0.0
  }
  # total amount of wetting since lastdmc
  w <- lastdmc - dmc
  return(w)
}

dc_wetting <- function(rain_total, lastdc) {
  if (rain_total <= DC_INTERCEPT) {
    # no wetting if below intercept threshold
    return(0.0)
  }
  rw <- 0.83 * rain_total - 1.27
  smi <- 800 * exp(-lastdc / 400)
  return(400.0 * log(1.0 + 3.937 * rw / smi))
  # # total amount of wetting since lastdc
  # w <- 400.0 * log(1.0 + 3.937 * rw / smi)
  # # don't wet more than lastdc regardless of drying since then
  # if (w > lastdc) {
  #   w <- lastdc
  # }
  # return(w)
}

dmc_wetting_between <- function(rain_total_previous, rain_total, lastdmc) {
  if (rain_total_previous >= rain_total) {
    return(0.0)
  }
  # wetting is calculated based on initial dmc when rain started and rain since
  current <- dmc_wetting(rain_total, lastdmc)
  # recalculate instead of storing so we don't need to reset this too
  # NOTE: rain_total_previous != (rain_total - cur$prec) due to floating point math
  previous <- dmc_wetting(rain_total_previous, lastdmc)
  return(current - previous)
}

dc_wetting_between <- function(rain_total_previous, rain_total, lastdc) {
  if (rain_total_previous >= rain_total) {
    return(0.0)
  }
  # wetting is calculated based on initial dc when rain started and rain since
  current <- dc_wetting(rain_total, lastdc)
  # recalculate instead of storing so we don't need to reset this too
  # NOTE: rain_total_previous != (rain_total - cur$prec) due to floating point math
  previous <- dc_wetting(rain_total_previous, lastdc)
  return(current - previous)
}

dmc_drying_ratio <- function(temp, rh) {
  return(pmax(0.0, HOURLY_K_DMC * (temp + DMC_OFFSET_TEMP) * (100.0 - rh) * 0.0001))
}

duff_moisture_code <- function(
    last_dmc,
    temp,
    rh,
    ws,
    rain,
    mon,
    hour,
    solrad,
    sunrise,
    sunset,
    dmc_before_rain,
    rain_total_prev,
    rain_total) {
  if (0 == rain_total) {
    dmc_before_rain <- last_dmc
  }
  # apply wetting since last period
  dmc_wetting_hourly <- dmc_wetting_between(
    rain_total_prev,
    rain_total,
    dmc_before_rain
  )
  # at most apply same wetting as current value (don't go below 0)
  dmc <- pmax(0.0, last_dmc - dmc_wetting_hourly)
  sunrise_start <- round(sunrise + OFFSET_SUNRISE)
  sunset_start <- round(sunset + OFFSET_SUNSET)
  dmc_hourly <- ifelse(hour >= sunrise_start & hour < sunset_start,
    dmc_drying_ratio(temp, rh),
    0.0
  )
  dmc <- dmc + dmc_hourly
  # HACK: return two values since C uses a pointer to assign a value
  return(list(dmc = dmc, dmc_before_rain = dmc_before_rain))
}

dc_drying_hourly <- function(temp) {
  return(pmax(0.0, HOURLY_K_DC * (temp + DC_OFFSET_TEMP)))
}



drought_code <- function(
    last_dc,
    temp,
    rh,
    ws,
    rain,
    mon,
    hour,
    solrad,
    sunrise,
    sunset,
    dc_before_rain,
    rain_total_prev,
    rain_total) {

##########################################################################################
  #### for now we are using Mike's method for calculating DC
  
  
  if (0 == rain_total){
    dc_before_rain <- last_dc
  }
  
  offset <- 3.0
  mult <- 0.015
  pe <- 0
  rw <- 0
  mr <- 0
  mcdc <- 0
  
  last_mc_dc <- 400*exp(-last_dc/400)
  TIME_INCREMENT <- 1.0
  if(temp > 0){
    pe <- mult*temp + offset/16.0
  }
  
  invtau <- pe/400.0
  
  if((rain_total_prev + rain) <= 2.8){
    mr <- last_mc_dc
  }
  else {
    if(rain_total_prev <= 2.8){
      rw <- (rain_total_prev + rain)*0.83 - 1.27
    }
    else{
      rw <- rain*0.83
    }
    mr <- last_mc_dc + 3.937*rw/2.0
  }
  
  if(mr > 400.0){
    mr <- 400.0
  }
  
  is_daytime <- FALSE
  if(hour >= sunrise && hour <= sunset){
    is_daytime = TRUE
  }
  
  if(is_daytime){
    mcdc <- 0.0 + (mr+0.0)*exp(-1.0*TIME_INCREMENT*invtau)
  }
  else{
    mcdc <- mr
  }
  
  if(mcdc > 400.0){
    mcdc <- 400.0
  }
  
  dc <- 400.0*log(400.0/mcdc)
  
  
  return(list(dc = dc, dc_before_rain = dc_before_rain))
  ##########################################################################################

  
  if (0 == rain_total) {
    dc_before_rain <- last_dc
  }
  # apply wetting since last period
  dc_wetting_hourly <- dc_wetting_between(rain_total_prev, rain_total, dc_before_rain)
  # at most apply same wetting as current value (don't go below 0)
  dc <- pmax(0.0, last_dc - dc_wetting_hourly)
  dc_hourly <- dc_drying_hourly(temp)
  # print(sprintf("last_dc=%0.2f, dc_wetting_hourly=%0.2f, dc=%0.2f, dc_hourly=%0.2f\n",
  #        last_dc,
  #        dc_wetting_hourly,
  #        dc,
  #        dc_hourly))
  dc <- dc + dc_hourly
  # HACK: return two values since C uses a pointer to assign a value
  return(list(dc = dc, dc_before_rain = dc_before_rain))
}


# Calculate number of drying "units" this hour contributes
drying_units <- function(temp, rh, ws, rain, solrad) {
  # for now, just add 1 drying "unit" per hour
  return(1.0)
}

rain_since_intercept_reset <- function(temp,
                                       rh,
                                       ws,
                                       rain,
                                       mon,
                                       hour,
                                       solrad,
                                       sunrise,
                                       sunset,
                                       canopy) {
  # for now, want 5 "units" of drying (which is 1 per hour to start)
  TARGET_DRYING_SINCE_INTERCEPT <- 5.0
  if (0 < rain) {
    # no drying if still raining
    canopy$drying_since_intercept <- 0.0
  } else {
    canopy$drying_since_intercept <- canopy$drying_since_intercept + drying_units(temp, rh, ws, rain, solrad)
    if (canopy$drying_since_intercept >= TARGET_DRYING_SINCE_INTERCEPT) {
      # reset rain if intercept reset criteria met
      canopy$rain_total <- 0.0
      canopy$drying_since_intercept <- 0.0
    }
  }
  canopy$rain_total_prev <- canopy$rain_total
  canopy$rain_total <- canopy$rain_total + rain
  return(canopy)
}



#' Calculate hourly FWI indices from hourly weather stream for a single station.
#'
#' @param     w               hourly values weather stream
#' @param     ffmc_old        previous value for Fine Fuel Moisture Code
#' @param     dmc_old         previous value for Duff Moisture Code
#' @param     dc_old          previous value for Drought Code
#' @return                    hourly values FWI and weather stream
.stnHFWI <- function(w, ffmc_old, dmc_old, dc_old) {
  
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
  r <- as.data.table(copy(w))
  names(r) <- tolower(names(r))
  mcffmc <- fine_fuel_moisture_from_code(ffmc_old)
  mcgfmc <- mcffmc
  mcgfmc_standing <- mcffmc
  mcgfmc_matted <- mcffmc
  # just use previous index values from current hour regardless of time
  # # HACK: always start from daily value at noon
  # while (12 != r[1]$hr) {
  #   r <- r[2:nrow(r)]
  # }
  # cur <- r[1]
  # dmc_old <- daily_duff_moisture_code(dmc_old, cur$temp, cur$rh, cur$prec, cur$lat, cur$mon)
  # dc_old <- daily_drought_code(dc_old, cur$temp, cur$rh, cur$prec, cur$lat, cur$mon)
  # # HACK: start from when daily value should be "accurate"
  # prec_accum <- 0.0
  # while (HOUR_TO_START_FROM != r[1]$hr) {
  #   # tally up precip between noon and whenever we're applying the indices
  #   prec_accum <- prec_accum + r[1]$prec
  #   r <- r[2:nrow(r)]
  # }
  # cur <- r[1]
  # # HACK: add precip tally to current hour so it doesn't get omitted
  # cur$prec <- cur$prec + prec_accum
  dmc_ <- list(dmc = dmc_old, dmc_before_rain = dmc_old)
  dc_ <- list(dc = dc_old, dc_before_rain = dc_old)
  # FIX: just use loop for now so it matches C code
  canopy <- list(
    rain_total = 0.0,
    rain_total_prev = 0.0,
    drying_since_intercept = 0.0
  )
  results <- NULL
  N <- nrow(r)
  for (i in 1:N)
  {
    
    cur <- copy(r[i])
    canopy <- rain_since_intercept_reset(
      cur$temp,
      cur$rh,
      cur$ws,
      cur$prec,
      cur$mon,
      cur$hr,
      cur$solrad,
      cur$sunrise,
      cur$sunset,
      canopy
    )
    # use lesser of remaining intercept and current hour's rain
    rain_ffmc <- ifelse(canopy$rain_total <= FFMC_INTERCEPT,
      0.0,
      ifelse((canopy$rain_total - FFMC_INTERCEPT) > cur$prec,
        cur$prec,
        canopy$rain_total - FFMC_INTERCEPT
      )
    )
    mcffmc <- hourly_fine_fuel_moisture(cur$temp, cur$rh, cur$ws, rain_ffmc, mcffmc)
    cur$mcffmc <- mcffmc
    #  convert to code for output, but keep using moisture % for precision
    cur$ffmc <- fine_fuel_moisture_code(mcffmc)
    # not ideal, but at least encapsulates the code for each index
    dmc_ <- duff_moisture_code(
      dmc_$dmc,
      cur$temp,
      cur$rh,
      cur$ws,
      cur$prec,
      cur$mon,
      cur$hr,
      cur$solrad,
      cur$sunrise,
      cur$sunset,
      dmc_$dmc_before_rain,
      canopy$rain_total_prev,
      canopy$rain_total
    )
    cur$dmc <- dmc_$dmc
    dc_ <- drought_code(
      dc_$dc,
      cur$temp,
      cur$rh,
      cur$ws,
      cur$prec,
      cur$mon,
      cur$hr,
      cur$solrad,
      cur$sunrise,
      cur$sunset,
      dc_$dc_before_rain,
      canopy$rain_total_prev,
      canopy$rain_total
    )
    cur$dc <- dc_$dc
    cur$isi <- initial_spread_index(cur$ws, cur$ffmc)
    cur$bui <- buildup_index(cur$dmc, cur$dc)
    cur$fwi <- fire_weather_index(cur$isi, cur$bui)
    cur$dsr <- daily_severity_rating(cur$fwi)
    
    mcgfmc_matted <- hourly_grass_fuel_moisture(cur$temp, cur$rh, cur$ws, cur$prec, cur$solrad, mcgfmc_matted)
    #for standing grass we make a come very simplifying assumptions based on obs from the field (echo bay study):
    #standing not really affected by rain -- to introduce some effect we introduce just a simplification of the FFMC Rain absorption function
    #which averages 6% or so for rains  (<5mm...between 7% and 5%,    lower for larger rains)(NO intercept)
    #AND the solar radiation exposure is less, and the cooling from the wind is stronger.  SO we assume there is effectively no extra
    #heating of the grass from solar
    #working at the margin like this should make a nice bracket for moisture between the matted and standing that users can use
    #...reality will be in between the matt and stand
    mcgfmc_standing <- hourly_grass_fuel_moisture(cur$temp, cur$rh, cur$ws, cur$prec*0.06, 0.0, mcgfmc_standing)
    #mcgfmc <- hourly_grass_fuel_moisture(cur$temp, cur$rh, cur$ws, cur$prec, cur$solrad, mcgfmc)
    #print(mcgfmc)
    mcgfmc <- mcgfmc_standing
    standing <- TRUE
    if (julian(cur$mon, cur$day) < DATE_GRASS){
      standing <- FALSE
      mcgfmc <- mcgfmc_matted
    }

    
    cur$mcgfmc <- mcgfmc
    cur$gfmc <- grass_moisture_code(mcgfmc, cur$percent_cured, cur$ws)
    
    cur$gsi <- grass_spread_index(cur$ws, mcgfmc, cur$percent_cured, standing)
    cur$gfwi <- grass_fire_weather_index(cur$gsi, DEFAULT_GRASS_FUEL_LOAD)
    cur$grass_fuel_load <- DEFAULT_GRASS_FUEL_LOAD
    results <- rbind(results, cur)
  }
  return(results)
}

#' Calculate hourly FWI indices from hourly weather stream.
#'
#' @param     df_wx           hourly values weather stream
#' @param     timezone        integer offset from GMT to use for sun calculations
#' @param     ffmc_old        previous value for Fine Fuel Moisture Code
#' @param     dmc_old         previous value for Duff Moisture Code
#' @param     dc_old          previous value for Drought Code
#' @return                    hourly values FWI and weather stream
#' @export hFWI
hFWI <- function(
  df_wx,
  timezone,
  ffmc_old = FFMC_DEFAULT,
  dmc_old = DMC_DEFAULT,
  dc_old = DC_DEFAULT
  ) {
  # check df_wx class for data.frame or data.table
  wasDf <- class(df_wx) == "data.frame"
  if (wasDf) {
    wx <- as.data.table(copy(df_wx))
  } else if (class(df_wx) == "data.table") {
    wx <- copy(df_wx)
  } else {
    stop("Input weather stream df_wx needs to be a data.frame or data.table!")
  }
  # check for allowed alternative names for: ws, prec, yr, hr
  colnames(wx) <- toupper(colnames(wx))
  og_names <- names(wx)
  wasWind <- (!"WS" %in% og_names) && "WIND" %in% og_names
  wasRain <- (!"PREC" %in% og_names) && "RAIN" %in% og_names
  wasYear <- (!"YR" %in% og_names) && "YEAR" %in% og_names
  wasHour <- (!"HR" %in% og_names) && "HOUR" %in% og_names
  if (wasWind) {
    setnames(wx, c("WIND"), c("WS"))
  }
  if (wasRain) {
    setnames(wx, c("RAIN"), c("PREC"))
  }
  if (wasYear) {
    setnames(wx, c("YEAR"), c("YR"))
  }
  if (wasHour) {
    setnames(wx, c("HOUR"), c("HR"))
  }
  # check for required columns
  stopifnot(all(c('YR', 'MON', 'DAY', 'HR', 'TEMP', 'RH', 'WS', 'PREC') %in% names(wx)))
  # check for optional columns that have a default
  if (!"LAT" %in% og_names) {
    warning(paste0("Using default latitude value of ", DEFAULT_LATITUDE))
    wx[, LAT := DEFAULT_LATITUDE]
  }
  if (!"LONG" %in% og_names) {
    warning(paste0("Using default longitude value of ", DEFAULT_LONGITUDE))
    wx[, LONG := DEFAULT_LONGITUDE]
  }
  # add dummy columns if they don't exist
  hadStn <- "ID" %in% og_names
  hadMinute <- "MINUTE" %in% og_names
  if (!hadStn) {
    wx[, ID := "STN"]
  }
  if (!hadMinute) {
    wx[, MINUTE := 0]
  }
  # check for optional columns that can be calculated
  hadDate <- "DATE" %in% og_names
  hadTimestamp <- "TIMESTAMP" %in% og_names
  if (!hadDate) {
    wx[, DATE := as.character(as.Date(sprintf("%04d-%02d-%02d", YR, MON, DAY)))]
  }
  if (!hadTimestamp) {
    wx[, TIMESTAMP := as_datetime(sprintf("%04d-%02d-%02d %02d:%02d:00", YR, MON, DAY, HR, MINUTE))]
  }
  if (!"PERCENT_CURED" %in% og_names) {
    wx$PERCENT_CURED <- seasonal_curing(julian(wx$MON, wx$DAY))
  }
  if (!"SOLRAD" %in% og_names) {
    needs_solrad <- TRUE
  } else {
    needs_solrad <- FALSE
  }
  # check for unnecessary columns
  cols_extra_solar <- intersect(names(wx), c("SUNRISE", "SUNSET", "SUNLIGHT_HOURS"))
  if (0 < length(cols_extra_solar)) {
    warning(sprintf("Ignoring and recalculating columns: [%s]", paste0(cols_extra_solar, collapse = ", ")))
    wx <- wx[, -..cols_extra_solar]
  }
  # check for values outside valid ranges
  stopifnot(all(wx$RH >= 0 & wx$RH <= 100))
  stopifnot(all(wx$WS >= 0))
  stopifnot(all(wx$PREC >= 0))
  stopifnot(all(wx$MON >= 1 & wx$MON <= 12))
  stopifnot(all(wx$DAY >= 1 & wx$DAY <= 31))
  stopifnot(wx$SOLRAD >= 0)
  stopifnot(wx$GRASS_FUEL_LOAD >= 0)
  stopifnot(wx$PERCENT_CURED >= 0 & wx$PERCENT_CURED <= 100)
  stopifnot(ffmc_old >= 0 & ffmc_old <= 101)
  stopifnot(dmc_old >= 0)
  stopifnot(dc_old >= 0)

  # loop over every station year
  results <- NULL
  for (stn in unique(wx$ID)) {
    by_stn <- wx[ID == stn]
    for (yr in unique(by_stn$YR)) {
      by_year <- by_stn[YR == yr, ]
      print(paste0("Running ", stn, " for ", yr))
      # FIX: convert this to not need to do individual stations
      by_year[, TIMEZONE := timezone]
      w <- getSunlight(by_year, get_solrad = needs_solrad)
      r <- .stnHFWI(w, ffmc_old, dmc_old, dc_old)
      results <- rbind(results, r)
    }
  }

  # remove optional variables that we added
  names(results) <- toupper(names(results))
  if (!hadStn) {
    results <- results[, -c("ID")]
  }
  if (!hadMinute) {
    results <- results[, -c("MINUTE")]
  }
  if (!hadDate) {
    results <- results[, -c("DATE")]
  }
  if (!hadTimestamp) {
    results <- results[, -c("TIMESTAMP")]
  }
  # revert to alternative name if used
  if (wasWind) {
    setnames(results, c("WS"), c("WIND"))
  }
  if (wasRain) {
    setnames(results, c("PREC"), c("RAIN"))
  }
  if (wasYear) {
    setnames(results, c("YR"), c("YEAR"))
  }
  if (wasHour) {
    setnames(results, c("HR"), c("HOUR"))
  }
  names(results) <- tolower(names(results))
  if (wasDf) {
    results <- as.data.frame(results)
  }
  return(results)
}

# so this can be run via Rscript
if ("--args" %in% commandArgs()) {
  # parser <- ArgumentParser()
  # parser$add_argument()
  args <- commandArgs(trailingOnly = TRUE)
  if (6 == length(args)) {
    # args <- c("-6", "85", "6", "15", "./out/wx_diurnal_r.csv", "./out/wx_diurnal_fwi_r.csv")
    timezone <- as.double(args[1])
    ffmc_old <- as.double(args[2])
    dmc_old <- as.double(args[3])
    dc_old <- as.double(args[4])
    file_in <- args[5]
    file_out <- args[6]
    df_wx <- as.data.table(read.csv(file_in))
    df_fwi <- hFWI(
      df_wx,
      timezone = timezone,
      ffmc_old = ffmc_old,
      dmc_old = dmc_old,
      dc_old = dc_old
    )
    # reorganize columns
    colnames_out <- c(
      "lat",
      "long",
      "yr",
      "mon",
      "day",
      "hr",
      "temp",
      "rh",
      "ws",
      "prec",
      "date",
      "timestamp",
      "timezone",
      "solrad",
      "sunrise",
      "sunset",
      "ffmc",
      "dmc",
      "dc",
      "isi",
      "bui",
      "fwi",
      "dsr",
      "gfmc",
      "gsi",
      "gfwi",
      "mcffmc",
      "mcgfmc",
      "percent_cured",
      "grass_fuel_load"
    )
    print(colnames_out)
    if ("id" %in% names(df_fwi)) {
      colnames_out <- c("id", colnames_out)
    }
    df_fwi <- df_fwi[, ..colnames_out]
    save_csv(df_fwi, file_out)
  } else {
    if(args[1] != "SILENCE"){
      message("Wrong number of arguments") 
    }
  }
}

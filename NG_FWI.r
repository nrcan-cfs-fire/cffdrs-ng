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
FFMC_DEFAULT <- 85.0
DMC_DEFAULT <- 6.0
DC_DEFAULT <- 15.0
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

# Fine Fuel Moisture Code (FFMC) to fine fuel moisture content (%) conversion
ffmc_to_mcffmc <- function(ffmc) {
  return(MPCT_TO_MC * (101 - ffmc) / (59.5 + ffmc))
}

# fine fuel moisture content (%) to FFMC
mcffmc_to_ffmc <- function(mcffmc) {
  return(59.5 * (250 - mcffmc) / (MPCT_TO_MC + mcffmc))
}

# Duff Moisture Code (DMC) to duff moisture content (%)
dmc_to_mcdmc <- function(dmc) {
   return((280 / exp(dmc / 43.43)) + 20)
}

# duff moisture content (%) to DMC
mcdmc_to_dmc <- function(mcdmc) {
   return(43.43 * log(280 / (mcdmc - 20)))
}

# Drought Code (DC) to DC moisture content(%)
dc_to_mcdc <- function(dc) {
   return(400 * exp(-dc / 400))
}

# DC moisture content (%) to DC
mcdc_to_dc <- function(mcdc) {
   return(400 * log(400 / mcdc))
}

#' Calculate hourly fine fuel moisture content. Needs to be converted to get FFMC
#'
#' @param lastmc          Previous fine fuel moisture content (%)
#' @param temp            Temperature (Celcius)
#' @param rh              Relative Humidity (percent, 0-100)
#' @param ws              Wind Speed (km/h)
#' @param rain            Rainfall (mm)
#' @param time_increment  Duration of timestep (hr, default 1.0)
#' @return                Hourly fine fuel moisture content (%)
hourly_fine_fuel_moisture <- function(lastmc, temp, rh, ws, rain, time_increment = 1.0) {
  rf <- 42.5
  drf <- 0.0579
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
  m <- ifelse(mo < ed, ew, ed)
  if (mo != ed) {
    # these are the same formulas with a different value for a1
    a1 <- ifelse(mo > ed, rh / 100.0, (100.0 - rh) / 100.0)
    k0_or_k1 <- 0.424 * (1 - (a1^1.7)) + (0.0694 * sqrt(ws) * (1 - (a1^8)))
    kd_or_kw <- (1.0 / 0.50) * drf * k0_or_k1 * exp(0.0365 * temp)
    m <- m + (mo - m) * (10^(-kd_or_kw * time_increment))
  }
  return(m)
}

#' Calculate duff moisture content
#'
#' @param last_mcdmc             Previous duff moisture content (%)
#' @param hr                     Time of day (hr)
#' @param temp                   Temperature (Celcius)
#' @param rh                     Relative Humidity (%)
#' @param prec                   Hourly precipitation (mm)
#' @param sunrise                Sunrise (hr)
#' @param sunset                 Sunset (hr)
#' @param prec_cumulative_prev   Cumulative precipitation since start of rain (mm)
#' @param time_increment         Duration of timestep (hr, default 1.0)
#' @return                       Hourly duff moisture content (%)
duff_moisture_code <- function(
  last_mcdmc,
  hr,
  temp,
  rh,
  prec,
  sunrise,
  sunset,
  prec_cumulative_prev,
  time_increment = 1.0) {
  # wetting
  if (prec_cumulative_prev + prec > DMC_INTERCEPT) {  # prec_cumulative above threshold
    if (prec_cumulative_prev < DMC_INTERCEPT) {  # just passed threshold
      rw <- (prec_cumulative_prev + prec) * 0.92 - 1.27
    } else {
      rw <- prec * 0.92
    }

    last_dmc <- mcdmc_to_dmc(last_mcdmc)
    if (last_dmc <= 33) {
      b <- 100.0 / (0.3 * last_dmc + 0.5)
    } else if (last_dmc <= 65) {
      b <- -1.3 * log(last_dmc) + 14.0
    } else {
      b <- 6.2 * log(last_dmc) - 17.2
    }

    mr <- last_mcdmc + (1e3 * rw) / (b * rw + 48.77)
  } else {
    mr <- last_mcdmc
  }

  if (mr > 300.0) {
    mr <- 300.0
  }

  # drying
  sunrise_start <- sunrise + OFFSET_SUNRISE
  sunset_start <- sunset + OFFSET_SUNSET
  if (hr >= sunrise_start && hr <= sunset_start) {  # daytime
    if (temp < 0) {
      temp <- 0.0
    }
    rk <- HOURLY_K_DMC * 1e-4 * (temp + DMC_OFFSET_TEMP) * (100.0 - rh)
    invtau <- rk / 43.43
    mcdmc <- (mr - 20.0) * exp(-time_increment * invtau) + 20.0
  } else {  # nighttime
    mcdmc <- mr
  }

  if (mcdmc > 300.0) {
    mcdmc <- 300.0
  }

  return(mcdmc)
}

#' Calculate drought code moisture content
#'
#' @param last_mcdc              Previous drought code moisture content (%)
#' @param hr                     Time of day (hr)
#' @param temp                   Temperature (Celcius)
#' @param prec                   Hourly precipitation (mm)
#' @param sunrise                Sunrise (hr)
#' @param sunset                 Sunset (hr)
#' @param prec_cumulative_prev   Cumulative precipitation since start of rain (mm)
#' @param time_increment         Duration of timestep (hr, default 1.0)
#' @return                       Hourly drought code moisture content (%)
drought_code <- function(
  last_mcdc,
  hr,
  temp,
  prec,
  sunrise,
  sunset,
  prec_cumulative_prev,
  time_increment = 1.0) {
  # wetting
  if (prec_cumulative_prev + prec > DC_INTERCEPT) {  # prec_cumulative above threshold
    if (prec_cumulative_prev <= DC_INTERCEPT) {  # just passed threshold
      rw <- (prec_cumulative_prev + prec) * 0.83 - 1.27
    } else {  # previously passed threshold
      rw <- prec * 0.83
    }
    mr <- last_mcdc + 3.937 * rw / 2.0
  } else {
    mr <- last_mcdc
  }

  if (mr > 400.0) {
    mr <- 400.0
  }

  # drying
  sunrise_start <- sunrise + OFFSET_SUNRISE
  sunset_start <- sunset + OFFSET_SUNSET
  if (hr >= sunrise_start && hr <= sunset_start) {  # daytime
    offset <- 3.0
    mult <- 0.015
    if (temp > 0) {
      pe <- mult * temp + offset / 16.0
    } else {
      pe <- 0
    }
    invtau <- pe / 400.0
    mcdc <- mr * exp(-time_increment * invtau)
  } else {  # nighttime
    mcdc <- mr
  }

  if (mcdc > 400.0) {
    mcdc <- 400.0
  }

  return(mcdc)
}

#' Calculate Initial Spread Index (ISI)
#'
#' @param ws              Wind Speed (km/h)
#' @param ffmc            Fine Fuel Moisure Code
#' @return                Initial Spread Index
initial_spread_index <- function(ws, ffmc) {
  fm <- ffmc_to_mcffmc(ffmc)
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
#' @param lastmc          Previous grass fuel moisture (percent)
#' @param temp            Temperature (Celcius)
#' @param rh              Relative Humidity (percent, 0-100)
#' @param ws              Wind Speed (km/h)
#' @param rain            Rainfall (mm)
#' @param solrad          Solar radiation (kW/m^2)
#' @param load            Grassland Fuel Load (kg/m^2)
#' @return                Grass Fuel Moisture (percent)
hourly_grass_fuel_moisture <- function(
  lastmc,
  temp,
  rh,
  ws,
  rain,
  solrad,
  load,
  time_increment = 1.0) {
  # MARK II of the model (2016) wth new solar rad model specific to grass
  #
  # DRF of 1/16.1 comes from reducting the standard response time curve
  # at 26.7C, 20%RH, 2 km/h to 0.85hr.
  #
  # bmw

  rf <- 0.27
  drf <- 0.389633
  # use moisture directly instead of converting to/from ffmc
  # expects any rain intercept to already be applied
  mo <- lastmc
  if (rain != 0) {
    mo <- mo + (rain / load * 100.0)
    if (mo > 250.0) {
      mo <- 250.0
    }
  }
  # fuel temp from CEVW*/
  tf <- temp + 17.9 * solrad * exp(-0.034 * ws)
  # fuel humidity
  if (tf > temp) {
    rhf <- (rh * 6.107 * (10.0^(7.5 * temp / (temp + 237.0))) /
      (6.107 * (10.0^(7.5 * tf / (tf + 237.0)))))
  } else {
    rhf <- rh
  }
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

  if (moed == 0 || (moew >= 0 && moed < 0)) {
    m <- mo
    if (moed == 0) {
      e <- ed
    }
    if (moew >= 0) {
      e <- ew
    }
  } else {
    if (moed > 0) {
      a1 <- rhf / 100.0
      e <- ed
      moe <- moed
    } else {
      a1 <- (100.0 - rhf) / 100.0
      e <- ew
      moe <- moew
    }
    if (a1 < 0) {
      #avoids complex number in a1^1.7 xkd calculation
      a1 <- 0
    }
    xkd <- 0.424 * (1 - a1^1.7) + (0.0694 * sqrt(ws) * (1 - a1^8))
    xkd <- xkd * drf * exp(0.0365 * tf)
    m <- e + moe * exp(-1.0 * log(10.0) * xkd * time_increment)
  }
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




mcgfmc_to_gfmc <- function(mc, cur, wind) {
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
  return(mcffmc_to_ffmc(egmc))
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
  # print_out <- c(mc,ws)
  # print(print_out)
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


#' Calculate Grassland Spread Index (GSI)
#'
#' @param ws              Wind Speed (km/h)
#' @param mc              Grass moisture content (percent)
#' @param cur             Degree of curing (percent, 0-100)
#' @param standing        Grass standing (True/False)
#' @return                Grassland Spread Index
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


#' Calculate Grassland Fire Weather Index
#'
#' @param gsi               Grassland Spread Index
#' @param load              Grassland Fuel Load (kg/m^2)
#' @return                  Grassland Fire Weather Index
grass_fire_weather_index <- Vectorize(function(gsi, load) {
  # this just converts back to ROS in m/min
  ros <- gsi / 1.11
  Fint <- 300.0 * load * ros
  if (Fint > 100) {
    return(log(Fint / 60.0) / 0.14)
  } else {
    return(Fint / 25.0)
  }
})

# Calculate number of drying "units" this hour contributes
drying_units <- function() {  # temp, rh, ws, rain, solrad
  # for now, just add 1 drying "unit" per hour
  return(1.0)
}

rain_since_intercept_reset <- function(rain, canopy) {
  # for now, want 5 "units" of drying (which is 1 per hour to start)
  TARGET_DRYING_SINCE_INTERCEPT <- 5.0
  if (rain > 0 || canopy$rain_total_prev == 0) {  # if raining, reset drying
    canopy$drying_since_intercept <- 0.0
  } else {
    canopy$drying_since_intercept <- canopy$drying_since_intercept + drying_units()
    if (canopy$drying_since_intercept >= TARGET_DRYING_SINCE_INTERCEPT) {
      # reset rain if intercept reset criteria met
      canopy$rain_total_prev <- 0.0
      canopy$drying_since_intercept <- 0.0
    }
  }
  return(canopy)
}



#' Calculate hourly FWI indices from hourly weather stream for a single station.
#'
#' @param    w                    hourly values weather stream
#' @param    ffmc_or_mcffmc_old   previous value for FFMC or mcffmc
#' @param    is_mcffmc            is above a value of mcffmc or FFMC
#' @param    dmc_old              previous value for DMC
#' @param    dc_old               previous value for DC
#' @param    mcgfmc_matted_old    previous value for matted mcgfmc
#' @param    mcgfmc_standing_old  previous value for standing mcgfmc
#' @param    dmc_before_rain      DMC before rainfall
#' @param    dc_before_rain       DC before rainfall
#' @param    prec_cumulative      cumulative precipitation this rainfall
#' @param    canopy_drying        consecutive hours of no rain
#' @return                        hourly values FWI and weather stream
.stnHFWI <- function(
  w,
  ffmc_or_mcffmc_old,
  is_mcffmc,
  dmc_old,
  dc_old,
  mcgfmc_matted_old,
  mcgfmc_standing_old,
  prec_cumulative,
  canopy_drying) {
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
  if (length(na.omit(unique(w$GRASS_FUEL_LOAD))) != 1) {
    stop("Expected a single GRASS_FUEL_LOAD value")
  }
  r <- as.data.table(copy(w))
  names(r) <- tolower(names(r))
  if (!is_mcffmc) {
    mcffmc <- ffmc_to_mcffmc(ffmc_or_mcffmc_old)
  } else {
    mcffmc <- ffmc_or_mcffmc_old
  }
  mcgfmc_matted <- mcgfmc_matted_old
  mcgfmc_standing <- mcgfmc_standing_old
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
  mcdmc <- dmc_to_mcdmc(dmc_old)
  mcdc <- dc_to_mcdc(dc_old)
  # FIX: just use loop for now so it matches C code
  canopy <- list(rain_total_prev = prec_cumulative,
    drying_since_intercept = canopy_drying)
  results <- NULL
  N <- nrow(r)
  for (i in 1:N) {
    cur <- copy(r[i])
    canopy <- rain_since_intercept_reset(cur$prec, canopy)
    # determine rain for ffmc and whether or not intercept should happen now
    if (canopy$rain_total_prev + cur$prec <= FFMC_INTERCEPT) {  # not enough rain
      rain_ffmc <- 0.0
    } else if (canopy$rain_total_prev > FFMC_INTERCEPT) {  # already saturated canopy
      rain_ffmc <- cur$prec
    } else {
      rain_ffmc <- canopy$rain_total_prev + cur$prec - FFMC_INTERCEPT
    }

    # rain_ffmc <- ifelse(canopy$rain_total <= FFMC_INTERCEPT,
    #   0.0,
    #   ifelse((canopy$rain_total - FFMC_INTERCEPT) > cur$prec,
    #     cur$prec,
    #     canopy$rain_total - FFMC_INTERCEPT
    #   )
    # )
    mcffmc <- hourly_fine_fuel_moisture(mcffmc, cur$temp, cur$rh, cur$ws, rain_ffmc)
    cur$mcffmc <- mcffmc
    # convert to code for output, but keep using moisture % for precision
    cur$ffmc <- mcffmc_to_ffmc(mcffmc)
    # not ideal, but at least encapsulates the code for each index
    mcdmc <- duff_moisture_code(
      mcdmc,
      cur$hr,
      cur$temp,
      cur$rh,
      cur$prec,
      cur$sunrise,
      cur$sunset,
      canopy$rain_total_prev
    )
    cur$dmc <- mcdmc_to_dmc(mcdmc)
    mcdc <- drought_code(
      mcdc,
      cur$hr,
      cur$temp,
      cur$prec,
      cur$sunrise,
      cur$sunset,
      canopy$rain_total_prev
    )
    cur$dc <- mcdc_to_dc(mcdc)
    cur$isi <- initial_spread_index(cur$ws, cur$ffmc)
    cur$bui <- buildup_index(cur$dmc, cur$dc)
    cur$fwi <- fire_weather_index(cur$isi, cur$bui)
    cur$dsr <- daily_severity_rating(cur$fwi)
    # done using canopy, can update for next step
    canopy$rain_total_prev <- canopy$rain_total_prev + cur$prec
    # grass updates
    mcgfmc_matted <- hourly_grass_fuel_moisture(
      mcgfmc_matted,
      cur$temp,
      cur$rh,
      cur$ws,
      cur$prec,
      cur$solrad,
      cur$grass_fuel_load
    )
    #for standing grass we make a come very simplifying assumptions based on obs from the field (echo bay study):
    #standing not really affected by rain -- to introduce some effect we introduce just a simplification of the FFMC Rain absorption function
    #which averages 6% or so for rains  (<5mm...between 7% and 5%,    lower for larger rains)(NO intercept)
    #AND the solar radiation exposure is less, and the cooling from the wind is stronger.  SO we assume there is effectively no extra
    #heating of the grass from solar
    #working at the margin like this should make a nice bracket for moisture between the matted and standing that users can use
    #...reality will be in between the matt and stand
    mcgfmc_standing <- hourly_grass_fuel_moisture(
      mcgfmc_standing,
      cur$temp,
      cur$rh,
      cur$ws,
      cur$prec * 0.06,
      0.0,
      cur$grass_fuel_load
    )

    if (julian(cur$mon, cur$day) < DATE_GRASS) {
      standing <- FALSE
      mcgfmc <- mcgfmc_matted
    } else {
      standing <- TRUE
      mcgfmc <- mcgfmc_standing
    }

    cur$mcgfmc_matted <- mcgfmc_matted
    cur$mcgfmc_standing <- mcgfmc_standing
    cur$gfmc <- mcgfmc_to_gfmc(mcgfmc, cur$percent_cured, cur$ws)
    cur$gsi <- grass_spread_index(cur$ws, mcgfmc, cur$percent_cured, standing)
    cur$gfwi <- grass_fire_weather_index(cur$gsi, cur$grass_fuel_load)
    # save wetting variables for timestep-by-timestep runs
    cur$prec_cumulative <- canopy$rain_total_prev
    cur$canopy_drying <- canopy$drying_since_intercept
    # bind results for this row
    results <- rbind(results, cur)
  }
  return(results)
}

#' Calculate hourly FWI indices from hourly weather stream.
#'
#' @param    df_wx                hourly values weather stream
#' @param    timezone             integer offset from GMT to use for sun calculations
#' @param    ffmc_or_mcffmc_old   previous value for FFMC or mcffmc (startup 85)
#' @param    is_mcffmc            is above a value of mcffmc or FFMC (default False)
#' @param    dmc_old              previous value for DMC (startup 6)
#' @param    dc_old               previous value for DC (startup 15)
#' @param    mcgfmc_matted_old    previous value for matted mcgfmc (startup FFMC = 85)
#' @param    mcgfmc_standing_old  previous value for standing mcgfmc (startup FFMC = 85)
#' @param    dmc_before_rain      DMC before rainfall (default 0)
#' @param    dc_before_rain       DC before rainfall (default 0)
#' @param    prec_cumulative      cumulative precipitation this rainfall (default 0)
#' @param    canopy_drying        consecutive hours of no rain (default 0)
#' @param    silent               suppresses informative print statements (default False)
#' @param    round_out            decimals to truncate output to, NA for none (default 4)
#' @return                        hourly values FWI and weather stream
hFWI <- function(
  df_wx,
  timezone,
  ffmc_or_mcffmc_old = FFMC_DEFAULT,
  is_mcffmc = FALSE,
  dmc_old = DMC_DEFAULT,
  dc_old = DC_DEFAULT,
  mcgfmc_matted_old = ffmc_to_mcffmc(FFMC_DEFAULT),
  mcgfmc_standing_old = ffmc_to_mcffmc(FFMC_DEFAULT),
  prec_cumulative = 0.0,
  canopy_drying = 0.0,
  silent = FALSE,
  round_out = 4
  ) {  # not using dmc_old or dc_old reference to match Python
  # check df_wx class for data.frame or data.table
  wasDf <- is.data.frame(df_wx)
  if (wasDf) {
    wx <- copy(df_wx)
    setDT(wx)
  } else if (is.data.table(df_wx)) {
    wx <- copy(df_wx)
  } else {
    stop("Input weather stream df_wx needs to be a data.frame or data.table!")
  }
  colnames(wx) <- toupper(colnames(wx))
  og_names <- names(wx)
  # check for required columns
  stopifnot(all(c('YR', 'MON', 'DAY', 'HR', 'TEMP', 'RH', 'WS', 'PREC') %in% names(wx)))
  # check for one hour run and startup moisture all set to default
  if (nrow(wx) == 1 &&
    ffmc_or_mcffmc_old == FFMC_DEFAULT && is_mcffmc == FALSE &&
    dmc_old == DMC_DEFAULT && dc_old == DC_DEFAULT &&
    mcgfmc_matted_old == ffmc_to_mcffmc(FFMC_DEFAULT) &&
    mcgfmc_standing_old == ffmc_to_mcffmc(FFMC_DEFAULT)) {
    warning(paste("Startup moisture values set to default (instead of previous)",
      "in a one hour run"))
  }
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
  wx$TIMEZONE <- timezone
  if (!"GRASS_FUEL_LOAD" %in% og_names) {
    wx$GRASS_FUEL_LOAD <- DEFAULT_GRASS_FUEL_LOAD
  }
  if (!"PERCENT_CURED" %in% og_names) {
    wx$PERCENT_CURED <- seasonal_curing(julian(wx$MON, wx$DAY))
  }
  if (!"SOLRAD" %in% og_names) {
    if (!silent) {
      print("Solar Radiation not provided so will be calculated")
    }
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
  stopifnot(wx$GRASS_FUEL_LOAD > 0)
  stopifnot(wx$PERCENT_CURED >= 0 & wx$PERCENT_CURED <= 100)
  if (!is_mcffmc) {
    stopifnot(ffmc_or_mcffmc_old >= 0 & ffmc_or_mcffmc_old <= 101)
  }
  stopifnot(dmc_old >= 0)
  stopifnot(dc_old >= 0)

  # loop over every station year
  results <- NULL
  for (stn in unique(wx$ID)) {
    by_stn <- wx[ID == stn]
    for (yr in unique(by_stn$YR)) {
      by_year <- by_stn[YR == yr, ]
      if (!silent) {
        print(paste0("Running ", stn, " for ", yr))
      }
      # FIX: convert this to not need to do individual stations
      w <- getSunlight(by_year, get_solrad = needs_solrad)
      r <- .stnHFWI(w, ffmc_or_mcffmc_old, is_mcffmc, dmc_old, dc_old,
        mcgfmc_matted_old, mcgfmc_standing_old,
        prec_cumulative, canopy_drying)
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
  results <- results[, -c("TIMEZONE")]

  names(results) <- tolower(names(results))

  # format decimal places of output columns
  if (!is.na(round_out)) {
    outcols <- c("sunrise", "sunset", "sunlight_hours",
      "mcffmc", "ffmc", "dmc", "dc", "isi", "bui", "fwi", "dsr",
      "mcgfmc_matted", "mcgfmc_standing", "gfmc", "gsi", "gfwi",
      "dmc_before_rain", "dc_before_rain", "prec_cumulative", "canopy_drying")
    if (!"SOLRAD" %in% og_names) {
      outcols <- c('solrad', outcols)
    }
    set(results, j = outcols, value = round(results[, ..outcols], round_out))
  }

  if (wasDf) {
    setDF(results)
  }
  return(results)
}

# run hFWI by command line via Rscript, requires 3 args: input csv, output csv, timezone
# optional args: ffmc_or_mcffmc, is_mcffmc, dmc, dc, mcgfmc_matted, mcgfmc_standing,
#                prec_cumulative, canopy_drying, silent, round_out
if ("--args" %in% commandArgs() && sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 3) {
    stop("at least 3 arguments required: input csv, output csv, timezone")
  }
  input <- args[1]
  output <- args[2]
  timezone <- as.numeric(args[3])
  # load optional arguments if provided, or set to default
  if (length(args) >= 4) ffmc_or_mcffmc_old <- as.numeric(args[4])
  else ffmc_or_mcffmc_old <- FFMC_DEFAULT
  if (length(args) >= 5) is_mcffmc <- as.logical(args[5])
  else is_mcffmc <- FALSE
  if (length(args) >= 6) dmc_old <- as.numeric(args[6])
  else dmc_old <- DMC_DEFAULT
  if (length(args) >= 7) dc_old <- as.numeric(args[7])
  else dc_old <- DC_DEFAULT
  if (length(args) >= 8) mcgfmc_matted_old <- as.numeric(args[8])
  else mcgfmc_matted_old <- ffmc_to_mcffmc(FFMC_DEFAULT)
  if (length(args) >= 9) mcgfmc_standing_old <- as.numeric(args[9])
  else mcgfmc_standing_old <- ffmc_to_mcffmc(FFMC_DEFAULT)
  if (length(args) >= 10) prec_cumulative <- as.numeric(args[12])
  else prec_cumulative <- 0.0
  if (length(args) >= 11) canopy_drying <- as.numeric(args[13])
  else canopy_drying <- 0.0
  if (length(args) >= 12) silent <- as.logical(args[14])
  else silent <- FALSE
  if (length(args) >= 13) round_out <- args[15]
  else round_out <- 4
  if (length(args) >= 14) warning("Too many input arguments provided, some unused")

  df_in <- read.csv(input)
  df_out <- hFWI(df_in, timezone, ffmc_or_mcffmc_old, is_mcffmc, dmc_old, dc_old,
    mcgfmc_matted_old, mcgfmc_standing_old, prec_cumulative, canopy_drying,
    silent, round_out)
  write.csv(df_out, output, row.names = FALSE)
}

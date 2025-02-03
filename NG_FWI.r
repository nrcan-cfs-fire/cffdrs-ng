#' Computes hourly FWI indices for an input hourly weather stream
library(lubridate)
library(data.table)
source("util.r")
source("old_cffdrs.r")

DAILY_K_DMC_DRYING <- 1.894
DAILY_K_DC_DRYING <- 3.937

HOURLY_K_DMC <- 2.10
# HOURLY_K_DC <- 0.017066
# HOURLY_K_DMC <- 0.27
HOURLY_K_DC <- 0.017
DMC_OFFSET_TEMP <- 1.1
DC_OFFSET_TEMP <- 0.0

DC_DAILY_CONST <- 0.36
DC_HOURLY_CONST <- DC_DAILY_CONST / DAILY_K_DC_DRYING


OFFSET_SUNRISE <- 2.5
OFFSET_SUNSET <- 0.5

# Fuel Load (kg/m^2)
DEFAULT_GRASS_FUEL_LOAD <- 0.35
MAX_SOLAR_PROPAGATION <- 0.85

# default startup values
FFMC_DEFAULT <- 85
DMC_DEFAULT <- 6
DC_DEFAULT <- 15

# FIX: figure out what this should be
DEFAULT_LATITUDE <- 55.0
DEFAULT_LONGITUDE <- -120.0

# # just apply "daily" indices to noon directly
# HOUR_TO_START_FROM <- 12
# # result seemed to match better at noon so try starting from there instead
# # # start with daily indices at peak burn
# # HOUR_TO_START_FROM <- 16

MPCT_TO_MC <- 147.2772277
FFMC_INTERCEPT <- 0.5
DMC_INTERCEPT <- 1.5
DC_INTERCEPT <- 2.8

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
    kd_or_kw <- drf * k0_or_k1 * exp(0.0365 * temp)
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
  if (rain != 0) {
    #     mo+=rain*rf*exp(-100.0/(251.0-mo))*(1.0-exp(-6.93/rain))*/ # old routine*/
    # this new routine assumes layer is 0.3 kg/m2 so 0.3mm of rain adds +100%MC*/
    # *100 to convert to %...  *1/.3 because of 0.3mm=100%
    mo <- mo + (rain / 0.3 * 100.0)
    if (mo > 250.0) {
      mo <- 250.0
    }
  }
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
  m <- ifelse(mo < ed && mo < ew,
    ew,
    ed
  )
  # use ifelse so table works
  m <- ifelse(mo > ed || (mo < ed && mo < ew),
    {
      # these are the same formulas with a different value for a1
      a1 <- ifelse(mo > ed,
        rhf / 100.0,
        (100.0 - rhf) / 100.0
      )
      k0_or_k1 <- 0.424 * (1 - (a1^1.7)) + (0.0694 * sqrt(ws) * (1 - (a1^8)))
      kd_or_kw <- drf * k0_or_k1 * exp(0.0365 * tf)
      m + (mo - m) * (10^(-kd_or_kw * time))
    },
    m
  )
  return(m)
}


#' Calculate Grass Spread Index (GSI)
#'
#' @param ws              Wind Speed (km/h)
#' @param mc              Grass moisture content (percent)
#' @param cur             Degree of curing (percent, 0-100)
#' @return                Grass Spread Index
grass_spread_index <- Vectorize(function(ws, mc, cur) {
  fw <- 16.67 * ifelse(ws < 5, 0.054 + 0.209 * ws, 1.1 + 0.715 * (ws - 5.0) * 0.844)
  # NOTE: between [12, ~12.01754] the value for ws < 10 is greater than ws >= 10
  # using 0.6838 instead would mean this is always less than ws >= 10
  # mc < 23.9 because of check at start of function, so last expression is any ws >= 10
  fm <- ifelse(mc < 12,
    exp(-0.108 * mc),
    ifelse(mc < 20.0 && ws < 10.0,
      0.684 - 0.0342 * mc,
      ifelse(mc < 23.9 && ws >= 10.0,
        0.547 - 0.0228 * mc,
        0.0
      )
    )
  )
  # same as float curing(float PC)
  cf <- ifelse(cur > 20,
    1.034 / (1 + 104 * exp(-0.1 * (cur - 20))),
    0.0
  )
  return(1.11 * fw * fm * cf)
})

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
    lat,
    lon,
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
      cur$lat,
      cur$long,
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
    mcgfmc <- hourly_grass_fuel_moisture(cur$temp, cur$rh, cur$ws, cur$prec, cur$solrad, mcgfmc)
    cur$mcgfmc <- mcgfmc
    # QUESTION: replace with GFMC function from C code?
    cur$gfmc <- fine_fuel_moisture_code(mcgfmc)
    # still use mcgfmc
    cur$gsi <- grass_spread_index(cur$ws, mcgfmc, cur$percent_cured)
    cur$gfwi <- grass_fire_weather_index(cur$gsi, cur$grass_fuel_load)
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
hFWI <- function(df_wx, timezone, ffmc_old = 85, dmc_old = 6, dc_old = 15) {
  wx <- as.data.table(copy(df_wx))
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
    setnames(wx, c("RAIN"), c("PREC"))
  }
  if (wasYear) {
    setnames(wx, c("YEAR"), c("YR"))
  }
  if (wasHour) {
    setnames(wx, c("HOUR"), c("HR"))
  }
  if (!("PERCENT_CURED" %in% names(wx))) {
    wx$JULIAN <- julian(wx$MON, wx$DAY)
    wx$PERCENT_CURED <- seasonal_curing(wx$JULIAN)
  }
  if (!("GRASS_FUEL_LOAD" %in% names(wx))) {
    wx$GRASS_FUEL_LOAD <- DEFAULT_GRASS_FUEL_LOAD
  }
  cols_extra_solar <- intersect(names(wx), c("SOLRAD", "SUNRISE", "SUNSET", "SUNLIGHT_HOURS"))
  if (0 < length(cols_extra_solar)) {
    warning(sprintf("Ignoring and recalculating columns: [%s]", paste0(cols_extra_solar, collapse = ", ")))
    wx <- wx[, -..cols_extra_solar]
  }
  stopifnot(all(wx$RH >= 0 & wx$RH <= 100))
  stopifnot(all(wx$WS >= 0))
  stopifnot(all(wx$PREC >= 0))
  stopifnot(all(wx$MON >= 1 & wx$MON <= 12))
  stopifnot(all(wx$DAY >= 1 & wx$DAY <= 31))
  stopifnot(ffmc_old >= 0 & ffmc_old <= 101)
  stopifnot(dmc_old >= 0)
  stopifnot(dc_old >= 0)
  # HACK: just rename for now
  # setnames(wx, c("PREC"), c("RAIN"))
  if (!hadDate) {
    wx[, DATE := as.character(as.Date(sprintf("%04d-%02d-%02d", YR, MON, DAY)))]
  }
  if (!hadTimestamp) {
    wx[, TIMESTAMP := as_datetime(sprintf("%04d-%02d-%02d %02d:%02d:00", YR, MON, DAY, HR, MINUTE))]
  }
  # loop in hFWI function
  results <- NULL
  for (stn in unique(wx$ID)) {
    by_stn <- wx[ID == stn]
    for (yr in unique(by_stn$YR)) {
      by_year <- by_stn[YR == yr, ]
      print(paste0("Running ", stn, " for ", yr))
      dates <- as_datetime(unique(by_year$TIMESTAMP))
      latitude <- by_year$LAT[[1]]
      longitude <- by_year$LONG[[1]]
      sunlight <- getSunlight(dates, timezone, latitude, longitude)
      setnames(sunlight, c("DATE"), c("TIMESTAMP"))
      sunlight$TIMESTAMP <- as_datetime(sunlight$TIMESTAMP)
      w <- merge(by_year, sunlight, by = c("TIMESTAMP", "LAT", "LONG"))
      r <- .stnHFWI(w, ffmc_old, dmc_old, dc_old)
      results <- rbind(results, r)
    }
  }
  # # this is all just to remove dummy variables that we added
  # if (!is.null(results)) {
  #   names(results) <- toupper(names(results))
  #   if (!hadStn) {
  #     results <- results[, -c("ID")]
  #   }
  #   if (!hadMinute) {
  #     results <- results[, -c("MINUTE")]
  #   }
  #   if (!hadDate) {
  #     results <- results[, -c("DATE")]
  #   }
  #   if (!hadLatitude) {
  #     results <- results[, -c("LAT")]
  #   }
  #   if (!hadLongitude) {
  #     results <- results[, -c("LONG")]
  #   }
  #   if (!hadTimestamp) {
  #     results <- results[, -c("TIMESTAMP")]
  #   }
  #   # if (wasWind) {
  #   #   setnames(results, c("WS"), c("WIND"))
  #   # }
  #   # if (wasRain) {
  #   #   setnames(results, c("PREC"), c("RAIN"))
  #   # }
  #   # if (wasYear) {
  #   #   setnames(results, c("YR"), c("YEAR"))
  #   # }
  #   # if (wasHour) {
  #   #   setnames(results, c("HR"), c("HOUR"))
  #   # }
  #   setnames(results, new_names, old_names)
  #   # setnames(results, c("PREC"), c("RAIN"))
  # }
  # names(results) <- tolower(names(results))
  # should have gotten rid of all the fields we added to make the processing work
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
      "solrad",
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
    if ("id" %in% names(df_fwi)) {
      colnames_out <- c("id", colnames_out)
    }
    df_fwi <- df_fwi[, ..colnames_out]
    save_csv(df_fwi, file_out)
  } else {
    message("Wrong number of arguments")
  }
}

library(data.table)
library(lubridate)

#' Find specific humidity
#'
#' @param temp        Temperature (Celcius)
#' @param rh          Relative humidity (percent, 0-100)
#' @return            Specific humidity (g/kg)
findQ <- function(temp, rh)
{
  # find absolute humidity
  svp <- 6.108 * exp(17.27 * temp / (temp + 237.3))
  vp <- svp * rh / 100.0
  return(217 * vp / (273.17 + temp))
}

#' Find relative humidity
#'
#'  @param q           Specific humidity (g/kg)
#'  @param temp        Temperature (Celcius)
#'  @return            Relative humidity (percent, 0-100)
findrh <- function(q, temp)
{
  cur_vp <- (273.17 + temp) * q / 217
  return(100 * cur_vp / (6.108 * exp(17.27 * temp / (temp + 237.3))))
}


#' Find day of year. Does not properly deal with leap years.
#'
#' @param mon         Month
#' @param day         Day of month
#' @return            Day of year
julian <- function(mon, day)
{
  month <- c(0,31,59,90,120,151,181,212,242,273,304,334,365)
  return(month[mon]+day)
}

#' Find solar radiation at a give time and place
#'
#' @param dates             Datetimes to find solar radiation for
#' @param timezone          Offset from GMT in hours
#' @param latitude          Latitude (degrees)
#' @param longitude         Longitude (degrees)
#' @return                  Solar radiation (kW/m^2), sunrise, sunset, sunlight hours
getSunlight <- function(dates, timezone, latitude, longitude)
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

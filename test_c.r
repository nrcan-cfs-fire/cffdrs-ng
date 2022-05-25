source("hFWI.r")
library(ggplot2)
library(data.table)
library(lubridate)
DIR_DATA <- normalizePath("../data")

csv_bak_hourly <- paste0(DIR_DATA, "/BAK2018_hourly.csv")
bak <- as.data.table(read.csv(csv_bak_hourly, header=FALSE, col.names=c("lat", "long", "yr", "mon", "day", "hr", "temp", "rh", "ws", "prec")))


row <- bak[1,]
latitude <- row$lat
longitude <- row$long
tz <- tz_lookup_coords(latitude, longitude, method='accurate')
dates <- bak[, make_datetime(yr, mon, day, hr, tz=tz)]

d <- make_date(row$yr, row$mon, row$day)
offset <- tz_offset(d, tz)$utc_offset_h

tz <- tz_lookup_coords(latitude, longitude, method='accurate')
times <- suncalc::getSunlightTimes(date(dates), latitude, longitude, tz=tz)
print(times)

lat <- latitude
lon <- longitude
yr <- row$yr
mon <- row$mon
day <- row$day
hour <- row$hr
timezone <- tz_offset(d, tz)$utc_offset_h
DST <- ifelse(tz_offset(d, tz)$is_dst, 1, 0)
# sun <- function(lat, lon, d, timezone, DST)
# {
#   dechour <- 12.0
#   jd <- yday(d)
#   fracyear <- 2.0*pi/365.0*( jd-1.0+(dechour-12.0)/24.0);
#   
#   eqtime <- 229.18*( 0.000075+ 0.001868*cos(fracyear) - 0.032077*sin (fracyear) - 0.014615*cos(2.0*fracyear) - 0.040849*sin(2.0*fracyear) )
#   
#   decl <- 0.006918-0.399912*cos(fracyear) + 0.070257*sin(fracyear) - 0.006758*cos(fracyear*2.0)+0.000907*sin(2.0*fracyear) - 0.002697*cos(3.0*fracyear) + 0.00148*sin(3.0*fracyear)
#   timeoffset <- eqtime+4*lon-60*timezone
#   
#   tst <- (hour(d)-DST)*60.0+timeoffset
#   hourangle <- tst/4-180
#   zenith <- acos(sin(lat*pi/180)*sin(decl)+cos(lat*pi/180)*cos(decl)*cos(hourangle*pi/180) )
#   solrad <- 0.95*cos(zenith)
#   if(solrad<0)
#   {
#     solrad <- 0.0
#   }
#   #printf(" SOLAR: %d  %d fracyear=%f dec=%f  toff=%f  tst=%fha=%f zen=%f  solrad=%f\n",jd,hour,fracyear,decl,timeoffset,tst,hourangle,zenith,solrad);
#   
#   zenith <- 90.833*pi/180.0;
#   
#   halfday <- 180.0/pi*acos( cos(zenith)/(cos(lat*pi/180.0)*cos(decl))-tan(lat*pi/180.0)*tan(decl) )
#   sunrise <- (720.0-4.0*(lon+halfday)-eqtime)/60+timezone+DST
#   sunset <- (720.0-4.0*(lon-halfday)-eqtime)/60+timezone+DST
#   return(c(solrad, sunrise, sunset))
# }
# r <- NULL
# for (n in 1:length(dates))
# {
#   d <- dates[[n]]
#   print(d)
#   tzo <- tz_offset(d, tz)
#   DST <- ifelse(tzo$is_dst, 1, 0)
#   timezone <- tzo$utc_offset_h - DST
#   r <- rbind(r, c(d, sun(lat, lon, d, timezone, DST)))
# }
result <- hFWI(bak)
result[, MIN_RH := sprintf("%0.1f", round(MIN_RH, 1))]
result[, temp := sprintf("%0.1f", round(temp, 1))]
result[, rh := sprintf("%0.0f", round(rh, 0))]
result[, ws := sprintf("%0.1f", round(ws, 1))]
result[, prec := sprintf("%0.1f", round(prec, 1))]
result[, SOLPROP := sprintf("%0.5f", round(SOLPROP, 5))]
result[, SOLRAD := sprintf("%0.5f", round(SOLRAD, 5))]
result[, SUNRISE := sprintf("%0.3f", round(SUNRISE, 5))]
result[, SUNSET := sprintf("%0.3f", round(SUNSET, 5))]
result[, FFMC := sprintf("%0.1f", round(FFMC, 1))]
result[, DMC := sprintf("%0.1f", round(DMC, 1))]
result[, DC := sprintf("%0.1f", round(DC, 1))]
result[, ISI := sprintf("%0.1f", round(ISI, 1))]
result[, BUI := sprintf("%0.1f", round(BUI, 1))]
result[, FWI := sprintf("%0.1f", round(FWI, 1))]
result[, DSR := sprintf("%0.1f", round(DSR, 1))]
result[, MCGMC := sprintf("%0.1f", round(MCGMC, 1))]
result[, GFMC := sprintf("%0.1f", round(GFMC, 1))]
result[, GSI := sprintf("%0.1f", round(GSI, 1))]
result[, GFWI := sprintf("%0.1f", round(GFWI, 1))]
# write.csv(result[, -c("lat", "long", "DSR")], "./result.csv", row.names=FALSE, quote=FALSE)
write.csv(result[, c("yr", "mon", "day", "hr", "SOLPROP", "SOLRAD", "SUNRISE", "SUNSET", "MIN_RH", "temp", "rh", "ws", "prec", "FFMC", "DMC", "DC", "ISI", "BUI", "FWI", "MCGMC", "GFMC", "GSI", "GFWI")], "./result.csv", row.names=FALSE, quote=FALSE)


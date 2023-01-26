#' Various utility functions used by the other files
library(data.table)
library(lubridate)

#' Determine if data is sequential at intervals of 1 unit
#'
#' @param data          data to check
#' @return              whether each entry in data is 1 unit from the next entry
isSequential <- function(data)
{
  v <- na.omit(unique(data - data.table::shift(data, 1)))
  return(1 == v[[1]] && length(v) == 1)
}

#' Determine if data is sequential days
#'
#' @param df            data to check
#' @return              whether each entry is 1 day from the next entry
isSequentialDays <- function(df)
{
  return(isSequential(as.Date(df$DATE)))
}

#' Determine if data is sequential hours
#'
#' @param df            data to check
#' @return              whether each entry is 1 hour from the next entry
isSequentialHours <- function(df)
{
  return(isSequential(as.POSIXct(df$TIMESTAMP)))
}

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
getSunlight <- function(dates, temprange,timezone, latitude, longitude)
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
  
  ###########################################################################################
  ##################################### DMC-UPDATE ##########################################
  ## calculateing solar radiation using Hargraeves model suggested at: 
  ## (https://github.com/derekvanderkampcfs/open_solar_model/tree/V1#conclusions)
  ## And calculate atm. transmissivity (for calculation of downwelling longwave )
  
  df[, zenith := ifelse(zenith >= pi/2,pi/2,zenith)]
  df[, solradExt := 1.367*cos(zenith)] ## Extraterrestrial solar radiation in kW m-2
  df[, solradExtDaySum := sum(solradExt)*3600,by = c("d")] ## Daily total of Extra. Solar Rad in kJ m-2 day-1
  df[, solradDaySum := 0.11*solradExtDaySum*temprange^0.59] ## Daily surface Solar Rad in kJ m-2 day-1
  df[, solradHar := cos(zenith)/sum(cos(zenith))*solradDaySum/3600,by = c("d")] ## hourly surface solar rad in kW m-2
  df[, trans := solradDaySum/solradExtDaySum,by = c("d")] ## Atmospheric Transmissivity
  ###########################################################################################
  ###########################################################################################
  df[, solrad := 0.95*cos(zenith)]
  df[, solrad := ifelse(solrad < 0, 0, solrad)]
  colnames(df) <- toupper(colnames(df))
  df[, LAT := latitude]
  df[, LONG := longitude]
  ###########################################################################################
  ##################################### DMC-UPDATE ##########################################
  ## return Zenith, Hargreaves Solar Radiation and atm Trans.
  result <- df[, c("DATE", "LAT", "LONG", "SOLRAD", "SUNRISE", "SUNSET","ZENITH","SOLRADHAR","TRANS")]
  ###########################################################################################
  result[, SUNLIGHT_HOURS := SUNSET - SUNRISE]
  return(result)
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
  # wx[, FOR_DATE := DATE]
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

######################################################################################
########################## DMC-Update Drying rate ####################################

#' Calculate subcanopy PET
#'
#' @param dmc             dmc dataframe
getPET <- function(dmc) {
  
  C_TO_K <- 273
  LofVap <- 2260 # latent heat of vaporisation (kJ/kg) (from https://link.springer.com/referenceworkentry/10.1007%2F978-90-481-2642-2_327)
  psychro <- 64 # Psychometric constant (Pa/C) (taken from the 500 m elevation value from Table 2.2 of https://www.fao.org/3/x0490e/x0490e0j.htm#annex%202.%20meteorological%20tables)
  SB <- 5.67*(10^-11) # Stefan Boltzmann constant (kW m-2 K-4 )
  RHO_A <- 1.09266 # density of air (kg/m^3)
  C_A <- 1.00467 # specific heat capacity of air (kJ kg-1 K-1) From Stull (1988)
  k <- 0.40 # von Karman constant (from https://glossary.ametsoc.org/wiki/Von_k%C3%A1rm%C3%A1n%27s_constant)
  wind_height = 1.5 # m HOW TO GET HEIGHT FROM THE ORIGINAL DATAFRAME???
  disp_height = 0# m displacement height THIS IS CURRENTLY AN UNINFORMED GUESS
  z_o = 0.01 # m roughness length Taken from the sharpsands study
  e_veg = 0.95  # emissivity of vegetation used for downwelling canopy longwave and upwelling surface longwave
  albedo = 0.23
  time_res = 3600
  
  ##############################################
  ## Calculate the sub-canopy solar radiation using the model developed here:
  ## https://github.com/nrcan-cfs-fire/DMC-update/tree/Version_2#subcanopoy-shortwave-radiation-model 
  ## and stored here:
  ## https://github.com/nrcan-cfs-fire/DMC-update/blob/Version_2/data/final_shortwave_model.rda
  
  dmc[,altitude := pi/2 - ZENITH]
  dmc[,subSolar :=  0.353253 * SOLRADHAR^2.818619 + -0.058822 * SOLRADHAR +     0.001206  * altitude/pi*180]
  
  ############################################## 
  ## Caculate subcanopy RH using the model developed here:
  ## https://github.com/nrcan-cfs-fire/DMC-update/tree/Version_2#subcanpoy-rh-and-temperature-models
  ## and stored here:
  ## https://github.com/nrcan-cfs-fire/DMC-update/blob/Version_2/data/final_rh_model.rda
  
  rh_bias_df <- data.frame(
    HR = 0:23,
    RHBias = c(-2.2847941,
               -2.0408235,
               -2.0181548,
               -1.7936145,
               -1.7417073,
               -1.7010671,
               -1.0306402,
               2.5758537,
               5.8378354,
               6.5925602,
               6.0272866,
               4.7488655,
               4.3057229,
               3.8612550,
               4.2249008,
               4.4562103,
               5.8786012,
               6.9020536,
               7.4646726,
               6.9460714,
               2.4787202,
               -0.7590774,
               -1.7829464,
               -2.2175595)
  )
  
  ##############################################
  ## Caculate subcanopy temp using the model developed here:
  ## https://github.com/nrcan-cfs-fire/DMC-update/tree/Version_2#subcanpoy-rh-and-temperature-models
  ## and stored here:
  ## https://github.com/nrcan-cfs-fire/DMC-update/blob/Version_2/data/final_airtemp_model.rda
  
  dmc <- merge(dmc,rh_bias_df,by = "HR")
  dmc[,RH_sub:= RH + RHBias]
  dmc[,RH_sub:= ifelse(RH_sub<0,0,RH_sub)]
  dmc[,RH_sub:=  ifelse(RH_sub>100,100,RH_sub)]
  
  TEMP_bias_df <- data.frame(
    HR = 0:23,
    TEMPBias = c(   0.19805294,
                 0.21284118,
                 0.15879464,
                 0.16797590,
                 0.16553659,
                 0.08685671,
                 -0.22863720,
                 -1.22987195,
                 -1.56739329,
                 -1.56968574,
                 -1.39858841,
                 -1.08181727,
                 -0.91129819,
                 -0.82157731,
                 -0.82742063,
                 -1.00142460,
                 -1.26506845,
                 -1.53447619,
                 -1.65734226,
                 -1.53294940,
                 -0.55947619,
                 0.08084524,
                 0.20993750,
                 0.22197024)
  )

  
  dmc <- merge(dmc,TEMP_bias_df,by = "HR")
  dmc[,TEMP_sub:= TEMP + TEMPBias]

  ##############################################
  ## Caculate subcanopy WS using the model developed here:
  ## https://github.com/derekvanderkampcfs/WAF_Data
  ## and stored here:
  ## ???????
  
  ## Convert ftrom km/hr to m/s
 # dmc[,WS := WS/3.6]
  
  wind_thresh <- 2.78
  PAI <- 1.73 ## PAI for "JPP2" site at the Petawawa site ??? ACTUAL SOURCE ???

  ## predict wind speeds using the high wind speed model
  dmc[,WAF :=  (0.6100 - 0.1721) * exp(-3.360 * PAI) + 0.1721]

  ## predict the adjustment factor used for low wind speeds
  dmc[,WSA := exp(-0.2703 * (WS - 2.78))]

  ## calculate the final wind adjustment factor by applying the low wind speed adjustment factor to the lower winds
  dmc[,WAF_full := ifelse(WS> 2.78,WAF,WSA*WAF)]

  dmc[,WS_sub := WS*WAF_full]

  # #############################################
  # ## PM - PET MODEL
  # 
  dmc[,SVP_sub := 0.6108*exp(17.27*TEMP_sub/(237.3+TEMP_sub))*1000] # Sat vapour pressure (Pa) (factor of 1000 required to convert from kPa to Pa) from: https://www.fao.org/3/x0490e/x0490e0j.htm#annex%202.%20meteorological%20tables
  dmc[,satSlope_sub := 4098*(SVP_sub)/(TEMP_sub + 237.3)^2] # slope of VP curve (Pa/C) from: https://www.fao.org/3/x0490e/x0490e0j.htm#annex%202.%20meteorological%20tables
  dmc[,VP_sub := (RH_sub*SVP_sub)/100] #vapour pressure (Pa)
  dmc[,VPD_sub := SVP_sub-VP_sub]
  dmc[,SVP_open := 0.6108*exp(17.27*TEMP/(237.3+TEMP))*1000 ]# Sat vapour pressure (Pa) (factor of 1000 required to convert from kPa to Pa) from: https://www.fao.org/3/x0490e/x0490e0j.htm#annex%202.%20meteorological%20tables
  dmc[,satSlope_open  := 4098*(SVP_open)/(TEMP + 237.3)^2 ]# slope of VP curve (Pa/C) from: https://www.fao.org/3/x0490e/x0490e0j.htm#annex%202.%20meteorological%20tables
  dmc[,VP_open := (RH*SVP_open)/100] #vapour pressure (Pa)
  dmc[,VPD_open := SVP_open-VP_open]
  dmc[,e_clear := 1.24*(VP_open/100/(TEMP+C_TO_K))^(1/7)] # clear sky emissivity from: https://doi.org/10.1002/hyp.6383 (factor of 1/100 for converting VP from Pa to mb)
  dmc[,ra_sub := (log((wind_height-disp_height)/z_o))^2/(k^2*WS_sub)] #aerodynamic resistance (s m-1) (from equation 4 of https://www.fao.org/3/x0490E/x0490e06.htm )
  dmc[,Canopy_L := e_veg*SB*((TEMP_sub+C_TO_K)^4)] #Canopy downwelling longwave (kW m-2)
  dmc[,Lup := e_veg*SB*((TEMP_sub+C_TO_K)^4)] # upwelling longwave (W m-2)
  dmc[, MEANRH := mean(RH), by=c("DATE")] # Daily mean RH
  dmc[,F.daily := 0.87 + 0.64*MEANRH/100 - 0.46*TRANS] ## increase in atm emissivity (from the clear sky case) due to cloud emissions. From second equation in Table 1 of https://doi.org/10.1002/hyp.6383
  dmc[,e.clear.daily := mean(e_clear),by = c("YR","MON","DAY","LAT")]
  dmc[,T.daily := mean(subSolar)/mean(SOLRADHAR),by = c("YR","MON","DAY","LAT")] ## canopy transmission of solar radiation (frac)
  dmc[,Lsky := e.clear.daily*F.daily*SB*((TEMP+C_TO_K)^4)] ## downwelling sky longwave (kW m-2)
  dmc[,Ldown := T.daily*Lsky+(1-T.daily)*Canopy_L] # subcanopy downwelling longwave (kW m-2)
  dmc[,`Q*` := subSolar-(subSolar*albedo)+(Ldown-Lup)]  # net radiation (kW m-2). 
  dmc[,radterm_sub  :=  satSlope_sub*`Q*`]
  dmc[,vpdterm_sub  :=  RHO_A*C_A*(VPD_sub)/ra_sub]
  dmc[,PET_sub  :=  (radterm_sub + vpdterm_sub)/(LofVap*(satSlope_sub + psychro ))]  ## potential evapotransp (kg/(m2·s)) (from https://en.wikipedia.org/wiki/Penman_equation)
  dmc[,PET_mm_sub  :=  PET_sub*time_res]  ## potential evapotransp (mm/time step)
  dmc[,PET_mm_sub_daily := sum(PET_mm_sub),by = c("YR","MON","DAY","LAT")]
  
  
  ##################################################
  ## Calculate PET-Based DMC log-drying rate using approach from
  ## ?????
  ## Specifically using regression model "RK_PET_lm.rda" stored at:
  
  dmc[,DELTA_dry := (PET_mm_sub_daily -1.06394)/0.3597646*1.172863 + 2.201021	]
  #write.csv(dmc,file = "dmc_dump.csv",append = file.exists("dmc_dump.csv"),quote = F, row.names = F)
  result = dmc[,c("TIMESTAMP","LAT", "LONG","PET_mm_sub","DELTA_dry")]
  
  return(result)
  
  #####################################################################################
  ######################################################################################
  
}
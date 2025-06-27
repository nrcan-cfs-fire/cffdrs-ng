library(data.table)
library(lubridate)

#stops NG_FWI console from reporting wrong number of parameters
args <- c()
if("--args" %in% commandArgs()){
  args <- commandArgs(trailingOnly = TRUE)
  commandArgs <- function(...) c("SILENCE")  
}

source("NG_FWI.r")
source("util.r")


smooth_5pt <- function(source) {
  #binomial smoother  ... specifically for the 24 hour day
  #1pt = 1
  #3pt = (1 2 1) = 4
  #5pt = (1 4 6 4 1) = 16
  #7pt = (1 6 15 20 15 6 1) = 64
  
  cap <- length(source) #normally 24 edge cases on data set input though
  
  dest <- numeric(cap)
  
  dest[1] <- source[1]
  dest[cap] <- source[cap]
  
  miss <- 0
  for (i in 1:3){
    if (source[i] < -90.0){
      miss <- miss + 1
    }
  }
  if (miss == 0){
    dest[2] <- (0.25 * source[1]) + (0.5 * source[2]) + (0.25 * source[3])
  }
  else {
    dest[2] <- source[2]
  }
  
  
  for (i in 3:(cap-2)){
    miss <- 0
    for (j in (i-2):(i+2)){
      
      if (source[j] < -90.0){
        miss <- miss + 1
      }
    }
    if (miss == 0){
      dest[i] <- (1.0/16.0 * source[i - 2]) + (4.0/16.0 * source[i - 1]) + (6.0/16.0 * source[i]) + (4.0/16.0 * source[i + 1]) + (1.0/16.0 * source[i + 2])
    }
    else {
      dest[i] <- source[i]
    }
  }
  
  
  miss <- 0 
  for (i in (cap-2):cap){
    if (source[i] < -90.0){
      miss <- miss + 1
    }
  }
  if (miss == 0){
    dest[cap - 1] <- (0.25 * source[cap - 2]) + (0.5 * source[cap - 1]) + (0.25 * source[cap])
  }
  else {
    dest[cap - 1] <- source[cap - 1]
  }
  
  return(dest)
}


#smooth_7pt <- function(source) {
#binomial smoother  ... specifically for the 24 hour day
#1pt = 1
#3pt = (1 2 1) = 4
#5pt = (1 4 6 4 1) = 16
#7pt = (1 6 15 20 15 6 1) = 64


#  dest <- numeric(24)

#  dest[1] <- source[1]
#  dest[24] <- source[24]

#  miss <- 0
#  for (i in 1:3){
#    if (source[i] < -90.0){
#      miss <- miss + 1
#    }
#  }
#  if (miss == 0){
#    dest[2] <- (0.25 * source[1]) + (0.5 * source[2]) + (0.25 * source[3])
#  }
#  else {
#    dest[2] <- source[2]
#  }


#  miss <- 0
#  for (i in 1:5){
#    if (source[i] < -90.0){
#      miss <- miss + 1
#    }
#  }
#  if (miss == 0){
#    dest[3] <- (1.0/16.0 * source[1]) + (4.0/16.0 * source[2]) + (6.0/16.0 * source[3]) + (4.0/16.0 * source[4]) + (1.0/16.0 * source[5])
#  }
#  else {
#    dest[3] <- source[3]
#  }


#  for (i in 4:21){
#    miss <- 0
#    for (j in (i-3):(i+3)){
#      if (source[j] < -90.0){
#        miss <- miss + 1
#      }
#    }
#    if (miss == 0){
#      dest[i] <- (1.0/64.0 * source[i - 3]) + (6.0/64.0 * source[i - 2]) + (15.0/64.0 * source[i - 1]) + (20.0/64.0 * source[i]) + (15.0/64.0 * source[i + 1]) + (6.0/64.0 * source[i + 2]) + (1.0/64.0 * source[i + 3])
#    }
#    else {
#      dest[i] <- source[i]
#    }
#  }


#  miss <- 0
#  for (i in 20:24){
#    if (source[i] < -90.0){
#      miss <- miss + 1
#    }
#  }
#  if (miss == 0){
#    dest[22] <- (1.0/16.0 * source[20]) + (4.0/16.0 * source[21]) + (6.0/16.0 * source[22]) + (4.0/16.0 * source[23]) + (1.0/16.0 * source[24])
#  }
#  else {
#    dest[3] <- source[3]
#  }


#  miss <- 0 
#  for (i in 22:24){
#    if (source[i] < -90.0){
#      miss <- miss + 1
#    }
#  }
#  if (miss == 0){
#    dest[23] <- (0.25 * source[22]) + (0.5 * source[23]) + (0.25 * source[24])
#  }
#  else {
#    dest[23] <- source[23]
#  }


#  return(dest)
#}

#this function calculates a fake date based on 5am-5am instead of midnight to midnight
#used for generating daily summaries to make data look at 5am-5am instead
#output form is "year-julian_day", with the julian day rounded back if the time is before 5am
#for Jan 1st where the julian day rollback would make it zero it gets bumped to the last day of the previous year
pseudo_date <- function(year, month, day, hour) {
  
  
  
  adjusted_jd <- ifelse(hour >= 5, 
                        julian(month,day),
                        julian(month,day) - 1)
  adjusted_year <- ifelse(adjusted_jd == 0, 
                          year - 1,
                          year)
  adjusted_jd <- ifelse(adjusted_jd == 0,
                        julian(12,31),
                        adjusted_jd)
  
  out <- sprintf("%d-%d",adjusted_year,adjusted_jd)
  return(out)
  
}


generate_daily_summaries <- function(hourly_data){
  #note: need to account for spill over inlast day after pseudo_date calc where there is not 24 hours in the data
  Spread_Threshold_ISI <- 5.0
  
  results <- NULL
  for (stn in unique(hourly_data$id)) {
    print(stn)
    by_stn <- hourly_data[id == stn]
    by_stn[,pseudo_DATE := pseudo_date(yr,mon,day,hr)]
    
    for (p_date in unique(by_stn$pseudo_DATE)) {
      
      by_date <- by_stn[pseudo_DATE == p_date, ]
      
      peak_time_traditional_spot <- which(by_date$hr == 17)
      if(length(peak_time_traditional_spot) == 0){
        next
      }
      
      peak_time <- -1
      duration <- 0
      wind_smooth <- smooth_5pt(by_date$ws)
      peak_isi_smooth <- -1
      peak_gsi_smooth <- -1
      max_ffmc <- 0
      ffmc <- 0
      gfmc <- 0
      dmc <- 0
      dc <- 0
      isi <- 0
      gsi <- 0
      bui <- 0
      fwi <- 0
      gfwi<- 0
      dsr <- 0
      
      for (i in 1:nrow(by_date)){
        smooth_isi <- 0
        if (wind_smooth[i] > -90.0 && by_date[i,ffmc] > -90.0) {
          smooth_isi <- initial_spread_index(wind_smooth[i],by_date[i,ffmc])
        }
        else{
          smooth_isi <- -98.9
        }
        
        if (smooth_isi > peak_isi_smooth){
          peak_time <- i
          peak_isi_smooth <- smooth_isi
        }
        if (by_date[i,ffmc] > max_ffmc){
          max_ffmc <- by_date[i,ffmc]
        }
        if (smooth_isi > Spread_Threshold_ISI){
          duration = duration + 1
        }
      }
      
      
      if (smooth_isi < 5 && duration == 24){
        duration <- 0
      } 
      
      if (max_ffmc < 85.0) {
        peak_time <- peak_time_traditional_spot
      }
      
      ffmc <- by_date[peak_time, ffmc]
      dmc <- by_date[peak_time, dmc]
      dc <- by_date[peak_time, dc]
      isi <- by_date[peak_time, isi]
      bui <- by_date[peak_time, bui]
      fwi <- by_date[peak_time, fwi]
      dsr <- by_date[peak_time, dsr]
      smooth_ws_peak <- wind_smooth[peak_time]
      
      gfmc <- by_date[peak_time, gfmc]
      gsi <- by_date[peak_time, gsi]
      gfwi <- by_date[peak_time, gfwi]
      
      
      
      pick_year <- unique(by_date$yr)
      if(length(pick_year) > 1){
        pick_year <- pick_year[1]
      }
      pick_month <- unique(by_date$mon)
      if(length(pick_month) > 1){
        pick_month <- pick_month[1]
      }
      pick_day <- unique(by_date$day)
      if(length(pick_day) > 1){
        pick_day <- pick_day[1]
      }
      
      
      standing <- TRUE
      if (julian(pick_month, pick_day) < DATE_GRASS){
        standing <- FALSE
      }
      print_out <- c(pick_year, pick_month, pick_day, julian(pick_month,pick_day))
      print(print_out)
      peak_gsi_smooth <- grass_spread_index(smooth_ws_peak,gfmc,by_date[i,percent_cured], standing)
      
      
      sunrise_val <- by_date[peak_time, sunrise]
      sunset_val <- by_date[peak_time, sunset]
      
      peak_time <- by_date[peak_time,hr]
      
      sunrise <- sprintf("%d:%d", as.integer(sunrise_val), as.integer(60*(sunrise_val - as.integer(sunrise_val))))
      sunset <- sprintf("%d:%d", as.integer(sunset_val), as.integer(60*(sunset_val - as.integer(sunset_val))))
      
      daily_report <- c(unique(by_date$id), pick_year, pick_month, pick_day, peak_time, duration, smooth_ws_peak, peak_isi_smooth, peak_gsi_smooth, ffmc, dmc, dc, isi, bui, fwi, dsr, gfmc, gsi, gfwi, max_ffmc, sunrise, sunset)
      
      
      
      results <- rbind(results, daily_report)
      
      
      
    }
  }
  
  colnames(results) <- c("wstind","yr","mon","day","peak_time","duration","wind_speed_smoothed","peak_isi_smoothed", "peak_gsi_smoothed", "ffmc","dmc","dc","isi","bui","fwi","dsr", "gfmc", "gsi", "gfwi", "max_ffmc", "sunrise", "sunset")

  results <- results[,c("wstind","yr","mon","day","peak_time","duration","wind_speed_smoothed","peak_isi_smoothed","peak_gsi_smoothed","ffmc","dmc","dc","isi","bui","fwi","dsr", "gfmc", "gsi", "gfwi","sunrise", "sunset")]
  
  results <- data.frame(results)
  
  return(results)
}




# so this can be run via Rscript
print("echo test 1")
#pulled command line args out a top of script
if (length(args)>0){
  if (2 == length(args)) {
    # args: --input_file --output_file
    input <- args[1]
    output <- args[2]
    df_input <- as.data.table(read.csv(input))
    df_summaries <- generate_daily_summaries(df_input)
    save_csv(df_summaries, output)
  } 
  else {
    message("Wrong number of arguments: arguments are <input_file> <output_file>")
  }
}








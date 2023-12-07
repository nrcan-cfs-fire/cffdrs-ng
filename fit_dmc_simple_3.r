library(promises)
library(future)
library(future.apply)
library(progressr)

options(future.globals.maxSize = 1024 * 1024^2)


source("load_data.r")
source("test_hFWI.r")
source("NG_FWI_fitting.r")

rmse  <- function(x, y) {
  return(sqrt(mean((x - y) ^ 2)))
}

rrmse <- function(x, y) {
  return(sqrt(mean((x - y) ^ 2) / sum(x ^ 2)))
}

HOUR_PEAK <<- 16

FCT_SCORE <- c(rmse=rmse, rrmse=rrmse)
TIMEZONES <- c(-6, -5, NA)
FCTS_NOON <- c(
  dmc=function(temp, rh, mon) {
    EL_DMC <- list(
      c(6.5, 7.5, 9.0, 12.8, 13.9, 13.9, 12.4, 10.9, 9.4, 8.0, 7.0, 6.0),
      c(7.9, 8.4, 8.9, 9.5, 9.9, 10.2, 10.1, 9.7, 9.1, 8.6, 8.1, 7.8),
      c(10.1, 9.6, 9.1, 8.5, 8.1, 7.8, 7.9, 8.3, 8.9, 9.4, 9.9, 10.2),
      c(11.5, 10.5, 9.2, 7.9, 6.8, 6.2, 6.5, 7.4, 8.7, 10, 11.2, 11.8),
      c(9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0)
    )
    return(1.894 * (temp + 1.1) * (100 - rh) * EL_DMC[[1]][mon] * 1e-04)
  })


do_test <- function(v,
                    do_plot=FALSE) {
  fct_noon <- FCTS_NOON[[v$for_what]]
  df_hourly <- copy(df_wx)
  timezones <- unique(df_hourly$TIMEZONE)
  df_hourly[, DATE := as.Date(TIMESTAMP)]
  df_hourly[, FOR_DATE := fifelse(HR <= hour_split, DATE, DATE + 1)]
  df_hourly[, PREC24 := sum(PREC), by=list(ID, YR, FOR_DATE)]
  df_hourly[, HAD_RAIN := ifelse(PREC24 > 0, "T", "F")]
  if (!is.na(v$timezone)) {
    df_hourly <- df_hourly[TIMEZONE == v$timezone,]
  }
  if (!is.na(v$offset_sunrise)) {
    stopifnot(!is.na(v$offset_sunset))
    # exclude these from fitting functions so we don't keep calculating them
    df_hourly[, SUNRISE := round(SUNRISE + v$offset_sunrise)]
    df_hourly[, SUNSET := round(SUNSET + v$offset_sunset)]
  }
  df_hourly[, IS_DAYLIGHT := ifelse((HR >= SUNRISE) & (HR < SUNSET), "T", "F")]
  df_hourly[, SUNLIGHT_HOURS := SUNSET - SUNRISE]
  df_noon <- df_hourly[12 == HR, ]
  df_noon[, PE_DAILY := fct_noon(TEMP, RH, MON)]
  df_noon <- df_noon[, list(ID, FOR_DATE, PE_DAILY)]
  df_unfiltered <- copy(df_hourly)
  # BEGIN ######## filter to subset we want to fit on ######## BEGIN #
  # remove hours that wouldn't have worked in daily system
  df_hourly <- df_hourly[TEMP > v$temp_fit_lower, ]
  # remove partial days
  df_hours <- df_hourly[, list(num_hours=.N), by=list(ID, FOR_DATE)]
  df_hours <- df_hours[num_hours == 24, ]
  df_hourly <- merge(df_hours, df_hourly)
  # always fit on days without rain
  df_hourly <- df_hourly["F" == HAD_RAIN,]
  # if sunlight offsets specified then filter to daylight
  if (!is.na(v$offset_sunrise)) {
    stopifnot(!is.na(v$offset_sunset))
    df_hourly <- df_hourly["T" == IS_DAYLIGHT,]
  }
  if (!is.na(v$mon_lower)) {
    if (0 == nrow(df_hourly[MON <= v$mon_lower, ])) {
      # can't subset if nothing outside of range
      return(NULL)
    }
    df_hourly <- df_hourly[MON >= v$mon_lower, ]
  }
  if (!is.na(v$mon_upper)) {
    if (0 == nrow(df_hourly[MON >= v$mon_upper, ])) {
      # can't subset if nothing outside of range
      return(NULL)
    }
    df_hourly <- df_hourly[MON <= v$mon_upper, ]
  }
  # END ########## filter to subset we want to fit on ########## END #
  df_hourly <- df_hourly[, list(ID, FOR_DATE, MON, HR, TEMP, RH)]
  # if nothing left after filtering then can't fit
  if (0 == nrow(df_hourly)) {
    return(NULL)
  }
  # define in here so we don't need global variables for these to work
  fcts_fit <- c(
    dmc_hourly=function(k, offset_temp, temp, rh, mon) {
      return(k * (temp + offset_temp) * (100.0 - rh) * 0.0001)
    },
    dmc_vpd=function(k, offset_temp, temp, rh, mon) {
      # no offset_temp or month
      temp <- temp + offset_temp
      vapour_pressure_saturation <- 0.61078 * exp(17.269 * temp / (temp + 237.3))
      vapour_pressure_deficit <- vapour_pressure_saturation * (1.0 - rh / 100.0)
      return(k * vapour_pressure_deficit)
    }
  )
  dmc_test_rate <- fcts_fit[[v$fct_fit]]
  do_compare <- function(x, df_hourly) {
    k <- x[[1]]
    if (is.na(v$offset_temp)) {
      offset_temp <- x[[2]]
    } else {
      offset_temp <- v$offset_temp
    }
    offset_temp <- ifelse(is.na(offset_temp), 0.0, offset_temp)
    df <- copy(df_hourly)
    # already either excluded daylight or not, so no need to check here
    df[, PE_HOURLY := dmc_test_rate(k, offset_temp, TEMP, RH, MON)]
    # easier to exclude negative values all at once
    if (v$only_positive) {
      df <- df[PE_HOURLY > 0, ]
    }
    df_pe <- df[, list(PE_SUM=sum(PE_HOURLY)), by=list(ID, FOR_DATE)]
    df <- merge(df_noon[, c("ID", "FOR_DATE", "PE_DAILY")], df_pe, by=c("ID", "FOR_DATE"))
    return(df)
  }
  do_score <- function(x, df_hourly, fct_score) {
    df <- do_compare(x, df_hourly)
    return(fct_score(df$PE_DAILY, df$PE_SUM))
  }
  fit_score <- function(x) {
    return(do_score(x, df_hourly, FCT_SCORE[[v$fct_score]]))
  }
  r <- ifelse(is.na(v$offset_temp),
              optim(list(k=1.0, offset_temp=0.0), fit_score),
              optim(list(k=1.0), fit_score, method="Brent", lower=1E-6, upper=10))
  r <- as.list(unlist(r))
  v$k <- r[[1]]
  v$score_ON_subset <- do_score(r, df_hourly, rrmse)
  v$score_ON <- do_score(r, df_unfiltered, rrmse)
  # df_compare <- do_compare(df_hourly, r)
  # # always use rrmse as score at end, regardless of metric for fitting, or else we can't compare the fits
  # v$score_ON <- rrmse(df$PE_DAILY, df$PE_SUM)
  if (is.na(v$offset_temp)) {
    v$offset_temp <- r[[2]]
  }
  # dmc_drying_rate <<- function(hour, sunrise, sunset, temp, rh, mon) {
  #   return(dmc_test_rate(v$k, v$offset_temp, hour, sunrise, sunset, temp, rh, mon))
  # }
  # title_daylight <- ifelse(!is.na(v$offset_sunrise),
  #                          sprintf("Daylight hours from round(sunrise + %0.1f) to round(sunset + %0.1f)", v$offset_sunrise, v$offset_sunset),
  #                          "24 hours")
  # title <- sprintf("Function: %s\nTIMEZONE=%d\n%s\npe <- K * (temp +`` %f) * (100 - rh) * 0.0001\nHOURLY_K_DMC <- %f", v$fct_fit, v$timezone, title_daylight, v$offset_temp_, v$k)
  # print(sprintf("Fit is:\n%s", title))
  # df_test <- fread("./data/wx_hourly.csv")
  # df_test$prec <- 0.0
  # df_fwi <- NULL
  # for (i in 1:length(timezones)) {
  #   # # HACK: just set id for now
  #   df_test$id <- i
  #   df_tz <- test_hfwi(df_test, timezones[[i]])
  #   df_fwi <- rbind(df_fwi, df_tz)
  # }
  # df_peak <- df_fwi[hour(TIMESTAMP) == 16, ]
  # v$score_example <- fct_score(df_peak$DMC, df_peak$DDMC)
  # title <- sprintf("%s: %0.3f\n%s", v$fct_score, v$score_example, title)
  # if (do_plot) {
  #   print(ggplot(df_fwi) +
  #           # ggtitle(sprintf("Equation %dx%dx%d", eqn_k, eqn, eqn_j)) +
  #           ggtitle(title) +
  #           geom_line(aes(TIMESTAMP, DMC), colour="red") +
  #           geom_point(aes(TIMESTAMP, DDMC), data = df_fwi[hour(TIMESTAMP) == 16]))
  # }
  return(as.data.table(v))
}

df_all <- df_wx[, -c("SOLRAD", "SUNRISE", "SUNSET", "SUNLIGHT_HOURS")]
df_all$PREC <- 0.0

offsets <- list(c(NA, NA), c(0.0, 0.0), c(3.0, 0.0), c(2.5, 0.5), c(2.0, 1.0), c(1.5, 1.5), c(1.0, 2.0), c(0.5, 2.5), c(0.0, 3.0))
df_check <- NULL
v <- list()
for (for_what in c("dmc")) {
  v$for_what <- for_what
  # exclude hours that gave 0 for old daily dmc
  TEMPS_FIT_LOWER <- c(dmc=1.1, dc=2.8)
  v$temp_fit_lower <- TEMPS_FIT_LOWER[v$for_what]
  for (hour_split in c(HOUR_PEAK)) {
    v$hour_split <- hour_split
    for (only_positive in c(TRUE, FALSE)) {
      v$only_positive <- only_positive
      for (fct_score in names(FCT_SCORE)) {
        v$fct_score <- fct_score
        for (k in 1:length(offsets)) {
          v$offset_sunrise <- offsets[[k]][[1]]
          v$offset_sunset <- offsets[[k]][[2]]
          if (is.na(v$offset_sunrise)) {
            stopifnot(is.na(v$offset_sunset))
          }
          months <- c(NA, sort(unique(df_wx$MON)))
          for (mon_lower in months) {
            v$mon_lower <- mon_lower
            for (mon_upper in months[months >= v$mon_lower]) {
              v$mon_upper <- mon_upper
              # timezone isn't really fitting, it's a subset we're looking at
              for (timezone in TIMEZONES) {
                v$timezone <- timezone
                OFFSETS_TEMP <- list(dmc_hourly=c(NA, 1.1, 0.0), dmc_vpd=c(0.0))
                for (fct_fit in names(OFFSETS_TEMP)) {
                  v$fct_fit <- fct_fit
                  for (offset_temp in OFFSETS_TEMP[[v$fct_fit]]) {
                    v$offset_temp <- offset_temp
                    df_check <- rbind(df_check, as.data.table(v))
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

total <- nrow(df_check)

#
# results <- NULL
# for (i in 1:nrow(df_check)) {
#   v <- df_check[i]
#   if (is.null(results) || (0 == nrow(merge(results, v, by=names(v))))) {
#     results <- rbind(results, do_test(v, do_plot=FALSE))
#   }
# }
# setorder(results, score_ON)

plan(multisession)
handlers("progress")
system.time({
  with_progress({
    p <- progressor(along = xs)
    results <- future_lapply(1:nrow(df_check), function(i, ...) {
      v <- df_check[i, ]
      # p(sprintf("x=%g", v$i))
      p()
      # return(v)
      return(do_test(v, do_plot=FALSE))
    })
  })
  df_results <- rbindlist(results)
  setorder(df_results, score_ON)
})


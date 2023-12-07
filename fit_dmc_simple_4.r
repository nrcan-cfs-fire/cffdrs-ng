library(promises)
library(future)
library(future.apply)
library(progressr)

options(future.globals.maxSize = 1024 * 1024^2)
# plan(multisession)
# handlers(global = TRUE)
# handlers("progress")


source("load_data.r")
source("test_hFWI.r")
source("NG_FWI_fitting.r")

rmse  <- function(x, y) {
  return(sqrt(mean((x - y) ^ 2)))
}

rrmse <- function(x, y) {
  return(sqrt(mean((x - y) ^ 2) / sum(x ^ 2)))
}
#
# signed_score <- function(x, y) {
#   s <- sign(mean(x - y))
#   return(s * sqrt(mean((x - y) ^ 2) / sum(x ^ 2)))
#   return(sum(x - y) / length(x))
# }

HOUR_PEAK <<- 16
HOUR_SPLIT <- HOUR_PEAK
# doesn't seem like there's a point in rmse when we look at rrmse
FCT_SCORE <- c(
  # rmse=rmse,
  # rrmse_non_abs=rrmse_non_abs,
  rrmse=rrmse)
TIMEZONES <- c(-6, -5, NA)
OFFSETS_SUNLIGHT <- {
  x <- list(c(NA, NA), c(0.0, 0.0), c(3.0, 0.0), c(2.5, 0.5), c(2.0, 1.0), c(1.5, 1.5), c(1.0, 2.0), c(0.5, 2.5), c(0.0, 3.0))
  x <- as.data.table(t(as.data.table(x)))
  colnames(x) <- c("offset_sunrise", "offset_sunset")
  x
}
OFFSETS_TEMP <- list(dmc=list(hourly=c(NA, 1.1, 0.0), vpd=c(0.0)))
TEMPS_FIT_LOWER <- c(dmc=1.1, dc=2.8)
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
# define in here so we don't need global variables for these to work
FCTS_FIT <- list(dmc=list(
  hourly=function(k, offset_temp, temp, rh, mon) {
    return(k * (temp + offset_temp) * (100.0 - rh) * 0.0001)
  },
  vpd=function(k, offset_temp, temp, rh, mon) {
    # no offset_temp or month
    temp <- temp + offset_temp
    vapour_pressure_saturation <- 0.61078 * exp(17.269 * temp / (temp + 237.3))
    vapour_pressure_deficit <- vapour_pressure_saturation * (1.0 - rh / 100.0)
    return(k * vapour_pressure_deficit)
  }
))


test_index <- function(for_what, df_check_base) {

}

cross <- function(x, y) {
  if (is.null(x)) {
    return(as.data.table(y))
  }
  if (is.null(y)) {
    return(as.data.table(x))
  }
  return(as.data.table(merge(as.data.frame(x), as.data.frame(y), all=TRUE)))
}

df_check_base <- NULL
# timezone isn't really fitting, it's a subset we're looking at
df_check_base <- cross(df_check_base, list(timezone=TIMEZONES))
df_check_base <- cross(df_check_base, list(only_positive=c(TRUE, FALSE)))
df_check_base <- cross(df_check_base, list(fct_score=names(FCT_SCORE)))
df_check_base <- cross(df_check_base, list(fct_score=names(FCT_SCORE)))
df_check_base <- cross(df_check_base, OFFSETS_SUNLIGHT)

df_results <- NULL
for (for_what in c("dmc")) {
  df_base <- copy(df_wx)
  timezones <- unique(df_base$TIMEZONE)
  fct_noon <- FCTS_NOON[[for_what]]
  # exclude hours that gave 0 for old daily dmc
  temp_fit_lower <- TEMPS_FIT_LOWER[for_what]
  offsets_temp_by_fct <- OFFSETS_TEMP[[for_what]]
  df_base[, DATE := as.Date(TIMESTAMP)]
  df_base[, FOR_DATE := fifelse(HR <= HOUR_SPLIT, DATE, DATE + 1)]
  df_base[, PREC24 := sum(PREC), by=list(ID, YR, FOR_DATE)]
  df_base[, HAD_RAIN := ifelse(PREC24 > 0, "T", "F")]
  # start by removing things that are already not full days
  df_hours <- df_base[, list(num_hours=.N), by=list(ID, FOR_DATE)][24 == num_hours,]
  df_base <- merge(df_hours, df_base)[, -c("num_hours")]
  df_unfiltered <- copy(df_base)
  # always fit on days without rain
  df_base <- df_base["F" == HAD_RAIN,]
  # remove hours that wouldn't have worked in daily system
  df_base <- df_base[TEMP > temp_fit_lower, ]
  # remove partial days
  df_hours <- df_base[, list(num_hours=.N), by=list(ID, FOR_DATE)][24 == num_hours,]
  df_base <- merge(df_hours, df_base)[, -c("num_hours")]
  df_noon <- df_base[12 == HR, ]
  df_noon[, PE_DAILY := fct_noon(TEMP, RH, MON)]
  df_noon <- df_noon[, list(ID, FOR_DATE, PE_DAILY)]
  do_test <- function(v) {
    # BEGIN ######## filter to subset we want to fit on ######## BEGIN #
    # only copy if not subsetting because we don't want to modify original
    if (!is.na(v$timezone)) {
      df_hourly <- df_base[TIMEZONE == v$timezone,]
    } else {
      df_hourly <- copy(df_base)
    }
    if (!is.na(v$offset_sunrise)) {
      stopifnot(!is.na(v$offset_sunset))
      # exclude these from fitting functions so we don't keep calculating them
      df_hourly[, SUNRISE := round(SUNRISE + v$offset_sunrise)]
      df_hourly[, SUNSET := round(SUNSET + v$offset_sunset)]
    }
    df_hourly[, IS_DAYLIGHT := ifelse((HR >= SUNRISE) & (HR < SUNSET), "T", "F")]
    df_hourly[, SUNLIGHT_HOURS := SUNSET - SUNRISE]
    # if sunlight offsets specified then filter to daylight
    if (!is.na(v$offset_sunrise)) {
      stopifnot(!is.na(v$offset_sunset))
      df_hourly <- df_hourly["T" == IS_DAYLIGHT,]
    }
    if (!is.na(v$mon_lower)) {
      # if (0 == nrow(df_hourly[MON <= v$mon_lower, ])) {
      #   # can't subset if nothing outside of range
      #   return(NULL)
      # }
      df_hourly <- df_hourly[MON >= v$mon_lower, ]
    }
    if (!is.na(v$mon_upper)) {
      # if (0 == nrow(df_hourly[MON >= v$mon_upper, ])) {
      #   # can't subset if nothing outside of range
      #   return(NULL)
      # }
      df_hourly <- df_hourly[MON <= v$mon_upper, ]
    }
    # END ########## filter to subset we want to fit on ########## END #
    df_hourly <- df_hourly[, list(ID, FOR_DATE, MON, HR, TEMP, RH)]
    # if nothing left after filtering then can't fit
    if (0 == nrow(df_hourly)) {
      return(NULL)
    }
    fct_test_rate <- FCTS_FIT[[v$for_what]][[v$fct_fit]]
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
      df[, PE_HOURLY := fct_test_rate(k, offset_temp, TEMP, RH, MON)]
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
    # always use rrmse as score at end, regardless of metric for fitting, or else we can't compare the fits
    v$score_ON_subset <- do_score(r, df_hourly, rrmse)
    v$score_ON <- do_score(r, df_unfiltered, rrmse)
    # df_compare <- do_compare(df_hourly, r)
    # v$score_ON <- rrmse(df$PE_DAILY, df$PE_SUM)
    if (is.na(v$offset_temp)) {
      v$offset_temp <- r[[2]]
    }
    return(as.data.table(v))
  }
  df_check <- copy(df_check_base)
  df_check$for_what <- for_what
  df_check$temp_fit_lower <- temp_fit_lower
  # no point in including NA because it'll just duplicate outer bound
  months <- sort(unique(df_base$MON))
  df_months <- NULL
  for (mon_lower in months) {
    df_months <- rbind(df_months,
                       cross(list(mon_lower=mon_lower),
                             list(mon_upper=months[months >= mon_lower])))
  }
  df_check <- cross(df_check, df_months)
  offsets_temp <- NULL
  for (fct_fit in names(offsets_temp_by_fct)) {
    offsets_temp <- rbind(offsets_temp,
                          cross(list(fct_fit=fct_fit),
                                list(offset_temp=offsets_temp_by_fct[[fct_fit]])))
  }
  df_check <- cross(df_check, offsets_temp)
  plan(multisession)
  handlers(global = TRUE)
  handlers("progress")
  run_tests <- function() {
    # expect a bit of a delay between progress bar showing up and anything happening
    n <- nrow(df_check)
    xs <- 1:n
    p <- progressor(along = xs)
    results <- future_lapply(xs, function(i, ...) {
      v <- df_check[i, ]
      # result <- v
      result <- do_test(v)
      p(sprintf("[%g / %g]", i, n))
      # p()
      return(result)
    })
    return(rbindlist(results))
    # results <- NULL
    # for (i in xs) {
    #   v <- df_check[i, ]
    #   p(sprintf("[%g / %g]", i, nrow(df_check)))
    #   results <- rbind(results, do_test(v))
    # }
    # return(results)
  }
  system.time({
    df_results <- rbind(df_results, run_tests())
  })
}

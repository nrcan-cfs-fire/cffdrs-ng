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

# rmse  <- function(x, y) {
#   return(sqrt(mean((x - y) ^ 2)))
# }

rrmse <- function(x, y) {
  return(sqrt(mean((x - y) ^ 2) / sum(x ^ 2)))
}

# signed_score <- function(x, y) {
#   s <- sign(mean(x - y))
#   return(s * sqrt(mean((x - y) ^ 2) / sum(x ^ 2)))
#   return(sum(x - y) / length(x))
# }

HOUR_PEAK <<- 16
HOUR_SPLIT <- HOUR_PEAK
# doesn't seem like there's a point in rmse when we look at rrmse
# FCT_SCORE <- c(
#   # rmse=rmse,
#   rrmse_by_stn_by_year=function(df_cmp) {
#     # find total difference by end of year
#     df_by_stn <- df_cmp[, list(PE_DAILY=sum(PE_DAILY), PE_SUM=sum(PE_SUM)), by=list(ID, year(FOR_DATE))]
#     return(rrmse(df_by_stn$PE_DAILY, df_by_stn$PE_SUM))
#   },
#   rrmse=function(df_cmp) { rrmse(df_cmp$PE_DAILY, df_cmp$PE_SUM) })
SCORE_BY <- c(
  stn=function(df_cmp) {
    # find total difference by end of year
    return(df_cmp[, list(PE_DAILY=sum(PE_DAILY), PE_SUM=sum(PE_SUM)), by=list(ID)])
  },
  stn_by_year=function(df_cmp) {
    # find total difference by end of year
    return(df_cmp[, list(PE_DAILY=sum(PE_DAILY), PE_SUM=sum(PE_SUM)), by=list(ID, year(FOR_DATE))])
  },
  none=function(df_cmp) { df_cmp })
TIMEZONES <- c(-6, -5, NA)
# OFFSETS_SUNLIGHT <- {
#   x <- list(c(NA, NA), c(0.0, 0.0), c(3.0, 0.0), c(2.5, 0.5), c(2.0, 1.0), c(1.5, 1.5), c(1.0, 2.0), c(0.5, 2.5), c(0.0, 3.0))
#   x <- as.data.table(t(as.data.table(x)))
#   colnames(x) <- c("offset_sunrise", "offset_sunset")
#   x
# }
OFFSETS_SUNLIGHT <- rbind(c(NA, NA),
                          merge(data.frame(offset_sunrise=0.5 * -6:6), data.frame(offset_sunset=0.5 * -6:6), all=TRUE))
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
df_check_base <- cross(df_check_base, list(score_by=names(SCORE_BY)))
df_check_base <- cross(df_check_base, OFFSETS_SUNLIGHT)

# df_base <- copy(df_wx)
df_base <- df_wx[, -c("DAY", "MINUTE", "WS", "SOLRAD", "SUNLIGHT_HOURS")]
timezones <- unique(df_base$TIMEZONE)
df_base[, DATE := as.Date(TIMESTAMP)]
df_base[, FOR_DATE := fifelse(HR <= HOUR_SPLIT, DATE, DATE + 1)]
df_base[, PREC24 := sum(PREC), by=list(ID, FOR_DATE)]
df_base[, HAD_RAIN := ifelse(PREC24 > 0, "T", "F")]
# start by removing things that are already not full days
df_hours <- df_base[, list(num_hours=.N), by=list(ID, FOR_DATE)][24 == num_hours,]
df_base <- merge(df_hours, df_base)[, -c("num_hours")]
df_unfiltered <- copy(df_base)
# always fit on days without rain
df_base <- df_base["F" == HAD_RAIN,]

df_results <- NULL
for (for_what in c("dmc")) {
  fct_noon <- FCTS_NOON[[for_what]]
  # exclude hours that gave 0 for old daily dmc
  temp_fit_lower <- TEMPS_FIT_LOWER[for_what]
  offsets_temp_by_fct <- OFFSETS_TEMP[[for_what]]
  # remove hours that wouldn't have worked in daily system
  df_for_what <- df_base[TEMP > temp_fit_lower, ]
  # remove partial days
  df_hours <- df_for_what[, list(num_hours=.N), by=list(ID, FOR_DATE)][24 == num_hours,]
  df_for_what <- merge(df_hours, df_for_what)[, -c("num_hours")]
  df_noon <- df_for_what[12 == HR, ]
  df_noon[, PE_DAILY := fct_noon(TEMP, RH, MON)]
  df_noon <- df_noon[, list(ID, FOR_DATE, PE_DAILY)]
  do_test <- function(v) {
    # BEGIN ######## filter to subset we want to fit on ######## BEGIN #
    # make sure first operation is subsetting if we're going to modify anything
    df_hourly <- df_for_what
    if (!is.na(v$timezone)) {
      df_hourly <- df_hourly[TIMEZONE == v$timezone,]
    }
    if (!is.na(v$offset_sunrise)) {
      stopifnot(!is.na(v$offset_sunset))
      # just filter and don't keep calculations in columns
      df_hourly <- df_hourly[(HR >= round(SUNRISE + v$offset_sunrise)) &
                               (HR < round(SUNSET + v$offset_sunset)), ]
    }
    if (!is.na(v$mon_lower)) {
      df_hourly <- df_hourly[MON >= v$mon_lower, ]
    }
    if (!is.na(v$mon_upper)) {
      df_hourly <- df_hourly[MON <= v$mon_upper, ]
    }
    # END ########## filter to subset we want to fit on ########## END #
    df_hourly <- df_hourly[, list(ID, FOR_DATE, YR, MON, HR, TEMP, RH)]
    # should be no reason there wouldn't be rows?
    stopifnot(0 < nrow(df_hourly))
    # # if nothing left after filtering then can't fit
    # if (0 == nrow(df_hourly)) {
    #   return(NULL)
    # }
    fct_test_rate <- FCTS_FIT[[v$for_what]][[v$fct_fit]]
    do_compare <- function(x, df_hourly) {
      k <- x[[1]]
      if (is.na(v$offset_temp)) {
        offset_temp <- x[[2]]
      } else {
        offset_temp <- v$offset_temp
      }
      # already either excluded daylight or not, so no need to check here
      df_pe_hourly <- df_hourly[, list(ID=ID,
                                       FOR_DATE=FOR_DATE,
                                       HR=HR,
                                       PE_HOURLY=fct_test_rate(k, offset_temp, TEMP, RH, MON))]
      # easier to exclude negative values all at once
      if (v$only_positive) {
        df_pe_hourly <- df_pe_hourly[PE_HOURLY > 0, ]
      }
      df_pe <- df_pe_hourly[, list(PE_SUM=sum(PE_HOURLY)), by=list(ID, FOR_DATE)]
      df_cmp <- merge(df_noon, df_pe, by=c("ID", "FOR_DATE"))
      return(df_cmp)
    }
    do_score <- function(x, df_hourly, score_by) {
      df_cmp <- do_compare(x, df_hourly)
      # pass whole thing into scoring so we can group if we want
      score_groups <- SCORE_BY[[score_by]](df_cmp)
      return(rrmse(score_groups$PE_DAILY, score_groups$PE_SUM))
    }
    fit_score <- function(x) {
      return(do_score(x, df_hourly, v$score_by))
    }
    r <- ifelse(is.na(v$offset_temp),
                optim(list(k=1.0, offset_temp=0.0), fit_score),
                optim(list(k=1.0), fit_score, method="Brent", lower=1E-6, upper=10))
    r <- as.list(unlist(r))
    v$k <- r[[1]]
    if (is.na(v$offset_temp)) {
      v$offset_temp <- r[[2]]
    }
    # always use rrmse as score at end, regardless of metric for fitting, or else we can't compare the fits
    v$score_ON_subset <- do_score(r, df_hourly, "none")
    v$score_ON <- do_score(r, df_unfiltered, "none")
    v$score_ON_stn <- do_score(r, df_unfiltered, "stn")
    v$score_ON_stn_by_year <- do_score(r, df_unfiltered, "stn_by_year")
    return(as.data.table(v))
  }
  df_check <- copy(df_check_base)
  df_check$for_what <- for_what
  df_check$temp_fit_lower <- temp_fit_lower
  # no point in including NA because it'll just duplicate outer bound
  months <- sort(unique(df_for_what$MON))
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
  handlers(handler_progress(complete = "#"))
  run_tests <- function() {
    n <- nrow(df_check)
    xs <- 1:n
    # xs <- 1:100
    p <- progressor(along = xs)
    # expect a bit of a delay between progress bar showing up and anything happening
    results <- future_lapply(xs, function(i, ...) {
      # result <- i
      v <- df_check[i, ]
      result <- do_test(v)
      # result <- v
      p()
      return(result)
    })
    return(rbindlist(results))
  }
  with_progress(system.time({
    df_results <- rbind(df_results, run_tests())
  }))
}

setorder(df_results, score_ON_stn_by_year)
write.csv(df_results, "dmc_k_ON_simple_6.csv", row.names=FALSE, quote=FALSE)


#
#
# plot(score_ON_stn_by_year ~ k, df_results["hourly" == fct_fit & 0 == offset_temp,])
# plot(score_ON_stn_by_year ~ k, df_results["vpd" == fct_fit & 0 == offset_temp,])
#
# df_no_offset <- df_results[(1.1 == offset_temp) | (0 == offset_temp),]
# ddf_no_offset[, fit_type := sprintf("%s%s_%s",
#                                     ifelse(only_positive, "+", "+/-"),
#                                     ifelse(is.na(offset_sunrise), "24hr", sprintf("%0.1f_%0.1f", offset_sunrise, offset_sunset)),
#                                     fct_fit)]
# print(ggplot(df_no_offset) +
#         ggtitle("Error by station by year") +
#         geom_point(aes(k, score_ON_stn_by_year, colour=fit_type))) +
#   geom_line(aes(k, min(score_ON_stn_by_year)))
#
# df_better <- df_no_offset[summary(df_no_offset$score_ON_stn_by_year)[[2]] > score_ON_stn_by_year,]
# df_better$fit_type <- as.character(df_better$fit_type)
# print(ggplot(df_better) +
#         ggtitle("Error by station by year") +
#         geom_point(aes(k, score_ON_stn_by_year, colour=fit_type))) +
#   geom_line(aes(k, min(score_ON_stn_by_year)))


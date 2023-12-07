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

# TODO: find k value with VPD instead

do_test <- function(fit_temp, fit_daylight, offset_temp=1.1, timezone = -6, offset_sunrise=2.5, offset_sunset=0.5, do_plot=FALSE, fct_score=rrmse) {
  FIT_TEMP_DMC <<- fit_temp
  FIT_DAYLIGHT_DMC <<- fit_daylight
  OFFSET_SUNRISE <<- offset_sunrise
  OFFSET_SUNSET <<- offset_sunset
  OFFSET_TEMP_DMC <<- offset_temp
  hour_split <- HOUR_PEAK
  df_hourly <- copy(df_wx)
  if (!is.na(timezone)) {
    df_hourly <- df_hourly[TIMEZONE == timezone,]
  }
  timezones <- unique(df_hourly$TIMEZONE)
  df_hourly[, DATE := as.Date(TIMESTAMP)]
  df_hourly[, FOR_DATE := fifelse(HR <= hour_split, DATE, DATE + 1)]
  df_hours <- df_hourly[, list(num_hours=.N), by=list(ID, FOR_DATE)]
  # remove hours that wouldn't have worked in daily system
  df_hourly <- df_hourly[TEMP > -1.1, ]
  # remove partial days
  df_hours <- df_hours[num_hours == 24, ]
  df_hourly <- merge(df_hours, df_hourly)
  df_hourly[, PREC24 := sum(PREC), by=list(ID, YR, FOR_DATE)]
  df_hourly[, HAD_RAIN := ifelse(PREC24 > 0, "T", "F")]
  df_hourly[, SUNRISE := round(SUNRISE + offset_sunrise)]
  df_hourly[, SUNSET := round(SUNSET + offset_sunset)]
  df_hourly[, IS_DAYLIGHT := ifelse((HR >= SUNRISE) & (HR < SUNSET), "T", "F")]
  df_hourly[, SUNLIGHT_HOURS := SUNSET - SUNRISE]
  df_hourly <- df_hourly["F" == HAD_RAIN,]
  df_noon <- df_hourly[12 == HR, ]
  # df_noon[, PE_DAILY := dmc_drying_daily(TEMP, MON)]
  # df_noon[, PE_DAILY := dmc_drying_daily(TEMP, MON)]
  df_noon[, PE_DAILY := 1.894 * (TEMP + 1.1) * (100 - RH) * EL_DMC[[1]][MON] * 1e-04]
  df_noon <- df_noon[, list(ID, FOR_DATE, PE_DAILY)]
  df_hourly <- df_hourly[, list(ID, FOR_DATE, HR, IS_DAYLIGHT, TEMP, RH)]
  cmp_df <- function(x) {
    k <- x[[1]]
    if (fit_temp) {
      offset_temp <- x[[2]]
    } else {
      offset_temp <- OFFSET_TEMP_DMC
    }
    df <- copy(df_hourly)
    if (FIT_DAYLIGHT_DMC) {
      df <- df["T" == IS_DAYLIGHT,]
    }
    df[, DMC_DRY_HOURLY := pmax(0.0, k * (TEMP + offset_temp) * (100.0 - RH) * 0.0001)]
    df_pe <- df[, list(PE_SUM=sum(DMC_DRY_HOURLY)), by=list(ID, FOR_DATE)]
    df <- merge(df_noon[, c("ID", "FOR_DATE", "PE_DAILY")], df_pe, by=c("ID", "FOR_DATE"))
    return(fct_score(df$PE_DAILY, df$PE_SUM))
    # return(sqrt(mean(df[, PE_DAILY - PE_SUM] ^ 2)))
  }
  r <- ifelse(fit_temp,
              optim(list(k=1.0, offset_temp=OFFSET_TEMP_DMC), cmp_df),
              optim(list(k=1.0), cmp_df, method="Brent", lower=1E-6, upper=10))
  r <- as.list(unlist(r))
  score_all <- cmp_df(r)
  HOURLY_K_DMC <<- r[[1]]
  if (FIT_TEMP_DMC) {
    # OFFSET_TEMP_DMC <<- r$offset_temp
    OFFSET_TEMP_DMC <<- r[[2]]
  }
  title_daylight <- ifelse(fit_daylight,
                           sprintf("Daylight hours from round(sunrise + %0.1f) to round(sunset + %0.1f)", OFFSET_SUNRISE, OFFSET_SUNSET),
                           "24 hours")
  title <- sprintf("TIMEZONE=%d\n%s\npe <- K * (temp +`` %f) * (100 - rh) * 0.0001\nHOURLY_K_DMC <- %f", timezone, title_daylight, OFFSET_TEMP_DMC, HOURLY_K_DMC)
  print(sprintf("Fit is:\n%s", title))
  df_test <- fread("./data/wx_hourly.csv")
  df_test$prec <- 0.0
  df_fwi <- NULL
  for (i in 1:length(timezones)) {
    # # HACK: just set id for now
    df_test$id <- i
    df_tz <- test_hfwi(df_test, timezones[[i]])
    df_fwi <- rbind(df_fwi, df_tz)
  }
  df_peak <- df_fwi[hour(TIMESTAMP) == 16, ]
  score <- sqrt(mean(df_peak[, (DMC - DDMC)] ^ 2))
  score_rmse <- rmse(df_peak$DMC, df_peak$DDMC)
  score_rrmse <- rrmse(df_peak$DMC, df_peak$DDMC)

  title <- sprintf("RMSE: %0.3f\n%s", score, title)
  if (do_plot) {
    print(ggplot(df_fwi) +
            # ggtitle(sprintf("Equation %dx%dx%d", eqn_k, eqn, eqn_j)) +
            ggtitle(title) +
            geom_line(aes(TIMESTAMP, DMC), colour="red") +
            geom_point(aes(TIMESTAMP, DDMC), data = df_fwi[hour(TIMESTAMP) == 16]))
  }
  # df_fwi_all <- test_hfwi(df_all[TIMEZONE == timezone,], timezone)
  # score_all <- sqrt(mean(df_fwi_all[hour(TIMESTAMP) == 16, (DMC - DDMC)] ^ 2))
  # df_test_all <- copy(df_wx)
  # df_test_all$PREC <- 0.0
  # df_fwi_all <- NULL
  # for (timezone in unique(df_test_all$TIMEZONE)) {
  #   df_fwi_all <- rbind(df_fwi_all, test_hfwi(df_test_all[TIMEZONE == timezone,], timezone))
  # }
  # score_ON <- sqrt(mean(df_fwi_all[hour(TIMESTAMP) == 16, (DMC - DDMC)] ^ 2))
  return(as.data.table(list(offset_sunrise=ifelse(fit_daylight, offset_sunrise, NA),
                            offset_sunset=ifelse(fit_daylight, offset_sunset, NA),
                            fit_temp=fit_temp,
                            fit_daylight=fit_daylight,
                            timezone=timezone,
                            offset_temp=OFFSET_TEMP_DMC,
                            k=HOURLY_K_DMC,
                            # score_ON=score_ON,
                            score_all=score_all,
                            score=score)))
}

df_all <- df_wx[, -c("SOLRAD", "SUNRISE", "SUNSET", "SUNLIGHT_HOURS")]
df_all$PREC <- 0.0

TIMEZONES <- c(-6, -5, NA)


FCT_SCORE <- c(rmse=rmse, rrmse=rrmse)

results <- NULL
offsets <- list(c(0.0, 0.0), c(3.0, 0.0), c(2.5, 0.5), c(2.0, 1.0), c(1.5, 1.5), c(1.0, 2.0), c(0.5, 2.5), c(0.0, 3.0))
for (fct_name in names(FCT_SCORE)) {
  fct_score <- FCT_SCORE[[fct_name]]
  for (k in 1:length(offsets)) {
    offset_sunrise=offsets[[k]][[1]]
    offset_sunset=offsets[[k]][[2]]
    for (fit_daylight in c(FALSE, TRUE)) {
      if (fit_daylight || (1 == k)) {
        for (timezone in TIMEZONES) {
          for (fit_temp in c(TRUE, FALSE)) {
            if (fit_temp) {
              results <- rbind(results, cbind(fct_score=fct_name, do_test(fit_temp=fit_temp, fit_daylight=fit_daylight, timezone=timezone, offset_sunrise=offset_sunrise, offset_sunset=offset_sunset, fct_score=fct_score)))
            } else {
              for (offset_temp in c(1.1, 0.0)) {
                results <- rbind(results, cbind(fct_score=fct_name, do_test(fit_temp=fit_temp, fit_daylight=fit_daylight, offset_temp=offset_temp, timezone=timezone, offset_sunrise=offset_sunrise, offset_sunset=offset_sunset, fct_score=fct_score)))
              }
            }
          }
        }
      }
    }
  }
}

setorder(results, score_all)


do_apply <- function(v) {
  FIT_TEMP_DMC <<- v$fit_temp
  FIT_DAYLIGHT_DMC <<- v$fit_daylight
  OFFSET_SUNRISE <<- v$offset_sunrise
  OFFSET_SUNSET <<- v$offset_sunset
  hour_split <- HOUR_PEAK
  HOURLY_K_DMC <<- v$k
  OFFSET_TEMP_DMC <<- v$offset_temp
  timezone <<- v$timezone
  calc_fit <- function(df_test) {
    names(df_test) <- toupper(names(df_test))
    # HACK: just set id for now
    df_test$ID <- 1
    df_fwi <- test_hfwi(df_test, timezone)
    df_peak <- df_fwi[hour(TIMESTAMP) == 16, ]
    score <- sqrt(mean(df_peak[, (DMC - DDMC)] ^ 2))
    score_rmse <- rmse(df_peak$DMC, df_peak$DDMC)
    score_rrmse <- rrmse(df_peak$DMC, df_peak$DDMC)
    return(list(df_fwi=df_fwi, score=score, score_rmse=score_rmse, score_rrmse=score_rrmse))
  }
  plot_fit <- function(df_test) {
    title_daylight <- ifelse(FIT_DAYLIGHT_DMC,
                             sprintf("Daylight hours from round(sunrise + %0.1f) to round(sunset + %0.1f)", OFFSET_SUNRISE, OFFSET_SUNSET),
                             "24 hours")
    title <- sprintf("TIMEZONE=%d\n%s\npe <- K * (temp + %f) * (100 - rh) * 0.0001\nHOURLY_K_DMC <- %f", timezone, title_daylight, OFFSET_TEMP_DMC, HOURLY_K_DMC)
    print(sprintf("Fit is:\n%s", title))
    x <- calc_fit(df_test)
    Qdf_fwi <- x$df_fwi
    score <- x$score
    title <- sprintf("RMSE: %0.3f\n%s", score, title)
    print(ggplot(df_fwi) +
            # ggtitle(sprintf("Equation %dx%dx%d", eqn_k, eqn, eqn_j)) +
            ggtitle(title) +
            geom_line(aes(TIMESTAMP, DMC), colour="red") +
            geom_point(aes(TIMESTAMP, DDMC), data = df_fwi[hour(TIMESTAMP) == 16]))
    return(x)
  }
  df_test <- fread("./data/wx_hourly.csv")
  x <- plot_fit(df_test)
  df_test$prec <- 0.0
  plot_fit(df_test)
  # # df_test_all <- df_all[TIMEZONE == timezone,]
  # # df_test_all <- df_test_all[ID == "SAU",]
  # # PREC already 0
  # # results_all <- calc_fit(df_test_all)
  # # df_fwi_all <- results_all$df_fwi
  # # score_all <- results_all$score
  # # df_score_all <- df_fwi_all[hour(TIMESTAMP) == 16, list(score=(DMC - DDMC)^2), by=list(DATE=date(TIMESTAMP))]
  # # boxplot(score ~ year(DATE), df_score_all)
  # # score_all <- sqrt(mean(df_fwi_all[hour(TIMESTAMP) == 16, (DMC - DDMC)] ^ 2))
  # # return(list(offset_sunrise=ifelse(fit_daylight, offset_sunrise, NA),
  # #             offset_sunset=ifelse(fit_daylight, offset_sunset, NA),
  # #             fit_temp=fit_temp,
  # #             fit_daylight=fit_daylight,
  # #             timezone=timezone,
  # #             offset_temp=OFFSET_TEMP_DMC,
  # #             k=HOURLY_K_DMC,
  # #             score=score))
  # # rely on this being done in test_dmc_k_simple.r
  # boxplot(estimate ~ YR, df_results, ylim=c(0.01, 0.03))
  # boxplot(estimate ~ MON, df_results, ylim=c(0.01, 0.03))
  # df_yr <- df_results[, list(estimate=mean(estimate)), by=list(YR)]
  # df_mon <- df_results[, list(estimate=mean(estimate)), by=list(MON)]
  # for (i in 1:nrow(df_mon)) {
  #   k <- df_mon$estimate[i]
  #   HOURLY_K_DMC <<- k
  #   df_test <- fread("./data/wx_hourly.csv")
  #   plot_fit(df_test)
  #   df_test$prec <- 0.0
  #   plot_fit(df_test)
  # }
}

df_test <- fread("./data/wx_hourly.csv")
df_fwi <- calc_fit(df_test)$df_fwi
print(ggplot(df_fwi) +
        ggtitle("DMC for k = 2.1") +
        geom_line(aes(TIMESTAMP, DMC), colour="red") +
        geom_point(aes(TIMESTAMP, DDMC), data = df_fwi[hour(TIMESTAMP) == 16]))
df_test$prec <- 0.0
df_fwi <- calc_fit(df_test)$df_fwi
print(ggplot(df_fwi) +
        ggtitle("DMC for k = 2.1") +
        geom_line(aes(TIMESTAMP, DMC), colour="red") +
        geom_point(aes(TIMESTAMP, DDMC), data = df_fwi[hour(TIMESTAMP) == 16]))

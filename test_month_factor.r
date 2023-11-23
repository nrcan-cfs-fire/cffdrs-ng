library(ggplot2)
library(ggrepel)
source("test_hFWI_fitting.r")

plot_cmp <- function(df, timezone) {
  FLAG_NIGHT_DRYING <<- FALSE
  r_old <- test_hfwi(df, timezone, FLAG_NO_MONTH_FACTOR = FALSE)
  r_direct <- test_hfwi(df, timezone, FLAG_NO_MONTH_FACTOR = TRUE)
  FLAG_NIGHT_DRYING <<- TRUE
  r_24hr <- test_hfwi(df, timezone, FLAG_NO_MONTH_FACTOR = TRUE)
  FLAG_NIGHT_DRYING <<- FALSE
  print(ggplot(r_old) +
    geom_line(aes(TIMESTAMP, DMC)) +
    geom_point(aes(TIMESTAMP, DDMC), data = r_old[hour(TIMESTAMP) == 16]) +
    geom_line(aes(TIMESTAMP, DMC), data = r_direct, colour = "red") +
    geom_line(aes(TIMESTAMP, DMC), data = r_24hr, colour = "blue"))
}

plot_cmp_timezones <- function(df, timezone) {
  r_all <- NULL
  FLAG_NIGHT_DRYING <<- FALSE
  r_old <- test_hfwi(df, timezone, FLAG_NO_MONTH_FACTOR = FALSE)
  r_old$TZ <- paste0("DEFAULT_", timezone, "_OLD")
  r_all <- rbind(r_all, r_old)
  r_direct <- test_hfwi(df, timezone, FLAG_NO_MONTH_FACTOR = TRUE)
  r_direct$TZ <- paste0("DEFAULT_", timezone)
  r_all <- rbind(r_all, r_direct)
  FLAG_NIGHT_DRYING <<- TRUE
  r_24hr <- test_hfwi(df, timezone, FLAG_NO_MONTH_FACTOR = TRUE)
  r_24hr$TZ <- "24HR"
  r_all <- rbind(r_all, r_24hr)
  FLAG_NIGHT_DRYING <<- FALSE
  r_tz <- list()
  # for (tz in -12:12) {
  for (tz in -7:7) {
    df_tz <- test_hfwi(df, tz, FLAG_NO_MONTH_FACTOR = TRUE)
    df_tz$TZ <- tz
    r_all <- rbind(r_all, df_tz)
    r_tz[tz] <- df_tz
  }
  date_max <- max(r_all$TIMESTAMP)
  # print(ggplot(data=r_all, mapping=aes(x=TIMESTAMP, y=DMC, group=TZ, colour=TZ)) +
  #         geom_line() +
  #         geom_point(aes(TIMESTAMP, DDMC), data = r_old[hour(TIMESTAMP) == 16]))
  #
  # print(ggplot(data=r_all, mapping=aes(x=TIMESTAMP, y=DMC, group=TZ, colour=TZ)) +
  #         geom_line() +
  #         geom_text(data = subset(r_all, TIMESTAMP == as.POSIXct(date_max)), aes(label = TZ, colour = TZ, x = Inf, y = DMC), hjust = -.1) +
  #         geom_point(aes(TIMESTAMP, DDMC), data = r_old[hour(TIMESTAMP) == 16]))

  print(ggplot(data = r_all, mapping = aes(x = TIMESTAMP, y = DMC, group = TZ, colour = TZ)) +
    geom_line() +
    geom_label_repel(aes(label = TZ), data = r_all[TIMESTAMP == date_max], nudge_x = 1, na.rm = TRUE) +
    geom_point(aes(TIMESTAMP, DDMC), data = r_old[hour(TIMESTAMP) == 16]))



  # p <- (ggplot(r_old) +
  #         geom_line(aes(TIMESTAMP, DMC)) +
  #         geom_point(aes(TIMESTAMP, DDMC), data = r_old[hour(TIMESTAMP) == 16]) +
  #         geom_line(aes(TIMESTAMP, DMC), data=r_direct, colour="red") +
  #         geom_line(aes(TIMESTAMP, DMC), data=r_24hr, colour="blue"))
  # for (tz in -12:12) {
  #   p <- p + geom_line(aes(TIMESTAMP, DMC), data=r_tz[tz])
  # }
  # p <- p + labs()
  print(p)
}

test_month_factor <- function() {
  df_wx <- as.data.table(read.csv("./data/test_hffmc.csv"))
  df_hourly <- as.data.table(read.csv("./data/wx_hourly.csv"))
  df_prf <- as.data.table(read.csv("./data/wx_prf.csv"))

  df_wx_no_prec <- copy(df_wx)
  df_wx_no_prec$prec <- 0.0

  df_hourly_no_prec <- copy(df_hourly)
  df_hourly_no_prec$prec <- 0.0


  df_prf_no_prec <- copy(df_prf)
  df_prf_no_prec$prec <- 0.0
  # df_prf_no_prec[, TIMESTAMP := make_datetime(yr, mon, day, hr, 0) - hours(5)]


  plot_cmp(df_wx, timezone = -6)
  plot_cmp(df_hourly, timezone = -6)

  plot_cmp(df_wx_no_prec, timezone = -6)
  plot_cmp(df_hourly_no_prec, timezone = -6)
  plot_cmp(df_prf_no_prec, timezone = -5)
  plot_cmp_timezones(df_prf_no_prec, timezone = -5)
}

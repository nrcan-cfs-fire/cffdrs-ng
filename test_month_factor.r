source("test_hFWI.r")

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
          geom_line(aes(TIMESTAMP, DMC), data=r_direct, colour="red") +
          geom_line(aes(TIMESTAMP, DMC), data=r_24hr, colour="blue"))

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
  
  
}

source("test_hFWI.r")

plot_cmp <- function(df) {
  r_old <- test_hfwi(df, FLAG_NO_MONTH_FACTOR = FALSE)
  r_direct <- test_hfwi(df, FLAG_NO_MONTH_FACTOR = TRUE)
  print(ggplot(r_old) +
          geom_line(aes(TIMESTAMP, DMC)) +
          geom_point(aes(TIMESTAMP, DDMC), data = r_old[hour(TIMESTAMP) == 16]) +
          geom_line(aes(TIMESTAMP, DMC), data=r_direct, colour="red"))

}

test_month_factor <- function() {
  df_wx <- as.data.table(read.csv("./data/test_hffmc.csv"))
  df_hourly <- as.data.table(read.csv("./data/wx_hourly.csv"))
  
  df_wx_no_prec <- copy(df_wx)
  df_wx_no_prec$prec <- 0.0
  
  df_hourly_no_prec <- copy(df_hourly)
  df_hourly_no_prec$prec <- 0.0
  
  
  plot_cmp(df_wx)
  plot_cmp(df_hourly)
  
  plot_cmp(df_wx_no_prec)
  plot_cmp(df_hourly_no_prec)
}

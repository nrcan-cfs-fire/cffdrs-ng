source("NG_FWI.r")
source("NG_FWI_old.r")
bak <- as.data.table(read.csv("./bak_hourly.csv"))


plot_compare <- function(df)
{
  result_old <- hFWI_old(df, timezone=-6)
  result <- hFWI(df, timezone=-6)
  # compare old results to new results
  plot(result_old$DC ~ result$DC)
  result_old[, TIMESTAMP := make_datetime(year, mon, day, hour)]
  result[, TIMESTAMP := make_datetime(year, mon, day, hour)]
  print(ggplot() + geom_line(mapping=aes(TIMESTAMP, DC), data=result, linetype="dashed", color="red") + geom_line(mapping=aes(TIMESTAMP, DC), data=result_old))
  print(ggplot() + geom_line(mapping=aes(TIMESTAMP, DC), data=result[mon==6], linetype="dashed", color="red") + geom_line(mapping=aes(TIMESTAMP, DC), data=result_old[mon==6]))
  print(ggplot() + geom_line(mapping=aes(TIMESTAMP, DC), data=result[mon==7], linetype="dashed", color="red") + geom_line(mapping=aes(TIMESTAMP, DC), data=result_old[mon==7]))
}

plot_compare(bak)

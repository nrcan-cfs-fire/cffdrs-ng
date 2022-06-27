library("gridExtra")
source("NG_FWI.r")
source("NG_FWI_old.r")
bak <- as.data.table(read.csv("./bak_hourly.csv"))


plot_compare <- function(df, main="")
{
  result_old <- hFWI_old(df, ffmc_old=85, dmc_old=40, dc_old=200, timezone=-6)
  result <- hFWI(df, ffmc_old=85, dmc_old=40, dc_old=200, timezone=-6)
  # compare old results to new results
  result_old[, TIMESTAMP := make_datetime(year, mon, day, hour)]
  result[, TIMESTAMP := make_datetime(year, mon, day, hour)]
  return(ggplot() + ggtitle(main) + geom_line(mapping=aes(TIMESTAMP, FFMC), data=result, linetype="dashed", color="red") + geom_line(mapping=aes(TIMESTAMP, FFMC), data=result_old))
}

bak_3day <- bak[1:72]
# bak_3day$rh <- 80
# bak_3day$temp <- 20
# bak_3day$wind <- 10
p_3day <- plot_compare(bak_3day, "3 Day")
bak_3day$rain <- 0.1
p_0_1 <- plot_compare(bak_3day, "0.1mm/hr")
bak_3day$rain <- 0.25
p_0_25 <- plot_compare(bak_3day, "0.25mm/hr")
bak_3day$rain <- 0.5
p_0_5 <- plot_compare(bak_3day, "0.5mm/hr")
grid.arrange(p_3day, p_0_1, p_0_25, p_0_5, nrow=2)

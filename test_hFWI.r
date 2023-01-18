#' Plots a comparison between hourly and daily values
source("NG_FWI.r")


test_hfwi <- function(df=cffdrs::test_hffmc, timezone=-6)
{
  # set up as if we had called hFWI
  weatherstream <- data.table(df)
  r <- hFWI(weatherstream, timezone=timezone, ffmc_old=FFMC_DEFAULT, dmc_old=DMC_DEFAULT, dc_old=DC_DEFAULT)
  # want to figure out what daily values would have been with old function
  w <- copy(weatherstream)
  w[, timestamp := as_datetime(sprintf('%04d-%02d-%02d %02d:%02d:00', yr, mon, day, hr, 0))]
  r[, TIMESTAMP := as_datetime(sprintf('%04d-%02d-%02d %02d:%02d:00', yr, mon, day, hr, 0))]
  colnames(w) <- toupper(colnames(w))
  colnames(r) <- toupper(colnames(r))
  d <- toDaily(w)
  d[, LAT := DEFAULT_LATITUDE]
  d[, LONG := DEFAULT_LONGITUDE]
  daily <- cffdrs::fwi(d, init=c(FFMC_DEFAULT, DMC_DEFAULT, DC_DEFAULT))
  setnames(daily,
           c('FFMC', 'DMC', 'DC', 'ISI', 'BUI', 'FWI', 'DSR'),
           c('DFFMC', 'DDMC', 'DDC', 'DISI', 'DBUI', 'DFWI', 'DDSR'))
  r <- merge(r,
             daily[, c('YR', 'MON', 'DAY', 'DFFMC', 'DDMC', 'DDC', 'DISI', 'DBUI', 'DFWI', 'DDSR')],
             by=c('YR', 'MON', 'DAY'))
  r[, DISI := cffdrs:::.ISIcalc(DFFMC, WS, fbpMod = FALSE)]
  r[, DBUI := cffdrs:::.buiCalc(DDMC, DDC)]
  r[, DFWI := cffdrs:::.fwiCalc(DISI, DBUI)]
  # taken from package code
  r[, DDSR := 0.0272 * (DFWI ^ 1.77)]
  # output input and FWI columns so git can tell us if they change
  r <- r[, c('TIMESTAMP', 'TEMP', 'WS', 'RH', 'PREC',
             'FFMC', 'DMC', 'DC', 'ISI', 'BUI', 'FWI', 'DSR',
             'DFFMC', 'DDMC', 'DDC', 'DISI', 'DBUI', 'DFWI', 'DDSR')]
  write.table(r, file='out.csv', sep=',', row.names=FALSE)
  return(r)
}

plot_comparison <- function(r)
{
  print(ggplot(r) + geom_line(aes(TIMESTAMP, DMC)) + geom_point(aes(TIMESTAMP, DDMC), data=r[hour(TIMESTAMP) == 16]))
  print(ggplot(r) + geom_line(aes(TIMESTAMP, DC)) + geom_point(aes(TIMESTAMP, DDC), data=r[hour(TIMESTAMP) == 16]))
}

plot_test <- function(df=cffdrs::test_hffmc)
{
  r <- test_hfwi(df)
  plot_comparison(r)
}

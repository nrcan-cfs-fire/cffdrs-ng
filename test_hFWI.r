source("hFWI.r")

toDaily <- function(w, all=FALSE)
{
  # split into morning and afternoon so we can assign rain to the proper fwi 'day'
  # NOTE: actually need to figure out what makes the most sense
  # - if we split at 12 then that means we're in LST not LDT
  # - the rain at 12 is from 1100-1200, so that should be part of today's calculation, not tomorrow's
  wx <- copy(w)
  # set DATE field in case there's only a TIMESTAMP
  wx[, DATE := as.character(as.Date(TIMESTAMP))]
  # use toDecimal() so we only need TIMESTAMP field and we can deal with minutes or even seconds
  wx[, FOR_DATE := ifelse(toDecimal(TIMESTAMP) <= 12, as.character(DATE), as.character(as.Date(DATE) + 1))]
  # wx[, FOR_DATE := DATE]
  precip <- wx[, list(PREC = sum(PREC, na.rm=TRUE)), by=c('FOR_DATE')]
  setnames(precip, 'FOR_DATE', 'DATE')
  merged <- merge(wx[toDecimal(TIMESTAMP) == 12, -c('FOR_DATE', 'PREC')], precip, by=c('DATE'), all=all)
  merged$PREC <- nafill(merged$PREC, fill=0.0)
  if (all)
  {
    # fix up columns that would be missing values if no noon value for a day
    merged[, TIMESTAMP := as_datetime(sprintf('%s 12:00:00', as.character(DATE)))]
    merged[, YR := year(TIMESTAMP)]
    merged[, MON := month(TIMESTAMP)]
    merged[, DAY := day(TIMESTAMP)]
    merged[, HR := hour(TIMESTAMP)]
    merged[, MINUTE := minute(TIMESTAMP)]
    merged[, ID := na.omit(unique(merged$ID)[[1]])]
    merged[, LAT := na.omit(unique(merged$LAT)[[1]])]
    merged[, LONG := na.omit(unique(merged$LONG)[[1]])]
    # use default drying day indices from weather guide
    merged$TEMP <- nafill(merged$TEMP, fill=21.1)
    merged$RH <- nafill(merged$RH, fill=45)
    merged$WS <- nafill(merged$WS, fill=13)
  }
  return(merged)
}

test_hfwi <- function(df=test_hffmc, timezone=-6)
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
  daily <- fwi(d, init=c(FFMC_DEFAULT, DMC_DEFAULT, DC_DEFAULT))
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

plot_test <- function(df=test_hffmc)
{
  r <- test_hfwi(df)
  plot_comparison(r)
}

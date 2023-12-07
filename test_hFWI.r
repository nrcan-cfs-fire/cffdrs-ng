#' Plots a comparison between hourly and daily values
source("NG_FWI.r")
source("old_cffdrs.r")

test_hfwi <- function(df = read.csv("./data/test_hffmc.csv"), timezone = -6, filename="out.csv") {
  # set up as if we had called hFWI
  weatherstream <- data.table(df)
  r <- hFWI(weatherstream, timezone = timezone, ffmc_old = FFMC_DEFAULT, dmc_old = DMC_DEFAULT, dc_old = DC_DEFAULT)
  # want to figure out what daily values would have been with old function
  w <- copy(weatherstream)
  colnames(w) <- toupper(colnames(w))
  colnames(r) <- toupper(colnames(r))
  w[, TIMESTAMP := as_datetime(sprintf("%04d-%02d-%02d %02d:%02d:00", YR, MON, DAY, HR, 0))]
  r[, TIMESTAMP := as_datetime(sprintf("%04d-%02d-%02d %02d:%02d:00", YR, MON, DAY, HR, 0))]
  d <- toDaily(w)
  if (!("LAT" %in% names(d))) {
    d[, LAT := DEFAULT_LATITUDE]
  }
  if (!("LONG" %in% names(d))) {
    d[, LONG := DEFAULT_LONGITUDE]
  }
  daily <- daily_fwi(d, init = c(FFMC_DEFAULT, DMC_DEFAULT, DC_DEFAULT))
  setnames(
    daily,
    c("FFMC", "DMC", "DC", "ISI", "BUI", "FWI", "DSR"),
    c("DFFMC", "DDMC", "DDC", "DISI", "DBUI", "DFWI", "DDSR")
  )
  cols_id <- intersect(names(r), c("ID", "LAT", "LONG", "TIMEZONE", "YR", "MON", "DAY"))
  cols_daily <- c(cols_id, "DFFMC", "DDMC", "DDC", "DISI", "DBUI", "DFWI", "DDSR")
  daily <- daily[, ..cols_daily]
  r <- merge(r,
    daily,
    by = cols_id
  )
  r[, DISI := Vectorize(initial_spread_index)(WS, DFFMC)]
  r[, DBUI := Vectorize(buildup_index)(DDMC, DDC)]
  r[, DFWI := Vectorize(fire_weather_index)(DISI, DBUI)]
  # taken from package code
  r[, DDSR := Vectorize(daily_severity_rating)(DFWI^1.77)]
  # output input and FWI columns so git can tell us if they change
  cols_cmp <- c(cols_id,
                "TIMESTAMP", "TEMP", "WS", "RH", "PREC",
                "FFMC", "DMC", "DC", "ISI", "BUI", "FWI", "DSR",
                "DFFMC", "DDMC", "DDC", "DISI", "DBUI", "DFWI", "DDSR"
  )
  r <- r[, ..cols_cmp]
  if (!is.null(filename)) {
    write.table(r, file = filename, sep = ",", row.names = FALSE)
  }
  return(r)
}

plot_comparison <- function(r) {
  # print(ggplot(r) +
  #   geom_line(aes(TIMESTAMP, DMC)) +
  #   geom_point(aes(TIMESTAMP, DDMC), data = r[hour(TIMESTAMP) == 16]))
  print(ggplot(r) +
    # ggtitle(sprintf("Equation %dx%dx%d", eqn_k, eqn, eqn_j)) +
    ggtitle(sprintf("K <- 0.0914 * 3.937 / 24.0 * F\n pe <- K * (temp + %f)\nF <- %f", OFFSET_TEMP, DC_K_HOURLY)) +
    geom_line(aes(TIMESTAMP, DC)) +
    geom_point(aes(TIMESTAMP, DDC), data = r[hour(TIMESTAMP) == 16]))
}

plot_test <- function(df = read.csv("./data/test_hffmc.csv"), timezone = -6) {
  r <- test_hfwi(df, timezone)
  plot_comparison(r)
}

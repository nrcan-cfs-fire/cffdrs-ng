# HACK: include code we use from cffdrs library so it doesn't add dependency for terra
#source('NG_FWI.r')

FFMCcalc <- function (ffmc_yda, temp, rh, ws, prec) 
{
  wmo <- 147.27723 * (101 - ffmc_yda)/(59.5 + ffmc_yda)
  ra <- ifelse(prec > 0.5, prec - 0.5, prec)
  wmo <- ifelse(prec > 0.5, ifelse(wmo > 150, wmo + 0.0015 * 
                                     (wmo - 150) * (wmo - 150) * sqrt(ra) + 42.5 * ra * exp(-100/(251 - 
                                                                                                    wmo)) * (1 - exp(-6.93/ra)), wmo + 42.5 * ra * exp(-100/(251 - 
                                                                                                                                                               wmo)) * (1 - exp(-6.93/ra))), wmo)
  wmo <- ifelse(wmo > 250, 250, wmo)
  ed <- 0.942 * (rh^0.679) + (11 * exp((rh - 100)/10)) + 0.18 * 
    (21.1 - temp) * (1 - 1/exp(rh * 0.115))
  ew <- 0.618 * (rh^0.753) + (10 * exp((rh - 100)/10)) + 0.18 * 
    (21.1 - temp) * (1 - 1/exp(rh * 0.115))
  z <- ifelse(wmo < ed & wmo < ew, 0.424 * (1 - (((100 - rh)/100)^1.7)) + 
                0.0694 * sqrt(ws) * (1 - ((100 - rh)/100)^8), 0)
  x <- z * 0.581 * exp(0.0365 * temp)
  wm <- ifelse(wmo < ed & wmo < ew, ew - (ew - wmo)/(10^x), 
               wmo)
  z <- ifelse(wmo > ed, 0.424 * (1 - (rh/100)^1.7) + 0.0694 * 
                sqrt(ws) * (1 - (rh/100)^8), z)
  x <- z * 0.581 * exp(0.0365 * temp)
  wm <- ifelse(wmo > ed, ed + (wmo - ed)/(10^x), wm)
  ffmc1 <- (59.5 * (250 - wm))/(147.2 + wm)
  ffmc1 <- ifelse(ffmc1 > 101, 101, ffmc1)
  ffmc1 <- ifelse(ffmc1 < 0, 0, ffmc1)
  return(ffmc1)
}

DMCcalc <- function (dmc_yda, temp, rh, prec, lat, mon, lat.adjust = TRUE) 
{
  ell01 <- c(6.5, 7.5, 9, 12.8, 13.9, 13.9, 12.4, 10.9, 9.4, 
             8, 7, 6)
  ell02 <- c(7.9, 8.4, 8.9, 9.5, 9.9, 10.2, 10.1, 9.7, 9.1, 
             8.6, 8.1, 7.8)
  ell03 <- c(10.1, 9.6, 9.1, 8.5, 8.1, 7.8, 7.9, 8.3, 8.9, 
             9.4, 9.9, 10.2)
  ell04 <- c(11.5, 10.5, 9.2, 7.9, 6.8, 6.2, 6.5, 7.4, 8.7, 
             10, 11.2, 11.8)
  temp <- ifelse(temp < (-1.1), -1.1, temp)
  rk <- 1.894 * (temp + 1.1) * (100 - rh) * ell01[mon] * 1e-04
  if (lat.adjust) {
    rk <- ifelse(lat <= 30 & lat > 10, 1.894 * (temp + 1.1) * 
                   (100 - rh) * ell02[mon] * 1e-04, rk)
    rk <- ifelse(lat <= -10 & lat > -30, 1.894 * (temp + 
                                                    1.1) * (100 - rh) * ell03[mon] * 1e-04, rk)
    rk <- ifelse(lat <= -30 & lat >= -90, 1.894 * (temp + 
                                                     1.1) * (100 - rh) * ell04[mon] * 1e-04, rk)
    rk <- ifelse(lat <= 10 & lat > -10, 1.894 * (temp + 1.1) * 
                   (100 - rh) * 9 * 1e-04, rk)
  }
  pr <- ifelse(prec <= 1.5, dmc_yda, {
    ra <- prec
    rw <- 0.92 * ra - 1.27
    wmi <- 20 + 280/exp(0.023 * dmc_yda)
    b <- ifelse(dmc_yda <= 33, 100/(0.5 + 0.3 * dmc_yda), 
                ifelse(dmc_yda <= 65, 14 - 1.3 * log(dmc_yda), 6.2 * 
                         log(dmc_yda) - 17.2))
    wmr <- wmi + 1000 * rw/(48.77 + b * rw)
    43.43 * (5.6348 - log(wmr - 20))
  })
  pr <- ifelse(pr < 0, 0, pr)
  dmc1 <- pr + rk
  dmc1 <- ifelse(dmc1 < 0, 0, dmc1)
  return(dmc1)
}

DCcalc <- function (dc_yda, temp, rh, prec, lat, mon, lat.adjust = TRUE) 
{
  fl01 <- c(-1.6, -1.6, -1.6, 0.9, 3.8, 5.8, 6.4, 5, 2.4, 0.4, 
            -1.6, -1.6)
  fl02 <- c(6.4, 5, 2.4, 0.4, -1.6, -1.6, -1.6, -1.6, -1.6, 
            0.9, 3.8, 5.8)
  temp <- ifelse(temp < (-2.8), -2.8, temp)
  pe <- (0.36 * (temp + 2.8) + fl01[mon])/2
  if (lat.adjust) {
    pe <- ifelse(lat <= -20, (0.36 * (temp + 2.8) + fl02[mon])/2, 
                 pe)
    pe <- ifelse(lat > -20 & lat <= 20, (0.36 * (temp + 2.8) + 
                                           1.4)/2, pe)
  }
  pe <- ifelse(pe < 0, 0, pe)
  ra <- prec
  rw <- 0.83 * ra - 1.27
  smi <- 800 * exp(-1 * dc_yda/400)
  dr0 <- dc_yda - 400 * log(1 + 3.937 * rw/smi)
  dr0 <- ifelse(dr0 < 0, 0, dr0)
  dr <- ifelse(prec <= 2.8, dc_yda, dr0)
  dc1 <- dr + pe
  dc1 <- ifelse(dc1 < 0, 0, dc1)
  return(dc1)
}

fwi <- function (input, init = data.frame(ffmc = 85, dmc = 6, dc = 15, 
                                   lat = 55), batch = TRUE, out = "all", lat.adjust = TRUE, 
          uppercase = TRUE) 
{
  if (!is.na(charmatch("input", search()))) {
    detach(input)
  }
  names(input) <- tolower(names(input))
  if (is.vector(init)) {
    init <- as.data.frame(t(init))
  }
  names(init) <- tolower(names(init))
  if (substr(names(init), 1, 1)[1] == "x" | substr(names(init), 
                                                   1, 1)[1] == "v") {
    if (ncol(init) == 3) {
      names(init) <- c("ffmc", "dmc", "dc")
      init$lat <- 55
    }
    else if (ncol(init) == 4) {
      names(init) <- c("ffmc", "dmc", "dc", 
                       "lat")
    }
  }
  ffmc_yda <- init$ffmc
  dmc_yda <- init$dmc
  dc_yda <- init$dc
  if ("lat" %in% names(input)) {
    lat <- input$lat
  }
  else {
    warning("latitude was not provided, assign default value 55")
    lat <- rep(55, nrow(input))
  }
  if ("long" %in% names(input)) {
    long <- input$long
  }
  else {
    warning("long was not provided, assign a default number -120")
    long <- rep(-120, nrow(input))
  }
  if ("yr" %in% names(input)) {
    yr <- as.numeric(as.character(input$yr))
  }
  else {
    warning("Year was not provided, assigned default number 5000")
    yr <- rep(5000, nrow(input))
  }
  if ("mon" %in% names(input)) {
    mon <- as.numeric(as.character(input$mon))
  }
  else {
    warning("Month was not provided, assigned the default value, July")
    mon <- rep(7, nrow(input))
  }
  if ("day" %in% names(input)) {
    day <- as.numeric(as.character(input$day))
  }
  else {
    warning("Day was not provided, assigned default number -99")
    day <- rep(-99, nrow(input))
  }
  if (batch) {
    if ("id" %in% names(input)) {
      input <- input[with(input, order(yr, mon, day, id)), 
      ]
      n <- length(unique(input$id))
      if (length(unique(input[1:n, "id"])) != n) {
        stop("Multiple stations have to start and end at the same dates, and \n             input data must be sorted by date/time and id")
      }
    }
    else {
      n <- 1
    }
  }
  else {
    n <- nrow(input)
  }
  temp <- input$temp
  prec <- input$prec
  ws <- input$ws
  rh <- input$rh
  if (!exists("temp") | is.null(temp)) 
    stop("temperature (temp) is missing!")
  if (!exists("prec") | is.null(prec)) 
    stop("precipitation (prec) is missing!")
  if (length(prec[prec < 0]) > 0) 
    stop("precipiation (prec) cannot be negative!")
  if (!exists("ws") | is.null(ws)) 
    stop("wind speed (ws) is missing!")
  if (length(ws[ws < 0]) > 0) 
    stop("wind speed (ws) cannot be negative!")
  if (!exists("rh") | is.null(rh)) 
    stop("relative humidity (rh) is missing!")
  if (length(rh[rh < 0]) > 0) 
    stop("relative humidity (rh) cannot be negative!")
  if (length(temp)%%n != 0) 
    warning("Missing records may generate wrong outputs")
  if (nrow(init) == 1 & n > 1) {
    warning("Same initial data were used for multiple weather stations")
    ffmc_yda <- rep(ffmc_yda, n)
    dmc_yda <- rep(dmc_yda, n)
    dc_yda <- rep(dc_yda, n)
  }
  if (nrow(init) > 1 & nrow(init) != n) {
    stop("Number of initial values do not match with number of weather \n         stations")
  }
  n0 <- length(temp)/n
  ffmc <- dmc <- dc <- isi <- bui <- fwi <- dsr <- NULL
  for (i in 1:n0) {
    k <- ((i - 1) * n + 1):(i * n)
    rh[k] <- ifelse(rh[k] >= 100, 99.9999, rh[k])
    ffmc1 = FFMCcalc(ffmc_yda, temp[k], rh[k], ws[k], prec[k])
    dmc1 = DMCcalc(dmc_yda, temp[k], rh[k], prec[k], lat[k], 
                    mon[k], lat.adjust)
    dc1 <- DCcalc(dc_yda, temp[k], rh[k], prec[k], lat[k], 
                   mon[k], lat.adjust)
    isi1 <- ISIcalc(ffmc1, ws[k])
    bui1 <- BUIcalc(dmc1, dc1)
    fwi1 <- FWIcalc(isi1, bui1)
    dsr1 <- 0.0272 * (fwi1^1.77)
    ffmc <- c(ffmc, ffmc1)
    dmc <- c(dmc, dmc1)
    dc <- c(dc, dc1)
    isi <- c(isi, isi1)
    bui <- c(bui, bui1)
    fwi <- c(fwi, fwi1)
    dsr <- c(dsr, dsr1)
    ffmc_yda <- ffmc1
    dmc_yda <- dmc1
    dc_yda <- dc1
  }
  if (out == "fwi") {
    new_FWI <- data.frame(ffmc = ffmc, dmc = dmc, dc = dc, 
                          isi = isi, bui = bui, fwi = fwi, dsr = dsr)
    if (uppercase) {
      names(new_FWI) <- toupper(names(new_FWI))
    }
  }
  else {
    if (out == "all") {
      new_FWI <- cbind(input, ffmc, dmc, dc, isi, bui, 
                       fwi, dsr)
      if (uppercase) {
        names(new_FWI) <- toupper(names(new_FWI))
      }
    }
  }
  return(new_FWI)
}

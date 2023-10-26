library(ggplot2)
library(ggrepel)
source("test_hFWI.r")

HOUR_PEAK <- 16

prep_df <- function(df, hour_split=HOUR_PEAK)
{
  df[, DATE := as.Date(make_date(YR, MON, DAY))]
  df_noon <- df[12 == HR, ]
  df_noon[, PE_DAILY := dmc_drying_direct(LAT, LONG, TEMP, RH, WS, RAIN, MON, k=DEFAULT_K_DMC_DRYING) * EL_DMC[[1]][MON]]
  df_noon[, FOR_DATE := DATE]
  df[, FOR_DATE := fifelse(HR <= hour_split, DATE, DATE + 1)]
  df[, SUNRISE := round(SUNRISE)]
  df[, SUNSET := round(SUNSET)]
  df[, IS_DAYLIGHT := ((HR >= SUNRISE) & (HR < SUNSET))]
  df_both <- merge(df_noon[, c("FOR_DATE", "PE_DAILY")], df, by=c("FOR_DATE"))
  return(df_both)
}

cmp_dmc_drying <- function(df_both, k = DEFAULT_K_DMC_DRYING) {
    # calculate from split to split since that's when we're saying the daily value applies, so drying must be from last split
    # warning about this if we just use IS_DAYLIGHT
    df_daylight <- df_both[TRUE == IS_DAYLIGHT,]
    df_daylight[, PE := dmc_drying_direct(LAT, LONG, TEMP, RH, WS, RAIN, MON, k)]
    df_pe <- df_daylight[, list(PE=sum(PE)), by=list(FOR_DATE)]
    df_score <- merge(df_both, df_pe, by=c("FOR_DATE"))
    df_score[, DIFF := PE - PE_DAILY]
    df_score[, SCORE := abs(DIFF)]
    # score is expected difference from PE_DAILY per day
    score <- sum(df_score$SCORE) / nrow(df_pe)
    return(score)
}

find_k <- function(df, hour_split=HOUR_PEAK) {
  df_both <- prep_df(df, hour_split)
  cmp_df <- function(k) {
    return(cmp_dmc_drying(df_both, k))
  }
  nlm(cmp_df, DEFAULT_K_DMC_DRYING)
}


read_file <- function(file_in = "./data/test_hffmc.csv", timezone = -6) {
    df <- as.data.table(read.csv(file_in))
    names(df) <- tolower(names(df))
    # lat <- ifelse("lat" %in% names(df), df$lat[[1]], DEFAULT_LATITUDE)
    # long <- ifelse("long" %in% names(df), df$long[[1]], DEFAULT_LONGITUDE)
    if (!("lat" %in% names(df))) {
      df$lat <- DEFAULT_LATITUDE
    }
    if (!("long" %in% names(df))) {
      df$long <- DEFAULT_LATITUDE
    }
    names(df) <- toupper(names(df))
    df$TIMEZONE <- timezone
    df[, TIMESTAMP := as_datetime(sprintf("%04d-%02d-%02d %02d:%02d:00", YR, MON, DAY, HR, 0))]
    df <- getSunlightDT(df)
    return(df)
}

df_wx <- read_file("./data/test_hffmc.csv")
df_hourly <- read_file("./data/wx_hourly.csv")
df_prf <- read_file("./data/wx_prf.csv")


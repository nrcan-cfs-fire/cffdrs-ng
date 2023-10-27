library(ggplot2)
library(ggrepel)
source("test_hFWI.r")

HOUR_PEAK <- 16
COLS_DMC_K <- c("ID", "LAT", "LONG", "FOR_DATE", "YR", "MON", "DAY", "HR", "PREC24", "TEMP", "RH")

prep_df <- function(df_wx) {
  df <- copy(df_wx)
  df[, DATE := as.Date(TIMESTAMP)]
  df[, FOR_DATE := fifelse(HR <= hour_split, DATE, DATE + 1)]
  df[, PREC24 := sum(PREC), by=list(ID, YR, FOR_DATE)]
  df[, SUNRISE := round(SUNRISE)]
  df[, SUNSET := round(SUNSET)]
  df[, IS_DAYLIGHT := ((HR >= SUNRISE) & (HR < SUNSET))]
  df_daylight <- df[TRUE == IS_DAYLIGHT,]
  df_daylight <- df_daylight[, ..COLS_DMC_K]
  return(df_daylight)
}
#
# find_k <- function(df, hour_split=HOUR_PEAK, do_prep=TRUE) {
#   if (do_prep) {
#     df <- prep_df(df)
#   }
#   df_noon <- df[12 == HR, ]
#   df_noon[, PE_DAILY := dmc_drying_direct(TEMP, RH, k=DEFAULT_K_DMC_DRYING) * EL_DMC[[1]][MON]]
#   df_noon <- df_noon[, c("FOR_DATE", "PE_DAILY")]
#   cmp_df <- function(k) {
#     # calculate from split to split since that's when we're saying the daily value applies, so drying must be from last split
#     # warning about this if we just use IS_DAYLIGHT
#     # df_daylight[, PE := dmc_drying_direct(TEMP, RH, k)]
#     # df_pe <- df_daylight[, list(PE=sum(PE)), by=list(FOR_DATE)]
#     df_pe <- df[, list(PE=sum(dmc_drying_direct(TEMP, RH, k))), by=list(FOR_DATE)]
#     df_score <- merge(df_noon, df_pe, by=c("FOR_DATE"))
#     # df_score[, DIFF := PE - PE_DAILY]
#     # df_score[, SCORE := abs(DIFF)]
#     # # score is expected difference from PE_DAILY per day
#     # score <- sum(df_score$SCORE) / nrow(df_score)
#     score <- sum(abs(df_score$PE - df_score$PE_DAILY)) / nrow(df_score)
#     return(score)
#   }
#   nlm(cmp_df, DEFAULT_K_DMC_DRYING)
# }



solve_k <- function(df_wx, hour_split=HOUR_PEAK) {
  df_prepared <- prep_df(df_wx)
  df_noon <- df_prepared[12 == HR, ]
  df_noon[, PE_DAILY := dmc_drying_direct(TEMP, RH, k=DEFAULT_K_DMC_DRYING) * EL_DMC[[1]][MON]]
  df_noon <- df_noon[, c("ID", "YR", "FOR_DATE", "PE_DAILY")]
  find_k <- function(df) {
    cmp_df <- function(k) {
      # return(0.0)
      # calculate from split to split since that's when we're saying the daily value applies, so drying must be from last split
      # warning about this if we just use IS_DAYLIGHT
      # df_daylight[, PE := dmc_drying_direct(TEMP, RH, k)]
      # df_pe <- df_daylight[, list(PE=sum(PE)), by=list(FOR_DATE)]
      df_pe <- df[, list(PE=sum(dmc_drying_direct(TEMP, RH, k))), by=list(ID, FOR_DATE)]
      df_score <- merge(df_noon, df_pe, by=c("ID", "FOR_DATE"))
      # df_score[, DIFF := PE - PE_DAILY]
      # df_score[, SCORE := abs(DIFF)]
      # # score is expected difference from PE_DAILY per day
      # score <- sum(df_score$SCORE) / nrow(df_score)
      score <- sum(abs(df_score$PE - df_score$PE_DAILY)) / nrow(df_score)
      return(score)
    }
    nlm(cmp_df, DEFAULT_K_DMC_DRYING)
  }
  score_df <- function(df, cols_groups) {
    # n <- 0
    # total <- 0
    # add_total <- function(df_sub, v = NULL) {
    #   # total <<- total + nrow(df_sub)
    #   if (is.null(v))
    #   {
    #     v <- nrow(unique(df_sub))
    #   }
    #   n <<- n + v
    # }
    # add_total(df, v = 1)
    # df_groups <- df[, ..cols_groups]
    # add_total(df_groups)
    # for (col in cols_groups) {
    #   add_total(df[, list(col)])
    # }
    # # # if set of groups contains all the rows overall then it's based on unique ways to pick subsetss
    # # total <- (2 ^ length(cols_groups)) * nrow(df)
    # for_groups <- function(fct, df_group, cols_left=cols_groups) {
    #   fct(df_group)
    #   # shouldn't loop if cols_left is empty
    #   for (col in cols_left) {
    #     by_col <- split(df_group, df_group[[col]])
    #     for (v in names(by_col)) {
    #       df_sub <- by_col[[v]]
    #       cols_sub <- setdiff(cols_left, c(col))
    #       if (0 < length(cols_sub)) {
    #         for_groups(fct, df_sub, cols_sub)
    #       }
    #     }
    #   }
    # }
    for_groups <- function(fct, df_group, cols_set=list(), cols_left=cols_groups) {
      if (0 == length(cols_left)) {
        # have group if cols_left is empty
        fct(df_group, cols_set)
      }
      else {
        col <- cols_left[[1]]
        cols_sub <- setdiff(cols_left, c(col))
        # call with whole thing but less columns first
        cols_set[col] <- "__ALL__"
        for_groups(fct, df_group, cols_set, cols_sub)
        by_col <- split(df_group, df_group[[col]])
        for (v in names(by_col)) {
          cols_set[col] <- v
          df_sub <- by_col[[v]]
          for_groups(fct, df_sub, cols_set, cols_sub)
        }
      }
    }
    n <- 0
    total <- 0
    add_total <- function(df_sub, cols_set) {
      total <<- total + nrow(df_sub)
      n <<- n + 1
    }
    for_groups(add_total, df)
    # n <- 0
    # # if set of groups contains all the rows overall then it's based on unique ways to pick subsets, plus 1 for no subset
    # total <- (2 ^ length(cols_groups)) * nrow(df)
    # for (col in cols_groups) {
    #
    # }
    print(sprintf("Processing %d groups with a total of %d rows", n, total))
    # tqdm bar is:
    # 100%|███████████████████████████████████████| 100/100 [00:01<00:00, 97.89it/s]
    pbar_format <- "[:percent] |:bar| [:i/:n groups, :current/:total rows] [:elapsedfull<:eta, :tick_rate rows/s]"
    pb_all <- progress_bar$new(
      total=total,
      format = pbar_format,
      clear=FALSE
    )
    # show progress right away since first fit takes longest
    i <- 0
    results <- NULL
    add_row <- function(df_group, cols_set) {
      # # HACK: some wasted effort because we already subset by these, but do this
      # #       so maybe recursion is easier?
      # for_group <- list()
      # for (col in cols_groups) {
      #   values <- unique(df_group[[col]])
      #   if (1 == length(values)) {
      #     for_group[col] <- values[1]
      #   } else {
      #     for_group[col] <- "__ALL__"
      #   }
      # }
      for_group <- copy(cols_set)
      for_group["LAT"] <- mean(df_group$LAT)
      for_group["LONG"] <- mean(df_group$LONG)
      # print(as.data.table(for_group))
      r_k <- find_k(df_group)
      r <- cbind(
        as.data.table(for_group),
        as.data.table(r_k)
      )
      results <<- rbind(results, r)
      i <<- i + 1
      pb_all$tick(nrow(df_group), tokens = list(i = i, n = n))
    }
    # add_subsets <- function(df_group, cols_left) {
    #   add_row(df_group)
    #   # shouldn't loop if cols_left is empty
    #   for (col in cols_left) {
    #     by_col <- split(df_group, df_group[[col]])
    #     for (v in names(by_col)) {
    #       df_sub <- by_col[[v]]
    #       cols_sub <- setdiff(cols_left, c(col))
    #       if (0 < length(cols_sub)) {
    #         add_subsets(df_sub, cols_sub)
    #       }
    #     }
    #   }
    # }
    # add_subsets(df, cols_groups)
    for_groups(add_row, df)
    # add_row(df, "__ALL__", "__ALL__")
    # years <- unique(df$YR)
    # for (year in sort(years)) {
    #   add_row(df[year == YR], "__ALL__", year)
    # }
    # for (stn in stns) {
    #   df_stn <- df[ID == stn, ]
    #   add_row(df_stn, stn, "__ALL__")
    #   for (year in sort(unique(df_stn$YR))) {
    #     add_row(df_stn[year == YR], stn, year)
    #   }
    # }
    df_results <- data.table(results)
    # df_results$ID <- as.factor(df_results$ID)
    # df_results$YR <- as.factor(df_results$YR)
    for (col in cols_groups) {
      df_results[[col]] <- as.factor(df_results[[col]])
    }
    # print(i)
    return(df_results)
  }
  # #df_prepared <- df_prepared[YR >= 2018]
  # cols_groups <- c("ID", "YR", "MON")
  # df_results_all <- score_df(df_prepared, cols_groups)
  # df_results_no_prec <- score_df(df_prepared[0.0 == PREC24], cols_groups)
  df_prepared[, HAD_RAIN := PREC24 > 0]
  df_relevant <- df_prepared[, -c("DAY", "HR", "PREC24")]
  df_results <- score_df(df_relevant, c("ID", "YR", "MON", "HAD_RAIN"))
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


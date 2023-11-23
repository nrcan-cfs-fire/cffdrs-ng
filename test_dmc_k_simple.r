library(ggplot2)
library(ggrepel)
library(promises)
source("test_hFWI_fitting.r")

HOUR_PEAK <- 16

dc_drying_rate <- function(temp, k=1.0) {
   return(k * temp)
}


prep_df <- function(df_wx, hour_split) {
  df_hourly <- copy(df_wx)
  df_hourly[, DATE := as.Date(TIMESTAMP)]
  df_hourly[, FOR_DATE := fifelse(HR <= hour_split, DATE, DATE + 1)]
  # remove any hours with TEMP <= -2.8
  df_hourly <- df_hourly[TEMP > -2.8, ]
  df_hours <- df_hourly[, list(num_hours=.N), by=list(ID, FOR_DATE)]
  # remove partial days
  df_hours <- df_hours[num_hours == 24, ]
  df_hourly <- merge(df_hours, df_hourly)
  df_hourly[, PREC24 := sum(PREC), by=list(ID, YR, FOR_DATE)]
  df_hourly[, HAD_RAIN := ifelse(PREC24 > 0, "T", "F")]
  df_hourly <- df_hourly["F" == HAD_RAIN,]
  df_noon <- df_hourly[12 == HR, ]
  df_noon[, PE_DAILY := dc_drying_daily(TEMP, MON)]
  df_noon <- df_noon[, list(ID, FOR_DATE, PE_DAILY)]
  df_hourly[, DC_DRYING := dc_drying_rate(TEMP)]
  df_hourly <- df_hourly[, list(ID, LAT, LONG, FOR_DATE, HR, DC_DRYING)]
  df_pe <- df_hourly[, list(PE_SUM=sum(DC_DRYING)), by=list(ID, LAT, LONG, FOR_DATE)]
  df_pe[, YR := year(FOR_DATE)]
  df_pe[, MON := month(FOR_DATE)]
  df <- merge(df_pe, df_noon[, c("ID", "FOR_DATE", "PE_DAILY")], by=c("ID", "FOR_DATE"))
  return(df)
}


solve_k <- function(df_wx, hour_split=HOUR_PEAK) {
  score_df <- function(df, cols_groups) {
    print(paste0("Running with groups based on: [", paste0(cols_groups, collapse=", "), "]"))
    # df_noon <- unique(df[, c("ID", "FOR_DATE", "PE_DAILY")])
    find_k <- function(df) {
      cmp_df <- function(k) {
        return(mean(abs(df[, list(PE_DAILY=min(PE_DAILY), PE=sum(k * PE_SUM)), by=list(ID, FOR_DATE)][, PE_DAILY - PE])))
      }
      return(optimize(cmp_df, c(1E-6, 10)))
    }
    for_groups <- function(fct, df_group, cols_set=list(), cols_left=cols_groups) {
      if (0 == length(cols_left)) {
        # have group if cols_left is empty
        return(fct(df_group, cols_set))
      }
      else {
        col <- cols_left[[1]]
        cols_left <- setdiff(cols_left, c(col))
        # call with whole thing but less columns first
        cols_set[col] <- "*"
        results_group <- NULL
        results_group <- rbind(results_group, for_groups(fct, df_group, cols_set, cols_left))
        by_col <- split(df_group, df_group[[col]])
        for (v in names(by_col)) {
          cols_set[col] <- v
          df_sub <- by_col[[v]]
          results_group <- rbind(results_group, for_groups(fct, df_sub, cols_set, cols_left))
        }
      }
      return(results_group)
    }
    add_total <- function(df_sub, cols_set) {
      return(data.table(n=1, total=nrow(df_sub)))
    }
    cnt <- for_groups(add_total, df)
    n <- sum(cnt$n)
    total <- sum(cnt$total)
    # seems like there should be a way to get number of groups without looping but can't think of it
    stopifnot((total / nrow(df)) == (2 ^ length(cols_groups)))
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
    add_row <- function(df_group, cols_set) {
      OUTPUT_PRECISION <- 4
      # promises::future_promise({
      for_group <- copy(cols_set)
      for_group["LAT"] <- round(mean(df_group$LAT), OUTPUT_PRECISION)
      for_group["LONG"] <- round(mean(df_group$LONG), OUTPUT_PRECISION)
      # print(as.data.table(for_group))
      r_k <- find_k(df_group)
      r <- cbind(
        as.data.table(for_group),
        round(as.data.table(r_k), OUTPUT_PRECISION)
      )
      i <<- i + 1
      pb_all$tick(nrow(df_group), tokens = list(i = i, n = n))
      return(r)
      # })
    }
    results <- for_groups(add_row, df)
    df_results <- data.table(results)
    for (col in cols_groups) {
      df_results[[col]] <- as.factor(df_results[[col]])
    }
    setnames(df_results, c("minimum", "objective"), c("estimate", "score"))
    # print(i)
    return(df_results)
  }

  file_out <- sprintf("dc_k_ON.csv")
  df <- prep_df(df_wx, hour_split)
  cols_groups <- c("ID", "YR", "MON")
  df_results <- score_df(df, cols_groups)
  write.csv(df_results, file_out, row.names=FALSE, quote=FALSE)
  q <- summary(df_results$estimate)
  print(q)
  return(df_results)
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


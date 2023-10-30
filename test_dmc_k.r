library(ggplot2)
library(ggrepel)
library(promises)
source("test_hFWI.r")

HOUR_PEAK <- 16
COLS_DMC <- c("LAT", "LONG", "FOR_DATE", "PE_DAILY", "DMC_DRYING_RATIO", "DMC_SUNLIGHT_RATIO")
# maximum drying factor to try using
DMC_K_LIMIT <- 10

prep_df <- function(df_wx, hour_split) {
  df <- copy(df_wx)
  df[, DATE := as.Date(TIMESTAMP)]
  df[, FOR_DATE := fifelse(HR <= hour_split, DATE, DATE + 1)]
  df[, PREC24 := sum(PREC), by=list(ID, YR, FOR_DATE)]
  df[, SUNRISE := round(SUNRISE)]
  df[, SUNSET := round(SUNSET)]
  df[, IS_DAYLIGHT := ifelse((HR >= SUNRISE) & (HR < SUNSET), "T", "F")]
  df[, SUNLIGHT_HOURS := SUNSET - SUNRISE]
  df[, HAD_RAIN := ifelse(PREC24 > 0, "T", "F")]
  df[, DMC_DRYING_RATIO := dmc_drying_ratio(TEMP, RH)]
  df_noon <- df[12 == HR, ]
  df_noon[, PE_DAILY := DMC_DRYING_RATIO * DAILY_K_DMC_DRYING * EL_DMC[[1]][MON]]
  # df_noon <- df_noon[, c("ID", "YR", "FOR_DATE", "PE_DAILY")]
  df_pe <- df[, list(DMC_DRYING_RATIO=sum(DMC_DRYING_RATIO)), by=list(ID, FOR_DATE, IS_DAYLIGHT)]
  df_daily <- merge(df_noon[, -c("IS_DAYLIGHT", "DMC_DRYING_RATIO")], df_pe, by=c("ID", "FOR_DATE"))
  df_daily[, DMC_SUNLIGHT_RATIO := EL_DMC[[1]][MON] / SUNLIGHT_HOURS]
  # df_relevant <- df[, -c("DATE", "FOR_DATE", "PREC24", "SUNRISE", "SUNSET", "DAY", "HR")]
  # cols <- c(c("IS_DAYLIGHT", "HAD_RAIN", "ID", "FOR_DATE", "YR", "MON", "SUNLIGHT_HOURS"), COLS_DMC)
  # cols <- c(c("IS_DAYLIGHT", "HAD_RAIN", "ID", "FOR_DATE", "YR", "SUNLIGHT_HOURS"), COLS_DMC)
  cols <- c(c("IS_DAYLIGHT", "HAD_RAIN", "ID", "FOR_DATE", "YR", "MON", "SUNLIGHT_HOURS"), COLS_DMC)
  df <- df_daily[, ..cols]
  return(df)
}

solve_k <- function(df_wx, hour_split=HOUR_PEAK) {
  df_prepared <- prep_df(df_wx, hour_split)
  score_df <- function(df, cols_groups) {
    print(paste0("Running with groups based on: [", paste0(cols_groups, collapse=", "), "]"))
    # df_noon <- unique(df[, c("ID", "FOR_DATE", "PE_DAILY")])
    find_k <- function(df) {
      cmp_df <- function(k) {
        # HACK: min(PE_DAILY) should just be PE_DAILY since they're all the same by day
        # df_pe <- df[, list(PE_DAILY=min(PE_DAILY), PE=sum(k * DMC_DRYING_RATIO)), by=list(ID, FOR_DATE)]
        # df_score <- merge(df_noon, df_pe, by=c("ID", "FOR_DATE"))
        return(mean(abs(df[, list(PE_DAILY=min(PE_DAILY), PE=sum(k * DMC_DRYING_RATIO)), by=list(ID, FOR_DATE)][, PE_DAILY - PE])))
        # score <- sum(abs(df_score$PE - df_score$PE_DAILY)) / nrow(df_score)
        # return(score)
        # return(mean(df_score$SCORE))
      }
      # return(nlm(cmp_df, DAILY_K_DMC_DRYING))
      # return(nlminb(list(k=DAILY_K_DMC_DRYING), cmp_df, lower=1E-6))
      # return(optim(list(k=DAILY_K_DMC_DRYING), cmp_df, method="Brent", lower=1E-6, upper=DMC_K_LIMIT))
      return(optimize(cmp_df, c(1E-6, DMC_K_LIMIT)))
      # return(optim(list(k=DAILY_K_DMC_DRYING), cmp_df))
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
        for_group["DMC_SUNLIGHT_RATIO_MEAN"] <- round(mean(df_group$DMC_SUNLIGHT_RATIO), OUTPUT_PRECISION)
        for_group["DMC_SUNLIGHT_RATIO_MIN"] <- round(min(df_group$DMC_SUNLIGHT_RATIO), OUTPUT_PRECISION)
        for_group["DMC_SUNLIGHT_RATIO_MAX"] <- round(max(df_group$DMC_SUNLIGHT_RATIO), OUTPUT_PRECISION)
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
  df <- df_prepared
  # df <- df[, -c("MON", "SUNLIGHT_HOURS")]
  # df <- df[TRUE == IS_DAYLIGHT]
  # df <- df[, -c("IS_DAYLIGHT")]
  df <- df["T" == IS_DAYLIGHT & "F" == HAD_RAIN, -c("IS_DAYLIGHT", "HAD_RAIN")]
  cols_groups <- setdiff(names(df), COLS_DMC)
  df_results <- score_df(df, cols_groups)
  # df_results[, CONDITION_DAY := ifelse("T" == IS_DAYLIGHT, "DAY", ifelse("F" == IS_DAYLIGHT, "NIGHT", "24HR"))]
  # df_results[, CONDITION_RAIN := ifelse("T" == HAD_RAIN, "RAIN_ONLY", ifelse("F" == HAD_RAIN, "NO_RAIN", "ALL"))]
  # df_results[, CONDITIONS := paste0(CONDITION_DAY, "_", CONDITION_RAIN)]
  q <- summary(df_results$estimate)
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


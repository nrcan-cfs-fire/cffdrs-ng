library(ggplot2)
library(ggrepel)
library(promises)
library(future)
library(future.apply)
source("test_hFWI.r")

HOUR_PEAK <- 16
COLS_DC <- c("LAT", "LONG", "FOR_DATE", "PE_DAILY", "DC_DRYING_RATIO", "DC_SUNLIGHT_RATIO")
# maximum drying factor to try using
DC_K_LIMIT <- 10

DAILY_K_DC_DRYING <- 3.94


dc_drying_daily <- function(temp, mon) {
  # if (temp <= -2.8) {
  #   temp <- -2.8
  # }
  # pe <- (0.36 * (temp + 2.8) + FL_DC[[1]][mon]) / 2.0
  pe <- (0.36 * (max(-2.8, temp) + 2.8) + FL_DC[[1]][mon]) / 2.0
  return(ifelse(pe < 0.0, 0.0, pe))
}


dc_drying_ratio <- function(temp, mon, dc_factor) {
  # if (temp <= -2.8) {
  #   # this doesn't cancel out, so can't just return 0 if <= -2.8
  #   temp <- -2.8
  # }
  # if we add this for every hour then the term can't be the daily value
  Epot_adj <- FL_DC[[1]][mon] / DAILY_K_DC_DRYING / dc_factor
  # return((0.0914 * (temp + 2.8)) + Epot_adj)
  # divide by 2 to rescale for 0-400 like DC is instead of 0-800 like SMI
  # not allowed to be negative
  return(max(0.0, (((0.0914 * (max(-2.8, temp) + 2.8)) + Epot_adj) / dc_factor) / 2.0))
}

prep_df <- function(eqn, df_wx, hour_split, offset_sunrise=0, offset_sunset=0) {
  df <- copy(df_wx)
  df[, DATE := as.Date(TIMESTAMP)]
  df[, FOR_DATE := fifelse(HR <= hour_split, DATE, DATE + 1)]
  df[, PREC24 := sum(PREC), by=list(ID, YR, FOR_DATE)]
  df[, SUNRISE := round(SUNRISE + offset_sunrise)]
  df[, SUNSET := round(SUNSET + offset_sunset)]
  df[, IS_DAYLIGHT := ifelse((HR >= SUNRISE) & (HR < SUNSET), "T", "F")]
  df[, SUNLIGHT_HOURS := SUNSET - SUNRISE]
  df[, HAD_RAIN := ifelse(PREC24 > 0, "T", "F")]
  # HACK: just avoid using minimum temp boundary conditions
  df <- df[TEMP > -2.8]
  if (1 == eqn){
    # eq1: 24 hours a day
    df[, DC_HOURS := 24.0]
    df[, DC_FACTOR := DC_HOURS]
  }  else if (2 == eqn) {
    # eq2: # of hours based on DMC lookup table + 3
    df[, DC_HOURS := (EL_DMC[[1]][MON] + 3)]
    df[, DC_FACTOR := DC_HOURS]
  }  else if (3 == eqn) {
    # eq3: eq2, but (h / 12)^2 to mimic PET_hamon
    df[, DC_HOURS := (EL_DMC[[1]][MON] + 3)]
    df[, DC_FACTOR := (DC_HOURS / 12.0) ^ 2]
  }  else if (4 == eqn) {
    # eq4: eq3, but with actual sunlight hours
    df[, DC_HOURS := SUNLIGHT_HOURS]
    df[, DC_FACTOR := (DC_HOURS / 12.0) ^ 2]
  }  else if (5 == eqn) {
    # eq5: eq4, but without square
    df[, DC_HOURS := SUNLIGHT_HOURS]
    df[, DC_FACTOR := (DC_HOURS / 12.0)]
  } else {
    stop(sprintf("invalid equation specified: %d", eqn))
  }
  df[, DC_DRYING_RATIO := dc_drying_ratio(TEMP, MON, DC_FACTOR)]
  df_noon <- df[12 == HR, ]
  # df_noon[, PE_DAILY := DC_DRYING_RATIO * DAILY_K_DC_DRYING * FL_DC[[1]][MON]]
  # # already including month factor in the equation for dc_drying_ratio
  # df_noon[, PE_DAILY := DC_DRYING_RATIO * DAILY_K_DC_DRYING * DC_FACTOR]
  # easier to understand if we just do the old daily calculation
  df_noon[, PE_DAILY := dc_drying_daily(TEMP, MON)]
  # df_noon <- df_noon[, c("ID", "YR", "FOR_DATE", "PE_DAILY")]
  df_pe <- df[, list(DC_DRYING_RATIO=sum(DC_DRYING_RATIO)), by=list(ID, FOR_DATE, IS_DAYLIGHT)]
  df_daily <- merge(df_noon[, -c("IS_DAYLIGHT", "DC_DRYING_RATIO")], df_pe, by=c("ID", "FOR_DATE"))
  df_daily[, DC_SUNLIGHT_RATIO := FL_DC[[1]][MON] / SUNLIGHT_HOURS]
  # df_relevant <- df[, -c("DATE", "FOR_DATE", "PREC24", "SUNRISE", "SUNSET", "DAY", "HR")]
  # cols <- c(c("IS_DAYLIGHT", "HAD_RAIN", "ID", "FOR_DATE", "YR", "MON", "SUNLIGHT_HOURS"), COLS_DC)
  # cols <- c(c("IS_DAYLIGHT", "HAD_RAIN", "ID", "FOR_DATE", "YR", "SUNLIGHT_HOURS"), COLS_DC)
  cols <- c(c("IS_DAYLIGHT", "HAD_RAIN", "ID", "FOR_DATE", "YR", "MON", "SUNLIGHT_HOURS"), COLS_DC)
  df <- df_daily[, ..cols]
  return(df)
}

solve_k <- function(df_wx, hour_split=HOUR_PEAK) {
  score_df <- function(df, cols_groups, offset_sunrise, offset_sunset) {
    print(paste0("Running with groups based on: [", paste0(cols_groups, collapse=", "), "]"))
    # df_noon <- unique(df[, c("ID", "FOR_DATE", "PE_DAILY")])
    find_k <- function(df) {
      cmp_df <- function(k) {
        # return(mean(abs(df[, list(PE_DAILY=min(PE_DAILY), PE=sum(k * DC_DRYING_RATIO)), by=list(ID, FOR_DATE)][, PE_DAILY - PE])))
        return(mean((df[, list(PE_DAILY=min(PE_DAILY), PE=sum(k * DC_DRYING_RATIO)), by=list(ID, FOR_DATE)][, PE_DAILY - PE]) ^ 2))
      }
      return(optimize(cmp_df, c(1E-6, DC_K_LIMIT)))
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
      for_group["DC_SUNLIGHT_RATIO_MEAN"] <- round(mean(df_group$DC_SUNLIGHT_RATIO), OUTPUT_PRECISION)
      for_group["DC_SUNLIGHT_RATIO_MIN"] <- round(min(df_group$DC_SUNLIGHT_RATIO), OUTPUT_PRECISION)
      for_group["DC_SUNLIGHT_RATIO_MAX"] <- round(max(df_group$DC_SUNLIGHT_RATIO), OUTPUT_PRECISION)
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
    df_results[, OFFSET_SUNRISE := offset_sunrise]
    df_results[, OFFSET_SUNSET := offset_sunset]
    # print(i)
    return(df_results)
  }
  df_results_all <- NULL
  # check a bunch of sunrise offsets to see if anything fits better
  # OFFSET_MAX <- 6
  OFFSET_MAX <- 3
  n <- 0
  vals <- list()
  for (i in (0.5 * (-(2 * OFFSET_MAX):(2 * OFFSET_MAX)))) {
    # don't do more than 6 hours overall
    offset_remaining <- (OFFSET_MAX - abs(i))
    # for (j in (0.5 * (-12:12))) {
    for (j in (0.5 * (-(2 * offset_remaining):(2 * offset_remaining)))) {
      # for (j in (0.5 * ((-OFFSET_MAX + (2 * i)):(OFFSET_MAX + (2 * i))))) {
      n <- n + 1
      vals <- rbind(vals, list(offset_sunrise=i, offset_sunset=j))
    }
  }
  # default is 500mb which is not enough
  options(future.globals.maxSize = 1024 * 1024^2)
  plan(multisession)
  dir_main <- sprintf("dc_k_ON_%s", strftime(today(), "%Y%m%d"))
  R.utils::mkdirs(dir_main)
  df_results_by_eqn <- NULL
  for (eqn in 1:5) {
    message("Running for equation ", eqn)
    lbl <- sprintf("eq%d", eqn)
    dir_out <- sprintf("%s/dc_k_ON_%s.%s", dir_main, strftime(today(), "%Y%m%d"), lbl)
    R.utils::mkdirs(dir_out)
    fct <- function(k) {
      v <- vals[k, ]
      i <- v$offset_sunrise
      j <- v$offset_sunset
      print(sprintf("(OFFSET_SUNRISE=%0.1f, OFFSET_SUNSET=%0.1f)", i, j))
      df <- prep_df(eqn, df_wx, hour_split, offset_sunrise=i, offset_sunset=j)
      df <- df["T" == IS_DAYLIGHT & "F" == HAD_RAIN, -c("IS_DAYLIGHT", "HAD_RAIN")]
      cols_groups <- setdiff(names(df), COLS_DC)
      df_current <- score_df(df, cols_groups, i, j)
      file_out <- sprintf("%s/dc_k_ON_%s_%0.1f_%0.1f.csv", dir_out, lbl, i, j)
      write.csv(df_current, file_out, row.names=FALSE, quote=FALSE)
      return(df_current)
    }
    r <- future_lapply(1:nrow(vals), FUN=fct)
    df_results_all <- rbindlist(r)
    df_results_all[, equation := eqn]
    df_results_by_eqn <- rbind(df_results_by_eqn, df_results_all)
    # write.csv(df_results_all, "dc_k_ON_sunrise_all.csv", row.names=FALSE, quote=FALSE)
    ################# plot

    write.csv(df_results_all, sprintf("%s/dc_k_ON_%s_all.csv", dir_out, lbl), row.names=FALSE, quote=FALSE)

    # eq1_monthly.png
    png(sprintf("%s/%s_monthly.png", dir_main, lbl))
    boxplot(estimate ~ MON, df_results_all["*" == SUNLIGHT_HOURS,])
    dev.off()

    # boxplot(estimate ~ YR, df_results_all["*" == SUNLIGHT_HOURS,])

    df_offset <- df_results_all["*" == SUNLIGHT_HOURS,][2.5 == OFFSET_SUNRISE,][0.5 == OFFSET_SUNSET,]
    # eq1_monthly_2.5_0.5.png
    png(sprintf("%s/%s_monthly_2.5_0.5.png", dir_main, lbl))
    boxplot(estimate ~ MON, df_offset)
    dev.off()

    df_offset <- df_results_all["*" == SUNLIGHT_HOURS,][0.0 == OFFSET_SUNRISE,][0.0 == OFFSET_SUNSET,]
    # eq1_monthly_0.0_0.0.png
    png(sprintf("%s/%s_monthly_0.0_0.0.png", dir_main, lbl))
    boxplot(estimate ~ MON, df_offset)
    dev.off()


    s <- list()
    for (i in unique(df_results_all$OFFSET_SUNRISE)) {
      print(i)
      df_o <- df_results_all[i == OFFSET_SUNRISE]
      # plot(estimate ~ DC_SUNLIGHT_RATIO_MEAN, df_o, main=sprintf("OFFSET_SUNRISE == %0.1f", i))
      # abline((lm(estimate ~ DC_SUNLIGHT_RATIO_MEAN, df_o)))
      # print(lm(estimate ~ DC_SUNLIGHT_RATIO_MEAN, df_o))
      # print(sd(df_o$estimate))
      s[as.character(i)] <- sd(df_o$estimate)
    }

    df_s <- as.data.table(list("offset_sunrise"=names(s), "std_dev"=unlist(s)))
    # # plot(sd(estimate) ~ unique(OFFSET_SUNRISE), df_results_all)
    # plot(unlist(s) ~ names(s))
    # eq1_sunrise_offset.png
    png(sprintf("%s/%s_sunrise_offset.png", dir_main, lbl))
    plot(std_dev ~ offset_sunrise, df_s)
    dev.off()
  #######################
  }
  df <- df_results_by_eqn[equation >= 3 & OFFSET_SUNRISE == 2.5 & OFFSET_SUNSET == 0.5 & "*" == SUNLIGHT_HOURS & "*" == YR]
  s <- list()
  m <- list()
  for (i in unique(df$equation)) {
    print(i)
    df_o <- df[i == equation]
    s[as.character(i)] <- sd(df_o$estimate)
    m[as.character(i)] <- mean(df_o$estimate)
  }

  df_eq <- as.data.table(list("equation"=names(s), "std_dev"=unlist(s), "mean"=unlist(m)))
  df_eq[, mag := (std_dev * 100) / mean]
  # # plot(sd(estimate) ~ unique(OFFSET_SUNRISE), df_results_all)
  # plot(unlist(s) ~ names(s))
  # eq1_sunrise_offset.png
  png(sprintf("%s/%s_sunrise_offset.png", dir_main, lbl))
  plot(std_dev ~ offset_sunrise, df_eq)
  dev.off()
  return(df_results_by_eqn)
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


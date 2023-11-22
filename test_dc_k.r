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

# DAILY_K_DC_DRYING <- 3.94
# actually 100th" to mm
DAILY_K_DC_DRYING <- 3.937

dc_drying_daily <- function(temp, mon) {
  # if (temp <= -2.8) {
  #   temp <- -2.8
  # }
  # pe <- (0.36 * (temp + 2.8) + FL_DC[[1]][mon]) / 2.0
  pe <- (0.36 * (pmax(-2.8, temp) + 2.8) + FL_DC[[1]][mon]) / 2.0
  return(ifelse(pe < 0.0, 0.0, pe))
}
#
# # assume temp >= -2.8
# pe <- (0.36 * (temp + 2.8) + FL_DC[[1]][mon]) / 2.0

# FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / (3 + EL_DMC[[1]][i]) }))
# >   print(df_eq)
# equation    std_dev      mean      mag
# 1:        1 0.27054130 7.6848435 3.520453
# 2:        2 0.17942117 7.3482116 2.441698
# 3:        3 0.01918835 0.7864054 2.440007
# 4:        4 0.02699837 0.6359517 4.245349
# 5:        5 0.01307374 0.5514095 2.370967
# 6:        6 0.01494957 0.6123490 2.441347
# 7:        7 0.14446547 5.9177429 2.441226
# 8:        8 0.15690480 6.6169156 2.371268
# 9:        9 0.01165397 0.4767714 2.444351


# # PRETTY SURE pmax() was affecting these (since magnitude is always the same)
# # did full fits on these because max should have been pmax
# # FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / (((3 + EL_DMC[[1]][i])) / (12.0)) ^ 2 }))
# # # > df_eq
# # # equation     std_dev      mean      mag
# # # 1:        1 0.101293939 2.9862252 3.392040
# # # 2:        2 0.136835077 2.7302898 5.011742
# # # 3:        3 0.011784136 0.2351102 5.012175
# # # 4:        4 0.008826399 0.1760776 5.012790
# # # 5:        5 0.010111676 0.2018354 5.009863
# # # 6:        6 0.011405217 0.2275218 5.012803
# # # 7:        7 0.103723731 2.0697347 5.011451
# # # 8:        8 0.121384378 2.4220313 5.011677
# # # 9:        9 0.010771876 0.2149449 5.011459
# # #
# # # 0.36 seems alright
# #
# # FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / EL_DMC[[1]][i] }))
# # # >   print(df_eq)
# # # equation     std_dev      mean      mag
# # # 1:        1 0.114765993 3.3832966 3.392135
# # # 2:        2 0.151341789 3.0197014 5.011813
# # # 3:        3 0.013027720 0.2600279 5.010124
# # # 4:        4 0.009922557 0.1979259 5.013270
# # # 5:        5 0.011188252 0.2232320 5.011940
# # # 6:        6 0.012608607 0.2516422 5.010530
# # # 7:        7 0.114722165 2.2891299 5.011606
# # # 8:        8 0.134246165 2.6787694 5.011486
# # # 9:        9 0.012200516 0.2435252 5.009961
# # #
# # # nothing really seems to fit well, so this is probably the wrong FL_HDC
#
#
# FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / (3 + EL_DMC[[1]][i]) }))
# # > df_eq
# # equation     std_dev      mean      mag
# # 1:        1 0.115144524 3.3944565 3.392134
# # 2:        2 0.151838059 3.0296619 5.011716
# # 3:        3 0.013071113 0.2608891 5.010218
# # 4:        4 0.009961516 0.1986925 5.013533
# # 5:        5 0.011219641 0.2239660 5.009529
# # 6:        6 0.012653386 0.2524721 5.011796
# # 7:        7 0.115102610 2.2966789 5.011698
# # 8:        8 0.134693782 2.6876014 5.011673
# # 9:        9 0.012243040 0.2443279 5.010906
#

###################################################################
###################################################################

# not sure if any of this makes sense because it looks like max() should have been pmax() for calculating PE_DAILY
#
# #
# # ##########################################
# # # NOTE: ALL OF THIS WAS USING (K / 0.9) and not just K
# # # FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / EL_DMC[[1]][i] }))
# # # FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / 12.0 }))
# # # # 0.42, 1.0 seems not terrible with this
# # # FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / (3 + EL_DMC[[1]][i]) }))
# # # 0.41 looks a bit high but close, and then too low right at the end
# # # FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING * (3 + EL_DMC[[1]][i]) / (12.0 ^ 2) }))
# # # FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / ((3 + EL_DMC[[1]][i]) ^ 2) }))
# # # 0.36 is pretty good until september when it's too low
# # # FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / ((3 + EL_DMC[[1]][i]) ^ 0.5) }))
# # # FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / ((EL_DMC[[1]][i]) ^ 0.5) }))
# # # way too high
# # # FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / ((12.0 / EL_DMC[[1]][i]) ^ 0.5) }))
# # # 0.27 seems okay - higher in middle, lower at end
# # # FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / ((EL_DMC[[1]][i] / 12.0) ^ 0.5) }))
# # # eq5's 0.2712259 seems pretty good with this
# # FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / (((EL_DMC[[1]][i] ^ 2) / (12.0 ^ 2)) ^ 0.5) }))
# # # not even sure what this would be related to. Too high in middle with 0.36
# # # FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / ((12.0 / (3 + EL_DMC[[1]][i]) ^ 2) ^ 0.5) / 12.0 }))
# # # too low at end of year
# # # FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / ((((3 + EL_DMC[[1]][i]) ^ 2) / 12.0)  ^ 0.5) }))
# # # FL_HDC <- unlist(lapply(1:12, function(i) { 12.0 * FL_DC[[1]][i] / DAILY_K_DC_DRYING / ((3 + EL_DMC[[1]][i]) ^ 2) }))
# # # FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / (3 + EL_DMC[[1]][i]) / ((12.0 / (3 + EL_DMC[[1]][i])) ^ 2) }))
# # # FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / (3 + EL_DMC[[1]][i]) / (((3 + EL_DMC[[1]][i]) / 12.0) ^ 2) }))
# # ##########################################
#
#
#
# # > df_eq
# # equation      std_dev      mean         mag
# # 1:        1 0.0001917893 4.9669218 0.003861332
# # 2:        2 0.0968812117 4.0118163 2.414896492
# # 3:        3 0.0073980268 0.3064585 2.414038676
# # 4:        4 0.0049116173 0.2034259 2.414450923
# # 5:        5 0.0065509812 0.2712259 2.415323312
# # 6:        6 0.0080712757 0.3343259 2.414194326
# # 7:        7 0.0704541532 2.9176939 2.414720534
# # 8:        8 0.0786035643 3.2548503 2.414967083
# # 9:        9 0.0086324096 0.3574184 2.415211532
#
#
# # eq3 quite a bit too low
# # eq6 too low
# # eq9 extremely close
# # FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / (((3 + EL_DMC[[1]][i]) ^ 2) / (12.0 ^ 2)) }))
#
# # eq6 too low
# # eq9 a bit high in summer, evens out again in oct
# FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / (((3 + EL_DMC[[1]][i])) / (12.0)) ^ 2 }))
#
# # eq3 too low
# # eq6 too high in summer
# # eq9 too high
# # FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / ((((3 + EL_DMC[[1]][i]) ^ 2) / (12.0 ^ 2)) ^ 0.5) }))
#
# #eq3 too high
# #eq6 way too high
# # FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / (((12.0 ^ 2) / ((3 + EL_DMC[[1]][i]) ^ 2)) ^ 0.5) }))
#
# #eq3 too high
# # FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / ((12.0) / (3 + EL_DMC[[1]][i])) }))
#
# # eq3 obscenely high
# # FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / ((12.0) / (3 + EL_DMC[[1]][i]) ^ 2) }))
#
# # eq3 still too high in the middle
# # FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / ((EL_DMC[[1]][i] / 12.0) ^ 0.5) }))
#
# # df_eq$mean[3] (0.3064585) is not bad
# # FL_HDC <- unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / (((EL_DMC[[1]][i] ^ 2) / (12.0 ^ 2)) ^ 0.5) }))

DC_DAILY_CONST <- 0.36
DC_HOURLY_CONST <- DC_DAILY_CONST / DAILY_K_DC_DRYING
dc_drying_ratio <- function(temp, mon, dc_factor) {
  # return((0.0914 * (temp + 2.8)) + Epot_adj)
  # divide by 2 to rescale for 0-400 like DC is instead of 0-800 like SMI
  # not allowed to be negative
  return(pmax(0.0, (((DC_HOURLY_CONST * (pmax(-2.8, temp) + 2.8)) + FL_HDC[mon]) / dc_factor) / 2.0))
}



# dc_drying_ratio <- function(temp, mon, dc_factor) {
#   # if (temp <= -2.8) {
#   #   # this doesn't cancel out, so can't just return 0 if <= -2.8
#   #   temp <- -2.8
#   # }
#   # if we add this for every hour then the term can't be the daily value
#   Epot_adj <- FL_DC[[1]][mon] / DAILY_K_DC_DRYING / dc_factor
#   # return((0.0914 * (temp + 2.8)) + Epot_adj)
#   # divide by 2 to rescale for 0-400 like DC is instead of 0-800 like SMI
#   # not allowed to be negative
#   return(pmax(0.0, ((((0.36 / DAILY_K_DC_DRYING) * (pmax(-2.8, temp) + 2.8)) + Epot_adj) / dc_factor) / 2.0))
# }

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
  # if (4 == eqn) {
  #   # eq4: eq3, but with actual sunlight hours
  #   df[, DC_HOURS := SUNLIGHT_HOURS]
  #   df[, DC_FACTOR := (DC_HOURS / 12.0) ^ 2]
  # }  else if (5 == eqn) {
  #   # eq5: eq4, but without square
  #   df[, DC_HOURS := SUNLIGHT_HOURS]
  #   df[, DC_FACTOR := (DC_HOURS / 12.0)]
  # } else if (1 == eqn){
  #   # eq1: divide drying over 24 hours a day
  #   df[, DC_HOURS := 24.0]
  #   df[, DC_FACTOR := DC_HOURS]
  # }  else if (2 == eqn) {
  #   # eq2: # of hours based on DMC lookup table + 3
  #   df[, DC_HOURS := (EL_DMC[[1]][MON] + 3)]
  #   df[, DC_FACTOR := DC_HOURS]
  # }  else if (3 == eqn) {
  #   # eq3: eq2, but (h / 12)^2 to mimic PET_hamon
  #   df[, DC_HOURS := (EL_DMC[[1]][MON] + 3)]
  #   df[, DC_FACTOR := (DC_HOURS / 12.0) ^ 2]
  # }  else if (6 == eqn) {
  #   # eq6: # of hours based on DMC lookup table + 3, as a ratio vs 12
  #   df[, DC_HOURS := (EL_DMC[[1]][MON] + 3)]
  #   df[, DC_FACTOR := DC_HOURS / 12.0]
  # } else if (7 == eqn) {
  #   # eq7: # of hours based on DMC lookup table
  #   df[, DC_HOURS := EL_DMC[[1]][MON]]
  #   df[, DC_FACTOR := DC_HOURS]
  # } else if (8 == eqn) {
  #   # eq8: # of hours based on sunlight hours
  #   df[, DC_HOURS := SUNLIGHT_HOURS]
  #   df[, DC_FACTOR := DC_HOURS]
  # } else if (9 == eqn) {
  #   # eq9: no factor
  #   df[, DC_FACTOR := 1.0]
  # }  else {
  #   stop(sprintf("invalid equation specified: %d", eqn))
  # }
  df[, DC_FACTOR := 1]
  # use all day
  eqn <- 1
  # K <- 0.0914 * 3.937 / 24.0
  K <- 1.0
  # offset_temp <- 2.8
  # v <- pmax(0.0, K * (temp + offset_temp))
  df[, DC_DRYING_RATIO := (K * pmax(0.0, TEMP + offset_temp))]
  # df[, DC_DRYING_RATIO := dc_drying_ratio(TEMP, MON, DC_FACTOR)]
  df_noon <- df[12 == HR, ]
  # df_noon[, PE_DAILY := DC_DRYING_RATIO * DAILY_K_DC_DRYING * FL_DC[[1]][MON]]
  # # already including month factor in the equation for dc_drying_ratio
  # df_noon[, PE_DAILY := DC_DRYING_RATIO * DAILY_K_DC_DRYING * DC_FACTOR]
  # easier to understand if we just do the old daily calculation
  df_noon[, PE_DAILY := dc_drying_daily(TEMP, MON)]
  # df_noon <- df_noon[, c("ID", "YR", "FOR_DATE", "PE_DAILY")]
  if (1 != eqn) {
    df <- df["T" == IS_DAYLIGHT,]
  }
  df_pe <- df[, list(DC_DRYING_RATIO=sum(DC_DRYING_RATIO)), by=list(ID, FOR_DATE)]
  df_daily <- merge(df_noon[, -c("IS_DAYLIGHT", "DC_DRYING_RATIO")], df_pe, by=c("ID", "FOR_DATE"))
  df_daily[, DC_SUNLIGHT_RATIO := FL_DC[[1]][MON] / SUNLIGHT_HOURS]
  cols <- c(c("HAD_RAIN", "ID", "FOR_DATE", "YR", "MON", "SUNLIGHT_HOURS"), COLS_DC)
  df <- df_daily[, ..cols]
  # df <- df["T" == IS_DAYLIGHT & "F" == HAD_RAIN, -c("IS_DAYLIGHT", "HAD_RAIN")]
  df <- df["F" == HAD_RAIN, -c("HAD_RAIN")]
  return(df)
}

# for (t in -50:50) {
#
# }
# this was wrong because it didn't use k
# > r
# $par
# k offset_temp
# 0.8541033   3.2449588
#
# $value
# [1] 0.9950586
#
# $counts
# function gradient
# 37       NA
#
# $convergence
# [1] 0
#
# $message
# NULL


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
    # avoid warning about shallow copy
    df_results <- copy(df_results)
    df_results[, OFFSET_SUNRISE := offset_sunrise]
    df_results[, OFFSET_SUNSET := offset_sunset]
    # print(i)
    return(df_results)
  }
  df_results_all <- NULL
  # default is 500mb which is not enough
  options(future.globals.maxSize = 1024 * 1024^2)
  plan(multisession)
  dir_main <- sprintf("dc_k_ON_%s", strftime(today(), "%Y%m%d"))
  R.utils::mkdirs(dir_main)
  df_results_by_eqn <- NULL
  fct <- function(eqn) {
    message("Running for equation ", eqn)
    lbl <- sprintf("eq%d", eqn)
    dir_out <- sprintf("%s/dc_k_ON_%s.%s", dir_main, strftime(today(), "%Y%m%d"), lbl)
    R.utils::mkdirs(dir_out)
    i <- 2.5
    j <- 0.5
    print(sprintf("(OFFSET_SUNRISE=%0.1f, OFFSET_SUNSET=%0.1f)", i, j))
    df <- prep_df(eqn, df_wx, hour_split, offset_sunrise=i, offset_sunset=j)
    # df <- df["T" == IS_DAYLIGHT & "F" == HAD_RAIN, -c("IS_DAYLIGHT", "HAD_RAIN")]
    # # stop trying to find a relationship with sunlight hours
    # df <- df[, -c("SUNLIGHT_HOURS")]
    cols_groups <- setdiff(names(df), c("SUNLIGHT_HOURS", COLS_DC))
    df_current <- score_df(df, cols_groups, i, j)
    df_current[, equation := eqn]
    file_out <- sprintf("%s/dc_k_ON_%s_%0.1f_%0.1f.csv", dir_out, lbl, i, j)
    write.csv(df_current, file_out, row.names=FALSE, quote=FALSE)
    return(df_current)
  }
  r <- future_lapply(1:9, FUN=fct)
  df_results_all <- rbindlist(r)
  df_results_by_eqn <- rbind(df_results_by_eqn, df_results_all)
  df <- df_results_by_eqn[OFFSET_SUNRISE == 2.5 & OFFSET_SUNSET == 0.5 & "*" == YR]
  # df <- df_results_by_eqn[OFFSET_SUNRISE == 2.5 & OFFSET_SUNSET == 0.5 & "*" == SUNLIGHT_HOURS & "*" == YR]
  # df <- df[equation >= 3,]
  df <- df[7 == MON,]
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
  boxplot(estimate ~ equation, df)
  print(df_eq)
  # # plot(sd(estimate) ~ unique(OFFSET_SUNRISE), df_results_all)
  # plot(unlist(s) ~ names(s))
  # eq1_sunrise_offset.png
  # png(sprintf("%s/%s_sunrise_offset.png", dir_main, lbl))
  # plot(std_dev ~ offset_sunrise, df_eq)
  # dev.off()
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
#
# df_wx <- read_file("./data/test_hffmc.csv")
# df_hourly <- read_file("./data/wx_hourly.csv")
# df_prf <- read_file("./data/wx_prf.csv")
#

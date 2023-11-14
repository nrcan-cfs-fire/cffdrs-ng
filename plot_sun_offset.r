df_near <- df_results_all[abs(DMC_SUNLIGHT_RATIO_MEAN - 1.0) < 0.05]
df_near$SUNLIGHT_HOURS <- as.factor(as.character(df_near$SUNLIGHT_HOURS))

v <- lm(estimate ~ LAT, df_near)
k_lat <- v$coefficients["LAT"]
c_lat <- v$coefficients[[1]]

df_lat <- copy(df_results_all)
df_lat[, K_ADJUSTED := estimate - (k_lat - c_lat * LAT)]


s <- list()
for (i in unique(df_results_all$OFFSET_SUNRISE)) {
  print(i)
  df_o <- df_results_all[i == OFFSET_SUNRISE]
  plot(estimate ~ DMC_SUNLIGHT_RATIO_MEAN, df_o, main=sprintf("OFFSET_SUNRISE == %0.1f", i))
  abline((lm(estimate ~ DMC_SUNLIGHT_RATIO_MEAN, df_o)))
  print(lm(estimate ~ DMC_SUNLIGHT_RATIO_MEAN, df_o))
  print(sd(df_o$estimate))
  s[as.character(i)] <- sd(df_o$estimate)
}

# plot(sd(estimate) ~ unique(OFFSET_SUNRISE), df_results_all)
plot(unlist(s) ~ names(s))


setorder(df_results_all, OFFSET_SUNRISE, OFFSET_SUNSET)
persp(df_results_all$OFFSET_SUNRISE, df_results$OFFSET_SUNSET, df_results_all$estimate)

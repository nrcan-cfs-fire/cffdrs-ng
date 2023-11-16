lbl <- "eq2"

write.csv(df_results_all, sprintf("dc_k_ON_%s_all.csv", lbl), row.names=FALSE, quote=FALSE)

# eq1_monthly.png
png(sprintf("%s_monthly.png", lbl))
boxplot(estimate ~ MON, df_results_all["*" == SUNLIGHT_HOURS,])
dev.off()

# boxplot(estimate ~ YR, df_results_all["*" == SUNLIGHT_HOURS,])

df_offset <- df_results_all["*" == SUNLIGHT_HOURS,][2.5 == OFFSET_SUNRISE,][0.5 == OFFSET_SUNSET,]
# eq1_monthly_2.5_0.5.png
png(sprintf("%s_monthly_2.5_0.5.png", lbl))
boxplot(estimate ~ MON, df_offset)
dev.off()

df_offset <- df_results_all["*" == SUNLIGHT_HOURS,][0.0 == OFFSET_SUNRISE,][0.0 == OFFSET_SUNSET,]
# eq1_monthly_0.0_0.0.png
png(sprintf("%s_monthly_0.0_0.0.png", lbl))
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
png(sprintf("%s_sunrise_offset.png", lbl))
plot(std_dev ~ offset_sunrise, df_s)
dev.off()

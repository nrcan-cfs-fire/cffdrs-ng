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


d <- df_results_all
d$OFFSET_SUNRISE <- as.double(d$OFFSET_SUNRISE)
d$OFFSET_SUNSET <- as.double(d$OFFSET_SUNSET)
setorder(d, OFFSET_SUNSET)
setorder(d, OFFSET_SUNRISE)
x <- sort(unique(d$OFFSET_SUNRISE))
y <- sort(unique(d$OFFSET_SUNSET))
# z <- matrix(nrow=length(x), ncol=length(y))
# d_all <- d[ID == "*" & YR == "*" & MON == "*" & SUNLIGHT_HOURS == "*"]
# for (i in 1:length(x)) {
#   for (j in 1:length(y)) {
#     v <- d_all[OFFSET_SUNRISE == x[[i]] & OFFSET_SUNSET == y[j][]]
#     if (0 < nrow(v)) {
#       z[i, j] <- v$estimate
#     }
#   }
# }
# persp3d(x=x, y=y, z=z, main="DMC hourly K", ticktype='detailed')
# bg3d('white')
# view3d(userMatrix=rotationMatrix(-60*pi/180, 1, -0.5, -1))
# rglwidget()
#

# z <- matrix(nrow=length(x), ncol=length(y))
# for (i in 1:length(x)) {
#   for (j in 1:length(y)) {
#     v <- d[OFFSET_SUNRISE == x[[i]] & OFFSET_SUNSET == y[j][]]
#     if (0 < nrow(v)) {
#       z[i, j] <- sd(v$estimate)
#     }
#   }
# }
# persp3d(x=x, y=y, z=z, main="DMC hourly K", ticktype='detailed')
# bg3d('white')
# view3d(userMatrix=rotationMatrix(-60*pi/180, 1, -0.5, -1))
# rglwidget()

z <- matrix(nrow=length(x), ncol=length(y))
for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    v <- d[OFFSET_SUNRISE == x[[i]] & OFFSET_SUNSET == y[j][]]
    if (0 < nrow(v)) {
      z[i, j] <- mean(v$estimate)
    }
  }
}
persp3d(x=x, y=y, z=z, main="DMC hourly K", ticktype='detailed')
bg3d('white')
view3d(userMatrix=rotationMatrix(-60*pi/180, 1, -0.5, -1))
rglwidget()

g <- d[, list(std_dev=sd(estimate), k=mean(estimate)), by=c("OFFSET_SUNRISE", "OFFSET_SUNSET")]


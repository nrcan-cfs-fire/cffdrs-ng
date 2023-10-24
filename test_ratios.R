df_wx <- as.data.table(read.csv("./data/test_hffmc.csv"))
df_hourly <- as.data.table(read.csv("./data/wx_hourly.csv"))

df_wx_no_prec <- copy(df_wx)
df_wx_no_prec$prec <- 0.0

df_hourly_no_prec <- copy(df_hourly)
df_hourly_no_prec$prec <- 0.0

df <- df_hourly_no_prec

r_old <- test_hfwi(df, FLAG_NO_MONTH_FACTOR = FALSE)
r_direct <- test_hfwi(df, FLAG_NO_MONTH_FACTOR = TRUE)

df_daily <- r_direct[16 == hour(TIMESTAMP),]
DMC_DEFAULT <- 6
n <- nrow(df_daily)
dry_daily <- df_daily$DDMC - c(DMC_DEFAULT, shift(df_daily$DDMC)[2:n])
dry_hourly <- df_daily$DMC - c(DMC_DEFAULT, shift(df_daily$DMC)[2:n])
dry_ratio <- dry_hourly / dry_daily
df_daily$DRY_RATIO <- dry_ratio
print(ggplot(df_daily) +
        geom_line(aes(TIMESTAMP, DRY_RATIO)))

timezone <- -6
latitude <- df$lat[[1]]
longitude <- df$long[[1]]
w <- add_sunlight(df_daily, latitude, longitude, timezone)

EL_DMC <- c(6.5, 7.5, 9.0, 12.8, 13.9, 13.9, 12.4, 10.9, 9.4, 8.0, 7.0, 6.0)
w[, EL := EL_DMC[month(TIMESTAMP)]]
w[, HRS_RATIO := EL / SUNLIGHT_HOURS]

print(ggplot(w) +
        geom_line(aes(TIMESTAMP, DRY_RATIO)) +
        geom_line(aes(TIMESTAMP, HRS_RATIO), colour="blue"))

w[, DRY_RATIO_NORM := ((DRY_RATIO - mean(DRY_RATIO)) / (max(DRY_RATIO) - min(DRY_RATIO)))]
w[, HRS_RATIO_NORM := ((HRS_RATIO - mean(HRS_RATIO)) / (max(HRS_RATIO)- min(HRS_RATIO)))]
print(ggplot(w) +
        geom_line(aes(TIMESTAMP, DRY_RATIO_NORM)) +
        geom_line(aes(TIMESTAMP, HRS_RATIO_NORM), colour="blue"))


w[, DRY_RATIO_NORM := ((DRY_RATIO - mean(DRY_RATIO)) / (max(DRY_RATIO) - mean(DRY_RATIO)))]
w[, HRS_RATIO_NORM := ((HRS_RATIO - mean(HRS_RATIO)) / (max(HRS_RATIO)- mean(HRS_RATIO)))]
print(ggplot(w) +
        geom_line(aes(TIMESTAMP, DRY_RATIO_NORM)) +
        geom_line(aes(TIMESTAMP, HRS_RATIO_NORM), colour="blue"))


# 
# w[, DRY_RATIO_SHIFT := (DRY_RATIO - mean(DRY_RATIO))]
# w[, HRS_RATIO_SHIFT := (HRS_RATIO - mean(HRS_RATIO))]
# w[, DR_NORM := (DRY_RATIO_SHIFT / (max(DRY_RATIO_SHIFT) - min(DRY_RATIO_SHIFT)))]
# w[, HR_NORM := (HRS_RATIO_SHIFT / (max(HRS_RATIO_SHIFT) - min(HRS_RATIO_SHIFT)))]
# 
# print(ggplot(w) +
#         geom_line(aes(TIMESTAMP, DR_NORM)) +
#         geom_line(aes(TIMESTAMP, HR_NORM), colour="blue"))


w[, DR_NORM := ((DRY_RATIO - mean(DRY_RATIO)) / (max(DRY_RATIO) - min(DRY_RATIO)))]
w[, HR_NORM := ((HRS_RATIO - mean(HRS_RATIO)) / (max(HRS_RATIO) - min(HRS_RATIO)))]

print(ggplot(w) +
        geom_line(aes(TIMESTAMP, DR_NORM)) +
        geom_line(aes(TIMESTAMP, HR_NORM), colour="blue"))


# 
# w[, DRY_RATIO_RANGE := DRY_RATIO / mean(DRY_RATIO)]
# w[, HRS_RATIO_RANGE := HRS_RATIO / mean(HRS_RATIO)]
# print(ggplot(w) +
#         geom_line(aes(TIMESTAMP, DRY_RATIO_RANGE)) +
#         geom_line(aes(TIMESTAMP, HRS_RATIO_RANGE), colour="blue"))

# 
# w[, DRY_RATIO_NORM := (DRY_RATIO - mean(DRY_RATIO))]
# w[, HRS_RATIO_NORM := (HRS_RATIO - mean(HRS_RATIO))]
# print(ggplot(w) +
#         geom_line(aes(TIMESTAMP, DRY_RATIO_NORM)) +
#         geom_line(aes(TIMESTAMP, HRS_RATIO_NORM), colour="blue"))
# 
# # w[, DRY_RATIO_NORM_RANGE := (DRY_RATIO_NORM / max(abs(min(DRY_RATIO)), abs(max(DRY_RATIO))))]
# # w[, HRS_RATIO_NORM_RANGE := (HRS_RATIO_NORM / max(abs(min(HRS_RATIO)), abs(max(HRS_RATIO))))]
# # print(ggplot(w) +
# #         geom_line(aes(TIMESTAMP, DRY_RATIO_NORM_RANGE)) +
# #         geom_line(aes(TIMESTAMP, HRS_RATIO_NORM_RANGE), colour="blue"))
# 
# w[, DRY_RATIO_NORM_RANGE := (DRY_RATIO_NORM / (max(DRY_RATIO) - min(DRY_RATIO)))]
# w[, HRS_RATIO_NORM_RANGE := (HRS_RATIO_NORM / (max(HRS_RATIO)- min(HRS_RATIO)))]
# print(ggplot(w) +
#         geom_line(aes(TIMESTAMP, DRY_RATIO_NORM_RANGE)) +
#         geom_line(aes(TIMESTAMP, HRS_RATIO_NORM_RANGE), colour="blue"))


# w[, DRY_RATIO_NORM := (DRY_RATIO / (max(DRY_RATIO) - min(DRY_RATIO)))]
# w[, HRS_RATIO_NORM := (HRS_RATIO / (max(HRS_RATIO)- min(HRS_RATIO)))]
# print(ggplot(w) +
#         geom_line(aes(TIMESTAMP, DRY_RATIO_NORM)) +
#         geom_line(aes(TIMESTAMP, HRS_RATIO_NORM), colour="blue"))


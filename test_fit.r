source("NG_FWI_fitting.r")


FL_HDC_ALL <- list(
  unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / (EL_DMC[[1]][i]) })),
  unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / (3 + EL_DMC[[1]][i]) })),
  unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / 12.0 })),
  unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING * (3 + EL_DMC[[1]][i]) / (12.0 ^ 2) })),
  unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / ((3 + EL_DMC[[1]][i]) ^ 2) })),
  unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / ((3 + EL_DMC[[1]][i]) ^ 0.5) })),
  unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / ((EL_DMC[[1]][i]) ^ 0.5) })),
  unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / ((12.0 / EL_DMC[[1]][i]) ^ 0.5) })),
  unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / ((EL_DMC[[1]][i] / 12.0) ^ 0.5) })),
  unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / (((EL_DMC[[1]][i] ^ 2) / (12.0 ^ 2)) ^ 0.5) })),
  unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / ((12.0 / (3 + EL_DMC[[1]][i]) ^ 2) ^ 0.5) / 12.0 })),
  unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / ((((3 + EL_DMC[[1]][i]) ^ 2) / 12.0)  ^ 0.5) })),
  unlist(lapply(1:12, function(i) { 12.0 * FL_DC[[1]][i] / DAILY_K_DC_DRYING / ((3 + EL_DMC[[1]][i]) ^ 2) })),
  unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / (3 + EL_DMC[[1]][i]) / ((12.0 / (3 + EL_DMC[[1]][i])) ^ 2) })),
  unlist(lapply(1:12, function(i) { FL_DC[[1]][i] / DAILY_K_DC_DRYING / (3 + EL_DMC[[1]][i]) / (((3 + EL_DMC[[1]][i]) / 12.0) ^ 2) }))
)


for (k in 1:length(FL_HDC_ALL)) {
  FL_HDC <<- FL_HDC_ALL[[k]]
  eqn_k <<- k
  #for (i in 1:9) {
  for (i in 1:20) {
    eqn <<- i
    for (j in 1:9) {
      eqn_j <<- j
      png(sprintf("fit_%dx%dx%d.png", k, i, j))
      plot_test(read.csv("./data/wx_hourly.csv"))
      dev.off()
    }
  }
}

# makes no sense to use fit from different eqn? what about sunlight_hours ones?

# potentially useful
# 1x2x1
# 1x2x2
# 1x4x4
# 1x5x5



# 2x2x1
# 2x2x2
# 2x9x9


# meh
# 1x2x8
# 1x3x3
# 1x4x5
# 1x4x6
# 1x4x9
# 1x5x4
# 1x5x6
# 1x5x9
# 1x6x4
# 1x6x5



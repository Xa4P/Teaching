###############################
#### Practical Discounting ####
###############################

#----------------------------------#
##### Create data for practical ####
#----------------------------------#
rm(list  = ls())

df_thx <- as.data.frame(cbind(
  Year = c(0:12),
  Inc_cost_A = c(rep(20000, 3), rep(0, 10)),
  Inc_QALY_A = c(0, rep(1, 4), rep(0, 8)),
  Inc_cost_B = c(rep(20000, 3), rep(0, 10)),
  Inc_QALY_B = c(rep(0, 9), rep(1, 4)),
  Inc_cost_C = c(rep(10000, 6), rep(0, 7)),
  Inc_QALY_C = c(0, rep(0.5, 8), rep(0, 4))
))

saveRDS(df_thx, file = "Basics/Discounting/data_discount.rds")
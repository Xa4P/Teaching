###############################
#### Practical Discounting ####
###############################

######################
##### Create data ####
######################
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

saveRDS(df_thx, file = "Basics/data_discount.rds")

##################
#### Exercise ####
##################

rm(list  = ls()) # clear environment
df_thx <- readRDS("Basics/data_discount.rds")# Load data

# Define discount rates
r_disc <- 0.05

# Question 1
v_res_1 <- apply(df_thx[, c(2:ncol(df_thx))], 2, sum) # Calculate total undiscounted costs
v_icer_undisc <- c(ICER_A = v_res_1[1]/v_res_1[2],
                   ICER_B = v_res_1[3]/v_res_1[4],
                   ICER_C = v_res_1[5]/v_res_1[6]
) # calculate undiscounted ICERs

# Question 2 
v_disc <- 1/(1+r_disc)^df_thx$Year # create vector of discount rates
v_res_2 <- apply(df_thx[, c(2:ncol(df_thx))], 2, function(x) x %*% v_disc) # discount costs and effects: matrix multiplication of vector of costs and effects x vector dicsount rates
v_icer_disc <- c(ICER_A = v_res_2[1]/v_res_2[2],
                   ICER_B = v_res_2[3]/v_res_2[4],
                   ICER_C = v_res_2[5]/v_res_2[6]
) # calculate discounted ICERs

# Question 3
r_disc_2 <- 0.1
v_disc_2 <- 1/(1+r_disc_2)^df_thx$Year # create vector of discount rates
v_res_3 <- apply(df_thx[, c(2:ncol(df_thx))], 2, function(x) x %*% v_disc_2) # matrix multiplication of vector of costs and effects x vector dicsount rates
v_icer_disc_2 <- c(ICER_A = v_res_3[1]/v_res_3[2],
                 ICER_B = v_res_3[3]/v_res_3[4],
                 ICER_C = v_res_3[5]/v_res_3[6]
) # calculate discounted ICERs, with 10% discount rate

# Comparison all results
m_res_total <- rbind(v_icer_undisc,
                   v_icer_disc,
                   v_icer_disc_2)
m_res_total # show results


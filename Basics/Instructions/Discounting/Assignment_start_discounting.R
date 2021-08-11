###############################
#### Practical Discounting ####
###############################

#--------------#
#### Set up ####
#--------------#
rm(list  = ls()) # clear environment
options(scipen = 999) # remove scientific notation
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path)) 

df_thx <- readRDS("data_discount.rds") # Load data

# 1.  Define a new object `r_disc`, which is the discount rate that will be used in the current assignment, and set its value to `0.05` (meaning 5% discount annualy)
r_disc

# 2. Calculate the total undiscounted incremental costs and effects for each intervention
v_res_undisc 

#3. Calculate the **undiscounted** ICERS for each of these interventions using the total incremental costs and effects you calculated under step 2
v_icer_undisc 

#3.a. & 3.b. questions to answer  



# 4. Create a vector of discount weights, `v_disc`, for Years 0 to 12, using `r_disc`, the `Year` column of the `df_thx` data fram
v_disc 

#Apply discounting on the incremental effects and costs (by multiplying each column by the vector of discount weights for instance) to convert these to their present value. Report the discounted effects and costs in new columns of `df_thx` called Inc_cost_A_d, Inc_QALY_A_d, Inc_cost_B_d, Inc_QALY_B_d, Inc_cost_C_d, Inc_QALY_C_d.



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


###############################
#### Practical Discounting ####
###############################

#--------------#
#### Set up ####
#--------------#
rm(list  = ls()) # clear environment
options(scipen = 999) # remove scientific notation
#install.packages("rstudioapi") # if not already installed
#install.packages("tidyverse") # for plotting, if not already installed

library(rstudioapi)
library(tidyverse)

setwd(dirname(getActiveDocumentContext()$path)) # set working directory to current location

df_thx <- readRDS("data_discount.rds") # Load data, CHANGE PATH ACCORDINGLY IF THE DATA DOES NOT LOAD AUTOMATICALLY!

#-------------------#
#### Assignments ####
#-------------------#


# 1.  Define a new object `r_disc`, which is the discount rate that will be used in the current assignment, and set its value to `0.05` (meaning 5% discount annualy)
r_disc

# 2. Calculate the total undiscounted incremental costs and effects for each intervention
v_res_undisc 

# 3. Calculate the **undiscounted** ICERS for each of these interventions using the total incremental costs and effects you calculated under step 2
v_icer_undisc 

# 4. Create a vector of discount weights, `v_disc`, for Years 0 to 12, using `r_disc`, the `Year` column of the `df_thx` data fram
v_disc 

# 5. Apply discounting on the incremental effects and costs (by multiplying each column by the vector of discount weights for instance) to convert these to their present value. Report the discounted effects and costs in new columns of `df_thx` called Inc_cost_A_d, Inc_QALY_A_d, Inc_cost_B_d, Inc_QALY_B_d, Inc_cost_C_d, Inc_QALY_C_d.
df_thx$Inc_cost_A_d <- df_thx$Inc_QALY_A_d <- df_thx$Inc_cost_B_d <- df_thx$Inc_QALY_B_d <- df_thx$Inc_cost_C_d <- df_thx$Inc_QALY_C_d <- 0

df_thx$Inc_cost_A_d
df_thx$Inc_QALY_A_d
df_thx$Inc_cost_B_d
df_thx$Inc_QALY_B_d
df_thx$Inc_cost_C_d
df_thx$Inc_QALY_C_d


# 6. Calculate the total **discounted** incremental costs and effects, and ICERs for each intervention. 
v_res_disc
v_icer_disc

# 7. Now apply a discount rate of **10%**  and calculate the **discounted** incremental costs and effects, and ICERs for each intervention again.
r_disc_10
v_disc_10

v_res_disc_10
v_icer_disc_10
#--------------------------#
#### Assignment HSTM 2B ####
#--------------------------#

#--------------#
#### Set up ####
#--------------#
rm(list = ls())
options(scipen = 999)

#----------------------------------#
#### 0. Define model parameters ####
#----------------------------------#

# Setting parameters
n_cycles <- 10 # number of cycles
r_d_effects <- 0.015 # annual discount rate, health effects
r_d_costs <- 0.04 # annual discount rate, costs
v_names_hs <- c("Well", "Post-minor_stroke", "Post-major_stroke", "Post-MI", "Death_stroke", "Death_MI", "Death_other") # vector of names of health states
n_hs <- length(v_names_hs) # number of health states
n_ind <- 100000 # number of individuals to simulate  
v_start_hs <- c(n_ind, 0, 0, 0, 0, 0, 0) # vector of starting position in the model  

# Input parameters

## Rates & probabilities
r_fatal_mi	<- 0.25 # rate fatal MI
r_fatal_stroke	<- 0.3 # rate fatal stroke
r_inc_mi <- 400 / 100000 # yearly incidence rate MI
r_inc_stroke <- 50 / 100000 # yearly incidence rate stroke
r_mort <- 650 / 100000 # yearly rate of death

r_mort_age_dependent <-  0.95^(c(115:20)-19) # mortality rate (age dependent)
# Determine mortality probability for each age
df_mort <- data.frame(cbind(age = c(20:115),
                            p_mort = 1 - exp(-r_mort_age_dependent)
                            )
)
df_mort[nrow(df_mort), 2] <- 1 # Assumption that everybody dies at 
#plot(df_mort[, 1], df_mort[,2], type = 'l')

## Treatment effectiveness
eff_mi	<- 0.6 # Treatment effectiveness of Aspirin on the probability of experiencing a MI
eff_stroke	<- 1.2 # Treatment effectiveness of Aspirin on the probability of experiencing a stroke

## Utility values 
u_healthy	<- 1 # utility value health state: Well
u_post_mi	<- 0.85 # utility value health state: Post-MI
u_post_minor_stroke	<- 0.75 # utility value health state: Post-minor stroke
u_post_major_stroke	<- 0.5 # utility value health state: Post-major stroke
u_aspirin_use	<- 0.999 # utility value health state: Well when using aspiring

## Costs
c_aspirin_use	<- 100 # yearly costs of using aspiring
c_post_mi	<- 8000 # yearly costs after having experienced a NON-FATAL MI
c_post_minor_stroke	<- 2000 # yearly costs after having experienced a NON-FATAL minor stroke
c_post_major_stroke	<- 20000 # yearly costs after having experienced a NON_FATAL major stroke

# PARAMETERS TO CALCULATE YOURSELF!
p_post_major_stroke	<- 235 / 1000 # probability to transit to "Post-major stroke" after a NON-FATAL stroke occured
p_post_minor_stroke	<- 1 - p_post_major_stroke # probability to transit to "Post-minor stroke" after a NON-FATAL stroke occured

#---------------------------------------------#
#### 1. Define 3D array "No aspirin" group ####
#---------------------------------------------#

# Define start age cohort
n_start_age <- 45

# Create a vector containing the mortality probability values for the age `n_start_age` to `n_start_age`+9 and call it `v_p_mort`
v_p_mort <- df_mort[c(which(df_mort$age == n_start_age):
                        which(df_mort$age == n_start_age + 9)
                      ), "p_mort"]

# Initialise array's
a_tp_comp <- array(0, dim = c(n_hs, n_hs, n_cycles),
                               dimnames = list(v_names_hs, v_names_hs, 1:n_cycles)
                               )

# Fill in the arrays using the background mortality for each age 
a_tp_comp["Well", "Well", ]   <- 1 - v_p_mort - r_inc_mi - r_inc_stroke  # EXAMPLE! Calculate the remaining transition probabilities
##Notice, that only using the first 2 elements of the array will fill these transition probabilities for all 10 transition matrices used in all cycles

a_tp_comp["Well", "Post-minor_stroke", ] <- r_inc_stroke * (1 - r_fatal_stroke) * p_post_minor_stroke
a_tp_comp["Well", "Post-major_stroke", ] <- r_inc_stroke * (1 - r_fatal_stroke) * p_post_major_stroke
a_tp_comp["Well", "Post-MI", ] <- r_inc_mi * (1 - r_fatal_mi)
a_tp_comp["Well", "Death_stroke", ] <- r_inc_stroke * r_fatal_stroke
a_tp_comp["Well", "Death_MI", ] <- r_inc_mi * r_fatal_mi
a_tp_comp["Well", "Death_other", ] <- v_p_mort

a_tp_comp["Post-minor_stroke", "Post-minor_stroke", ] <- 1 - v_p_mort
a_tp_comp["Post-minor_stroke", "Death_other", ] <- v_p_mort

a_tp_comp["Post-major_stroke", "Post-major_stroke", ] <- 1 - v_p_mort
a_tp_comp["Post-major_stroke", "Death_other", ] <- v_p_mort

a_tp_comp["Post-MI", "Post-MI", ] <- 1 - v_p_mort
a_tp_comp["Post-MI", "Death_other", ] <- v_p_mort

a_tp_comp["Death_stroke", "Death_stroke",] <- 1
a_tp_comp["Death_MI", "Death_MI",] <- 1
a_tp_comp["Death_other", "Death_other",] <- 1

# Inspect whether the sum of all TP's for all health states over all cycles = 1
m_check_comp <- matrix(NA,
                  nrow = n_cycles,
                  ncol = n_hs)

for (i in 1:n_cycles){
  v_res <- round(rowSums(a_tp_comp[ , ,i]), 5) == 1
  m_check_comp[i,] <- paste(v_res)
  
}
#m_check_comp # visualise the check

#----------------------------------------------------------------#
#### 2. Fill the cohort simulation for the "No aspirin" group ####
#----------------------------------------------------------------#

# Create a matrix to store the cohort simulation (`m_hs_comp`)
m_hs_comp <- matrix(0,
                    ncol = length(v_names_hs),
                    nrow = n_cycles + 1,
                    dimnames = list(c(0:n_cycles),
                                    v_names_hs)
                    ) 



# Define the start position of individuals (all in "well")
m_hs_comp[1,] <- v_start_hs

# Perform the matrix multiplication using the 3D array.
for(cycle in 1:n_cycles){
  m_hs_comp[cycle + 1,] <- m_hs_comp[cycle,] %*% a_tp_comp[, , cycle]
  
}

#----------------------------------------------------#
#### 3. Fill the 3D array for the "Aspirin" group ####
#----------------------------------------------------#

# Make a copy of `a_tp_comp` and call it `a_tp_int`
a_tp_int <- a_tp_comp

# Modify `a_tp_int` since Aspirin has an 
a_tp_int["Well", "Well", ]   <- 1 - v_p_mort - r_inc_mi * eff_mi - r_inc_stroke * eff_stroke
a_tp_int["Well", "Post-minor_stroke", ] <- r_inc_stroke * eff_stroke * (1 - r_fatal_stroke) * p_post_minor_stroke
a_tp_int["Well", "Post-major_stroke", ] <- r_inc_stroke * eff_stroke * (1 - r_fatal_stroke) * p_post_major_stroke
a_tp_int["Well", "Post-MI", ] <- r_inc_mi * eff_mi * (1 - r_fatal_mi)
a_tp_int["Well", "Death_stroke", ] <- r_inc_stroke * eff_stroke * r_fatal_stroke
a_tp_int["Well", "Death_MI", ] <- r_inc_mi * eff_mi * r_fatal_mi

# Inspect whether the sum of all TP's for all health states over all cycles = 1
m_check_int <- matrix(NA,
                       nrow = n_cycles,
                       ncol = n_hs)

for (i in 1:n_cycles){
  v_res <- round(rowSums(a_tp_int[ , ,i]), 5) == 1
  m_check_int[i,] <- paste(v_res)
  
}
#m_check_int # visualise the check

#-------------------------------------------------------------#
#### 4. Fill the cohort simulation for the "Aspirin" group ####
#-------------------------------------------------------------#

# Create a matrix to store the cohort simulation (`m_hs_int`)
m_hs_int <- matrix(0,
                    ncol = length(v_names_hs),
                    nrow = n_cycles + 1,
                    dimnames = list(c(0:n_cycles),
                                    v_names_hs)
) 



# Define the start position of individuals (all in "well")
m_hs_int[1,] <- v_start_hs

# Perform the matrix multiplication using the 3D array.
for(cycle in 1:n_cycles){
  m_hs_int[cycle + 1,] <- m_hs_int[cycle,] %*% a_tp_int[, , cycle]
  
}

#-----------------------------------------------------------------------------------------#
#### 5. Calculate life years, quality-adjusted life years, and costs for both strategy ####
#-----------------------------------------------------------------------------------------#

# Life years: EXAMPLE! DO THE SAME FOR QALY's AND COSTS
## Determine the number of life year won by 1 individual during 1 cycle
v_ly_comp <- c("Well" = 1,
               "Post-minor_stroke" = 1, 
               "Post-major_stroke" = 1, 
               "Post-MI" = 1, 
               "Death_stroke" = 0, 
               "Death_MI" = 0, 
               "Death_other" = 0)
v_ly_int <- c("Well" = 1,
               "Post-minor_stroke" = 1, 
               "Post-major_stroke" = 1, 
               "Post-MI" = 1, 
               "Death_stroke" = 0, 
               "Death_MI" = 0, 
               "Death_other" = 0)

## Determine the number of life year gained over the cycles (reward at the end of the cycle!)
v_t_ly_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_ly_comp
v_t_ly_int  <- m_hs_int[2:nrow(m_hs_int),] %*% v_ly_int

## Determine the cumulative number of life year gained over the cycles (reward at the end of the cycle!)
v_cum_ly_comp <- cumsum(v_t_ly_comp)
v_cum_ly_int  <- cumsum(v_t_ly_int)

## Determine the total number of life year gained (sum of all cycles; reward at the end of the cycle!)
n_t_ly_comp <- sum(v_t_ly_comp)
n_t_ly_int <- sum(v_t_ly_int)

# QALY's
## Determine the number of QALYs won by 1 individual during 1 cycle
v_qaly_comp <- c("Well" = u_healthy,
                 "Post-minor_stroke" = u_post_minor_stroke, 
                 "Post-major_stroke" = u_post_major_stroke, 
                 "Post-MI" = u_post_mi, 
                 "Death_stroke" = 0, 
                 "Death_MI" = 0, 
                 "Death_other" = 0)

v_qaly_int <- c("Well" = u_aspirin_use,
                "Post-minor_stroke" = u_post_minor_stroke, 
                "Post-major_stroke" = u_post_major_stroke, 
                "Post-MI" = u_post_mi, 
                "Death_stroke" = 0, 
                "Death_MI" = 0, 
                "Death_other" = 0)

## Determine the number of QALYs gained over the cycles (reward at the end of the cycle!)
v_t_qaly_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_qaly_comp
v_t_qaly_int  <- m_hs_int[2:nrow(m_hs_int),] %*% v_qaly_int

## Determine the cumulative number of QALYsr gained over the cycles (reward at the end of the cycle!)
v_cum_qaly_comp <- cumsum(v_t_qaly_comp)
v_cum_qaly_int <- cumsum(v_t_qaly_int)

## Determine the total number of QALYs gained (sum of all cycles; reward at the end of the cycle!)
n_t_qaly_comp <- sum(v_t_qaly_comp)
n_t_qaly_int  <- sum(v_t_qaly_int)

# Costs
## Determine the costs accrued by 1 individual during 1 cycle
v_c_comp <- c("Well" = 0,
              "Post-minor_stroke" = c_post_minor_stroke, 
              "Post-major_stroke" = c_post_major_stroke, 
              "Post-MI" = c_post_mi, 
              "Death_stroke" = 0, 
              "Death_MI" = 0, 
              "Death_other" = 0)

v_c_int <- c("Well" = c_aspirin_use,
             "Post-minor_stroke" = c_post_minor_stroke, 
             "Post-major_stroke" = c_post_major_stroke, 
             "Post-MI" = c_post_mi, 
             "Death_stroke" = 0, 
             "Death_MI" = 0, 
             "Death_other" = 0)

## Determine the costs accrued over the cycles (reward at the end of the cycle!)
v_t_c_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_c_comp
v_t_c_int  <- m_hs_int[2:nrow(m_hs_int),] %*% v_c_int

## Determine the costs accrued over the cycles (reward at the end of the cycle!)
v_cum_c_comp <- cumsum(v_t_c_comp)
v_cum_c_int  <- cumsum(v_t_c_int)

## Determine the total costs accrued (sum of all cycles; reward at the end of the cycle!)
n_t_c_comp <- sum(v_t_c_comp)
n_t_c_int  <- sum(v_t_c_int)

# Mean outcomes per individual
## incrementals and ICER
cbind(Strategy = c("No aspirin", "Aspirin"),
      LY = round( c(n_t_ly_comp, n_t_ly_int) / n_ind, 3),
      QALY = round( c(n_t_qaly_comp, n_t_qaly_int) / n_ind, 3),
      COSTS = round( c(n_t_c_comp, n_t_c_int) / n_ind, 0),
      INC_QALY = c("-", round(((n_t_qaly_int - n_t_qaly_comp) / n_ind), 3)),
      INC_COSTS = c("-", round(((n_t_c_int - n_t_c_comp) / n_ind), 0)),
      ICER = c("-", round( ((n_t_c_int - n_t_c_comp) / n_ind) / ((n_t_qaly_int - n_t_qaly_comp) / n_ind), 0))
      )

#---------------------------------------#
#### 6. Calculate discounted results ####
#---------------------------------------#

# Life years
## Define discount weights per cycle (years in this case)
v_dw_e <- 1 / (1 + r_d_effects) ^ c(1:n_cycles)

## Total discounted life years, using matrix multiplication
n_t_ly_comp_d <- t(v_t_ly_comp) %*% v_dw_e
n_t_ly_int_d  <- t(v_t_ly_int) %*% v_dw_e

##[ALTERNATIVE]##
### Multiply the number of life year gained in each cycle by the discount weight of each cycle
v_t_ly_comp_d <- v_t_ly_comp * v_dw_e
v_t_ly_int_d  <- v_t_ly_int * v_dw_e

### Sum to obtain total
n_t_ly_comp_d2 <- sum(v_t_ly_comp_d)
n_t_ly_int_d2  <- sum(v_t_ly_int_d)

### Check whether results are the same
round(n_t_ly_comp_d, 5) == round(n_t_ly_comp_d2, 5) # TRUE
round(n_t_ly_int_d, 5) == round(n_t_ly_int_d2, 5) # TRUE

# QALYs
## Define discount weights per cycle (years in this case)
#v_dw_e <- 1 / (1 + r_d_effects) ^ c(1:n_cycles)

## Total discounted life years, using matrix multiplication
n_t_qaly_comp_d <- t(v_t_qaly_comp) %*% v_dw_e
n_t_qaly_int_d  <- t(v_t_qaly_int) %*% v_dw_e

# Costs
## Define discount weights per cycle (years in this case)
v_dw_c <- 1 / (1 + r_d_costs) ^ c(1:n_cycles)

## Total discounted life years, using matrix multiplication
n_t_c_comp_d <- t(v_t_c_comp) %*% v_dw_c
n_t_c_int_d  <- t(v_t_c_int) %*% v_dw_c

# Mean discounted outcomes per individual
## incrementals and ICER
cbind(Strategy = c("No aspirin", "Aspirin"),
      LY = round( c(n_t_ly_comp_d, n_t_ly_int_d) / n_ind, 3),
      QALY = round( c(n_t_qaly_comp_d, n_t_qaly_int_d) / n_ind, 3),
      COSTS = round( c(n_t_c_comp_d, n_t_c_int_d) / n_ind, 0),
      INC_QALY = c("-", round(((n_t_qaly_int_d - n_t_qaly_comp_d) / n_ind), 3)),
      INC_COSTS = c("-", round(((n_t_c_int_d - n_t_c_comp_d) / n_ind), 0)),
      ICER = c("-", round( ((n_t_c_int_d - n_t_c_comp_d) / n_ind) / ((n_t_qaly_int_d - n_t_qaly_comp_d) / n_ind), 0))
)

#---------------------------------------------------------------#
#### 7. Impact of changing the discount rates on the results ####
#---------------------------------------------------------------#

# 7.a. Both discount rates to 6%
# Change discount rate
r_d_effects <- 0.06
r_d_costs <- 0.06

# Life years
## Define discount weights per cycle (years in this case)
v_dw_e <- 1 / (1 + r_d_effects) ^ c(1:n_cycles)

## Total discounted life years, using matrix multiplication
n_t_ly_comp_d <- t(v_t_ly_comp) %*% v_dw_e
n_t_ly_int_d  <- t(v_t_ly_int) %*% v_dw_e

##[ALTERNATIVE]##
### Multiply the number of life year gained in each cycle by the discount weight of each cycle
v_t_ly_comp_d <- v_t_ly_comp * v_dw_e
v_t_ly_int_d  <- v_t_ly_int * v_dw_e

### Sum to obtain total
n_t_ly_comp_d2 <- sum(v_t_ly_comp_d)
n_t_ly_int_d2  <- sum(v_t_ly_int_d)

### Check whether results are the same
round(n_t_ly_comp_d, 5) == round(n_t_ly_comp_d2, 5) # TRUE
round(n_t_ly_int_d, 5) == round(n_t_ly_int_d2, 5) # TRUE

# QALYs
## Define discount weights per cycle (years in this case)
#v_dw_e <- 1 / (1 + r_d_effects) ^ c(1:n_cycles)

## Total discounted life years, using matrix multiplication
n_t_qaly_comp_d <- t(v_t_qaly_comp) %*% v_dw_e
n_t_qaly_int_d  <- t(v_t_qaly_int) %*% v_dw_e

# Costs
## Define discount weights per cycle (years in this case)
v_dw_c <- 1 / (1 + r_d_costs) ^ c(1:n_cycles)

## Total discounted life years, using matrix multiplication
n_t_c_comp_d <- t(v_t_c_comp) %*% v_dw_c
n_t_c_int_d  <- t(v_t_c_int) %*% v_dw_c

# Mean discounted outcomes per individual
## incrementals and ICER
cbind(Strategy = c("No aspirin", "Aspirin"),
      LY = round( c(n_t_ly_comp_d, n_t_ly_int_d) / n_ind, 3),
      QALY = round( c(n_t_qaly_comp_d, n_t_qaly_int_d) / n_ind, 3),
      COSTS = round( c(n_t_c_comp_d, n_t_c_int_d) / n_ind, 0),
      INC_QALY = c("-", round(((n_t_qaly_int_d - n_t_qaly_comp_d) / n_ind), 3)),
      INC_COSTS = c("-", round(((n_t_c_int_d - n_t_c_comp_d) / n_ind), 0)),
      ICER = c("-", round( ((n_t_c_int_d - n_t_c_comp_d) / n_ind) / ((n_t_qaly_int_d - n_t_qaly_comp_d) / n_ind), 0))
)

# 7.b. Both discount rates to 1.5%
# Change discount rate
r_d_effects <- 0.015
r_d_costs <- 0.015

# Life years
## Define discount weights per cycle (years in this case)
v_dw_e <- 1 / (1 + r_d_effects) ^ c(1:n_cycles)

## Total discounted life years, using matrix multiplication
n_t_ly_comp_d <- t(v_t_ly_comp) %*% v_dw_e
n_t_ly_int_d  <- t(v_t_ly_int) %*% v_dw_e

##[ALTERNATIVE]##
### Multiply the number of life year gained in each cycle by the discount weight of each cycle
v_t_ly_comp_d <- v_t_ly_comp * v_dw_e
v_t_ly_int_d  <- v_t_ly_int * v_dw_e

### Sum to obtain total
n_t_ly_comp_d2 <- sum(v_t_ly_comp_d)
n_t_ly_int_d2  <- sum(v_t_ly_int_d)

### Check whether results are the same
round(n_t_ly_comp_d, 5) == round(n_t_ly_comp_d2, 5) # TRUE
round(n_t_ly_int_d, 5) == round(n_t_ly_int_d2, 5) # TRUE

# QALYs
## Define discount weights per cycle (years in this case)
#v_dw_e <- 1 / (1 + r_d_effects) ^ c(1:n_cycles)

## Total discounted life years, using matrix multiplication
n_t_qaly_comp_d <- t(v_t_qaly_comp) %*% v_dw_e
n_t_qaly_int_d  <- t(v_t_qaly_int) %*% v_dw_e

# Costs
## Define discount weights per cycle (years in this case)
v_dw_c <- 1 / (1 + r_d_costs) ^ c(1:n_cycles)

## Total discounted life years, using matrix multiplication
n_t_c_comp_d <- t(v_t_c_comp) %*% v_dw_c
n_t_c_int_d  <- t(v_t_c_int) %*% v_dw_c

# Mean discounted outcomes per individual
## incrementals and ICER
cbind(Strategy = c("No aspirin", "Aspirin"),
      LY = round( c(n_t_ly_comp_d, n_t_ly_int_d) / n_ind, 3),
      QALY = round( c(n_t_qaly_comp_d, n_t_qaly_int_d) / n_ind, 3),
      COSTS = round( c(n_t_c_comp_d, n_t_c_int_d) / n_ind, 0),
      INC_QALY = c("-", round(((n_t_qaly_int_d - n_t_qaly_comp_d) / n_ind), 3)),
      INC_COSTS = c("-", round(((n_t_c_int_d - n_t_c_comp_d) / n_ind), 0)),
      ICER = c("-", round( ((n_t_c_int_d - n_t_c_comp_d) / n_ind) / ((n_t_qaly_int_d - n_t_qaly_comp_d) / n_ind), 0))
)

#-------------------------------------------------------------#
#### 8. Impact of changing the starting age on the results ####
#-------------------------------------------------------------#

# 8.a. Starting age = 65
# Change discount rate
r_d_effects <- 0.015
r_d_costs <- 0.04
n_start_age <- 65

# Create a vector containing the mortality probability values for the age `n_start_age` to `n_start_age`+9 and call it `v_p_mort`
v_p_mort <- df_mort[c(which(df_mort$age == n_start_age):
                        which(df_mort$age == n_start_age + 9)
), "p_mort"]

# Initialise array's
a_tp_comp <- array(0, dim = c(n_hs, n_hs, n_cycles),
                   dimnames = list(v_names_hs, v_names_hs, 1:n_cycles)
)

# Fill in the arrays using the background mortality for each age 
a_tp_comp["Well", "Well", ]   <- 1 - v_p_mort - r_inc_mi - r_inc_stroke  # EXAMPLE! Calculate the remaining transition probabilities
##Notice, that only using the first 2 elements of the array will fill these transition probabilities for all 10 transition matrices used in all cycles

a_tp_comp["Well", "Post-minor_stroke", ] <- r_inc_stroke * (1 - r_fatal_stroke) * p_post_minor_stroke
a_tp_comp["Well", "Post-major_stroke", ] <- r_inc_stroke * (1 - r_fatal_stroke) * p_post_major_stroke
a_tp_comp["Well", "Post-MI", ] <- r_inc_mi * (1 - r_fatal_mi)
a_tp_comp["Well", "Death_stroke", ] <- r_inc_stroke * r_fatal_stroke
a_tp_comp["Well", "Death_MI", ] <- r_inc_mi * r_fatal_mi
a_tp_comp["Well", "Death_other", ] <- v_p_mort

a_tp_comp["Post-minor_stroke", "Post-minor_stroke", ] <- 1 - v_p_mort
a_tp_comp["Post-minor_stroke", "Death_other", ] <- v_p_mort

a_tp_comp["Post-major_stroke", "Post-major_stroke", ] <- 1 - v_p_mort
a_tp_comp["Post-major_stroke", "Death_other", ] <- v_p_mort

a_tp_comp["Post-MI", "Post-MI", ] <- 1 - v_p_mort
a_tp_comp["Post-MI", "Death_other", ] <- v_p_mort

a_tp_comp["Death_stroke", "Death_stroke",] <- 1
a_tp_comp["Death_MI", "Death_MI",] <- 1
a_tp_comp["Death_other", "Death_other",] <- 1

# Inspect whether the sum of all TP's for all health states over all cycles = 1
m_check_comp <- matrix(NA,
                       nrow = n_cycles,
                       ncol = n_hs)

for (i in 1:n_cycles){
  v_res <- round(rowSums(a_tp_comp[ , ,i]), 5) == 1
  m_check_comp[i,] <- paste(v_res)
  
}
#m_check_comp # visualise the check

# Create a matrix to store the cohort simulation (`m_hs_comp`)
m_hs_comp <- matrix(0,
                    ncol = length(v_names_hs),
                    nrow = n_cycles + 1,
                    dimnames = list(c(0:n_cycles),
                                    v_names_hs)
) 



# Define the start position of individuals (all in "well")
m_hs_comp[1,] <- v_start_hs

# Perform the matrix multiplication using the 3D array.
for(cycle in 1:n_cycles){
  m_hs_comp[cycle + 1,] <- m_hs_comp[cycle,] %*% a_tp_comp[, , cycle]
  
}

# Make a copy of `a_tp_comp` and call it `a_tp_int`
a_tp_int <- a_tp_comp

# Modify `a_tp_int` since Aspirin has an 
a_tp_int["Well", "Well", ]   <- 1 - v_p_mort - r_inc_mi * eff_mi - r_inc_stroke * eff_stroke
a_tp_int["Well", "Post-minor_stroke", ] <- r_inc_stroke * eff_stroke * (1 - r_fatal_stroke) * p_post_minor_stroke
a_tp_int["Well", "Post-major_stroke", ] <- r_inc_stroke * eff_stroke * (1 - r_fatal_stroke) * p_post_major_stroke
a_tp_int["Well", "Post-MI", ] <- r_inc_mi * eff_mi * (1 - r_fatal_mi)
a_tp_int["Well", "Death_stroke", ] <- r_inc_stroke * eff_stroke * r_fatal_stroke
a_tp_int["Well", "Death_MI", ] <- r_inc_mi * eff_mi * r_fatal_mi

# Inspect whether the sum of all TP's for all health states over all cycles = 1
m_check_int <- matrix(NA,
                      nrow = n_cycles,
                      ncol = n_hs)

for (i in 1:n_cycles){
  v_res <- round(rowSums(a_tp_int[ , ,i]), 5) == 1
  m_check_int[i,] <- paste(v_res)
  
}
#m_check_int # visualise the check

# Create a matrix to store the cohort simulation (`m_hs_int`)
m_hs_int <- matrix(0,
                   ncol = length(v_names_hs),
                   nrow = n_cycles + 1,
                   dimnames = list(c(0:n_cycles),
                                   v_names_hs)
) 



# Define the start position of individuals (all in "well")
m_hs_int[1,] <- v_start_hs

# Perform the matrix multiplication using the 3D array.
for(cycle in 1:n_cycles){
  m_hs_int[cycle + 1,] <- m_hs_int[cycle,] %*% a_tp_int[, , cycle]
  
}

# Life years: EXAMPLE! DO THE SAME FOR QALY's AND COSTS
## Determine the number of life year won by 1 individual during 1 cycle
v_ly_comp <- c("Well" = 1,
               "Post-minor_stroke" = 1, 
               "Post-major_stroke" = 1, 
               "Post-MI" = 1, 
               "Death_stroke" = 0, 
               "Death_MI" = 0, 
               "Death_other" = 0)
v_ly_int <- c("Well" = 1,
              "Post-minor_stroke" = 1, 
              "Post-major_stroke" = 1, 
              "Post-MI" = 1, 
              "Death_stroke" = 0, 
              "Death_MI" = 0, 
              "Death_other" = 0)

## Determine the number of life year gained over the cycles (reward at the end of the cycle!)
v_t_ly_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_ly_comp
v_t_ly_int  <- m_hs_int[2:nrow(m_hs_int),] %*% v_ly_int

## Determine the cumulative number of life year gained over the cycles (reward at the end of the cycle!)
v_cum_ly_comp <- cumsum(v_t_ly_comp)
v_cum_ly_int  <- cumsum(v_t_ly_int)

## Determine the total number of life year gained (sum of all cycles; reward at the end of the cycle!)
n_t_ly_comp <- sum(v_t_ly_comp)
n_t_ly_int <- sum(v_t_ly_int)

# QALY's
## Determine the number of QALYs won by 1 individual during 1 cycle
v_qaly_comp <- c("Well" = u_healthy,
                 "Post-minor_stroke" = u_post_minor_stroke, 
                 "Post-major_stroke" = u_post_major_stroke, 
                 "Post-MI" = u_post_mi, 
                 "Death_stroke" = 0, 
                 "Death_MI" = 0, 
                 "Death_other" = 0)

v_qaly_int <- c("Well" = u_aspirin_use,
                "Post-minor_stroke" = u_post_minor_stroke, 
                "Post-major_stroke" = u_post_major_stroke, 
                "Post-MI" = u_post_mi, 
                "Death_stroke" = 0, 
                "Death_MI" = 0, 
                "Death_other" = 0)

## Determine the number of QALYs gained over the cycles (reward at the end of the cycle!)
v_t_qaly_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_qaly_comp
v_t_qaly_int  <- m_hs_int[2:nrow(m_hs_int),] %*% v_qaly_int

## Determine the cumulative number of QALYsr gained over the cycles (reward at the end of the cycle!)
v_cum_qaly_comp <- cumsum(v_t_qaly_comp)
v_cum_qaly_int <- cumsum(v_t_qaly_int)

## Determine the total number of QALYs gained (sum of all cycles; reward at the end of the cycle!)
n_t_qaly_comp <- sum(v_t_qaly_comp)
n_t_qaly_int  <- sum(v_t_qaly_int)

# Costs
## Determine the costs accrued by 1 individual during 1 cycle
v_c_comp <- c("Well" = 0,
              "Post-minor_stroke" = c_post_minor_stroke, 
              "Post-major_stroke" = c_post_major_stroke, 
              "Post-MI" = c_post_mi, 
              "Death_stroke" = 0, 
              "Death_MI" = 0, 
              "Death_other" = 0)

v_c_int <- c("Well" = c_aspirin_use,
             "Post-minor_stroke" = c_post_minor_stroke, 
             "Post-major_stroke" = c_post_major_stroke, 
             "Post-MI" = c_post_mi, 
             "Death_stroke" = 0, 
             "Death_MI" = 0, 
             "Death_other" = 0)

## Determine the costs accrued over the cycles (reward at the end of the cycle!)
v_t_c_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_c_comp
v_t_c_int  <- m_hs_int[2:nrow(m_hs_int),] %*% v_c_int

## Determine the costs accrued over the cycles (reward at the end of the cycle!)
v_cum_c_comp <- cumsum(v_t_c_comp)
v_cum_c_int  <- cumsum(v_t_c_int)

## Determine the total costs accrued (sum of all cycles; reward at the end of the cycle!)
n_t_c_comp <- sum(v_t_c_comp)
n_t_c_int  <- sum(v_t_c_int)

# Life years
## Define discount weights per cycle (years in this case)
v_dw_e <- 1 / (1 + r_d_effects) ^ c(1:n_cycles)

## Total discounted life years, using matrix multiplication
n_t_ly_comp_d <- t(v_t_ly_comp) %*% v_dw_e
n_t_ly_int_d  <- t(v_t_ly_int) %*% v_dw_e

# QALYs
## Define discount weights per cycle (years in this case)
#v_dw_e <- 1 / (1 + r_d_effects) ^ c(1:n_cycles)

## Total discounted life years, using matrix multiplication
n_t_qaly_comp_d <- t(v_t_qaly_comp) %*% v_dw_e
n_t_qaly_int_d  <- t(v_t_qaly_int) %*% v_dw_e

# Costs
## Define discount weights per cycle (years in this case)
v_dw_c <- 1 / (1 + r_d_costs) ^ c(1:n_cycles)

## Total discounted life years, using matrix multiplication
n_t_c_comp_d <- t(v_t_c_comp) %*% v_dw_c
n_t_c_int_d  <- t(v_t_c_int) %*% v_dw_c

# Mean discounted outcomes per individual
## incrementals and ICER
cbind(Strategy = c("No aspirin", "Aspirin"),
      LY = round( c(n_t_ly_comp_d, n_t_ly_int_d) / n_ind, 3),
      QALY = round( c(n_t_qaly_comp_d, n_t_qaly_int_d) / n_ind, 3),
      COSTS = round( c(n_t_c_comp_d, n_t_c_int_d) / n_ind, 0),
      INC_QALY = c("-", round(((n_t_qaly_int_d - n_t_qaly_comp_d) / n_ind), 3)),
      INC_COSTS = c("-", round(((n_t_c_int_d - n_t_c_comp_d) / n_ind), 0)),
      ICER = c("-", round( ((n_t_c_int_d - n_t_c_comp_d) / n_ind) / ((n_t_qaly_int_d - n_t_qaly_comp_d) / n_ind), 0))
)

# 8.b. Starting age = 95
n_start_age <- 95

# Create a vector containing the mortality probability values for the age `n_start_age` to `n_start_age`+9 and call it `v_p_mort`
v_p_mort <- df_mort[c(which(df_mort$age == n_start_age):
                        which(df_mort$age == n_start_age + 9)
), "p_mort"]

# Initialise array's
a_tp_comp <- array(0, dim = c(n_hs, n_hs, n_cycles),
                   dimnames = list(v_names_hs, v_names_hs, 1:n_cycles)
)

# Fill in the arrays using the background mortality for each age 
a_tp_comp["Well", "Well", ]   <- 1 - v_p_mort - r_inc_mi - r_inc_stroke  # EXAMPLE! Calculate the remaining transition probabilities
##Notice, that only using the first 2 elements of the array will fill these transition probabilities for all 10 transition matrices used in all cycles

a_tp_comp["Well", "Post-minor_stroke", ] <- r_inc_stroke * (1 - r_fatal_stroke) * p_post_minor_stroke
a_tp_comp["Well", "Post-major_stroke", ] <- r_inc_stroke * (1 - r_fatal_stroke) * p_post_major_stroke
a_tp_comp["Well", "Post-MI", ] <- r_inc_mi * (1 - r_fatal_mi)
a_tp_comp["Well", "Death_stroke", ] <- r_inc_stroke * r_fatal_stroke
a_tp_comp["Well", "Death_MI", ] <- r_inc_mi * r_fatal_mi
a_tp_comp["Well", "Death_other", ] <- v_p_mort

a_tp_comp["Post-minor_stroke", "Post-minor_stroke", ] <- 1 - v_p_mort
a_tp_comp["Post-minor_stroke", "Death_other", ] <- v_p_mort

a_tp_comp["Post-major_stroke", "Post-major_stroke", ] <- 1 - v_p_mort
a_tp_comp["Post-major_stroke", "Death_other", ] <- v_p_mort

a_tp_comp["Post-MI", "Post-MI", ] <- 1 - v_p_mort
a_tp_comp["Post-MI", "Death_other", ] <- v_p_mort

a_tp_comp["Death_stroke", "Death_stroke",] <- 1
a_tp_comp["Death_MI", "Death_MI",] <- 1
a_tp_comp["Death_other", "Death_other",] <- 1

# Inspect whether the sum of all TP's for all health states over all cycles = 1
m_check_comp <- matrix(NA,
                       nrow = n_cycles,
                       ncol = n_hs)

for (i in 1:n_cycles){
  v_res <- round(rowSums(a_tp_comp[ , ,i]), 5) == 1
  m_check_comp[i,] <- paste(v_res)
  
}
#m_check_comp # visualise the check

# Create a matrix to store the cohort simulation (`m_hs_comp`)
m_hs_comp <- matrix(0,
                    ncol = length(v_names_hs),
                    nrow = n_cycles + 1,
                    dimnames = list(c(0:n_cycles),
                                    v_names_hs)
) 



# Define the start position of individuals (all in "well")
m_hs_comp[1,] <- v_start_hs

# Perform the matrix multiplication using the 3D array.
for(cycle in 1:n_cycles){
  m_hs_comp[cycle + 1,] <- m_hs_comp[cycle,] %*% a_tp_comp[, , cycle]
  
}

# Make a copy of `a_tp_comp` and call it `a_tp_int`
a_tp_int <- a_tp_comp

# Modify `a_tp_int` since Aspirin has an 
a_tp_int["Well", "Well", ]   <- 1 - v_p_mort - r_inc_mi * eff_mi - r_inc_stroke * eff_stroke
a_tp_int["Well", "Post-minor_stroke", ] <- r_inc_stroke * eff_stroke * (1 - r_fatal_stroke) * p_post_minor_stroke
a_tp_int["Well", "Post-major_stroke", ] <- r_inc_stroke * eff_stroke * (1 - r_fatal_stroke) * p_post_major_stroke
a_tp_int["Well", "Post-MI", ] <- r_inc_mi * eff_mi * (1 - r_fatal_mi)
a_tp_int["Well", "Death_stroke", ] <- r_inc_stroke * eff_stroke * r_fatal_stroke
a_tp_int["Well", "Death_MI", ] <- r_inc_mi * eff_mi * r_fatal_mi

# Inspect whether the sum of all TP's for all health states over all cycles = 1
m_check_int <- matrix(NA,
                      nrow = n_cycles,
                      ncol = n_hs)

for (i in 1:n_cycles){
  v_res <- round(rowSums(a_tp_int[ , ,i]), 5) == 1
  m_check_int[i,] <- paste(v_res)
  
}
#m_check_int # visualise the check

# Create a matrix to store the cohort simulation (`m_hs_int`)
m_hs_int <- matrix(0,
                   ncol = length(v_names_hs),
                   nrow = n_cycles + 1,
                   dimnames = list(c(0:n_cycles),
                                   v_names_hs)
) 



# Define the start position of individuals (all in "well")
m_hs_int[1,] <- v_start_hs

# Perform the matrix multiplication using the 3D array.
for(cycle in 1:n_cycles){
  m_hs_int[cycle + 1,] <- m_hs_int[cycle,] %*% a_tp_int[, , cycle]
  
}

# Life years: EXAMPLE! DO THE SAME FOR QALY's AND COSTS
## Determine the number of life year won by 1 individual during 1 cycle
v_ly_comp <- c("Well" = 1,
               "Post-minor_stroke" = 1, 
               "Post-major_stroke" = 1, 
               "Post-MI" = 1, 
               "Death_stroke" = 0, 
               "Death_MI" = 0, 
               "Death_other" = 0)
v_ly_int <- c("Well" = 1,
              "Post-minor_stroke" = 1, 
              "Post-major_stroke" = 1, 
              "Post-MI" = 1, 
              "Death_stroke" = 0, 
              "Death_MI" = 0, 
              "Death_other" = 0)

## Determine the number of life year gained over the cycles (reward at the end of the cycle!)
v_t_ly_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_ly_comp
v_t_ly_int  <- m_hs_int[2:nrow(m_hs_int),] %*% v_ly_int

## Determine the cumulative number of life year gained over the cycles (reward at the end of the cycle!)
v_cum_ly_comp <- cumsum(v_t_ly_comp)
v_cum_ly_int  <- cumsum(v_t_ly_int)

## Determine the total number of life year gained (sum of all cycles; reward at the end of the cycle!)
n_t_ly_comp <- sum(v_t_ly_comp)
n_t_ly_int <- sum(v_t_ly_int)

# QALY's
## Determine the number of QALYs won by 1 individual during 1 cycle
v_qaly_comp <- c("Well" = u_healthy,
                 "Post-minor_stroke" = u_post_minor_stroke, 
                 "Post-major_stroke" = u_post_major_stroke, 
                 "Post-MI" = u_post_mi, 
                 "Death_stroke" = 0, 
                 "Death_MI" = 0, 
                 "Death_other" = 0)

v_qaly_int <- c("Well" = u_aspirin_use,
                "Post-minor_stroke" = u_post_minor_stroke, 
                "Post-major_stroke" = u_post_major_stroke, 
                "Post-MI" = u_post_mi, 
                "Death_stroke" = 0, 
                "Death_MI" = 0, 
                "Death_other" = 0)

## Determine the number of QALYs gained over the cycles (reward at the end of the cycle!)
v_t_qaly_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_qaly_comp
v_t_qaly_int  <- m_hs_int[2:nrow(m_hs_int),] %*% v_qaly_int

## Determine the cumulative number of QALYsr gained over the cycles (reward at the end of the cycle!)
v_cum_qaly_comp <- cumsum(v_t_qaly_comp)
v_cum_qaly_int <- cumsum(v_t_qaly_int)

## Determine the total number of QALYs gained (sum of all cycles; reward at the end of the cycle!)
n_t_qaly_comp <- sum(v_t_qaly_comp)
n_t_qaly_int  <- sum(v_t_qaly_int)

# Costs
## Determine the costs accrued by 1 individual during 1 cycle
v_c_comp <- c("Well" = 0,
              "Post-minor_stroke" = c_post_minor_stroke, 
              "Post-major_stroke" = c_post_major_stroke, 
              "Post-MI" = c_post_mi, 
              "Death_stroke" = 0, 
              "Death_MI" = 0, 
              "Death_other" = 0)

v_c_int <- c("Well" = c_aspirin_use,
             "Post-minor_stroke" = c_post_minor_stroke, 
             "Post-major_stroke" = c_post_major_stroke, 
             "Post-MI" = c_post_mi, 
             "Death_stroke" = 0, 
             "Death_MI" = 0, 
             "Death_other" = 0)

## Determine the costs accrued over the cycles (reward at the end of the cycle!)
v_t_c_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_c_comp
v_t_c_int  <- m_hs_int[2:nrow(m_hs_int),] %*% v_c_int

## Determine the costs accrued over the cycles (reward at the end of the cycle!)
v_cum_c_comp <- cumsum(v_t_c_comp)
v_cum_c_int  <- cumsum(v_t_c_int)

## Determine the total costs accrued (sum of all cycles; reward at the end of the cycle!)
n_t_c_comp <- sum(v_t_c_comp)
n_t_c_int  <- sum(v_t_c_int)

# Life years
## Define discount weights per cycle (years in this case)
v_dw_e <- 1 / (1 + r_d_effects) ^ c(1:n_cycles)

## Total discounted life years, using matrix multiplication
n_t_ly_comp_d <- t(v_t_ly_comp) %*% v_dw_e
n_t_ly_int_d  <- t(v_t_ly_int) %*% v_dw_e

# QALYs
## Define discount weights per cycle (years in this case)
#v_dw_e <- 1 / (1 + r_d_effects) ^ c(1:n_cycles)

## Total discounted life years, using matrix multiplication
n_t_qaly_comp_d <- t(v_t_qaly_comp) %*% v_dw_e
n_t_qaly_int_d  <- t(v_t_qaly_int) %*% v_dw_e

# Costs
## Define discount weights per cycle (years in this case)
v_dw_c <- 1 / (1 + r_d_costs) ^ c(1:n_cycles)

## Total discounted life years, using matrix multiplication
n_t_c_comp_d <- t(v_t_c_comp) %*% v_dw_c
n_t_c_int_d  <- t(v_t_c_int) %*% v_dw_c

# Mean discounted outcomes per individual
## incrementals and ICER
cbind(Strategy = c("No aspirin", "Aspirin"),
      LY = round( c(n_t_ly_comp_d, n_t_ly_int_d) / n_ind, 3),
      QALY = round( c(n_t_qaly_comp_d, n_t_qaly_int_d) / n_ind, 3),
      COSTS = round( c(n_t_c_comp_d, n_t_c_int_d) / n_ind, 0),
      INC_QALY = c("-", round(((n_t_qaly_int_d - n_t_qaly_comp_d) / n_ind), 3)),
      INC_COSTS = c("-", round(((n_t_c_int_d - n_t_c_comp_d) / n_ind), 0)),
      ICER = c("-", round( ((n_t_c_int_d - n_t_c_comp_d) / n_ind) / ((n_t_qaly_int_d - n_t_qaly_comp_d) / n_ind), 0))
)
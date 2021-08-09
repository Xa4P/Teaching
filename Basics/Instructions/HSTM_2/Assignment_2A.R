##########################
#### HSTM_2 PRACTICAL ####
##########################

#--------------#
#### Set up ####
#--------------#
rm(list = ls())
options(scipen = 999)

#----------------------------------#
#### 1. Define model parameters ####
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
p_post_major_stroke	<- 235 / 1000 # TO CALCULATE (1.a.): probability to transit to "Post-major stroke" after a NON-FATAL stroke occured
p_post_minor_stroke	<- 1 - p_post_major_stroke # TO CALCULATE (1.a.): probability to transit to "Post-minor stroke" after a NON-FATAL stroke occured

r_inc_mi <- 400 / 100000 # TO CALCULATE (1.b.): yearly incidence rate MI
r_inc_stroke <- 50 / 100000 # TO CALCULATE (1.b.): yearly incidence rate stroke
r_mort <- 650 / 100000 # TO CALCULATE (1.b.): yearly rate of death

#-------------------------------------------------------------#
### 2. Define transition probability 'NO ASPIRIN' strategy ####
#-------------------------------------------------------------#

# 2. Based on the parameter values, determine the transition matrix of the "No aspirin" strategy, and call it `m_tp_comp` (for matrix transition probabilities of the comparator).
m_tp_comp <- matrix(0,
                    ncol = n_hs,
                    nrow = n_hs,
                    dimnames = list(v_names_hs,
                                    v_names_hs))
## Filling the transition matrix
m_tp_comp["Well", "Well"] <- 1 - r_inc_mi - r_inc_stroke - r_mort # EXAMPLE! Calculate the remaining transition probabilities
m_tp_comp["Well", "Post-minor_stroke"] <- r_inc_stroke * (1 - r_fatal_stroke) * p_post_minor_stroke
m_tp_comp["Well", "Post-major_stroke"] <- r_inc_stroke * (1 - r_fatal_stroke) * p_post_major_stroke
m_tp_comp["Well", "Post-MI"] <- r_inc_mi * (1 - r_fatal_mi)
m_tp_comp["Well", "Death_stroke"] <- r_inc_stroke * r_fatal_stroke
m_tp_comp["Well", "Death_MI"] <- r_inc_mi * r_fatal_mi
m_tp_comp["Well", "Death_other"] <- r_mort

m_tp_comp["Post-minor_stroke", "Post-minor_stroke"] <- 1 - r_mort
m_tp_comp["Post-minor_stroke", "Death_other"] <- r_mort

m_tp_comp["Post-major_stroke", "Post-major_stroke"] <- 1 - r_mort
m_tp_comp["Post-major_stroke", "Death_other"] <- r_mort

m_tp_comp["Post-MI", "Post-MI"] <- 1 - r_mort
m_tp_comp["Post-MI", "Death_other"] <- r_mort

m_tp_comp["Death_stroke", "Death_stroke"] <- 1
m_tp_comp["Death_MI", "Death_MI"] <- 1
m_tp_comp["Death_other", "Death_other"] <- 1

#2.a. Check whether the sum of all rows equals 1.  
rowSums(m_tp_comp)

#----------------------------------------------------------#
#### 3. Perform cohort simulation "No ASPIRIN" strategy ####
#----------------------------------------------------------#

# Define cohort simulation matrix
m_hs_comp <- matrix(0,
                    ncol = n_hs,
                    nrow = n_cycles + 1,
                    dimnames = list(c(0:n_cycles),
                                    v_names_hs)
                    )
#head(m_hs_comp)

# Define the starting position of the individuals in each health state
m_hs_comp[1, ] <- v_start_hs

# Perform the matrix multiplication (for loop) to fill in the cohort simulation
for(cycle in 1:n_cycles){
  
  m_hs_comp[cycle + 1, ] <- m_hs_comp[cycle, ] %*% m_tp_comp # matrix multiplication
  
}
#head(m_hs_comp)

#3.a. Check whether all rows sum up to 100,000 to ensure you do not 'loose' or 'add' any person to the cohort simulation
round(rowSums(m_hs_comp), 9) == n_ind

#-----------------------------------------------------------------------------------------------------#
#### 4. Calculate life years, quality-adjusted life years, and costs for the "No aspirin" strategy ####
#-----------------------------------------------------------------------------------------------------#

# Life years: EXAMPLE! DO THE SAME FOR QALY's AND COSTS
## Determine the number of life year won by 1 individual during 1 cycle
v_ly_comp <- c("Well" = 1,
               "Post-minor_stroke" = 1, 
               "Post-major_stroke" = 1, 
               "Post-MI" = 1, 
               "Death_stroke" = 0, 
               "Death_MI" = 0, 
               "Death_other" = 0)

## Determine the number of life year gained over the cycles (reward at the end of the cycle!)
v_t_ly_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_ly_comp

## Determine the cumulative number of life year gained over the cycles (reward at the end of the cycle!)
v_cum_ly_comp <- cumsum(v_t_ly_comp)

## Determine the total number of life year gained (sum of all cycles; reward at the end of the cycle!)
n_t_ly_comp <- sum(v_t_ly_comp)

# QALY's
## Determine the number of QALYs won by 1 individual during 1 cycle
v_qaly_comp <- c("Well" = u_healthy,
                 "Post-minor_stroke" = u_post_minor_stroke, 
                 "Post-major_stroke" = u_post_major_stroke, 
                 "Post-MI" = u_post_mi, 
                 "Death_stroke" = 0, 
                 "Death_MI" = 0, 
                 "Death_other" = 0)

## Determine the number of QALYs gained over the cycles (reward at the end of the cycle!)
v_t_qaly_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_qaly_comp

## Determine the cumulative number of QALYsr gained over the cycles (reward at the end of the cycle!)
v_cum_qaly_comp <- cumsum(v_t_qaly_comp)

## Determine the total number of QALYs gained (sum of all cycles; reward at the end of the cycle!)
n_t_qaly_comp <- sum(v_t_qaly_comp)

# Costs
## Determine the costs accrued by 1 individual during 1 cycle
v_c_comp <- c("Well" = 0,
              "Post-minor_stroke" = c_post_minor_stroke, 
              "Post-major_stroke" = c_post_major_stroke, 
              "Post-MI" = c_post_mi, 
              "Death_stroke" = 0, 
              "Death_MI" = 0, 
              "Death_other" = 0)

## Determine the costs accrued over the cycles (reward at the end of the cycle!)
v_t_c_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_c_comp

## Determine the costs accrued over the cycles (reward at the end of the cycle!)
v_cum_c_comp <- cumsum(v_t_c_comp)

## Determine the total costs accrued (sum of all cycles; reward at the end of the cycle!)
n_t_c_comp <- sum(v_t_c_comp)

# Mean outcomes per individual
round(c(LY = n_t_ly_comp, 
        QALY = n_t_qaly_comp, 
        Costs = n_t_c_comp) / n_ind, 2)

#---------------------------------------#
#### 5. Calculate discounted results ####
#---------------------------------------#

# Life years
## Define discount weights per cycle (years in this case)
v_dw_e <- 1 / (1 + r_d_effects) ^ c(1:n_cycles)

## Total discounted life years, using matrix multiplication
n_t_ly_comp_d <- t(v_t_ly_comp) %*% v_dw_e

##[ALTERNATIVE]##
### Multiply the number of life year gained in each cycle by the discount weight of each cycle
v_t_ly_comp_d <- v_t_ly_comp * v_dw_e
### Sum to obtain total
n_t_ly_comp_d2 <- sum(v_t_ly_comp_d)
### Check whether results are the same
round(n_t_ly_comp_d, 5) == round(n_t_ly_comp_d2, 5) # TRUE

# QALYs
## Define discount weights per cycle (years in this case)
#v_dw_e <- 1 / (1 + r_d_effects) ^ c(1:n_cycles)

## Total discounted life years, using matrix multiplication
n_t_qaly_comp_d <- t(v_t_qaly_comp) %*% v_dw_e

# Costs
## Define discount weights per cycle (years in this case)
v_dw_c <- 1 / (1 + r_d_costs) ^ c(1:n_cycles)

## Total discounted life years, using matrix multiplication
n_t_c_comp_d <- t(v_t_c_comp) %*% v_dw_c

# Mean discounted outcomes per individual
round(c(LY = n_t_ly_comp_d, 
        QALY = n_t_qaly_comp_d, 
        Costs = n_t_c_comp_d) / n_ind, 2)

# Determine mortality probability
df_mort <- data.frame(cbind(age = c(20:115),
                            p_mort = seq(from = 0.001, to = 0.55, length.out =  length(c(20:115))
                                         )
                            )
                      )
df_mort[nrow(df_mort), 2] <- 1
#save(list = ls(), file = "data_HSTM_2B.RData")
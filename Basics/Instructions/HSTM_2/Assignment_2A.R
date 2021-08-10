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
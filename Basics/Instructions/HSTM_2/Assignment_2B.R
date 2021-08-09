#--------------------------#
#### Assignment HSTM 2B ####
#--------------------------#

#--------------#
#### Set up ####
#--------------#
rm(list = ls())
options(scipen = 999)
library("rstudioapi")

load("Basics/Instructions/HSTM_2/data_HSTM_2B.RData") # load data assignment

# Inspect mortality probabilities
#head(df_mort)
#tail(df_mort)

# Define start age cohort
n_start_age <- 45

# Create a vector containing the mortality probability values for the age `n_start_age` to `n_start_age`+9 and call it `v_p_mort`
v_p_mort <- df_mort[c(which(df_mort$age == n_start_age):
                        which(df_mort$age == n_start_age + 9)
                      ), "p_mort"]

# Initialise array's
a_tp_int <- a_tp_comp <- array(0, dim = c(n_hs, n_hs, n_cycles),
                               dimnames = list(v_names_hs, v_names_hs, 1:n_cycles)
                               )

# Fill in the arrays using the background mortality for each  
a_tp_comp["Well", "Well",]   <- 1 - v_p_mort - r_inc_mi - r_inc_stroke  # EXAMPLE! Calculate the remaining transition probabilities
a_tp_comp["Well", "Post-minor_stroke",] <- r_inc_stroke * (1 - r_fatal_stroke) * p_post_minor_stroke
a_tp_comp["Well", "Post-major_stroke",] <- r_inc_stroke * (1 - r_fatal_stroke) * p_post_major_stroke
a_tp_comp["Well", "Post-MI",] <- r_inc_mi * (1 - r_fatal_mi)
a_tp_comp["Well", "Death_stroke",] <- r_inc_stroke * r_fatal_stroke
a_tp_comp["Well", "Death_MI",] <- r_inc_mi * r_fatal_mi
a_tp_comp["Well", "Death_other",] <- v_p_mort

a_tp_comp["Post-minor_stroke", "Post-minor_stroke",] <- 1 - v_p_mort
a_tp_comp["Post-minor_stroke", "Death_other",] <- v_p_mort

a_tp_comp["Post-major_stroke", "Post-major_stroke",] <- 1 - v_p_mort
a_tp_comp["Post-major_stroke", "Death_other",] <- v_p_mort

a_tp_comp["Post-MI", "Post-MI",] <- 1 - v_p_mort
a_tp_comp["Post-MI", "Death_other",] <- v_p_mort

a_tp_comp["Death_stroke", "Death_stroke",] <- 1
a_tp_comp["Death_MI", "Death_MI",] <- 1
a_tp_comp["Death_other", "Death_other",] <- 1

# Inspect whether the sum of all TP's for all health states over all cycles = 1

m_check <- matrix(NA,
                  nrow = n_cycles,
                  ncol = n_hs)

for (i in 1:n_cycles){
  v_res <- round(rowSums(a_tp_comp[ , ,i]), 5) == 1
  m_check[i,] <- paste(v_res)
  
  }

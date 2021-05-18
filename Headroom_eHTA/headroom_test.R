##### Syntaxt to test headroom function
rm(list = ls())
source(paste(getwd(),"/Headroom_eHTA/","headroom.R", sep = "")) # load function

#######################
#### Test function ####
#######################

# does function work?
v_res <- calc_headroom_test(prev = 1950,
                   perc_pos = 0.2,
                   wtp = 80000,
                   e_uc = 3.5 * 0.62,
                   e_tn = 3.5 * 0.62,
                   e_fn = 3.5 * 0.62,
                   e_tp = 5.8 * 0.78,
                   e_fp = 1.6 * 0.53,
                   c_uc = 10000,
                   c_tn = 10000,
                   c_fn = 10000,
                   c_tp = 50000,
                   c_fp = 50000,
                   c_test = 5000, 
                   p_sens = 0.8,
                   p_spec = 0.85) # works!
v_res

# does function provide same results expected?
n_tp <- 80 
n_fp <- 15
n_tn <- 85
n_fn <- 20

prev <- 1950
perc_pos <- 0.20

c_uc   <- 10000
c_immuno <- 50000

c_test <- 5000

wtp <- 80000

e_uc <- 3.5 * 0.62
e_tn <- e_fn <- e_uc
e_tp <- 5.8 * 0.78
e_fp <- 1.6 * 0.53

p_sens <- n_tp / (n_tp + n_fn)
p_spec <- n_tn / (n_tn + n_fp)

n_tp_test <- prev * perc_pos * p_sens
n_fn_test <- prev * perc_pos * (1 - p_sens)
n_tn_test <- prev * (1 - perc_pos) * p_spec
n_fp_test <- prev * (1 - perc_pos) * (1 - p_spec)
#sum(n_tp_test, n_fn_test, n_tn_test, n_fp_test)

e_tot_uc <- e_uc #total effect per patient usual care
c_tot_uc <- c_uc #total costs per patient usual care

e_tot_test <- (n_tp_test * e_tp + n_fp_test * e_fp + n_tn_test * e_tn + n_fn_test * e_fn) / prev
c_tot_test <- (n_tp_test * c_immuno + n_fp_test * c_immuno + n_tn_test * c_uc + n_fn_test * c_uc) / prev 

e_gap <- e_tot_test - e_tot_uc
c_max <- e_gap * wtp
c_max_adjust <- c_max - (c_tot_test - c_tot_uc)
roi <- (c_max_adjust - c_test) * prev

# yes, provides same results as hand calculations
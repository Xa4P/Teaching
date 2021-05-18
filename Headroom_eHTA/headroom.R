rm(list = ls())

#############################################################
#### Function to caclulate headroom of a diagnostic test ####
#############################################################

calc_headroom_test <- function(prev, # prevalence disease
                               perc_pos, # percentage positives in the prevalent population
                               wtp, # applied willingness-to-pay threshold to outcome
                               e_uc, # effectiveness usual care (no testing)
                               e_tn, # effectiveness treatment in true negative (after testing)
                               e_fn, # effectiveness treatment in true negatives (after testing)
                               e_tp, # effectiveness treatment in true positives (after testing)
                               e_fp, # effectiveness treatment in false positives (after testing)
                               c_uc, # cost ususal care (no testing)
                               c_tn, # cost treatment in true negatives (after testing)
                               c_fn, # cost treatment in false negatives (after testing)
                               c_tp, # cost treatment in true positives (after testing)
                               c_fp, # cost treatment in false positives (after testing)
                               c_test, # cost of test 
                               p_sens, # sensitivity of test
                               p_spec # specificity of test
                               ) {
  options(scipen = 999) # remove scientific notation
  
  v_res <- c('Effectiveness gap' = NA,
             'Max price' = NA, 
             'Max price (considering treatment)' = NA,
             'Return on investment' = NA,
             'Return on investment (considering treatment)' = NA) # prepare vector of results
  
  # calculate numbers of true and false positives, true and false negatives
  n_tp_test <- prev * perc_pos * p_sens
  n_fn_test <- prev * perc_pos * (1 - p_sens)
  n_tn_test <- prev * (1 - perc_pos) * p_spec
  n_fp_test <- prev * (1 - perc_pos) * (1 - p_spec)

  e_tot_uc <- e_uc # total effect per patient usual care
  c_tot_uc <- c_uc # total costs per patient usual care
  
  e_tot_test <- (n_tp_test * e_tp + n_fp_test * e_fp + n_tn_test * e_tn + n_fn_test * e_fn) / prev # total effect per patient new test strategy
  c_tot_test <- (n_tp_test * c_tp + n_fp_test * c_fp + n_tn_test * c_tn + n_fn_test * c_fn) / prev # total cost per patient new test strategy 
  
  e_gap <- e_tot_test - e_tot_uc # calculate effctiveness gap
  
  c_max <- e_gap * wtp # calculate maximum cost test
  c_max_adjust <- c_max - (c_tot_test - c_tot_uc) # calculate maximum cost test, taking treatment costs into account
  
  roi <- c_max * prev # calculate return on investment test
  roi_adjust <- (c_max_adjust - c_test) * prev # calculate return on investment test, taking treatment costs into account

  v_res[1:5] <- round(c(e_gap, c_max, c_max_adjust, roi, roi_adjust), 3) # store results
    
  return(v_res)
}

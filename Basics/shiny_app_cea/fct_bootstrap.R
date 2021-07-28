bootstrap_effects_costs <- function(df, num_it = 5000) {
  
  m_result <- matrix(NA, nrow = num_it, ncol = 6, 
                     dimnames = list(c(1:num_it),
                                     c("Qol_Open", "Qol_Robot",
                                       "Cost_Open", "Cost_Robot",
                                       "Inc_Qol", "Inc_Cost")
                     )
  )
  
  progress_bar <- txtProgressBar(min = 0, max = num_it, style = 3)
  
  for(i in 1:num_it){
    n_pt <- nrow(df[which(df$Procedure == 1),]) # number of patients in each group
    v_id <- sample(x = 1:n_pt, size = n_pt, replace = TRUE)# vector of patient to select to compute
    
    # Select participants in each group
    df_open <- subset(df, df$Procedure == 1)
    df_open <- df_open[v_id,]
    df_robot <- subset(df, df$Procedure == 2)
    df_robot <- df_robot[v_id,]
    
    df_boot <- rbind(df_open, df_robot)
    
    df_res <- df_boot %>%
      group_by(Procedure) %>%
      summarise(Effect = mean(Qol),
                Cost = mean(Total_costs)
      )
    inc_effect <- df_res$Effect[2] - df_res$Effect[1]
    inc_cost <- df_res$Cost[2] - df_res$Cost[1]
    
    m_result[i,] <- c(df_res$Effect[1], df_res$Effect[2], df_res$Cost[1], df_res$Cost[2], inc_effect, inc_cost)
    
    # update progress bar
    setTxtProgressBar(progress_bar, i)
    
  }
  
  close(progress_bar)
  
  df_result <- as.data.frame(m_result)
  return(df_result)
  
}
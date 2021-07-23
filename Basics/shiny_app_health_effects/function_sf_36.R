#### Function SF-36

calculate_sf_36 <- function(v_sf_36) {
  v_sf_36_recoded <- c(5 - v_sf_36[1],
                       5 - v_sf_36[2],
                       v_sf_36[3:19] - 1,
                       5 - v_sf_36[20],
                       6 - v_sf_36[21],
                       5 - v_sf_36[22],
                       6 - v_sf_36[23],
                       v_sf_36[24:25] - 1,
                       6 - v_sf_36[26:27],
                       v_sf_36[28:29] - 1,
                       6 - v_sf_36[30],
                       v_sf_36[31:33] - 1,
                       5 - v_sf_36[34],
                       v_sf_36[35] - 1,
                       5 - v_sf_36[36]
  )
  v_sf_36_transform <- c(v_sf_36_recoded[1] * 25,
                         v_sf_36_recoded[2] * 25,
                         v_sf_36_recoded[3:12] * 50,
                         v_sf_36_recoded[13:19] * 100,
                         v_sf_36_recoded[20] * 25,
                         v_sf_36_recoded[21] * 20,
                         v_sf_36_recoded[22] * 25,
                         v_sf_36_recoded[23:31] * 20,
                         v_sf_36_recoded[32:36] * 25
                         )
  
  df_plot <- data.frame(cbind(Group = c("Respondent", "Migraine", "Control"),
                              PF = c(sum(v_sf_36_transform[3:12])/10, 84.8, 86.1),
                              RP = c(sum(v_sf_36_transform[13:16])/4, 63.3, 76.6),
                              RE = c(sum(v_sf_36_transform[17:19])/3, 75.1, 81.2),
                              VT = c(sum(v_sf_36_transform[c(23, 27, 29, 31)])/4, 61.5, 68),
                              MH = c(sum(v_sf_36_transform[c(24, 25, 26, 28, 30)])/5, 72.2, 76.8),
                              SF = c(sum(v_sf_36_transform[c(20, 32)])/2, 76.6, 84.7),
                              BP = c(sum(v_sf_36_transform[c(21, 22)])/2, 69.4, 81.6),
                              GH = c(sum(v_sf_36_transform[c(1, 33, 34, 35, 36)])/5, 66.4, 72)
                              )
  )
  
  
  df_plot <- df_plot %>% 
    pivot_longer(c(PF, RP, RE, VT, MH, SF, BP, GH), names_to = "Domain", values_to = "Score")
  
  df_plot$Score <- as.numeric(as.character(df_plot$Score))
  
  return(df_plot)
  
}

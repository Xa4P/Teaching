#########################################
#### CREATE DATA SET TRIAL-BASED CEA ####
#########################################

# Set up
rm(list = ls())
set.seed(400)

# Create data set
df <- data.frame(cbind(
  ID         = rep(1:100, 2),
  Procedure  = c(rep(1, 100), rep(2, 100)),
  OR_time    = c(rnorm(100, 140, 10), rnorm(100, 125, 15)),
  Major_compl = c(ifelse(runif(100) <= 0.25, 1, 0), ifelse(runif(100) <= 0.15, 1, 0)),
  Minor_compl = c(ifelse(runif(100) <= 0.35, 1, 0), ifelse(runif(100) <= 0.5, 1, 0)),
  Length_icu = c(ifelse(runif(100) <= 0.7, 0, rpois(100, 5)), ifelse(runif(100) <= 0.7, 0, rpois(100, 4.5))),
  Length_mcu = c(rpois(100, 14), rpois(100, 16)),
  Death      = c(ifelse(runif(100) <= 0.15, 1, 0), ifelse(runif(100) <= 0.10, 1, 0)),
  Qol = c(rbeta(100, 0.5, 0.5), rbeta(100, 0.6, 0.5))
)
)
df$Qol <- ifelse(df$Death == 1, 0, df$Qol)

# Declare inputs of the analysis
c_Robot <- 3500 
c_Open <- 2000 
c_OK_hour <- 500 
c_Major_Compl <- 10000 
c_Minor_Compl <- 1500 
c_ICU_day <- 1250 
c_MCU_day <- 550 


df_boot <- df %>% 
  group_by(Procedure) %>%
  group_split()



bootstrap_effects_costs <- function(df, num_it = 5000) {
  
  m_result <- matrix(NA, nrow = num_it, ncol = 6, 
                     dimnames = list(c(1:num_it),
                     c("Qol_Open", "Qol_Robot",
                     "Cost_Open", "Cost_Robot",
                     "Inc_Qol", "Inc_Cost")
                     )
  )
  
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
  
  }
  df_result <- as.data.frame(m_result)
  return(df_result)
  
}
set.seed(200)
bootstrap_output <- bootstrap_effects_costs(df = df, num_it = 10)

# Save
save(list = ls(), file = "Basics/Instructions/Trial_based_CEA/trial_based_CEA.RData")
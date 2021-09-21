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
  OR_time    = c(rnorm(100, 140, 10), rnorm(100, 120, 20)),
  Major_compl = c(ifelse(runif(100) <= 0.25, 1, 0), ifelse(runif(100) <= 0.12, 1, 0)),
  Minor_compl = c(ifelse(runif(100) <= 0.35, 1, 0), ifelse(runif(100) <= 0.5, 1, 0)),
  Length_icu = c(ifelse(runif(100) <= 0.7, 0, rpois(100, 5)), ifelse(runif(100) <= 0.7, 0, rpois(100, 4))),
  Length_mcu = c(rpois(100, 14), rpois(100, 15)),
  Death      = c(ifelse(runif(100) <= 0.15, 1, 0), ifelse(runif(100) <= 0.10, 1, 0)),
  Qol = c(rbeta(100, 0.5, 0.5), rbeta(100, 0.6, 0.5))
)
)
df$Qol <- ifelse(df$Death == 1, 0, df$Qol)

# Declare inputs of the analysis
c_Robot <- 3000 
c_Open <- 2000 
c_OK_hour <- 500 
c_Major_Compl <- 10000 
c_Minor_Compl <- 1500 
c_ICU_day <- 1250 
c_MCU_day <- 550 

# Save
save(list = ls(), file = "C:/Users/PouwelsXGLV/Documents/Onderwijs/Teaching/Basics/Instructions/Trial_based_CEA/trial_based_CEA.RData")
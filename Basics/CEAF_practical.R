########################
#### Practical CEAF ####
########################

######################
##### Create data ####
######################
rm(list  = ls())

df_thx <- as.data.frame(cbind(
  Treatment = seq(from  = 0, to = 8, by = 1),
  Name = c("No screening (REF)",  
           "Screening once, at age 40",
           "Screening once, at age 60",  
           "Screening once, at age 80",
           "Screening twice, at age 40 and 60",  
           "Screening every 10 years from age 40 to 80",
           "Screening every 10 years from age 50 to 80",
           "Screening every 5 years from age 40 to 80",
           "Screening every 5 years from age 50 to 80"),
  Costs = c(0, 23500, 12500, 14000, 36000, 67500, 57500, 113000,  88000),
  QALYs = c(0.0, 1.1, 0.9, -0.5, 1.9, 3.3, 3.2, 4.1, 4.0)
  ))

saveRDS(df_thx, file = "Basics/data_CEAF.rds")


##################
#### Exercise ####
##################

rm(list  = ls()) # clear environment
options(scipen = 999) # remove scientific notation

df_thx <- readRDS("Basics/data_CEAF.rds")# Load data

# Question 1: create plot
df_thx <- df_thx[order(df_thx$QALYs),] # order df based on costs
plot(x = df_thx$QALYs, y = df_thx$Costs, xlab = "QALYs", ylab = "Costs") # plot
abline(v = 0, lty = 2)
abline(h = 0, lty = 2)

# Question 2: determine domination
df_thx$Dominated <- sapply(c(1:9), function(x) if(x != 9){
  ifelse(df_thx[x, "Costs"] > df_thx[x + 1, "Costs"], 1, 0)
  } else {
    0
  })
sum(df_thx$Dominated) # 2 strategies dominated
df_thx[which(df_thx$Dominated == 1), c("Treatment", "Name")] # overview dominated treatment

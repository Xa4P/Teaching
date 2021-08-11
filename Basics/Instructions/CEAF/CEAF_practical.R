
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

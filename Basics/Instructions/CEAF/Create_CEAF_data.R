########################
#### Practical CEAF ####
########################

#--------------------#
##### Create data ####
#--------------------#
rm(list  = ls())

df_thx <- as.data.frame(cbind(
  Treatment = seq(from  = 1, to = 9, by = 1),
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

df_thx[, c("Costs", "QALYs")] <- apply(df_thx[, c("Costs", "QALYs")], 2, function(x) as.numeric(as.character(x)))
df_thx[, "Treatment"] <- as.factor(as.character(df_thx[, "Treatment"]))

saveRDS(df_thx, file = "Basics/CEAF/data_CEAF.rds")
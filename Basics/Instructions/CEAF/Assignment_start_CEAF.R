########################
#### Practical CEAF ####
########################

#--------------#
#### Set up ####
#--------------#
rm(list  = ls()) # clear environment
options(scipen = 999) # remove scientific notation
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path)) 

df_thx <- readRDS("data_CEAF.rds") # Load data, CHANGE PATH ACCORDINGLY IF THE DATA DOES NOT LOAD AUTOMATICALLY!

#-------------------#
#### Assignments ####
#-------------------#

# 1. Look at the cost-effectiveness plane for the outcomes of strategies 1-9  
ggplot(data = df_thx, aes(x = QALYs, Costs, colour = Treatment)) +
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  ggtitle("Cost-effectiveness plane") +
  theme_bw()


# 2. Calculate the fully incremental ICERs of these screening strategies against each other  

# 3. Which interventions are dominated?   

## Order by increasing number of QALYs

## Identify dominated strategies

# 4. Which interventions are extendedly dominated?

## Calculate ICERs for non-dominated strategies.

## Calculate fully incremental ICERs

## Determine extended dominance

## Repeat these steps if necessary

# 5. Which strategies are on the cost-effectiveness acceptability frontier?i.e. not dominated and not extendedly dominated.

# 6. Which intervention is optimal if the WTP threshold is equal to €20,000/QALY?    

# 7. Which intervention is optimal if the WTP threshold is equal to €40,000/QALY?   

# 8. Which intervention is optimal if the WTP threshold is equal to €100,000/QALY?   
  
# 9. At which WTP threshold would intervention 5 be the optimal intervention?   

# 10. At which WTP threshold would intervention 8 be the optimal intervention?    

# 11. Calculate the Net Monetary Benefit (NMB) for each intervention for a WTP threshold of €20,000/QALY. Which intervention has the highest NMB? Does this correspond with your answer to question 6?    
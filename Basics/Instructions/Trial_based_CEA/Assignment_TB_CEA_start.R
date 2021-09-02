####################################
#### Assignment Trial-based CEA ####
####################################


#--------------#
#### Set up ####
#--------------#

rm(list  = ls()) # clear environment
options(scipen = 999) # Disable scientific notations

#install.packages("tidyverse") # if needed
#install.packages("knitr") # if needed
#install.packages("boot") # if needed

library(tidyverse)
library(knitr)
library(boot)

load(file = "trial_based_CEA.RData")

#-------------------#
#### Assignments ####
#-------------------#

# 1. Calculate the mean operating time, risk of complications, length of stay at ICU and MCU, risk of death, and quality of life after procedure, for both trial arms.
res_health

# 2. Create three new variables in `df` called Cost_procedure, Cost_compl, Cost_hosp, which respectively contain the costs related to the procedure (operatime time + procedure), the costs related to complications, and the costs related to hospitalization for each participant. To do so, use the objects beginning with `c_`.
df$Cost_procedure
df$Cost_compl
df$Cost_hosp

# 3. Create a new variable in `df` called Total_costs, which contains the total costs per participant. Calculate the mean costs per group.
df$Total_costs

# 4. Calculate the difference in mean quality of life and mean total costs between the new intervention and usual care (i.e. the incremental effects and incremental costs). Calculate the ICER using this information.
Inc_qol # difference in mean quality of life between groups
Inc_costs # difference in mean costs between groups
ICER

# 5. Save your dataframe `df` as a .csv file. Ensure that your total effects per participants are in the `Qol` variable and the total costs in the `Total_costs` variable. 
write.csv(df, "df_trial_cea.csv")

# 6. Open the R shiny app using the following command in R and follow the instructions.   
runGitHub("Teaching", "Xa4P", subdir = "Basics/shiny_app_cea/", ref = "main")
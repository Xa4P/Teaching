#################################
#### DECISION TREE PRACTICAL ####
#################################

# Set up
rm(list = ls()) # clear environment
options(scipen = 999) # disable scientific notation

#---------------------------------#
#### Step 1: Define parameters ####
#---------------------------------#

# Probabilities
p_Rupture <- 0.3
p_Stable <- 0
p_DeathRupture <- 0.4
p_DisabledRupture <- 0.35
p_SurvivalRupture <- 0.25
p_Anxious <- 0.6
p_NotAnxious <- 0.4
p_Clipping <- 0.3
p_Coiling <- 0.7
p_DeathClipping <- 0
p_DisabledClipping <- 0.25
p_SurvivalClipping <- 0.65
p_DeathCoiling <- 0.05
p_DisabledCoiling <- 0
p_SurvivalCoiling <- 0.8

# Utility values
u_Healthy  <- 0.80 # Utility of patient in good health
u_Disabled <- 0.40 # Utility of disabled patient
u_Anxious  <- 0.70 # Utility of anxious patient
u_Death    <- 0    # Utility of dying

# Costs
c_Rupture   <- 50000 # Cost of aneurysm rupture
c_Clipping	<- 20000 # Cost of aneurysm clipping
c_Coiling   <- 13000 # Cost of aneurysm coiling
c_Disabled  <- 5000  # Cost of disabled patient

#---------------------------------------------------#
#### Step 2: Calculate probabilities of pathways ####
#---------------------------------------------------#


## Calculate pStable, pDeathClipping and pDisabledCoiling, since these are set to 0 for now...  
## Use the decision tree diagram and the above-defined probabilities

p_Stable <- 1 - p_Rupture 
p_DeathClipping  <- 1 - p_DisabledClipping - p_SurvivalClipping
p_DisabledCoiling <- 1- p_DeathCoiling - p_SurvivalCoiling

## Calculate the probabilities of each pathway
## Use the above-define probabilities

p_path_A <- p_Rupture * p_DeathRupture
p_path_B <- p_Rupture * p_DisabledRupture
p_path_C <- p_Rupture * p_SurvivalRupture
p_path_D <- p_Stable * p_Anxious
p_path_E <- p_Stable * p_NotAnxious
p_path_F <- p_Clipping * p_DeathClipping
p_path_G <- p_Clipping * p_DisabledClipping
p_path_H <- p_Clipping * p_SurvivalClipping
p_path_I <- p_Coiling * p_DeathCoiling
p_path_J <- p_Coiling * p_DisabledCoiling
p_path_K <- p_Coiling * p_SurvivalCoiling

#----------------------------------------------------#
#### Step 3: Assign effects and costs to pathways ####
#----------------------------------------------------#

# These are the effects and costs that a person would incur if he/she follows a specific pathway!

## Assign the effects of each pathway
## Use the above-define utility parameters

e_path_A <- u_Death
e_path_B <- u_Disabled 
e_path_C <- u_Healthy
e_path_D <- u_Anxious
e_path_E <- u_Healthy
e_path_F <- u_Death
e_path_G <- u_Disabled
e_path_H <- u_Healthy
e_path_I <- u_Death
e_path_J <- u_Disabled
e_path_K <- u_Healthy

## Calculate the costs of each pathway
## Use the above-define utility parameters

c_path_A <- c_Rupture
c_path_B <- c_Rupture + c_Disabled
c_path_C <- c_Rupture
c_path_D <- 0
c_path_E <- 0
c_path_F <- c_Clipping
c_path_G <- c_Clipping + c_Disabled
c_path_H <- c_Clipping
c_path_I <- c_Coiling
c_path_J <- c_Coiling + c_Disabled
c_path_K <- c_Coiling

#----------------------------------------------------------------#
#### Step 4: Calculate expected effects and costs of pathways ####
#----------------------------------------------------------------#

# These are the effects and costs that a person would incur if he/she follows a specific pathway, weighted by the probability of occurence of the specific pathway


## Calculate the expected effects of each pathway
## Use the above-defined utility and probabilities of each pathway

te_path_A <- p_path_A * e_path_A
te_path_B <- p_path_B * e_path_B
te_path_C <- p_path_C * e_path_C
te_path_D <- p_path_D * e_path_D
te_path_E <- p_path_E * e_path_E
te_path_F <- p_path_F * e_path_F
te_path_G <- p_path_G * e_path_G
te_path_H <- p_path_H * e_path_H
te_path_I <- p_path_I * e_path_I
te_path_J <- p_path_J * e_path_J
te_path_K <- p_path_K * e_path_K

## Calculate the expected costs of each pathway
## Use the above-defined costs and probabilities of each pathway

tc_path_A <- p_path_A * c_path_A
tc_path_B <- p_path_B * c_path_B
tc_path_C <- p_path_C * c_path_C
tc_path_D <- p_path_D * c_path_D
tc_path_E <- p_path_E * c_path_E
tc_path_F <- p_path_F * c_path_F
tc_path_G <- p_path_G * c_path_G
tc_path_H <- p_path_H * c_path_H
tc_path_I <- p_path_I * c_path_I
tc_path_J <- p_path_J * c_path_J
tc_path_K <- p_path_K * c_path_K

#---------------------------------------------------------------------#
#### Step 5: Calculate expected effects and costs of each strategy ####
#---------------------------------------------------------------------#

## Calculate the expected effects and costs of each strategy
## Sum the expected costs and effects of the pathways that belong to each strategy

te_ww  <- sum(te_path_A, te_path_B, te_path_C, te_path_D, te_path_E) # expected effects watchful waiting
tc_ww  <- sum(tc_path_A, tc_path_B, tc_path_C, tc_path_D, tc_path_E) # expected costs watchful waiting

te_trt <- sum(te_path_F, te_path_G, te_path_H, te_path_I, te_path_J, te_path_K) # expected effects treatment strategy
tc_trt <- sum(tc_path_F, tc_path_G, tc_path_H, tc_path_I, tc_path_J, tc_path_K) # expected costs treatment strategy

#------------------------------------------------------------------------------------------------------------------------#
#### Step 6: Calculate incremental effects, costs, and incremental cost-effectiveness ratio of the treatment strategy ####
#------------------------------------------------------------------------------------------------------------------------#

## Calculate the incremental effects and costs of each the treatment strategy versus watchful waiting
## Calculate the incremental cost-effectiveness ratio
## Use the total effects and costs defined in previous step

inc_e_trt <- te_trt - te_ww
inc_c_trt <- tc_trt - tc_ww
icer <- inc_c_trt / inc_e_trt
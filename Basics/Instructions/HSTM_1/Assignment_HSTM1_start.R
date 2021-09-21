#################################
#### DECISION TREE PRACTICAL ####
#################################

# Set up
rm(list = ls()) # clear environment
options(scipen = 999) # disable scientific notation

#----------------------------#
#### 0: Define parameters ####
#----------------------------#

# Probabilities
p_NewAneurysm       <- 	0.10 # Probability that an individual develops a new intracranial aneurysm 
p_AneurysmDetection <- 	0.50 # Probability that a newly developed aneurysm is detected
p_TreatmentIsFatal  <- 	0.20 # Probability that treatment of a detected aneurysm induces a fatal complication
p_DeathOther        <- 	0.10 # Probability of death due to causes unrelated to aneurysms

#----------------------------------------------------------#
#### 2: Answer the questions and fill transition matrix ####
#----------------------------------------------------------#

#1. Use the model parameters (the `p_[NAME] parameters`) defined in the `Assignment.R` and the model structure to calculate the following probabilities/quantities (subquestions a to j). 
##a. The probability than an individual dies from aneurysm treatment in month 2

##b. The probability that an individual starts in the Healthy state and remains there, that is, survives and does not develop an aneurysm, in 1 year

##c. The probability that an individual develops 2 aneurysms and is treated successfully twice, in 6 months’ time

##d. The probability that an individual dies of other causes within the first month

##e. The probability that an individual dies of other causes within the first 2 months

##f. The probability that an individual with a new and untreated aneurysm (that remains untreated) develops an additional, new aneurysm, in 2 years’ time

##g. The excess mortality risk (expressed as relative risk) of individuals with successfully treated aneurysms compared to healthy individuals

##h. The risk of death from a car accident for an individual with a detected unruptured aneurysm

##i. [OPTIONAL] The sensitivity of the (unknown) test used to detect aneurysms after 4 months/repetitions

##j. [OPTIONAL] The probability that an individual with a new aneurysm dies before it is detected (you can give an approximation, or the exact value)

# Create a matrix (called `m_tp`) containing 5 rows and 5 columns (using the matrix() function)
# Assign the value 0 to all cells of the matrix to start with
# Name the rows and column (argument dimnames of the function): "Healthy", "NewAneurysm",	"DetectedAneurysm", "DeathOther",	"DeathTreatment"
m_tp <- matrix(0, 
               ncol = ,
               nrow = ,
               dimnames = )

# Fill the matrix with the values of the transition probabilities
# The rows indicate the health state from which persons transit
# The columns indicate the health state to which persons transit
## You can use the names of the rows and columns to assign the transition probabilities to each cell.
## Do not forget to define the probability of remaining in a health state!
m_tp["Healthy", "Healthy"]     <- 1- p_NewAneurysm - p_DeathOther # Example


# Check whether all rows sum up to one!
## Using the rowSums() function
rowSums(m_tp) == 1 # should be all true

#-----------------------------------------#
#### 3: Transition matrix in Shiny app ####
#-----------------------------------------#

# The following assignments should be performed using the R shiny app.
# Use the following command to open it and follow the instructions
runGitHub("Teaching", "Xa4P", subdir = "Basics/shiny_app_cea/", ref = "main")

#-----------------------------------------#
#### 4: Building the cohort simulation ####
#-----------------------------------------#

# This part of the assignment focuses on modelling the disease progression of healthy individuals
# using the matrix (m_tp) you just defined
# Source: https://arxiv-org.ezproxy2.utwente.nl/abs/2001.07824

## 4.a. (Re)define `m_tp` as in the previous assignment
## Using the transition probabiities
# Probabilities
p_NewAneurysm  	    <- 0.10 # Probability that an individual develops a new intracranial aneurysm 
p_AneurysmDetection <- 0.50 # Probability that a newly developed aneurysm is detected
p_TreatmentIsFatal  <- 0.20 # Probability that treatment of a detected aneurysm induces a fatal complication
p_DeathOther  	    <- 0.10 # Probability of death due to causes unrelated to aneurysms

m_tp <- matrix(0, 
               ncol = ,
               nrow = ,
               dimnames = 
                 )

# Fill the matrix with the values of the transition probabilities
# The rows indicate the health state from which persons transit
# The columns indicate the health state to which persons transit
## You can use the names of the rows and columns to assign the transition probabilities to each cell.
## Do not forget to define the probability of remaining in a health state!
m_tp["Healthy", "Healthy"]     <- 1- p_NewAneurysm - p_DeathOther # Example

## 4.b. To evaluate the progression of individuals over multiple cycle,
## we need to define:
n_cycles # the number of cycles to simulate, assume 3 years (monthly cycles)
n_pt  # the size of the cohort, assume 10000 persons
m_hs <- matrix(0, 
               nrow = ,
               ncol = ,
               dimnames = 
                 ) # a cohort state matrix, containing [n_cycles + 1] rows (because the first row is the start position), and as much column as the number of health states.
## fill this matrix with 0's for now

## We then need to define the starting positions of the cohort
## Assign all individuals to the "Healthy" health state in the first row of the `m_hs` matrix
v_start_hs
m_hs[1, ] <- v_start_hs

## Perform the matrix multiplication to determine the state membership over the cycles.
## To determine the number of individuals in each health state during each cycle, one need to multiply the vector of state membership in the previous cycle by the transition matrix
## Example: to calculate the number of individuals in each state in cycle 1, multiply the state membership in cycle 0 by the transition matrix
## HINT: to do so, use a a for loop over rows 2 to 41 of the `m_hs` matrix

for(cycle in 1:n_cycles){
  
## FILL IN THE CORRECT MATRIX MULTIPLICATION ##

  }

## Inspect the first row of the cohort simulation, using the head() function
head(m_hs)

## Inspect whether the sum of all rows equal 10000 (thus if you do not 'loose' or add any individuals in your simulation)
which(round(rowSums(m_hs), 10) != n_pt) # check rowsums  = 10000  --> SHOULD ALL BE 1000 

## 4.c.	Use your cohort simulation to calculate the number of individuals being alive after exactly 3 years. 
## Remember that individuals only remain alive if they are in one of the three health states “Healthy” , “ NewAneurysm”  and “ Detected Aneurysm”.

## 4.d. Use your cohort simulation to calculate the cumulative number of person-months that is spent in the health states “Healthy” , “ NewAneurysm”  and “ Detected Aneurysm”.  
## Assume that we count state membership at the beginning of the cycle  
## HINT: Use the cumsum() function

## 4.e. Can you determine the average number of months that individuals in this model are alive, over the 3 year time horizon? Hint: you know the cohort size (10,000).

#n_months_Healthy_3years <- cumsum(m_hs[2:37, "Healthy"])[36] # when counting health state membership at the end of the cycle
#n_months_NewAneurysm_3years <- cumsum(m_hs[2:37, "NewAneurysm"])[36]
#n_months_DetectedAneurysm_3years <- cumsum(m_hs[2:37, "DetectedAneurysm"])[36]

## 4.f.	If you decrease the value of the parameter “pDeathOther” by 50%, 
## that is, enter the value 0.05 for parameter “pDeathOther”
## and increase the value of the parameter “pTreatmentIsFatal” by 50%, 
## that is, enter the value 0.40 for parameter “pTreatmentIsFatal”,
## How do you think these changes will affect the average number of months that individuals in this model are alive? 
### HINT: To do so, clone the transition matrix, modify these two probabilities, and reiterate the cohort simulation

# Clone transition matrix
m_tp_2

# Re-define transition probabilities
p_NewAneurysm  	     # Probability that an individual develops a new intracranial aneurysm 
p_AneurysmDetection  # Probability that a newly developed aneurysm is detected
p_TreatmentIsFatal   # Probability that treatment of a detected aneurysm induces a fatal complication
p_DeathOther  	     # Probability of death due to causes unrelated to aneurysms

# Re-define transitions in m_tp_2
m_tp_2["Healthy", "Healthy"]     <- 1- p_NewAneurysm - p_DeathOther # Example

# Re-define matrix of health states
m_hs_2 <- matrix(0, 
                 nrow = ,
                 ncol = ,
                 dimnames =
                   )
v_start_has # define start position
m_hs_2[1, ] <- v_start_hs # Define state membership start

# matrix multiplication to fill cohort simulation
for(cycle in 1:n_cycles){
  
  ## FILL IN THE CORRECT MATRIX MULTIPLICATION ##
}

## Inspect the first row of the cohort simulation, using the head() function
head(m_hs_2)

## Inspect whether the sum of all rows equal 10000 (thus if you do not 'loose' or add any individuals in your simulation)
which(round(rowSums(m_hs_2), 10) != n_pt) # check rowsums  -> should all be 10,000!

## Re-calculate the cummulative number of individuals


## 4.g.	First reset the parameters pDeathOther to value 0.1 and pTreatmentIsFatal to value 0.2. 
## What do you think will happen if we increase the sensitivity of the test to detect new aneurysms, from looking at the model structure? 
## Change the value of the parameter “p_AneurysmDetection” from 0.50 to 0.80 and determine again the average number of months that individuals in this model are alive, after 3 years. Can you explain the result? 
### HINT: To do so, clone the transition matrix, modify these two probabilities, and reiterate the cohort simulation

# Clone transition matrix
m_tp_3

# Re-define transition probabilities
p_NewAneurysm  	     # Probability that an individual develops a new intracranial aneurysm 
p_AneurysmDetection  # Probability that a newly developed aneurysm is detected
p_TreatmentIsFatal   # Probability that treatment of a detected aneurysm induces a fatal complication
p_DeathOther  	     # Probability of death due to causes unrelated to aneurysms

# Re-define transitions in m_tp_3
m_tp_3["Healthy", "Healthy"]     <- 1- p_NewAneurysm - p_DeathOther # Example


# Re-define matrix of health states
m_hs_3 <- matrix(0, 
                 nrow = ,
                 ncol = ,
                 dimnames = 
                   )
v_start_hs # define start position
m_hs_3[1, ] <- v_start_hs # Define state membership start

# matrix multiplication to fill cohort simulation
for(cycle in 1:n_cycles){
  
  ## FILL IN THE CORRECT MATRIX MULTIPLICATION ##
  
}

## Inspect the first row of the cohort simulation, using the head() function
head(m_hs_3)

## Inspect whether the sum of all rows equal 10000 (thus if you do not 'loose' or add any individuals in your simulation)
which(round(rowSums(m_hs_3), 10) != 10000) # check rowsums  -> should all be 10,000

## Re-calculate the cummulative number of individuals



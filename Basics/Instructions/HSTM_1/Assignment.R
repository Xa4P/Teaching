##########################
#### HSTM_1 PRACTICAL ####
##########################

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
0 
## Explanation: It is not possible to reach the DeathTreatment state in 2 cycles

##b. The probability that an individual starts in the Healthy state and remains there, that is, survives and does not develop an aneurysm, in 1 year
(1 - p_NewAneurysm - p_DeathOther)^12
## Explanation: Survival without new aneurysm has probability 0.8 per cycle

##c. The probability that an individual develops 2 aneurysms and is treated successfully twice, in 6 months’ time
(p_NewAneurysm * p_AneurysmDetection * (1 - p_TreatmentIsFatal)) ^ 2
## OR: (p_NewAneurysm * p_AneurysmDetection * (1 - p_TreatmentIsFatal)) * (p_NewAneurysm * p_AneurysmDetection * (1 - p_TreatmentIsFatal))
## Explanation: probability of new anerism x probability it is detected * probability it is sucessfully treated
## This has to be exponentiated by because this process should occur twice

##d. The probability that an individual dies of other causes within the first month
p_DeathOther
## Explanantion: Just the probability of dying immediately in cycle 1

##e. The probability that an individual dies of other causes within the first 2 months
p_DeathOther + (1 - p_DeathOther - p_NewAneurysm) * p_DeathOther + p_NewAneurysm * p_DeathOther
## Explanation: Death from Healthy in cycle 1 or 2 plus death from NewAneurysm in cycle 2

##f. The probability that an individual with a new and untreated aneurysm (that remains untreated) develops an additional, new aneurysm, in 2 years’ time
0 
## Explanantion: In this model it is not possible to have more than 1 untreated aneurysm

##g. The excess mortality risk (expressed as relative risk) of individuals with successfully treated aneurysms compared to healthy individuals
1
## Explanation: Given survival of treatment all treated individuals will return to the Healthy state in the cycle following treatment
## Explanation: This Markov model has no 'memory': all individuals in a health state are similar, regardless of their history

##h. The risk of death from a car accident for an individual with a detected unruptured aneurysm
0
## Explanation: Due to the severity of aneurysm treatment death from other causes is ignored in the cycle patients are treated

##i. [OPTIONAL] The sensitivity of the (unknown) test used to detect aneurysms after 4 months/repetitions
p_AneurysmDetection + p_AneurysmDetection * (1 - p_AneurysmDetection - p_DeathOther) +
  p_AneurysmDetection * (1 - p_AneurysmDetection - p_DeathOther) ^ 2 +
  p_AneurysmDetection * (1 - p_AneurysmDetection - p_DeathOther) ^ 3
## Explanation: Every cycle there is 50% of detection in individuals still alive and without previous detection
## Explanation: Hence, for each cycle, the probability of staying in the 'Detected intracranial aneurysm health state is calculated (1 - p_AneurysmDetection - p_DeathOther) and is multiplied by the probability of detecting the aneurysm (0.5)

##j. [OPTIONAL] The probability that an individual with a new aneurysm dies before it is detected (you can give an approximation, or the exact value)
sum((p_DeathOther) * (1 - p_DeathOther - p_AneurysmDetection) ^ c(0:1)) # 2 cycles
sum((p_DeathOther) * (1 - p_DeathOther - p_AneurysmDetection) ^ c(0:2)) # 3 cycles
sum((p_DeathOther) * (1 - p_DeathOther - p_AneurysmDetection) ^ c(0:3)) # 4 cycles

sum((p_DeathOther) * (1 - p_DeathOther - p_AneurysmDetection) ^ c(0:1000)) # limit, the number 1000 can be increased if needed
plot(y = cumsum((p_DeathOther) * (1 - p_DeathOther - p_AneurysmDetection) ^ c(0:1000)), 
     x = log(c(0:1000)),
     type = "l") # graphic of the solution (x on the log scale)

## Explanation: The approximation for 2,3,4 cycles is 0.140, 0.156, 0.162 respectively, the true value (limit) equals 1/6 = 0.166
## Explanation: See http://www.mathsisfun.com/algebra/sequences-sums-geometric.html for a more extensive explanation
## Explanation: Here we have a=1/10 and r=2/5 so the sum of our geometric series converges to a*(1/(1-r)) = 1/10*(1/(1-2/5)) = 1/6


# Create a matrix (called `m_tp`) containing 5 rows and 5 columns (using the matrix() function)
# Assign the value 0 to all cells of the matrix to start with
# Name the rows and column (argument dimnames of the function): "Healthy", "NewAneurysm",	"DetectedAneurysm", "DeathOther",	"DeathTreatment"
v_names_hs <- c("Healthy", "NewAneurysm",	"DetectedAneurysm", "DeathOther",	"DeathTreatment")
m_tp <- matrix(0, 
               ncol = length(v_names_hs),
               nrow = length(v_names_hs),
               dimnames = list(v_names_hs,
                               v_names_hs))

# Fill the matrix with the values of the transition probabilities
# The rows indicate the health state from which persons transit
# The columns indicate the health state to which persons transit
## You can use the names of the rows and columns to assign the transition probabilities to each cell.
## Do not forget to define the probability of remaining in a health state!
m_tp["Healthy", "Healthy"]     <- 1- p_NewAneurysm - p_DeathOther # Example
m_tp["Healthy", "DeathOther"]  <- p_DeathOther
m_tp["Healthy", "NewAneurysm"] <- p_NewAneurysm
m_tp["NewAneurysm", "DetectedAneurysm"] <- p_AneurysmDetection
m_tp["NewAneurysm", "DeathOther"] <- p_DeathOther
m_tp["NewAneurysm", "NewAneurysm"] <- 1 - p_AneurysmDetection - p_DeathOther
m_tp["DetectedAneurysm", "Healthy"] <- 1 - p_TreatmentIsFatal
m_tp["DetectedAneurysm", "DeathTreatment"] <- p_TreatmentIsFatal
m_tp["DeathOther", "DeathOther"] <- 1
m_tp["DeathTreatment", "DeathTreatment"] <- 1

# Check whether all rows sum up to one!
## Using the rowSums() function
rowSums(m_tp) == 1 # all true!


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

v_names_hs <- c("Healthy", "NewAneurysm",	"DetectedAneurysm", "DeathOther",	"DeathTreatment")
m_tp <- matrix(0, 
               ncol = length(v_names_hs),
               nrow = length(v_names_hs),
               dimnames = list(v_names_hs,
                               v_names_hs))

# Fill the matrix with the values of the transition probabilities
# The rows indicate the health state from which persons transit
# The columns indicate the health state to which persons transit
## You can use the names of the rows and columns to assign the transition probabilities to each cell.
## Do not forget to define the probability of remaining in a health state!
m_tp["Healthy", "Healthy"]     <- 1- p_NewAneurysm - p_DeathOther # Example
m_tp["Healthy", "DeathOther"]  <- p_DeathOther
m_tp["Healthy", "NewAneurysm"] <- p_NewAneurysm
m_tp["NewAneurysm", "DetectedAneurysm"] <- p_AneurysmDetection
m_tp["NewAneurysm", "DeathOther"] <- p_DeathOther
m_tp["NewAneurysm", "NewAneurysm"] <- 1 - p_AneurysmDetection - p_DeathOther
m_tp["DetectedAneurysm", "Healthy"] <- 1 - p_TreatmentIsFatal
m_tp["DetectedAneurysm", "DeathTreatment"] <- p_TreatmentIsFatal
m_tp["DeathOther", "DeathOther"] <- 1
m_tp["DeathTreatment", "DeathTreatment"] <- 1

## 4.b. To evaluate the progression of individuals over multiple cycle,
## we need to define:
n_cycles <- 36 # the number of cycles to simulate, assume 3 years
n_pt <- 10000  # the size of the cohort, assume 10000 persons
m_hs <- matrix(0, 
               nrow = n_cycles + 1,
               ncol = length(v_names_hs),
               dimnames = list(c(0:n_cycles),
                               v_names_hs)) # a cohort state matrix, containing [n_cycles + 1] rows (because the first row is the start position), and as much column as the number of health states.
## fill this matrix with 0's for now

## We then need to define the starting positions of the cohort
## Assign all individuals to the "Healthy" health state in the first row of the `m_hs` matrix
v_start_hs <- c(n_pt, 0, 0, 0, 0)
m_hs[1, ] <- v_start_hs

## Perform the matrix multiplication to determine the state membership over the cycles.
## To determine the number of individuals in each health state during each cycle, one need to multiply the vector of state membership in the previous cycle by the transition matrix
## Example: to calculate the number of individuals in each state in cycle 1, multiply the state membership in cycle 0 by the transition matrix
## HINT: to do so, use a a for loop over rows 2 to 41 of the `m_hs` matrix

for(cycle in 1:n_cycles){
  
  # For your matrix of health state
  m_hs[cycle + 1, ] <- m_hs[cycle, ] %*% m_tp # matrix multiplication

}

## Inspect the first row of the cohort simulation, using the head() function
head(m_hs)

## Inspect whether the sum of all rows equal 10000 (thus if you do not 'loose' or add any individuals in your simulation)
which(round(rowSums(m_hs), 10) != 10000) # check rowsums  = 10000 --> 0 --> good!

png("cohort_simulation_plot.png", width = 20, height = 20*9/16, units = "cm", res = 300)
plot(y = m_hs[, "Healthy"]/ n_pt, 
     x = rownames(m_hs),
     type = 'l',
     main = 'Health state membership over model cycles',
     xlab = 'Cycle number',
     ylab = 'Proportion of individuals')
lines(y = m_hs[, "NewAneurysm"]/ n_pt, 
      x = rownames(m_hs), 
      col = 'red')
lines(y = m_hs[, "DetectedAneurysm"]/ n_pt, 
      x = rownames(m_hs), 
      col = 'blue')
lines(y = m_hs[, "DeathOther"]/ n_pt, 
      x = rownames(m_hs), 
      col = 'orange')
lines(y = m_hs[, "DeathTreatment"]/ n_pt, 
      x = rownames(m_hs), 
      col = 'lightgrey')
legend("right",  
       legend = v_names_hs, 
       col = c('black', 'red', 'blue', 'orange', 'lightgrey'),
       lty = 1
       )
dev.off()

## 4.c.	Use your cohort simulation to calculate the number of individuals being alive after exactly 3 years. 
## Remember that individuals only remain alive if they are in one of the three health states “Healthy” , “ NewAneurysm”  and “ Detected Aneurysm”.
round(sum(m_hs[37, "Healthy"], m_hs[37, "NewAneurysm"], m_hs[37, "DetectedAneurysm"]))

## 4.d. Use your cohort simulation to calculate the cumulative number of person-months that is spent in the health states “Healthy” , “ NewAneurysm”  and “ Detected Aneurysm”.  
## Assume that we count state membership at the beginning of the cycle  
## HINT: Use the cumsum() function
n_months_Healthy_3years <- cumsum(m_hs[, "Healthy"])[36]
n_months_NewAneurysm_3years <- cumsum(m_hs[, "NewAneurysm"])[36]
n_months_DetectedAneurysm_3years <- cumsum(m_hs[, "DetectedAneurysm"])[36]

## 4.e. e)	Can you determine the average number of months that individuals in this model are alive, over the 3 year time horizon? Hint: you know the cohort size (10,000).
sum(n_months_Healthy_3years,
    n_months_NewAneurysm_3years,
    n_months_DetectedAneurysm_3years
)/n_pt

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
m_tp_2 <- m_tp

# Re-define transition probabilities
p_TreatmentIsFatal  <- 0.40 # Probability that treatment of a detected aneurysm induces a fatal complication
p_DeathOther  	    <- 0.05 # Probability of death due to causes unrelated to aneurysms

# Re-define transitions in m_tp_2
m_tp_2["Healthy", "Healthy"]     <- 1- p_NewAneurysm - p_DeathOther # Example
m_tp_2["Healthy", "DeathOther"]  <- p_DeathOther
m_tp_2["Healthy", "NewAneurysm"] <- p_NewAneurysm
m_tp_2["NewAneurysm", "DetectedAneurysm"] <- p_AneurysmDetection
m_tp_2["NewAneurysm", "DeathOther"] <- p_DeathOther
m_tp_2["NewAneurysm", "NewAneurysm"] <- 1 - p_AneurysmDetection - p_DeathOther
m_tp_2["DetectedAneurysm", "Healthy"] <- 1 - p_TreatmentIsFatal
m_tp_2["DetectedAneurysm", "DeathTreatment"] <- p_TreatmentIsFatal
m_tp_2["DeathOther", "DeathOther"] <- 1
m_tp_2["DeathTreatment", "DeathTreatment"] <- 1

# Re-define matrix of health states
m_hs_2 <- matrix(0, 
               nrow = n_cycles + 1,
               ncol = length(v_names_hs),
               dimnames = list(c(0:n_cycles),
                               v_names_hs))
m_hs_2[1, ] <- v_start_hs # Define state membership start

# matrix multiplication to fill cohort simulation
for(cycle in 1:n_cycles){
  
  # For your matrix of health state
  m_hs_2[cycle + 1, ] <- m_hs_2[cycle, ] %*% m_tp_2 # matrix multiplication
  
}

## Inspect the first row of the cohort simulation, using the head() function
head(m_hs_2)

## Inspect whether the sum of all rows equal 10000 (thus if you do not 'loose' or add any individuals in your simulation)
which(round(rowSums(m_hs_2), 10) != 10000) # check rowsums  = 10000 --> 0 --> good!

## Re-calculate the cummulative number of individuals
n_months_Healthy_3years_2 <- cumsum(m_hs_2[1:36, "Healthy"])[36]
n_months_NewAneurysm_3years_2 <- cumsum(m_hs_2[1:36, "NewAneurysm"])[36]
n_months_DetectedAneurysm_3years_2 <- cumsum(m_hs_2[1:36, "DetectedAneurysm"])[36]

## Answer
sum(n_months_Healthy_3years_2,
    n_months_NewAneurysm_3years_2,
    n_months_DetectedAneurysm_3years_2
)/n_pt


## 4.f.	g)	First reset the parameters pDeathOther to value 0.1 and pTreatmentIsFatal to value 0.2. 
## What do you think will happen if we increase the sensitivity of the test to detect new aneurysms, from looking at the model structure? 
## Change the value of the parameter “p_AneurysmDetection” from 0.50 to 0.80 and determine again the average number of months that individuals in this model are alive, after 3 years. Can you explain the result? 
### HINT: To do so, clone the transition matrix, modify these two probabilities, and reiterate the cohort simulation

# Clone transition matrix
m_tp_3 <- m_tp

# Re-define transition probabilities
p_AneurysmDetection  <- 0.80 # Probability to detect the Aneurysm

# Re-define transitions in m_tp_3
m_tp_3["Healthy", "Healthy"]     <- 1- p_NewAneurysm - p_DeathOther # Example
m_tp_3["Healthy", "DeathOther"]  <- p_DeathOther
m_tp_3["Healthy", "NewAneurysm"] <- p_NewAneurysm
m_tp_3["NewAneurysm", "DetectedAneurysm"] <- p_AneurysmDetection
m_tp_3["NewAneurysm", "DeathOther"] <- p_DeathOther
m_tp_3["NewAneurysm", "NewAneurysm"] <- 1 - p_AneurysmDetection - p_DeathOther
m_tp_3["DetectedAneurysm", "Healthy"] <- 1 - p_TreatmentIsFatal
m_tp_3["DetectedAneurysm", "DeathTreatment"] <- p_TreatmentIsFatal
m_tp_3["DeathOther", "DeathOther"] <- 1
m_tp_3["DeathTreatment", "DeathTreatment"] <- 1

# Re-define matrix of health states
m_hs_3 <- matrix(0, 
                 nrow = n_cycles + 1,
                 ncol = length(v_names_hs),
                 dimnames = list(c(0:n_cycles),
                                 v_names_hs))
m_hs_3[1, ] <- v_start_hs # Define state membership start

# matrix multiplication to fill cohort simulation
for(cycle in 1:n_cycles){
  
  # For your matrix of health state
  m_hs_3[cycle + 1, ] <- m_hs_3[cycle, ] %*% m_tp_3 # matrix multiplication
  
}

## Inspect the first row of the cohort simulation, using the head() function
head(m_hs_3)

## Inspect whether the sum of all rows equal 10000 (thus if you do not 'loose' or add any individuals in your simulation)
which(round(rowSums(m_hs_3), 10) != 10000) # check rowsums  = 10000 --> 0 --> good!

## Re-calculate the cummulative number of individuals
n_months_Healthy_3years_3 <- cumsum(m_hs_3[1:36, "Healthy"])[36]
n_months_NewAneurysm_3years_3 <- cumsum(m_hs_3[1:36, "NewAneurysm"])[36]
n_months_DetectedAneurysm_3years_3 <- cumsum(m_hs_3[1:36, "DetectedAneurysm"])[36]

## Answer
sum(n_months_Healthy_3years_3,
    n_months_NewAneurysm_3years_3,
    n_months_DetectedAneurysm_3years_3
)/n_pt

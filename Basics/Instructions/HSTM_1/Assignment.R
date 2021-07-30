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
p_NewAneurysm = 	0.10 # Probability that an individual develops a new intracranial aneurysm 
p_AneurysmDetection = 	0.50 # Probability that a newly developed aneurysm is detected
p_TreatmentIsFatal = 	0.20 # Probability that treatment of a detected aneurysm induces a fatal complication
p_DeathOther = 	0.10 # Probability of death due to causes unrelated to aneurysms

#------------------------------------#
#### Step 2: Answer the questions ####
#------------------------------------#

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


#---------------------------------------------#
#### Step 3: Fill in the transition matrix ####
#---------------------------------------------#

# Create a matrix (called `m_tp`) containing 5 rows and 5 columns (using the matrix() function)
# Assign the value 0 to all cells of the matrix to start with
# Name the rows and column (argument dimnames of the function): "Healthy", "NewAneurysm",	"DetectedAneurysm", "DeathOther",	"DeathTreatment"
v_names_hs <- c("Healthy", "NewAneurysm",	"DetectedAneurysm", "DeathOther",	"DeathTreatment")
m_tp <- matrix(0, 
               ncol = 5,
               nrow = 5,
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


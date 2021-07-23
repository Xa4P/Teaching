############################
#### Shiny app EQ-5D-5L ####
############################

# Set up
rm(list= ls())
library(shiny)
library(tidyverse)
library("rstudioapi") 

setwd(dirname(getActiveDocumentContext()$path))

NL_tariff <- read.csv("EQ5D5L_NLtariff.csv", sep = ";")# load table decrements

#install.packages('plotly')
#library(plotly)

# Load function
source("EQ-5D-5L_fct.R")
source("function_sf_36.R")

# Define UI ----
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Welcome", fluid = TRUE,
             fluidPage(
               column(width = 12,
                      h2("Welcome to the assignment on measuring and valuing health effects. The overall aim of the assignment is for you to get acquainted with different methods to describe, measure, and value health benefits in health economic evaluations. Please go through the different tabs of the assignment and write your answers to the open question in a separate document.")
               )
             )
             ),
    # Assignment 1 - SF-36 ----
    tabPanel("Assignment 1 - SF-36", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(h3("In this assignment you will experience what it is like to fill out the SF-36 Health Survey, a descriptive health-related quality of life instrument.
You will start by filling in the SF-36 questionnaire. Subsequently you will enter your individual responses using the button below. These answers will automatically be rescored in the 8 sub-scales of the SF-36. Below a table and a graph will be drawn with your scores ('Respondent’). In these, the scores of a group of patients with migraine and a healthy control group are also displayed for comparison purposes."),
               h3("Instructions for completing the questionnaire: Please answer every question. Some questions may look like others, but each
one is different. Please take the time to read and answer each question carefully by filling in the bubble that best represents
your response."),
                            h4("1. In general, would you say your health is:"),
                            radioButtons(inputId = "it_1",
                                         label = NULL,
                                         choices = list("Excellent" = 1, 
                                                        "Very good" = 2, 
                                                        "Good" = 3,
                                                        "Fair" = 4,
                                                        "Poor" = 5),
                                         selected = 1),
                            h4("2. Compared to one year ago, how would you rate your health in general now?"),
                            radioButtons(inputId = "it_2",
                                         label = NULL,
                                         choices = list("Much better now than a year ago" = 1, 
                                                        "Somewhat better now than a year ago" = 2, 
                                                        "About the same as one year ago" = 3,
                                                        "Somewhat worse now than one year ago" = 4,
                                                        "Much worse now than one year ago" = 5),
                                         selected = 1),
                            h4("3. The following items are about activities you might do during a typical day. Does your health now limit you in these
activities? If so, how much?"),
                            h5("a. Vigorous activities, such as running, lifting heavy objects, participating in strenuous sports."),
                            radioButtons(inputId = "it_3a",
                                         label = NULL,
                                         choices = list("Yes, limited a lot." = 1, 
                                                        "Yes, limited a little." = 2, 
                                                        "No, not limited at all." = 3),
                                         selected = 3),
                            h5("b. Moderate activities, such as moving a table, pushing a vacuum cleaner, bowling, or playing golf?"),
                            radioButtons(inputId = "it_3b",
                                         label = NULL,
                                         choices = list("Yes, limited a lot." = 1, 
                                                        "Yes, limited a little." = 2, 
                                                        "No, not limited at all." = 3),
                                         selected = 3),
                            h5("c. Lifting or carrying groceries."),
                            radioButtons(inputId = "it_3c",
                                         label = NULL,
                                         choices = list("Yes, limited a lot." = 1, 
                                                        "Yes, limited a little." = 2, 
                                                        "No, not limited at all." = 3),
                                         selected = 3),
                            h5("d. Climbing several flights of stairs."),
                            radioButtons(inputId = "it_3d",
                                         label = NULL,
                                         choices = list("Yes, limited a lot." = 1, 
                                                        "Yes, limited a little." = 2, 
                                                        "No, not limited at all." = 3),
                                         selected = 3),
                            h5("e. Climbing one flight of stairs."),
                            radioButtons(inputId = "it_3e",
                                         label = NULL,
                                         choices = list("Yes, limited a lot." = 1, 
                                                        "Yes, limited a little." = 2, 
                                                        "No, not limited at all." = 3),
                                         selected = 3),
                            h5("f. Bending, kneeling or stooping."),
                            radioButtons(inputId = "it_3f",
                                         label = NULL,
                                         choices = list("Yes, limited a lot." = 1, 
                                                        "Yes, limited a little." = 2, 
                                                        "No, not limited at all." = 3),
                                         selected = 3),
                            h5("g. Walking more than one mile."),
                            radioButtons(inputId = "it_3g",
                                         label = NULL,
                                         choices = list("Yes, limited a lot." = 1, 
                                                        "Yes, limited a little." = 2, 
                                                        "No, not limited at all." = 3),
                                         selected = 3),
                            h5("h. Walking several blocks."),
                            radioButtons(inputId = "it_3h",
                                         label = NULL,
                                         choices = list("Yes, limited a lot." = 1, 
                                                        "Yes, limited a little." = 2, 
                                                        "No, not limited at all." = 3),
                                         selected = 3),
                            h5("i. Walking one block."),
                            radioButtons(inputId = "it_3i",
                                         label = NULL,
                                         choices = list("Yes, limited a lot." = 1, 
                                                        "Yes, limited a little." = 2, 
                                                        "No, not limited at all." = 3),
                                         selected = 3),
                            h5("j. Bathing or dressing yourself."),
                            radioButtons(inputId = "it_3j",
                                         label = NULL,
                                         choices = list("Yes, limited a lot." = 1, 
                                                        "Yes, limited a little." = 2, 
                                                        "No, not limited at all." = 3),
                                         selected = 3),
                            h4("4. During the past 4 weeks, have you had any of the following problems with your work or other regular daily activities as a
result of your physical health?"),
                            h5("a. Cut down the amount of time you spent on work or other activities?"),
                            radioButtons(inputId = "it_4a",
                                         label = NULL,
                                         choices = list("Yes" = 1, 
                                                        "No" = 2
                                                        ),
                                         selected = 2),
                            h5("b. Accomplished less than you would like?"),
                            radioButtons(inputId = "it_4b",
                                         label = NULL,
                                         choices = list("Yes" = 1, 
                                                        "No" = 2
                                         ),
                                         selected = 2),
                            h5("c. Were limited in the kind of work or other activities"),
                            radioButtons(inputId = "it_4c",
                                         label = NULL,
                                         choices = list("Yes" = 1, 
                                                        "No" = 2
                                         ),
                                         selected = 2),
                            h5("d. Had difficulty performing the work or other activities (for example, it took extra time)"),
                            radioButtons(inputId = "it_4d",
                                         label = NULL,
                                         choices = list("Yes" = 1, 
                                                        "No" = 2
                                         ),
                                         selected = 2),
                            h4("5. During the past 4 weeks, have you had any of the following problems with your work or other regular daily activities as a
result of any emotional problems (such as feeling depressed or anxious)?"),
                            h5("a. Cut down the amount of time you spent on work or other activities?"),
                            radioButtons(inputId = "it_5a",
                                         label = NULL,
                                         choices = list("Yes" = 1, 
                                                        "No" = 2
                                         ),
                                         selected = 2),
                            h5("b. Accomplished less than you would like?"),
                            radioButtons(inputId = "it_5b",
                                         label = NULL,
                                         choices = list("Yes" = 1, 
                                                        "No" = 2
                                         ),
                                         selected = 2),
                            h5("c. Didn't do work or other activities as carefully as usual"),
                            radioButtons(inputId = "it_5c",
                                         label = NULL,
                                         choices = list("Yes" = 1, 
                                                        "No" = 2
                                         ),
                                         selected = 2),
                            h4("6. During the past 4 weeks, to what extent has your physical health or emotional problems interfered with your normal social
activities with family, friends, neighbors, or groups?"),
                            radioButtons(inputId = "it_6",
                                         label = NULL,
                                         choices = list("Not at all" = 1, 
                                                        "Slightly" = 2, 
                                                        "Moderately" = 3,
                                                        "Quite a bit" = 4,
                                                        "Extremely" = 5),
                                         selected = 1),
                            h4("7. How much bodily pain have you had during the past 4 weeks?"),
                            radioButtons(inputId = "it_7",
                                         label = NULL,
                                         choices = list("Not at all" = 1, 
                                                        "Slightly" = 2, 
                                                        "Moderately" = 3,
                                                        "Quite a bit" = 4,
                                                        "Extremely" = 5),
                                         selected = 1),
                            h4("8. During the past 4 weeks, how much did pain interfere with your normal work (including both work outside the home and
housework)?"),
                            radioButtons(inputId = "it_8",
                                         label = NULL,
                                         choices = list("Not at all" = 1, 
                                                        "Slightly" = 2, 
                                                        "Moderately" = 3,
                                                        "Quite a bit" = 4,
                                                        "Extremely" = 5),
                                         selected = 1),
                            h4("9. These questions are about how you feel and how things have been with you during the past 4 weeks. For each question,
please give the one answer that comes closest to the way you have been feeling. How much of the time during the past 4
weeks."),
                            h5("a. did you feel full of pep?"),
                            radioButtons(inputId = "it_9a",
                                         label = NULL,
                                         choices = list("Almost all the time" = 1, 
                                                        "Most of the time" = 2, 
                                                        "A good bit of the time" = 3,
                                                        "Some of the time" = 4,
                                                        "A little bit of the time" = 5,
                                                        "None of the time" = 6
                                         ),
                                         selected = 1),
                            h5("b. have you been a very nervous person?"),
                            radioButtons(inputId = "it_9b",
                                         label = NULL,
                                         choices = list("Almost all the time" = 1, 
                                                        "Most of the time" = 2, 
                                                        "A good bit of the time" = 3,
                                                        "Some of the time" = 4,
                                                        "A little bit of the time" = 5,
                                                        "None of the time" = 6
                                         ),
                                         selected = 6),
                            h5("c. have you felt so down in the dumps nothing could cheer you up?"),
                            radioButtons(inputId = "it_9c",
                                         label = NULL,
                                         choices = list("Almost all the time" = 1, 
                                                        "Most of the time" = 2, 
                                                        "A good bit of the time" = 3,
                                                        "Some of the time" = 4,
                                                        "A little bit of the time" = 5,
                                                        "None of the time" = 6
                                         ),
                                         selected = 6),
                            h5("d. have you felt calm and peaceful?"),
                            radioButtons(inputId = "it_9d",
                                         label = NULL,
                                         choices = list("Almost all the time" = 1, 
                                                        "Most of the time" = 2, 
                                                        "A good bit of the time" = 3,
                                                        "Some of the time" = 4,
                                                        "A little bit of the time" = 5,
                                                        "None of the time" = 6
                                         ),
                                         selected = 1),
                            h5("e. did you have a lot of energy?"),
                            radioButtons(inputId = "it_9e",
                                         label = NULL,
                                         choices = list("Almost all the time" = 1, 
                                                        "Most of the time" = 2, 
                                                        "A good bit of the time" = 3,
                                                        "Some of the time" = 4,
                                                        "A little bit of the time" = 5,
                                                        "None of the time" = 6
                                         ),
                                         selected = 1),
                            h5("f. have you felt downhearted and blue?"),
                            radioButtons(inputId = "it_9f",
                                         label = NULL,
                                         choices = list("Almost all the time" = 1, 
                                                        "Most of the time" = 2, 
                                                        "A good bit of the time" = 3,
                                                        "Some of the time" = 4,
                                                        "A little bit of the time" = 5,
                                                        "None of the time" = 6
                                         ),
                                         selected = 6),
                            h5("g. did you feel worn out?"),
                            radioButtons(inputId = "it_9g",
                                         label = NULL,
                                         choices = list("Almost all the time" = 1, 
                                                        "Most of the time" = 2, 
                                                        "A good bit of the time" = 3,
                                                        "Some of the time" = 4,
                                                        "A little bit of the time" = 5,
                                                        "None of the time" = 6
                                         ),
                                         selected = 6),
                            h5("h. have you been a happy person?"),
                            radioButtons(inputId = "it_9h",
                                         label = NULL,
                                         choices = list("Almost all the time" = 1, 
                                                        "Most of the time" = 2, 
                                                        "A good bit of the time" = 3,
                                                        "Some of the time" = 4,
                                                        "A little bit of the time" = 5,
                                                        "None of the time" = 6
                                         ),
                                         selected = 1),
                            h5("i. did you feel tired?"),
                            radioButtons(inputId = "it_9i",
                                         label = NULL,
                                         choices = list("Almost all the time" = 1, 
                                                        "Most of the time" = 2, 
                                                        "A good bit of the time" = 3,
                                                        "Some of the time" = 4,
                                                        "A little bit of the time" = 5,
                                                        "None of the time" = 6
                                         ),
                                         selected = 6),
                            h4("10. During the past 4 weeks, how much of the time has your physical health or emotional problems interfered with your
social activities (like visiting friends, relatives, etc.)?"),
                            radioButtons(inputId = "it_10",
                                         label = NULL,
                                         choices = list("Almost all the time" = 1, 
                                                        "Most of the time" = 2,
                                                        "Some of the time" = 3,
                                                        "A little bit of the time" = 4,
                                                        "None of the time" = 5
                                         ),
                                         selected = 5),
                            h4("How TRUE or FALSE is each of the following statements for you?"),
                            h5("a. I seem to get sick a little easier than other people"),
                            radioButtons(inputId = "it_11a",
                                         label = NULL,
                                         choices = list("Definitely true" = 1, 
                                                        "Mostly true" = 2, 
                                                        "Don't know" = 3,
                                                        "Moslty false" = 4,
                                                        "False" = 5
                                         ),
                                         selected = 5),
                            h5("b. I am as healthy as anybody I know"),
                            radioButtons(inputId = "it_11b",
                                         label = NULL,
                                         choices = list("Definitely true" = 1, 
                                                        "Mostly true" = 2, 
                                                        "Don't know" = 3,
                                                        "Moslty false" = 4,
                                                        "False" = 5
                                         ),
                                         selected = 1),
                            h5("c. I expect my health to get worse"),
                            radioButtons(inputId = "it_11c",
                                         label = NULL,
                                         choices = list("Definitely true" = 1, 
                                                        "Mostly true" = 2, 
                                                        "Don't know" = 3,
                                                        "Moslty false" = 4,
                                                        "False" = 5
                                         ),
                                         selected = 5),
                            h5("d. My health is excellent"),
                            radioButtons(inputId = "it_11d",
                                         label = NULL,
                                         choices = list("Definitely true" = 1, 
                                                        "Mostly true" = 2, 
                                                        "Don't know" = 3,
                                                        "Moslty false" = 4,
                                                        "False" = 5
                                         ),
                                         selected = 1),
                            h3("Your results are plotted in the graph below, in comparison with the results of a group of migraine patients (N = 340) and a control group of healthy individuals (N = 467)"),
                            h3("A score of 100 indicates no impairment in the specific domain."),
                            h4("Abreviation of the domains"),
                            h4("BP = Bodily Pain"),
                            h4("GH = General Health"),
                            h4("MH = Mental Health"),
                            h4("PF = Physical Performance"),
                            h4("RE = Role Emotional"),
                            h4("RP = Role Performance"),
                            h4("SF = Social Functioning"),
                            h4("VT = Vitality"),
               h3("How do you feel about this result? Did you expect to score higher or lower? and why?"),
               h3("Given these results, can you express your quality of life in a single numeric value?"),
               
                            width = 12
               ),
               mainPanel(
                 h1("SF-36 results"),
                 plotOutput("SF_36_calc"),
                 tableOutput("SF_36_calc2")
               )
             )
    ),
    # Assignment 2 - QALY calculation ----
    tabPanel("Assignment 2 - QALY calculation", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(h3("In this assignment you have to calculate the number of QALYs for a patient who is healthy for the first 10 years of her life (Utility (U) = 0.9), has moderate health (U=0.60) in the next 5 years, and has poor health the last 10 years of her life (U=0.30)."),
                            h3("Fill in the utility values for each period of time in the boxes below, calculate the total number of QALY gained. The graph can help you to do so, i.e. the total number of QALY is equal to the area under the line. and check your answer by pushing the button"),
                            numericInput(inputId = "util_first_10",
                                         label = "Utility value first 10 years",
                                         value = 0,
                                         min = 0,
                                         max = 1),
                            numericInput(inputId = "util_5",
                                         label = "Utility value next 5 years",
                                         value = 0,
                                         min = 0,
                                         max = 1),
                            numericInput(inputId = "util_last_10",
                                         label = "Utility value last 10 years",
                                         value = 0,
                                         min = 0,
                                         max = 1),
                            numericInput(inputId = "n_QALYs",
                                         label = "How much QALY does this person accrue over the 25 years? Put your answer",
                                         value = 0,
                                         min = 0,
                                         max = 1),
                            actionButton(inputId = "check_QALY_calc",
                                         label = "Push to check your answer!")
               ),
               mainPanel(plotOutput("QALY_calc"),
                         h3(textOutput("TEXT_QALY_calc", container = span))
                         )
             )
             ),
    # Assignment 3 - QALY comparison ----
    tabPanel("Assignment 3 - QALYs comparison", fluid = TRUE,
             fluidPage(
               column(width = 12,
                      h3("The aim of this assignment is to compare the number of QALY gained by two interventions."),
                      h3("Intervention A has a survival of 10 years after the intervention with a utility of 0.7"), 
                      h3("Intervention B has a survival of 5 years after the intervention with a utility of 0.9 followed by 5 years in a health state with a utility of 0.5"),
                      h3("First use the fields to fill in graphs using the open field, and the answer the questions")
                      ),
               column(width = 4,
                      numericInput(inputId = "u_A_1",
                                   label = "Utility value first year - Intervention A",
                                   value = 0,
                                   min = 0,
                                   max = 1),
                      numericInput(inputId = "u_A_2",
                                   label = "Utility value second year - Intervention A",
                                   value = 0,
                                   min = 0,
                                   max = 1),
                      numericInput(inputId = "u_A_3",
                                   label = "Utility value third year - Intervention A",
                                   value = 0,
                                   min = 0,
                                   max = 1),
                      numericInput(inputId = "u_A_4",
                                   label = "Utility value fourth year - Intervention A",
                                   value = 0,
                                   min = 0,
                                   max = 1),
                      numericInput(inputId = "u_A_5",
                                   label = "Utility value fifth year - Intervention A",
                                   value = 0,
                                   min = 0,
                                   max = 1),
                      numericInput(inputId = "u_A_6",
                                   label = "Utility value sixth year - Intervention A",
                                   value = 0,
                                   min = 0,
                                   max = 1),
                      numericInput(inputId = "u_A_7",
                                   label = "Utility value seventh year - Intervention A",
                                   value = 0,
                                   min = 0,
                                   max = 1),
                      numericInput(inputId = "u_A_8",
                                   label = "Utility value eighth year - Intervention A",
                                   value = 0,
                                   min = 0,
                                   max = 1),
                      numericInput(inputId = "u_A_9",
                                   label = "Utility value ninth year - Intervention A",
                                   value = 0,
                                   min = 0,
                                   max = 1),
                      numericInput(inputId = "u_A_10",
                                   label = "Utility value 10th year - Intervention A",
                                   value = 0,
                                   min = 0,
                                   max = 1)
                      ),
               column(width = 4,
                      numericInput(inputId = "u_B_1",
                                   label = "Utility value first year - Intervention B",
                                   value = 0,
                                   min = 0,
                                   max = 1),
                      numericInput(inputId = "u_B_2",
                                   label = "Utility value second year - Intervention B",
                                   value = 0,
                                   min = 0,
                                   max = 1),
                      numericInput(inputId = "u_B_3",
                                   label = "Utility value third year - Intervention B",
                                   value = 0,
                                   min = 0,
                                   max = 1),
                      numericInput(inputId = "u_B_4",
                                   label = "Utility value fourth year - Intervention B",
                                   value = 0,
                                   min = 0,
                                   max = 1),
                      numericInput(inputId = "u_B_5",
                                   label = "Utility value fifth year - Intervention B",
                                   value = 0,
                                   min = 0,
                                   max = 1),
                      numericInput(inputId = "u_B_6",
                                   label = "Utility value sixth year - Intervention B",
                                   value = 0,
                                   min = 0,
                                   max = 1),
                      numericInput(inputId = "u_B_7",
                                   label = "Utility value seventh year - Intervention B",
                                   value = 0,
                                   min = 0,
                                   max = 1),
                      numericInput(inputId = "u_B_8",
                                   label = "Utility value eighth year - Intervention B",
                                   value = 0,
                                   min = 0,
                                   max = 1),
                      numericInput(inputId = "u_B_9",
                                   label = "Utility value ninth year - Intervention B",
                                   value = 0,
                                   min = 0,
                                   max = 1),
                      numericInput(inputId = "u_B_10",
                                   label = "Utility value 10th year - Intervention B",
                                   value = 0,
                                   min = 0,
                                   max = 1)
               ),
               column(width = 4,
                      h3("Which intervention provides the most QALYs?"),
                      radioButtons(inputId = "QALY_comp_answer",
                                   label = NULL,
                                   choices = list("Intervention A" = 1, 
                                                  "Intervention B" = 2, 
                                                  "None" = 3),
                                   selected = 0),
                      actionButton(inputId = "check_QALY_comp",
                                   label = "Push to check your answer!"),
                      h3(textOutput("TEXT_QALY_comp", container = span)),
                      h3("Based on the total number of QALYs gained, which intervention would you prefer?"),
                      h3("What if the quality of life with intervention A was 0.8, which one would you then prefer?"),
                      ),
               column(width = 12,
                 h3("Graphs displaying utility values of each intervention over time"),
                 plotOutput("QALY_comp")
                 
                 )
               )
             ),
    # Assignment 4 - Measuring you quality of life----
    tabPanel("Assignment 4 - Measuring your health", fluid = TRUE,
             fluidPage(
               column(width = 6,
                        h2("This assignment aims to get you acquainted with measuring health through the EQ-5D-5L. It is based on the sample version of the EQ-5D-5L that is available at https://euroqol.org/eq-5d-instruments/sample-demo/. It is composed of both a questionnaire and the visual analog scale (VAS)"),
                        h3("Instructions: Under each heading, please tick the ONE box that best describes your health TODAY. Then rate your quality of life using the VAS"),
                        h2("Mobility"),
                        radioButtons(inputId = "mo",
                                     label = NULL,
                                     choices = list("I have no problems in walking about" = 1, 
                                                   "I have slight problems in walking about" = 2, 
                                                   "I have moderate problems in walking about" = 3,
                                                   "I have severe problems in walking about" = 4,
                                                   "I am unable to walk about" = 5),
                                     selected = 1),
                        h2("Self-care"),
                        radioButtons(inputId = "sc",
                                     label = NULL,
                                     choices = list("I have no problems washing or dressing myself" = 1, 
                                             "I have slight problems washing or dressing myself" = 2, 
                                             "I have moderate problems washing or dressing myself" = 3,
                                             "I have severe problems washing or dressing myself" = 4,
                                             "I am unable to wash or dress myself" = 5),
                                      selected = 1),
                        h2("Usual activities"),
                        helpText("e.g. work, study, housework, family or leisure activities"),
                        radioButtons(inputId = "ua",
                                     label = NULL,
                                     choices = list("I have no problems doing my usual activities" = 1, 
                                             "I have slight problems doing my usual activities" = 2, 
                                             "I have moderate problems doing my usual activities" = 3,
                                             "I have severe problems doing my usual activities" = 4,
                                             "I am unable to do my usual activities" = 5),
                                      selected = 1),
                        h2("Pain/ discomfort"),
                        radioButtons(inputId = "pd",
                                     label = NULL,
                                     choices = list("I have no pain or discomfort" = 1, 
                                             "I have slight pain or discomfort" = 2, 
                                             "I have moderate pain or discomfort" = 3,
                                             "I have severe pain or discomfort" = 4,
                                             "I have extreme pain or discomfort" = 5),
                                     selected = 1),
                        h2("Anxiety/ depression"),
                        radioButtons(inputId = "ad",
                                     label = NULL,
                                     choices = list("I am not anxious or depressed" = 1, 
                                             "I am slightly anxious or depressed" = 2, 
                                             "I am moderately anxious or depressed" = 3,
                                             "I am severely anxious or depressed" = 4,
                                             "I am extremely anxious or depressed" = 5),
                                     selected = 1),
                        ),
               column(width = 6,
                      h2("Visual analog scale"),
                      h3("We would like to know how good or bad your health is TODAY. This scale is numbered from 0 to 100. 100 means the best health you can imagine. 0 means the worst health you can imagine. Please slide the button to indicate how your health is TODAY. Now, look at the number you marked on the scale on the right. (In the real EQ-5D questionnaire, the slide bar is vertical, not horizontal)"),
                      sliderInput("vas", "Your health today:", 
                                  min = 0, max = 1, value = 0.5, step= 0.01),
                      h2("EQ-5D-5L utility estimate based on the questionnaire"),
                      h3(textOutput("EQ5D_calc")),
                      h2("Utility estimate using the Visual Analog Scale"),
                      h3(textOutput("VAS")),
                      h3("Do you see differences between the results? Can you explain them?")
               )
             )
             ),
  # Assignment 5 - Using the EQ-5D ----
  tabPanel("Assignment 5 - Determining utility values", fluid = TRUE,
           fluidPage(
             column(width = 6,
                    h2("This assignment aims to teach you how to use a the Dutch EQ-5D-5L tarriff (Versteegh et al. - 2016) to calculate utility values based on a health state description."),
                    h3("Instructions: For each health state description, calculate the utility value using the tarriff provided in the table. The EQ-5D codes are organised as follows: mobility, self-care , usual activities, pain/ discomfort level, anxiety/ discomfort level. Provide your answer until 3 decimals"),
                    h3("Help: Level 1 means that someone has no impairment in a dimension. For each dimension with a Level different than 1, a disutility is applied to the constant."),
                    h3("Example: the code 12345 is interpreted as: I have no problems in walking about, I have slight problems washing or dressing myself, I have moderate problems doing my usual activities, I have severe pain or discomfort, I am extremely anxious or depressed"),
                    helpText("Reference: Matthijs M. Versteegh, Karin M. Vermeulen, Silvia M. A. A. Evers, G. Ardine de Wit, Rilana Prenger, Elly A. Stolk, Dutch Tariff for the Five-Level Version of EQ-5D, Value in Health, Volume 19, Issue 4, 2016, Pages 343-352, https://doi.org/10.1016/j.jval.2016.01.003"),
                    h4("EQ-5D-description first health state : 12423"),
                    h4("EQ-5D-description second health state : 25431"),
                    h4("EQ-5D-description third health state : 11122"),
                    numericInput(inputId = "res_EQ5D_1",
                                 label = "Utility value first health state",
                                 value = 0,
                                 min = 0,
                                 max = 1),
                    numericInput(inputId = "res_EQ5D_2",
                                 label = "Utility value second health state",
                                 value = 0,
                                 min = 0,
                                 max = 1),
                    numericInput(inputId = "res_EQ5D_3",
                                 label = "Utility value third health state",
                                 value = 0,
                                 min = 0,
                                 max = 1),
                    actionButton(inputId = "check_EQ5D_calc",
                                 label = "Push to check your answers!"),
                    h3("Question: What does a EQ-5D score of 55555 mean?")
             ),
             column(width = 6,
                    h3("Table with decrements for each level of each dimension"),
                    h4("Abbreviations table: mo = mobility, sc = self-care , ua = usual activities, pd = pain/ discomfort level, ad = anxiety/ discomfort level"),
                    tableOutput("tbl_NL_tariff"),
                    h3(textOutput("res_check_EQ5D", container = span))
                    )
           )
           ),
  # Assignment 6 - Valuing health states----
  tabPanel("Assignment 6 - Valuing health states", fluid = TRUE,
           fluidPage(
             column(width = 12,
                    h2("In this assignment, you will value the three health states that are described in the previous assignment, using the following techniques: Standard Gamble, Time-trade-off, and Visual analog scale")
                    ),
             column(width = 4,
                    h3("EQ-5D description first health state : 12423"),
                    h4("I have no problems in walking about, I have slight problems washing or dressing myself, I have severe problems doing my usual activities,I have slight pain or discomfort, and I am moderately anxious or depressed"),
                    h3("Standard Gamble"),
                    h4("At which probability of immediate death would you be indifferent between the Gamble and remaining in health state 12423 for 10 years?"),
                    helpText("Please indicate this probability using the slider"),
                    sliderInput("sg_hs1", "Probability of death in the gamble", 
                                min = 0, max = 1, value = 0.5, step = 0.01),
                    h3("Time trade off"),
                    h4("How much years of life in this health state (12423) would you trade to become indifferent between a) remaining in this health state for 10 years and b) living for 10 life years minus - the amount of traded life years in perfect health?"),
                    helpText("Please indicate this number of years using the slider"),
                    sliderInput("tto_hs1", "Number of traded years", 
                                min = 0, max = 10, value = 5, step = 0.1),
                    h3("Visual analog scale"),
                    h4("How do you rate the quality of life in this health state (12423)?"),
                    helpText("Please indicate this on the visual analog scale using the slider"),
                    sliderInput("vas_hs1", "Quality of life between worst imaginable health state, and best imaginable health state", 
                                min = 0, max = 1, value = 0.5, step = 0.01)
                    
             ),
             column(width = 8,
                    h3("Standard Gamble health state 12423"),
                    plotOutput("plot_sg_hs1"),
                    h4(textOutput("res_sg_hs1")),
                    h3("Time trade off health state 12423"),
                    plotOutput("plot_tto_hs1"),
                    h4(textOutput("res_tto_hs1")),
                    h3("Visual analog scale health state 12423"),
                    h4(textOutput("res_vas_hs1"))
             ),
             column(width = 4,
                    h3("EQ-5D description second health state : 25431"),
                    h4("I have slight problems in walking about, I am unable to wash or dress myself, I have severe problems doing my usual activities, I have moderate pain or discomfort, and I am not anxious or depressed"),
                    h3("Standard Gamble"),
                    h4("At which probability of immediate death would you be indifferent between the Gamble and remaining in health state 25431 for 10 years?"),
                    helpText("Please indicate this probability using the slider"),
                    sliderInput("sg_hs2", "Probability of death in the gamble", 
                                min = 0, max = 1, value = 0.5, step = 0.01),
                    h3("Time trade off"),
                    h4("How much years of life in this health state (25431) would you trade to become indifferent between a) remaining in this health state for 10 years and b) living for 10 life years minus - the amount of traded life years in perfect health?"),
                    helpText("Please indicate this number of years using the slider"),
                    sliderInput("tto_hs2", "Number of traded years", 
                                min = 0, max = 10, value = 5, step = 0.1),
                    h3("Visual analog scale"),
                    h4("How do you rate the quality of life in this health state (25431)?"),
                    helpText("Please indicate this on the visual analog scale using the slider"),
                    sliderInput("vas_hs2", "Quality of life between worst imaginable health state, and best imaginable health state", 
                                min = 0, max = 1, value = 0.5, step = 0.01)
             ),
             column(width = 8,
                    h3("Standard Gamble health state 25431"),
                    plotOutput("plot_sg_hs2"),
                    h4(textOutput("res_sg_hs2")),
                    h3("Time trade off health state 25431"),
                    plotOutput("plot_tto_hs2"),
                    h4(textOutput("res_tto_hs2")),
                    h3("Visual analog scale health state 25431"),
                    h4(textOutput("res_vas_hs2")),
             ),
             column(width = 4,
                    h3("EQ-5D description third health state : 11122"),
                    h4("I have no problems in walking about, I have no problems washing or dressing myself, I have no problems doing my usual activities, I have slight pain or discomfort, and I am slightly anxious or depressed"),
                    h3("Standard Gamble"),
                    h4("At which probability of immediate death would you be indifferent between the Gamble and remaining in health state 11122 for 10 years?"),
                    helpText("Please indicate this probability using the slider"),
                    sliderInput("sg_hs3", "Probability of death in the gamble", 
                                min = 0, max = 1, value = 0.5, step = 0.01),
                    h3("Time trade off"),
                    h4("How much years of life in this health state (11122) would you trade to become indifferent between a) remaining in this health state for 10 years and b) living for 10 life years minus - the amount of traded life years in perfect health?"),
                    helpText("Please indicate this number of years using the slider"),
                    sliderInput("tto_hs3", "Number of traded years", 
                                min = 0, max = 10, value = 5, step = 0.1),
                    h3("Visual analog scale"),
                    h4("How do you rate the quality of life in this health state (11122)?"),
                    helpText("Please indicate this on the visual analog scale using the slider"),
                    sliderInput("vas_hs3", "Quality of life between worst imaginable health state, and best imaginable health state", 
                                min = 0, max = 1, value = 0.5, step = 0.01),
                    
             ),
             column(width = 8,
                    h3("Standard Gamble health state 11122"),
                    plotOutput("plot_sg_hs3"),
                    h4(textOutput("res_sg_hs3")),
                    h3("Time trade off health state 11122"),
                    plotOutput("plot_tto_hs3"),
                    h4(textOutput("res_tto_hs3")),
                    h3("Visual analog scale health state 11122"),
                    h4(textOutput("res_vas_hs3"))
             )
           )
           ),
  # Assignment  7 - Valuing vignettes ----
  tabPanel("Assignment 7 - Valuing vignettes", fluid = TRUE,
           fluidPage(
             column(width = 12,
                    h2("In this assignment, you will value the two health states that are described as vignettes using the following techniques: Standard Gamble, Time-trade-off, and Visual analog scale. A vignette is a narrative description of a certain health state."),
                    h2("Vignette 1: Reumatoid arthritis (mild variant)"),
                    h3("Your mobility is limited by pain. Walking long distances and standing still increase these complaints/ When you exercise, you experience increased joint complaints the day after. You are fully capable of bathing and clothing yourself. However, putting on shoes with laces is not an option. Activities that require lots of movement or physical strength cannot be performed. You require some help with usual activities such as doing groceries. It is important to take things slowly! You experience a constant mild pain in the joints and there is always a chance a relapse occurs. During a relapse, you are often feeling depressed, but you experience no cognitive symptoms. Your vision is normal for your age and you have no hearing problems. Your speech is normal but you experience mild to severe problems using your hands. Typing and other light activities are still possible, but every exercise that requires strength from your fingers is painful. The coordination of yours fingers is also poor. Since other people do not directly observe where your pain is located, you experience some misunderstanding and miscomprehension. You are capable of maintaining normal social contacts, although the duration and frequency of such contacts are somewhat limited."),
                    ),
             column(width = 4,
                    h3("Standard Gamble"),
                    h4("At which probability of immediate death would you be indifferent between the Gamble and remaining in the above-described health state for 30 years?"),
                    helpText("Please indicate this probability using the slider"),
                    sliderInput("sg_v1", "Probability of death in the gamble", 
                                min = 0, max = 1, value = 0.5, step = 0.01),
                    h3("Time trade off"),
                    h4("How much years of life in the above-described health state would you trade to become indifferent between a) remaining in this health state for 30 years and b) living for 30 life years minus - the amount of traded life years in perfect health?"),
                    helpText("Please indicate this number of years using the slider"),
                    sliderInput("tto_v1", "Number of traded years", 
                                min = 0, max = 30, value = 15, step = 0.1),
                    h3("Visual analog scale"),
                    h4("How do you rate the quality of life in the above-described health state?"),
                    helpText("Please indicate this on the visual analog scale using the slider"),
                    sliderInput("vas_v1", "Quality of life between worst imaginable health state, and best imaginable health state", 
                                min = 0, max = 1, value = 0.5, step = 0.01)
                    
             ),
             column(width = 8,
                    h3("Standard Gamble health state vignette 1"),
                    plotOutput("plot_sg_v1"),
                    h4(textOutput("res_sg_v1")),
                    h3("Time trade off health state vignette 1"),
                    plotOutput("plot_tto_v1"),
                    h4(textOutput("res_tto_v1")),
                    h3("Visual analog scale health state vignette 1"),
                    h4(textOutput("res_vas_v1"))
             ),
             column(width = 12,
                    h2("Vignette 2: Neurological disorder - Cerebral Vascular Accident (serious case)"),
                    h3("Your mobility is severely limited. You are in a wheelchair during the day. Getting in and out of bed is only possible with (professional) help. You have to be straightened in your wheelchair several times per day and you are incapable of bathing and clothing yourself. You are incontinent and your are severely limited in the amount of daily activities that you can perform. You are still able to read a newspaper, listen to music, and watch television. You experience no pain but you experience some discomfort from sitting the whole day in your wheelchair. You are unable to swallow and are therefore fed intravenously. You experience emotional swings at least twice per day (spontaneous crying) and you are restless in the dark. You are able to see, but you are not able to recognize anything in your left visual space. You have severe aphasia (you cannot form sentences and have trouble finding the correct words). Only your partner and the nurses understand some of what you are trying to communicate. You are fully paralyzed on the right side of your body and you experience severe problems with coordination on the left side. There is only a limited form of social contact possible with the nursing staff and even with people who experience the same type of disorder.")
             ),
             column(width = 4,
                    h3("Standard Gamble"),
                    h4("At which probability of immediate death would you be indifferent between the Gamble and remaining in the above-described health state for 30 years?"),
                    helpText("Please indicate this probability using the slider"),
                    sliderInput("sg_v2", "Probability of death in the gamble", 
                                min = 0, max = 1, value = 0.5, step = 0.01),
                    h3("Time trade off"),
                    h4("How much years of life in the above-described health state would you trade to become indifferent between a) remaining in this health state for 30 years and b) living for 30 life years minus - the amount of traded life years in perfect health?"),
                    helpText("Please indicate this number of years using the slider"),
                    sliderInput("tto_v2", "Number of traded years", 
                                min = 0, max = 30, value = 15, step = 0.1),
                    h3("Visual analog scale"),
                    h4("How do you rate the quality of life in the above-described health state?"),
                    helpText("Please indicate this on the visual analog scale using the slider"),
                    sliderInput("vas_v2", "Quality of life between worst imaginable health state, and best imaginable health state", 
                                min = 0, max = 1, value = 0.5, step = 0.01)
                    
             ),
             column(width = 8,
                    h3("Standard Gamble health state vignette 2"),
                    plotOutput("plot_sg_v2"),
                    h4(textOutput("res_sg_v2")),
                    h3("Time trade off health state vignette 2"),
                    plotOutput("plot_tto_v2"),
                    h4(textOutput("res_tto_v2")),
                    h3("Visual analog scale health state vignette 2"),
                    h4(textOutput("res_vas_v2"))
             )
             
           )
  )
  )
  )

# Define server logic ----
server <- function(input, output) {
  
  # Assignment 1 ----
  output$SF_36_calc <- renderPlot({
    
    v_resp_sf36 <- c(as.numeric(as.character(input$it_1)),
                     as.numeric(as.character(input$it_2)),
                     as.numeric(as.character(input$it_3a)), 
                     as.numeric(as.character(input$it_3b)),
                     as.numeric(as.character(input$it_3c)),
                     as.numeric(as.character(input$it_3d)),
                     as.numeric(as.character(input$it_3e)), 
                     as.numeric(as.character(input$it_3f)),
                     as.numeric(as.character(input$it_3g)),
                     as.numeric(as.character(input$it_3h)),
                     as.numeric(as.character(input$it_3i)), 
                     as.numeric(as.character(input$it_3j)),
                     as.numeric(as.character(input$it_4a)),
                     as.numeric(as.character(input$it_4b)),
                     as.numeric(as.character(input$it_4c)), 
                     as.numeric(as.character(input$it_4d)),
                     as.numeric(as.character(input$it_5a)),
                     as.numeric(as.character(input$it_5b)),
                     as.numeric(as.character(input$it_5c)), 
                     as.numeric(as.character(input$it_6)),
                     as.numeric(as.character(input$it_7)),
                     as.numeric(as.character(input$it_8)),
                     as.numeric(as.character(input$it_9a)), 
                     as.numeric(as.character(input$it_9b)),
                     as.numeric(as.character(input$it_9c)),
                     as.numeric(as.character(input$it_9d)),
                     as.numeric(as.character(input$it_9e)), 
                     as.numeric(as.character(input$it_9f)),
                     as.numeric(as.character(input$it_9g)),
                     as.numeric(as.character(input$it_9h)),
                     as.numeric(as.character(input$it_9i)), 
                     as.numeric(as.character(input$it_10)),
                     as.numeric(as.character(input$it_11a)),
                     as.numeric(as.character(input$it_11b)),
                     as.numeric(as.character(input$it_11c)), 
                     as.numeric(as.character(input$it_11d))
                     )
    
    df_plot <- calculate_sf_36(v_resp_sf36)
    
    ggplot(data = df_plot, aes(x = Domain, y = Score, group = Group, color = Group)) +
      geom_line() +
      ylim(0, 100) +
      theme_bw()
  }
  )
  
  output$SF_36_calc2 <- renderTable({
    
    v_resp_sf36 <- c(as.numeric(as.character(input$it_1)),
                     as.numeric(as.character(input$it_2)),
                     as.numeric(as.character(input$it_3a)), 
                     as.numeric(as.character(input$it_3b)),
                     as.numeric(as.character(input$it_3c)),
                     as.numeric(as.character(input$it_3d)),
                     as.numeric(as.character(input$it_3e)), 
                     as.numeric(as.character(input$it_3f)),
                     as.numeric(as.character(input$it_3g)),
                     as.numeric(as.character(input$it_3h)),
                     as.numeric(as.character(input$it_3i)), 
                     as.numeric(as.character(input$it_3j)),
                     as.numeric(as.character(input$it_4a)),
                     as.numeric(as.character(input$it_4b)),
                     as.numeric(as.character(input$it_4c)), 
                     as.numeric(as.character(input$it_4d)),
                     as.numeric(as.character(input$it_5a)),
                     as.numeric(as.character(input$it_5b)),
                     as.numeric(as.character(input$it_5c)), 
                     as.numeric(as.character(input$it_6)),
                     as.numeric(as.character(input$it_7)),
                     as.numeric(as.character(input$it_8)),
                     as.numeric(as.character(input$it_9a)), 
                     as.numeric(as.character(input$it_9b)),
                     as.numeric(as.character(input$it_9c)),
                     as.numeric(as.character(input$it_9d)),
                     as.numeric(as.character(input$it_9e)), 
                     as.numeric(as.character(input$it_9f)),
                     as.numeric(as.character(input$it_9g)),
                     as.numeric(as.character(input$it_9h)),
                     as.numeric(as.character(input$it_9i)), 
                     as.numeric(as.character(input$it_10)),
                     as.numeric(as.character(input$it_11a)),
                     as.numeric(as.character(input$it_11b)),
                     as.numeric(as.character(input$it_11c)), 
                     as.numeric(as.character(input$it_11d))
    )
    
    df_plot <- calculate_sf_36(v_resp_sf36)
    df_plot

  }
  )
  
  # Assignment 2 ----
  output$QALY_calc <- renderPlot({
    v_u <-  c(rep(as.numeric(as.character(input$util_first_10)), 11),
              rep(as.numeric(as.character(input$util_5)), 6),
              rep(as.numeric(as.character(input$util_last_10)), 11)
              )# vector of utilities
    v_time <- c(c(0:10), 
                c(10:15),
                c(15:25)
                )# vector of time
    plot(x = v_time, y = v_u, type = 'l',
         xlab = "Time (in years)",
         ylab = "Utility value",
         ylim = c(0,1)
         )

  }
  )
  
  text_button_QALY_calc <- eventReactive(input$check_QALY_calc, {
    if(as.numeric(as.character(input$n_QALYs)) == 15){
      
      paste("Good job, this is correct! 
            The calculation behind this answer is 10 * 0.9 + 5 * 0.6 + 10 * 0.3")
      
    } else {
      paste("This is not the correct answer, try again! Hint: QALYs = quality of life (utility) x length of life")
    }
    
  })
    
  
  output$TEXT_QALY_calc <- renderText({
    text_button_QALY_calc()
  })
  
  # Assignment 3 ----
  output$QALY_comp <- renderPlot({
    
    v_u_A <-  c(as.numeric(as.character(input$u_A_1)),
                as.numeric(as.character(input$u_A_1)),
                as.numeric(as.character(input$u_A_2)),
                as.numeric(as.character(input$u_A_3)),
                as.numeric(as.character(input$u_A_4)),
                as.numeric(as.character(input$u_A_5)),
                as.numeric(as.character(input$u_A_6)),
                as.numeric(as.character(input$u_A_7)),
                as.numeric(as.character(input$u_A_8)),
                as.numeric(as.character(input$u_A_9)),
                as.numeric(as.character(input$u_A_10))) # vector of utilities - intervention A
    
    v_u_B <-  c(as.numeric(as.character(input$u_B_1)),
                as.numeric(as.character(input$u_B_1)),
                as.numeric(as.character(input$u_B_2)),
                as.numeric(as.character(input$u_B_3)),
                as.numeric(as.character(input$u_B_4)),
                as.numeric(as.character(input$u_B_5)),
                as.numeric(as.character(input$u_B_6)),
                as.numeric(as.character(input$u_B_7)),
                as.numeric(as.character(input$u_B_8)),
                as.numeric(as.character(input$u_B_9)),
                as.numeric(as.character(input$u_B_10))) # vector of utilities - intervention B
    
    v_time_2 <- c(0:10) # vector of time
    
    plot(x = v_time_2, y = v_u_A, type = 'l',
         xlab = "Time (in years)",
         ylab = "Utility value",
         ylim = c(0, 1),
         xlim = c(0, 10),
         col = 1
    )
    lines(x = v_time_2, y = v_u_B, col = 2)
    legend(x = "topright",          # Position
           legend = c("Intervention A", "Intervention B"),  # Legend texts
           lty = c(1, 1),           # Line types
           col = c(1, 2)           # Line colors
           )             
    
    
  }
  )
  
  text_button_QALY_comp <- eventReactive(input$check_QALY_comp, {
    if(as.numeric(as.character(input$QALY_comp_answer)) == 3){
      
      paste("Good job, this is correct (because the interventions provide the same number of QALYs)!")
      
    } else {
      paste("This is not the correct answer, try again! Hint: QALYs = quality of life (utility) x length of life")
    }
    
  })
  
  
  output$TEXT_QALY_comp <- renderText({
    text_button_QALY_comp()
  })
  
  # Assignment 4 ----
  output$EQ5D_calc <- renderText({ 

   paste("The estimated utility value is:",
         estimate_utility(mo = as.numeric(as.character(input$mo)),
                            ua = as.numeric(as.character(input$ua)),
                            sc = as.numeric(as.character(input$sc)),
                            pd = as.numeric(as.character(input$pd)),
                            ad = as.numeric(as.character(input$ad)),
                    tbl_decrements = NL_tariff))
 })
   
   output$VAS <- renderText({
     paste("Your health today is", input$vas)
   })
   
   # Assignment 5 ----
   output$tbl_NL_tariff <- renderTable({
     NL_tariff
   }, digits = 3)
   
   
   text_button_EQ5D_calc <- eventReactive(input$check_EQ5D_calc, {
     if(as.numeric(as.character(input$res_EQ5D_1)) ==  round(estimate_utility(1, 2, 4, 2, 3), 3) &
        as.numeric(as.character(input$res_EQ5D_2)) ==  round(estimate_utility(2, 5, 4, 3, 1), 3) &
        as.numeric(as.character(input$res_EQ5D_3)) ==  round(estimate_utility(1, 1, 1, 2, 2), 3)
        ){
       paste("Good job, these are the correct answers")
     } else {
       paste("Sorry, at least one answer is not correct... try again! Hint: did you forget to use the constant?")
     }
     
   })
   
   output$res_check_EQ5D <- renderText({
     text_button_EQ5D_calc()
   })
   
   # Assignment 6 ----
   ##hs1
   output$plot_sg_hs1 <- renderPlot({
     p <- input$sg_hs1
     
     v_names <- c("Current state", "Gamble", "Death", "Perfect health", "Remain in state")
     M <- matrix(nrow = 5, ncol = 5, byrow = TRUE, data = 0)
     M[5, 1] <- ""
     M[2, 1] <- ""
     M[3, 2] <- p
     M[4, 2] <- 1-p
     plotmat(M, pos = c(1, 1, 3), name = v_names, lwd = 1,
             curve = 0,
             box.lwd = 2, cex.txt = 1.2, box.size = 0.05,
             box.type = "square", box.prop = 0.5
             )
     
   })
   output$res_sg_hs1 <- renderText({
     paste0("You are considering a gamble with a ", round(input$sg_hs1*100,0), "% probability of immediate death. Your utility value for this health state is ",  1-input$sg_hs1, ".")
   })
   
   
   output$plot_tto_hs1 <- renderPlot({
     
     trade_off <- input$tto_hs1
     data <- data.frame(TTO = c(0, 10-trade_off, trade_off),
                        No_TTO = c(10, 0, 0)
     )
     
     barplot(as.matrix(data), col = c(4, 3, 2),
             legend.text = c("Current Health state",
                             "Healthy life years",
                             "Traded life years"),
             horiz = TRUE)
     
   })
   
   output$res_tto_hs1 <- renderText({
     paste0("You are indifferent between a) trading ", round(input$tto_hs1, 1), " years of life and living ", 10 - round(input$tto_hs1, 1),  " years of life in perfect health and b) living in this health state for 10 years. Your utility value for this health state is ",  (10 - input$tto_hs1)/10, ".")
   })
   
   output$res_vas_hs1 <- renderText({
     paste0("The quality of life (utility) you assign to this health state is ", input$vas_hs1, ".")
   })
   
   ##hs2
   output$plot_sg_hs2 <- renderPlot({
     p <- input$sg_hs2
     
     v_names <- c("Current state", "Gamble", "Death", "Perfect health", "Remain in state")
     M <- matrix(nrow = 5, ncol = 5, byrow = TRUE, data = 0)
     M[5, 1] <- ""
     M[2, 1] <- ""
     M[3, 2] <- p
     M[4, 2] <- 1-p
     plotmat(M, pos = c(1, 1, 3), name = v_names, lwd = 1,
             curve = 0,
             box.lwd = 2, cex.txt = 1.2, box.size = 0.05,
             box.type = "square", box.prop = 0.5
     )
     
   })
   output$res_sg_hs2 <- renderText({
     paste0("You are considering a gamble with a ", round(input$sg_hs2*100,0), "% probability of immediate death. Your utility value for this health state is ",  1-input$sg_hs2, ".")
   })
   
   
   output$plot_tto_hs2 <- renderPlot({
     
     trade_off <- input$tto_hs2
     data <- data.frame(TTO = c(0, 10-trade_off, trade_off),
                        No_TTO = c(10, 0, 0)
     )
     
     barplot(as.matrix(data), col = c(4, 3, 2),
             legend.text = c("Current Health state",
                             "Healthy life years",
                             "Traded life years"),
             horiz = TRUE)
     
   })
   
   output$res_tto_hs2 <- renderText({
     paste0("You are indifferent between a) trading ", round(input$tto_hs2, 1), " years of life and living ", 10 - round(input$tto_hs2, 1),  " years of life in perfect health and b) living in this health state for 10 years. Your utility value for this health state is ",  (10 - input$tto_hs2)/10, ".")
   })
   
   output$res_vas_hs2 <- renderText({
     paste0("The quality of life (utility) you assign to this health state is ", input$vas_hs2, ".")
   })
   
   ##hs3
   output$plot_sg_hs3 <- renderPlot({
     p <- input$sg_hs3
     
     v_names <- c("Current state", "Gamble", "Death", "Perfect health", "Remain in state")
     M <- matrix(nrow = 5, ncol = 5, byrow = TRUE, data = 0)
     M[5, 1] <- ""
     M[2, 1] <- ""
     M[3, 2] <- p
     M[4, 2] <- 1-p
     plotmat(M, pos = c(1, 1, 3), name = v_names, lwd = 1,
             curve = 0,
             box.lwd = 2, cex.txt = 1.2, box.size = 0.05,
             box.type = "square", box.prop = 0.5
     )
     
   })
   output$res_sg_hs3 <- renderText({
     paste0("You are considering a gamble with a ", round(input$sg_hs3*100,0), "% probability of immediate death. Your utility value for this health state is ",  1-input$sg_hs3, ".")
   })
   
   
   output$plot_tto_hs3 <- renderPlot({
     
     trade_off <- input$tto_hs3
     data <- data.frame(TTO = c(0, 10-trade_off, trade_off),
                        No_TTO = c(10, 0, 0)
     )
     
     barplot(as.matrix(data), col = c(4, 3, 2),
             legend.text = c("Current Health state",
                             "Healthy life years",
                             "Traded life years"),
             horiz = TRUE)
     
   })
   
   output$res_tto_hs3 <- renderText({
     paste0("You are indifferent between a) trading ", round(input$tto_hs3, 1), " years of life and living ", 10 - round(input$tto_hs3, 1),  " years of life in perfect health and b) living in this health state for 10 years. Your utility value for this health state is ",  (10 - input$tto_hs3)/10, ".")
   })
   
   output$res_vas_hs3 <- renderText({
     paste0("The quality of life (utility) you assign to this health state is ", input$vas_hs3, ".")
   })
   
   ##v1
   output$plot_sg_v1 <- renderPlot({
     p <- input$sg_v1
     
     v_names <- c("Current state", "Gamble", "Death", "Perfect health", "Remain in state")
     M <- matrix(nrow = 5, ncol = 5, byrow = TRUE, data = 0)
     M[5, 1] <- ""
     M[2, 1] <- ""
     M[3, 2] <- p
     M[4, 2] <- 1-p
     plotmat(M, pos = c(1, 1, 3), name = v_names, lwd = 1,
             curve = 0,
             box.lwd = 2, cex.txt = 1.2, box.size = 0.05,
             box.type = "square", box.prop = 0.5
     )
     
   })
   output$res_sg_v1 <- renderText({
     paste0("You are considering a gamble with a ", round(input$sg_v1*100,0), "% probability of immediate death. Your utility value for this health state is ",  1-input$sg_v1, ".")
   })
   
   
   output$plot_tto_v1 <- renderPlot({
     
     trade_off <- input$tto_v1
     data <- data.frame(TTO = c(0, 30-trade_off, trade_off),
                        No_TTO = c(30, 0, 0)
     )
     
     barplot(as.matrix(data), col = c(4, 3, 2),
             legend.text = c("Current Health state",
                             "Healthy life years",
                             "Traded life years"),
             horiz = TRUE)
     
   })
   
   output$res_tto_v1 <- renderText({
     paste0("You are indifferent between a) trading ", round(input$tto_v1, 1), " years of life and living ", 30 - round(input$tto_v1, 1),  " years of life in perfect health and b) living in this health state for 30 years. Your utility value for this health state is ",  round((30 - input$tto_v1)/30, 2), ".")
   })
   
   output$res_vas_v1 <- renderText({
     paste0("The quality of life (utility) you assign to this health state is ", input$vas_v1, ".")
   })
   
   ##v2
   output$plot_sg_v2 <- renderPlot({
     p <- input$sg_v2
     
     v_names <- c("Current state", "Gamble", "Death", "Perfect health", "Remain in state")
     M <- matrix(nrow = 5, ncol = 5, byrow = TRUE, data = 0)
     M[5, 1] <- ""
     M[2, 1] <- ""
     M[3, 2] <- p
     M[4, 2] <- 1-p
     plotmat(M, pos = c(1, 1, 3), name = v_names, lwd = 1,
             curve = 0,
             box.lwd = 2, cex.txt = 1.2, box.size = 0.05,
             box.type = "square", box.prop = 0.5
     )
     
   })
   output$res_sg_v2 <- renderText({
     paste0("You are considering a gamble with a ", round(input$sg_v2*100,0), "% probability of immediate death. Your utility value for this health state is ",  1-input$sg_v2, ".")
   })
   
   
   output$plot_tto_v2 <- renderPlot({
     
     trade_off <- input$tto_v2
     data <- data.frame(TTO = c(0, 30-trade_off, trade_off),
                        No_TTO = c(30, 0, 0)
     )
     
     barplot(as.matrix(data), col = c(4, 3, 2),
             legend.text = c("Current Health state",
                             "Healthy life years",
                             "Traded life years"),
             horiz = TRUE)
     
   })
   
   output$res_tto_v2 <- renderText({
     paste0("You are indifferent between a) trading ", round(input$tto_v2, 1), " years of life and living ", 30 - round(input$tto_v2, 1),  " years of life in perfect health and b) living in this health state for 30 years. Your utility value for this health state is ",  round((30 - input$tto_v2)/30, 2), ".")
   })
   
   output$res_vas_v2 <- renderText({
     paste0("The quality of life (utility) you assign to this health state is ", input$vas_v2, ".")
   })
   
   
}

# Run the app ----
shinyApp(ui = ui, server = server)
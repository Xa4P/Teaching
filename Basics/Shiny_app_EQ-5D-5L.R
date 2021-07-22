############################
#### Shiny app EQ-5D-5L ####
############################

# Set up
rm(list= ls())
NL_tariff <- read.csv(paste(getwd(), "/Basics/EQ5D5L_NLtariff.csv", sep = ""), sep = ";")# load table decrements
library(shiny)
library(tidyverse)
#install.packages('plotly')
#library(plotly)


# Load function
source(paste(getwd(),"/Basics/EQ-5D-5L_fct.R", sep= ""))
source(paste(getwd(),"/Basics/function_sf_36.R", sep= ""))

# Define UI ----
ui <- fluidPage(
  tabsetPanel(
    # Assignment 1 - SF-36 ----
    tabPanel("Assignment 1 - SF-36", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(h2("In this assignment you will experience what it is like to fill out the SF-36 Health Survey, a descriptive health-related quality of life instrument.
You will start by filling in the SF-36 questionnaire. Subsequently you will enter your individual responses using the button below. These answers will automatically be rescored in the 8 sub-scales of the SF-36. Below a table and a graph will be drawn with your scores ('Respondent’). In these, the scores of a group of patients with migraine and a healthy control group are also displayed for comparison purposes."),
               helpText("This is an example of the Short-Form 36. Instructions for completing the questionnaire: Please answer every question. Some questions may look like others, but each
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
                            h3("Fill in the utility values for each period of time in the boxes below, calculate the total number of QALY gained (the graph can help you to do so) and check your answer by pushing the button"),
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
    tabPanel("EQ-5D-5L", fluid = TRUE,
             sidebarLayout(
             sidebarPanel(helpText("This tab of the application is based on the sample version of the EQ-5D-5L that is available at https://euroqol.org/eq-5d-instruments/sample-demo/.Under each heading, please tick the ONE box that best describes your health TODAY."),
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
                              selected = 1)
                 ),
             mainPanel(
               h1("EQ-5D-5L utility estimate"),
               textOutput("EQ5D_calc")
               )
             )
             ),
    tabPanel("EQ-5D-5L VAS", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(helpText("We would like to know how good or bad your health is TODAY. This scale is numbered from 0 to 100. 100 means the best health you can imagine. 0 means the worst health you can imagine. Please slide the button to indicate how your health is TODAY. Now, look at the number you marked on the scale on the right. (In the real EQ-5D questionnaire, the slide bar is vertical, not horizontal)"),
                            sliderInput("vas", "Your health today:", 
                                        min = 0, max = 1, value = 0.5, step= 0.01),
               ), 
               mainPanel(
                 h1("Utility estimate"),
                 textOutput("VAS")
                 )
               )
             )
    )
)

# Define server logic ----
server <- function(input, output) {
  
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
}

# Run the app ----
shinyApp(ui = ui, server = server)
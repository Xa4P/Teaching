############################
#### Shiny app EQ-5D-5L ####
############################

# Set up
rm(list= ls())
NL_tariff <- read.csv(paste(getwd(), "/Basics/EQ5D5L_NLtariff.csv", sep = ""), sep = ";")# load table decrements
library(shiny)
#install.packages('plotly')
#library(plotly)


# Load function
source(paste(getwd(),"/Basics/EQ-5D-5L_fct.R", sep= ""))

# Define UI ----
ui <- fluidPage(
  tabsetPanel(
    tabPanel("EQ-5D-5L", fluid = TRUE,
             sidebarLayout(
             sidebarPanel(helpText("This Shiny application is based on the sample version that is available at https://euroqol.org/eq-5d-instruments/sample-demo/.Under each heading, please tick the ONE box that best describes your health TODAY."),
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
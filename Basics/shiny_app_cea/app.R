###############################################
#### Shiny app health economic evaluations ####
###############################################

# Set up
rm(list= ls())
library(shiny)
library(tidyverse)

# Load function
source("fct_bootstrap.R")

# Define UI ----
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Welcome", fluid = TRUE,
             fluidPage(
               column(width = 12,
                      h2("Welcome to the assignment on performing cost-effectiveness analyses.")
               )
             )
             ),
    # Assignment 1 - Trial-based CEA ----
    tabPanel("Assignment 1 - Trial-based CEA", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 h3("This part of the assignment focuses on bootstrapping the outcomes of the analysis you performed in R."),
                 h3("Instructions: upload your data. Determine the number of the random seed to use. Perform bootstrapping by pushing the button. Answer the questions of the assignment."),
                 fileInput(inputId = "file1", "Choose CSV File",
                           multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 tags$hr(),
                 numericInput(inputId = "num_it",
                              label = "Number of bootstrap samples",
                              value = 0,
                              min = 0,
                              max = 20000),
                 numericInput(inputId = "seed_num",
                              label = "Seed number to use",
                              value = 1,
                              min = 0,
                              max = Inf),
                 actionButton(inputId = "run_boot",
                              label = "Run bootstrap analysis")
                 ),
               mainPanel(
                 h3("Incremental cost-effectiveness plane"),
                 plotOutput("boot_ice"),
                 tags$hr(),
                 h3("Mean outcomes of the bootstrap and 95% CI"),
                 tableOutput("boot_tbl"),
                 tags$hr(),
                 h3("Percentage iterations in each quadrant"),
                 tableOutput("perc_tbl"),
                 tags$hr(),
                 h3("Mean outcomes of the sample"),
                 tableOutput("det_tbl")
                 )
             )
             
    ),
    # Assignment 3 - Transition matrix ----
    tabPanel("Assignment 3 - Transition matrix", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 h3("This part of the assignment illustrates the functioning of the transition matrix"),
                 h4("Instructions: Fill in the transition probabilities that you have used to define your transition matrix. The transition matrix is automatically completed based on your input. You can thus use it to check your own transition matrix. For these calculations, we assume that all persons start in the 'Healthy' health state."),
                 h4("The transition matrices to calculate the health state distribution after 2, 3, or N cycles is obtained by multiplying the transition matrix by itself 2, 3, and N times respectively. The health state distribution after 2, 3, and N cycles is obtained by multiplying these transition matrices by the start position."),
                 numericInput(inputId = "p_NewAneurysm",
                              label = "Probability of new aneurysm",
                              value = 0,
                              min = 0,
                              max = 1),
                 numericInput(inputId = "p_AneurysmDetection",
                              label = "Probability aneurysm is detected",
                              value = 0,
                              min = 0,
                              max = 1),
                 numericInput(inputId = "p_TreatmentIsFatal",
                              label = "Probability aneurysm's treatment is fatal",
                              value = 0,
                              min = 0,
                              max = 1),
                 numericInput(inputId = "p_DeathOther",
                              label = "Probability of death from other causes",
                              value = 0,
                              min = 0,
                              max = 1),
                 tags$hr(),
                 h4("Instructions: To obtain the health state distribution after N cycle, fill in a number of cycle (0-100)"),
                 numericInput(inputId = "n_cycle",
                              label = "Health state distribution after:",
                              value = 2,
                              min = 2,
                              max = 100),
                 actionButton(inputId = "calc_m_tp_n",
                              label = "Calculate matrix with after N number of cycles"),
               ),
               mainPanel(
                 h3("Transition matrix after 1 cycle"),
                 tableOutput("m_tp_1"),
                 h3("Health state distribution after 1 cycle"),
                 tableOutput("t_dist_1"),
                 tags$hr(),
                 h3("Transition matrix after 2 cycles"),
                 tableOutput("m_tp_2"),
                 h3("Health state distribution after 2 cycles"),
                 tableOutput("t_dist_2"),
                 tags$hr(),
                 h3("Transition matrix after 3 cycles"),
                 tableOutput("m_tp_3"),
                 h3("Health state distribution after 3 cycles"),
                 tableOutput("t_dist_3"),
                 tags$hr(),
                 h3(textOutput("txt_m_tp_n")),
                 tableOutput("m_tp_n"),
                 h3(textOutput("txt_dist_tp_n")),
                 tableOutput("t_dist_n"),
                 
                 tags$hr()
               )
             )
             
    )
    )
  )

# Define server logic ----
server <- function(input, output) {
  
  # Assignment 1 ----
  bootstrap <- eventReactive(input$run_boot, {
    
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    
    set.seed(as.numeric(as.character(input$seed_num)))
    
    df_res <- bootstrap_effects_costs(df, num_it = as.numeric(as.character(input$num_it)))
    
   return(df_res)
      
  }
  )
  
  
  output$boot_ice <- renderPlot({
    df_res <- bootstrap()
    
    ggplot(df_res) + 
      #ggtitle("Incremental cost-effectiveness plane") +
      geom_point(aes(x = Inc_Qol, y = Inc_Cost, colour = "Increments"), shape = 1) + 
      geom_point(aes(x = mean(Inc_Qol), y = mean(Inc_Cost), colour = "Mean"), size = 2) +
      xlab ("Incremental effects") + 
      ylab("Incremental costs") +
      geom_hline(yintercept = 0,  
                 color = "black") +
      geom_vline(xintercept = 0,  
                 color = "black") + 
      geom_abline(intercept = 0, slope = 20000, linetype= "dashed", 
                  color = "black") + # 20,000 per QALY threshold line
      #xlim(c(0,1)) +
      #scale_y_continuous(labels = dollar_format(prefix = "\u20ac ", suffix = "")) +
      scale_colour_manual(name = "",
                          values = c(Increments = "grey", 
                                     Mean = "darkblue"
                          )) +
      theme_bw() + 
      theme(legend.position="bottom") 
    
    
  })
  
  output$boot_tbl<- renderTable({
    
    df_res <- bootstrap()
    
    # Compute summary statistics
    df <- data.frame(cbind(
      Estimate = c(names(df_res), "ICER"),
      Mean = round(c(unname(colSums(df_res)/nrow(df_res)), mean(df_res$Inc_Cost) / mean(df_res$Inc_Qol)), 2),
      `Lower bound 95%CI` = c(round(apply(df_res, 2, function (x) quantile(x, 0.025)), 2), "-"),
      `Higher bound 95%CI` = c(round(apply(df_res, 2, function (x) quantile(x, 0.975)), 2), "-")
    )
    )
    
    df
  })
  
  output$perc_tbl<- renderTable({
    
    df_res <- bootstrap()
    
    # Compute percentage in each quadrant statistics
    df_perc <- data.frame(cbind(
      Quandrant = c("NorthEast (more effective, more expensive)", "SouthEast (more effective, less expensive)",
                    "SouthWest (less effective, less expensive)", "NorthWest (less effective, more expensive)"),
      `Percentage iterations` = c(paste0(round(length(which(df_res$Inc_Qol > 0 &
                                                              df_res$Inc_Cost > 0)) / nrow(df_res),2)*100, "%"),
                                  paste0(round(length(which(df_res$Inc_Qol > 0 &
                                                              df_res$Inc_Cost < 0)) / nrow(df_res),2)*100, "%"),
                                  paste0(round(length(which(df_res$Inc_Qol < 0 &
                                                              df_res$Inc_Cost < 0)) / nrow(df_res),2)*100, "%"),
                                  paste0(round(length(which(df_res$Inc_Qol < 0 &
                                                              df_res$Inc_Cost > 0)) / nrow(df_res),2)*100, "%")
      )
    ))
    df_perc
 
  }
)
  
data <- eventReactive(input$run_boot,{
    req(input$file1)
    read.csv(input$file1$datapath)
    }
    )
  
output$det_tbl<- renderTable({
  
  df_res <- data()
  
  # Compute summary statistics - sample
  res_tbl <- df_res %>%
    group_by(Procedure) %>%
    summarise(Mean_Qol = round(mean(Qol), 2),
              Mean_Total_Cost = round(mean(Total_costs), 2),
    )
  res_tbl <- cbind(res_tbl,
                   Inc_Qol = c("", round(as.numeric(as.character(res_tbl$Mean_Qol[2])) - as.numeric(as.character(res_tbl$Mean_Qol[1])), 2)),
                   Inc_Cost = c("", round(as.numeric(as.character(res_tbl$Mean_Total_Cost[2])) - as.numeric(as.character(res_tbl$Mean_Total_Cost[1])), 0)),
                   ICER = c("", round((as.numeric(as.character(res_tbl$Mean_Total_Cost[2])) - as.numeric(as.character(res_tbl$Mean_Total_Cost[1]))) / (as.numeric(as.character(res_tbl$Mean_Qol[2])) - as.numeric(as.character(res_tbl$Mean_Qol[1]))), 0))
  )
  res_tbl
  })

## Assignment 3 ----

mat_tp <- function() {
  v_names_hs <- c("Healthy", "NewAneurysm",	"DetectedAneurysm", "DeathOther",	"DeathTreatment")
  m_tp <- matrix(0, 
               ncol = 5,
               nrow = 5,
               dimnames = list(v_names_hs,
                               v_names_hs))

  m_tp["Healthy", "Healthy"]     <- 1- input$p_NewAneurysm - input$p_DeathOther # Example
  m_tp["Healthy", "DeathOther"]  <- input$p_DeathOther
  m_tp["Healthy", "NewAneurysm"] <- input$p_NewAneurysm
  m_tp["NewAneurysm", "DetectedAneurysm"] <- input$p_AneurysmDetection
  m_tp["NewAneurysm", "DeathOther"] <- input$p_DeathOther
  m_tp["NewAneurysm", "NewAneurysm"] <- 1 - input$p_AneurysmDetection - input$p_DeathOther
  m_tp["DetectedAneurysm", "Healthy"] <- 1 - input$p_TreatmentIsFatal
  m_tp["DetectedAneurysm", "DeathOther"] <- input$p_TreatmentIsFatal
  m_tp["DeathOther", "DeathOther"] <- 1
  m_tp["DeathTreatment", "DeathTreatment"] <- 1

  return(m_tp)
  }

output$m_tp_1 <- renderTable({
  cbind(` `=colnames(mat_tp()), mat_tp())
  
})

output$t_dist_1 <- renderTable({
  c(1,0,0,0,0) %*% mat_tp() 
})

output$m_tp_2 <- renderTable({
  m_tp_2 <- mat_tp() %*% mat_tp() 
  
  cbind(` `=colnames(mat_tp()), m_tp_2)
})

output$t_dist_2 <- renderTable({
  c(1,0,0,0,0) %*% (mat_tp() %*% mat_tp())
})

output$m_tp_3 <- renderTable({
  m_tp_3 <- (mat_tp() %*% mat_tp() %*% mat_tp()) 
  
  cbind(` `=colnames(mat_tp()), m_tp_3)
})

output$t_dist_3 <- renderTable({
  c(1,0,0,0,0) %*% (mat_tp() %*% mat_tp() %*% mat_tp())
})

mat_tp_n <- eventReactive(input$calc_m_tp_n, {
  for (i in 2:input$n_cycle) {
    if(i == 2) {
      m_tp_n <- mat_tp() %*% mat_tp()
    } else {
      m_tp_n <- m_tp_n %*% mat_tp()
    }
  }
  
  return(m_tp_n)
}
                          
)

output$m_tp_n <- renderTable({
  cbind(` `= colnames(mat_tp()), round(mat_tp_n(),3))
})

output$t_dist_n <- renderTable({
  c(1,0,0,0,0) %*% (round(mat_tp_n(), 3))
})

output$txt_m_tp_n <- renderText({
  paste("Transition matrix after", input$n_cycle ,"cycles", sep = " ")
})

output$txt_dist_tp_n <- renderText({
  paste("Health state distribution after", input$n_cycle ,"cycles", sep = " ")
})


}

# Run the app ----
shinyApp(ui = ui, server = server)
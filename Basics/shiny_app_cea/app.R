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

}

# Run the app ----
shinyApp(ui = ui, server = server)
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
                 tags$hr(),
                 h4("Health state distribution at start"),
                 numericInput(inputId = "n_Healthy",
                              label = "Proportion of individuals starting in health state: 'Healthy'",
                              value = 1,
                              min = 0,
                              max = 1),
                 numericInput(inputId = "n_NewAneurysm",
                              label = "Proportion of individuals starting in health state: 'New aneurysm'",
                              value = 0,
                              min = 0,
                              max = 1),
                 numericInput(inputId = "n_AneurysmDetection",
                              label = "Proportion of individuals starting in health state: 'Aneurysm detected'",
                              value = 0,
                              min = 0,
                              max = 1)
                 
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
             
    ),
    # Assignment 3 - Cohort simulation ----
    tabPanel("Assignment 3 - Cohort simulation", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 h3("This part of the assignment illustrates the functioning of the cohort simulation"),
                 h4("Instructions: Fill in the transition probabilities that you have used to define your transition matrix. The transition matrix is automatically completed based on your input. You can thus use it to check your own transition matrix. For these calculations, we assume that all persons start in the 'Healthy' health state."),
                 h4("You can use the following application to answer the questions 4.f. and 4.g. (or to check your answers)"),
                 h4("The left panel shows a plot of the health state membership during each cycle, the cohort simulation, and the cumulative number of months accrued in the 'Alive' health states during the cohort simulation"),
                 h4("Transition probabilities"),
                 numericInput(inputId = "p_NewAneurysm_2",
                              label = "Probability of new aneurysm",
                              value = 0,
                              min = 0,
                              max = 1),
                 numericInput(inputId = "p_AneurysmDetection_2",
                              label = "Probability aneurysm is detected",
                              value = 0,
                              min = 0,
                              max = 1),
                 numericInput(inputId = "p_TreatmentIsFatal_2",
                              label = "Probability aneurysm's treatment is fatal",
                              value = 0,
                              min = 0,
                              max = 1),
                 numericInput(inputId = "p_DeathOther_2",
                              label = "Probability of death from other causes",
                              value = 0,
                              min = 0,
                              max = 1),
                 tags$hr(),
                 h4("Health state distribution at start"),
                 numericInput(inputId = "n_Healthy_2",
                              label = "Proportion of individuals starting in health state: 'Healthy'",
                              value = 1,
                              min = 0,
                              max = 1),
                 numericInput(inputId = "n_NewAneurysm_2",
                              label = "Proportion of individuals starting in health state: 'New aneurysm'",
                              value = 0,
                              min = 0,
                              max = 1),
                 numericInput(inputId = "n_AneurysmDetection_2",
                              label = "Proportion of individuals starting in health state: 'Aneurysm detected'",
                              value = 0,
                              min = 0,
                              max = 1)
                 
               ),
               mainPanel(
                 h3("Plot cohort simulation"),
                 plotOutput("plot_cohort"),
                 h3("Cohort simulation"),
                 tableOutput("tbl_cohort"),
                 tags$hr(),
                 h3("Cumulative number of person-months over the 3 years"),
                 tableOutput("tbl_cum"),
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

## Assignment 3 - Transition matrix ----

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
  m_tp["DetectedAneurysm", "DeathTreatment"] <- input$p_TreatmentIsFatal
  m_tp["DeathOther", "DeathOther"] <- 1
  m_tp["DeathTreatment", "DeathTreatment"] <- 1

  return(m_tp)
  }

v_start <- function() {c(input$n_Healthy,
                         input$n_NewAneurysm,
                         input$n_AneurysmDetection,
                         0,
                         0)}

output$m_tp_1 <- renderTable({
  cbind(` `=colnames(mat_tp()), mat_tp())
  
})

output$t_dist_1 <- renderTable({
  v_start() %*% mat_tp() 
})

output$m_tp_2 <- renderTable({
  m_tp_2 <- mat_tp() %*% mat_tp() 
  
  cbind(` `=colnames(mat_tp()), m_tp_2)
})

output$t_dist_2 <- renderTable({
  v_start() %*% (mat_tp() %*% mat_tp())
})

output$m_tp_3 <- renderTable({
  m_tp_3 <- (mat_tp() %*% mat_tp() %*% mat_tp()) 
  
  cbind(` `=colnames(mat_tp()), m_tp_3)
})

output$t_dist_3 <- renderTable({
  v_start() %*% (mat_tp() %*% mat_tp() %*% mat_tp())
})

output$m_tp_n <- renderTable({
  
  for (i in 2:input$n_cycle) {
    if(i == 2) {
      m_tp_n <- mat_tp() %*% mat_tp()
    } else {
      m_tp_n <- m_tp_n %*% mat_tp()
    }
  }
  
  cbind(` `= colnames(mat_tp()), round(m_tp_n,3))
})

output$t_dist_n <- renderTable({
  
  for (i in 2:input$n_cycle) {
    if(i == 2) {
      m_tp_n <- mat_tp() %*% mat_tp()
    } else {
      m_tp_n <- m_tp_n %*% mat_tp()
    }
  }
  
  v_start() %*% m_tp_n
}, digits = 3)

output$txt_m_tp_n <- renderText({
  
  
  paste("Transition matrix after", input$n_cycle ,"cycles", sep = " ")
})

output$txt_dist_tp_n <- renderText({
  paste("Health state distribution after", input$n_cycle ,"cycles", sep = " ")
})


## Assignment 3 - Cohort simulation ----

m_hs_fct <- function() {
  v_names_hs <- c("Healthy", "NewAneurysm",	"DetectedAneurysm", "DeathOther",	"DeathTreatment")
  m_tp <- matrix(0, 
                 ncol = 5,
                 nrow = 5,
                 dimnames = list(v_names_hs,
                                 v_names_hs))
  
  m_tp["Healthy", "Healthy"]     <- 1- input$p_NewAneurysm_2 - input$p_DeathOther_2 # Example
  m_tp["Healthy", "DeathOther"]  <- input$p_DeathOther_2
  m_tp["Healthy", "NewAneurysm"] <- input$p_NewAneurysm_2
  m_tp["NewAneurysm", "DetectedAneurysm"] <- input$p_AneurysmDetection_2
  m_tp["NewAneurysm", "DeathOther"] <- input$p_DeathOther_2
  m_tp["NewAneurysm", "NewAneurysm"] <- 1 - input$p_AneurysmDetection_2 - input$p_DeathOther_2
  m_tp["DetectedAneurysm", "Healthy"] <- 1 - input$p_TreatmentIsFatal_2
  m_tp["DetectedAneurysm", "DeathTreatment"] <- input$p_TreatmentIsFatal_2
  m_tp["DeathOther", "DeathOther"] <- 1
  m_tp["DeathTreatment", "DeathTreatment"] <- 1
  
  
  v_start <- c(input$n_Healthy_2 * 10000,
                             input$n_NewAneurysm_2 * 10000,
                             input$n_AneurysmDetection_2 * 10000,
                             0,
                             0)
  
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
  m_hs[1, ] <- v_start
  
  ## Perform the matrix multiplication to determine the state membership over the cycles.
  ## To determine the number of individuals in each health state during each cycle, one need to multiply the vector of state membership in the previous cycle by the transition matrix
  ## Example: to calculate the number of individuals in each state in cycle 1, multiply the state membership in cycle 0 by the transition matrix
  ## HINT: to do so, use a a for loop over rows 2 to 41 of the `m_hs` matrix
  
  for(cycle in 1:n_cycles){
    
    # For your matrix of health state
    m_hs[cycle + 1, ] <- m_hs[cycle, ] %*% m_tp # matrix multiplication
    
  }
  
  
  return(m_hs)
}


output$plot_cohort<- renderPlot({
  
  n_pt <- 10000
  v_names_hs <- c("Healthy", "NewAneurysm",	"DetectedAneurysm", "DeathOther",	"DeathTreatment")
  m_hs <- m_hs_fct()
  
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
  
  
})

output$tbl_cohort <- renderTable({
  cbind("Cycle" = c(0:abs(nrow(m_hs_fct())-1)), m_hs_fct())
  }, 
  digits = 0)

output$tbl_cum <- renderTable({
  m_hs <- m_hs_fct()
  
  n_months_Healthy <- round(cumsum(m_hs[, "Healthy"])[36])
  n_months_NewAneurysm <- round(cumsum(m_hs[, "NewAneurysm"])[36])
  n_months_DetectedAneurysm <- round(cumsum(m_hs[, "DetectedAneurysm"])[36])
  n_total <- sum(n_months_Healthy, n_months_NewAneurysm, n_months_DetectedAneurysm)
  
  return(cbind(n_months_Healthy,
               n_months_NewAneurysm,
               n_months_DetectedAneurysm,
               n_total))
  
}, 
digits = 0)



}

# Run the app ----
shinyApp(ui = ui, server = server)
###############################################
#### Shiny app health economic evaluations ####
###############################################

# Set up
rm(list= ls())
library(shiny)
library(tidyverse)

# Load function
source("fct_bootstrap.R")
source("fct_pa.R")

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
             
    ),
    # Assignment HSTM 2B ----
    tabPanel("Assignment HSTM 2B", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 h3("This part of the assignment illustrates how discounting and the starting age of the cohort of a health state transition model may affect the outcomes of the model."),
                 h4("Instructions: Modify the discount rates and starting age to investigate the impact it has on the results."),
                 numericInput(inputId = "r_disc_e",
                              label = "Annual discount rate for health effects",
                              value = 0.015,
                              min = 0,
                              max = 1),
                 numericInput(inputId = "r_disc_c",
                              label = "Annual discount rate for costs",
                              value = 0.04,
                              min = 0,
                              max = 1),
                 numericInput(inputId = "n_start_age",
                              label = "Start age of the cohort",
                              value = 45,
                              min = 20,
                              max = 95)
               ),
               mainPanel(
                 h3("Plot cohort simulation - 'No Aspirin'"),
                 plotOutput("plot_cohort_no_asp"),
                 h3("Plot cohort simulation - 'Aspirin'"),
                 plotOutput("plot_cohort_asp"),
                 tags$hr(),
                 h3("Cumulative effects"),
                 plotOutput("plot_cum_e"),
                 tags$hr(),
                 h3("Cumulative costs"),
                 plotOutput("plot_cum_c"),
                 tags$hr(),
                 h3("Undiscounted results per individual"),
                 tableOutput("tbl_res"),
                 tags$hr(),
                 h3("Discounted results per individual"),
                 tableOutput("tbl_res_d"),
                 tags$hr()
               )
             )
             
    ),
    # Assignment HSTM 2C ----
    tabPanel("Assignment HSTM 2C - inputs", fluid = TRUE,
             fluidPage(
               column(width = 12,
                 h3("This part of the assignment illustrates the principle of the probabilistic analysis"),
                 h4("First, the mean parameters (used for the deterministic analysis), their standard error and the distribution used for the probabilistic analysis are provided"),
                 h4("Second, the summary statistics of the probabilistic parameters are provided."),
                 h4("Third, the first 100 sets of parameters are shown, these are used in the first 100 iterations of the probabilistic analysis"),
                 h4("On the next page, you can explore the results the probabilistic analysis"),
                 h4("Instructions: Determine the number of simulation (and seed number to use) you want to run (at least 1,000) and push the button to run the probabilistic analysis. You can also modify the mean and standard error of the efficacy of aspirin and starting age of the cohort."),
                 h4("Follow the instruction of the assignment and answer the questions. Keep them for the discussion."),
                 numericInput(inputId = "n_sim_pa",
                              label = "Number of iteration to perform (between 1 and 10,000)",
                              value = 1000,
                              min = 1,
                              max = 10000),
                 numericInput(inputId = "seed_num_pa",
                              label = "Seed number to use to generate probabilistic parameters",
                              value = 1,
                              min = 1,
                              max = Inf),
                 numericInput(inputId = "mean_eff",
                              label = "Mean efficacy of apirin on the occurence of MI",
                              value = 0.60,
                              min = 0,
                              max = 1),
                 numericInput(inputId = "se_eff",
                              label = "Standard error of the mean efficacy of apirin on the occurence of MI",
                              value = 0.15,
                              min = 0,
                              max = 0.5),
                 numericInput(inputId = "n_start_age_pa",
                              label = "Starting age cohort for probabilistic analysis",
                              value = 45,
                              min = 20,
                              max = 95),
                 actionButton(inputId = "do_pa",
                              label = "Push to perform the Probabilistic Analysis!")
               ),
               column(width = 6,
                 h3("Mean parameters, standard error, and associated distribution"),
                 tableOutput("inputs_det"),
                 tags$hr()
               ), 
               column(width = 6,
                 h3("Summary statistics of probabilistic inputs"),
                 tableOutput("inputs_pa_summary"),
                 tags$hr()
               ),
               column(width = 12,
                 h3("All probabilistic inputs"),
                 tableOutput("inputs_pa")
               )
             )
             
    ),
    tabPanel("Assignment HSTM 2C - Results", fluid = TRUE,
             fluidPage(
               column(
                 width = 12,
                 h3("This tab shows the probabilistic results in the incremental cost-effectiveness plane and the cost-effectiveness acceptability curve.")
               ),
               column(
                 width = 6,
                 h3("Cost-effectiveness plane"),
                 plotOutput("ce_pa"),
                 tags$hr()
               ),
               column(
                 width = 6,
                 h3("Summary statistics of the probabilistic results -  QALY and costs"),
                 tableOutput("tbl_res_pa"),
                 tags$hr()
               ),
               column(
                 width = 7,
                 h3("Incremental cost-effectiveness plane"),
                 plotOutput("ice_pa"),
                 tags$hr()
               ),
               column(
                 width = 5,
                 h3("Incremental cost-effectiveness plane - summary statistics"),
                 tableOutput("tbl_ice_pa"),
                 tags$hr()
               ),
               column(
                 width = 6,
                 h3("Cost-effectiveness acceptability curve"),
                 plotOutput("ceac_pa"),
                 tags$hr()
               ),
               column(
                 width = 6,
                 h3("Cost-effectiveness acceptability curve - summary"),
                 tableOutput("tbl_ceac"),
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

# Assignment HSTM 2B ----
l_output_hstm <-  function(){
  
  # Setting parameters
  n_cycles <- 10 # number of cycles
  r_d_effects <- input$r_disc_e # annual discount rate, health effects
  r_d_costs <- input$r_disc_c # annual discount rate, costs
  v_names_hs <- c("Well", "Post-minor_stroke", "Post-major_stroke", "Post-MI", "Death_stroke", "Death_MI", "Death_other") # vector of names of health states
  n_hs <- length(v_names_hs) # number of health states
  n_ind <- 100000 # number of individuals to simulate  
  v_start_hs <- c(n_ind, 0, 0, 0, 0, 0, 0) # vector of starting position in the model  
  
  # Input parameters
  ## Rates & probabilities
  r_fatal_mi	<- 0.25 # rate fatal MI
  r_fatal_stroke	<- 0.3 # rate fatal stroke
  r_inc_mi <- 400 / 100000 # yearly incidence rate MI
  r_inc_stroke <- 50 / 100000 # yearly incidence rate stroke
  r_mort <- 650 / 100000 # yearly rate of death
  
  r_mort_age_dependent <- r_mort_age_dependent <-  0.95^(c(115:20)-19) # mortality rate (age dependent) --> FAKE for this exercise
  
  # Determine mortality probability for each age
  df_mort <- data.frame(cbind(age = c(20:115),
                              p_mort = 1 - exp(-r_mort_age_dependent)
  )
  )
  df_mort[nrow(df_mort), 2] <- 1 # Assumption that everybody dies at 
  #plot(df_mort[, 1], df_mort[,2], type = 'l')
  
  ## Treatment effectiveness
  eff_mi	<- 0.6 # Treatment effectiveness of Aspirin on the probability of experiencing a MI
  eff_stroke	<- 1.2 # Treatment effectiveness of Aspirin on the probability of experiencing a stroke
  
  ## Utility values 
  u_healthy	<- 1 # utility value health state: Well
  u_post_mi	<- 0.85 # utility value health state: Post-MI
  u_post_minor_stroke	<- 0.75 # utility value health state: Post-minor stroke
  u_post_major_stroke	<- 0.5 # utility value health state: Post-major stroke
  u_aspirin_use	<- 0.999 # utility value health state: Well when using aspiring
  
  ## Costs
  c_aspirin_use	<- 100 # yearly costs of using aspiring
  c_post_mi	<- 8000 # yearly costs after having experienced a NON-FATAL MI
  c_post_minor_stroke	<- 2000 # yearly costs after having experienced a NON-FATAL minor stroke
  c_post_major_stroke	<- 20000 # yearly costs after having experienced a NON_FATAL major stroke
  
  p_post_major_stroke	<- 235 / 1000 # probability to transit to "Post-major stroke" after a NON-FATAL stroke occured
  p_post_minor_stroke	<- 1 - p_post_major_stroke # probability to transit to "Post-minor stroke" after a NON-FATAL stroke occured
  
  # Define 3D array's
  
  # Define start age cohort
  n_start_age <- input$n_start_age
  
  # Create a vector containing the mortality probability values for the age `n_start_age` to `n_start_age`+9 and call it `v_p_mort`
  v_p_mort <- df_mort[c(which(df_mort$age == n_start_age):
                          which(df_mort$age == n_start_age + 9)
  ), "p_mort"]
  
  # Initialise array's
  a_tp_comp <- array(0, dim = c(n_hs, n_hs, n_cycles),
                     dimnames = list(v_names_hs, v_names_hs, 1:n_cycles)
  )
  
  # Fill in the arrays using the background mortality for each age 
  a_tp_comp["Well", "Well", ]   <- 1 - v_p_mort - r_inc_mi - r_inc_stroke  # EXAMPLE! Calculate the remaining transition probabilities
  ##Notice, that only using the first 2 elements of the array will fill these transition probabilities for all 10 transition matrices used in all cycles
  
  a_tp_comp["Well", "Post-minor_stroke", ] <- r_inc_stroke * (1 - r_fatal_stroke) * p_post_minor_stroke
  a_tp_comp["Well", "Post-major_stroke", ] <- r_inc_stroke * (1 - r_fatal_stroke) * p_post_major_stroke
  a_tp_comp["Well", "Post-MI", ] <- r_inc_mi * (1 - r_fatal_mi)
  a_tp_comp["Well", "Death_stroke", ] <- r_inc_stroke * r_fatal_stroke
  a_tp_comp["Well", "Death_MI", ] <- r_inc_mi * r_fatal_mi
  a_tp_comp["Well", "Death_other", ] <- v_p_mort
  
  a_tp_comp["Post-minor_stroke", "Post-minor_stroke", ] <- 1 - v_p_mort
  a_tp_comp["Post-minor_stroke", "Death_other", ] <- v_p_mort
  
  a_tp_comp["Post-major_stroke", "Post-major_stroke", ] <- 1 - v_p_mort
  a_tp_comp["Post-major_stroke", "Death_other", ] <- v_p_mort
  
  a_tp_comp["Post-MI", "Post-MI", ] <- 1 - v_p_mort
  a_tp_comp["Post-MI", "Death_other", ] <- v_p_mort
  
  a_tp_comp["Death_stroke", "Death_stroke",] <- 1
  a_tp_comp["Death_MI", "Death_MI",] <- 1
  a_tp_comp["Death_other", "Death_other",] <- 1
  
  # Make a copy of `a_tp_comp` and call it `a_tp_int`
  a_tp_int <- a_tp_comp
  
  # Modify `a_tp_int` since Aspirin has an 
  a_tp_int["Well", "Well", ]   <- 1 - v_p_mort - r_inc_mi * eff_mi - r_inc_stroke * eff_stroke
  a_tp_int["Well", "Post-minor_stroke", ] <- r_inc_stroke * eff_stroke * (1 - r_fatal_stroke) * p_post_minor_stroke
  a_tp_int["Well", "Post-major_stroke", ] <- r_inc_stroke * eff_stroke * (1 - r_fatal_stroke) * p_post_major_stroke
  a_tp_int["Well", "Post-MI", ] <- r_inc_mi * eff_mi * (1 - r_fatal_mi)
  a_tp_int["Well", "Death_stroke", ] <- r_inc_stroke * eff_stroke * r_fatal_stroke
  a_tp_int["Well", "Death_MI", ] <- r_inc_mi * eff_mi * r_fatal_mi
  
  
  # Create matrices to store the cohort simulations
  m_hs_comp <- m_hs_int <- matrix(0,
                      ncol = length(v_names_hs),
                      nrow = n_cycles + 1,
                      dimnames = list(c(0:n_cycles),
                                      v_names_hs)
  ) 
  
  
  
  # Define the start position of individuals (all in "well")
  m_hs_comp[1,] <-  m_hs_int[1,] <- v_start_hs
  
  # Perform cohort simulation
  
  # Perform the matrix multiplication using the 3D array.
  for(cycle in 1:n_cycles){
    m_hs_comp[cycle + 1,] <- m_hs_comp[cycle,] %*% a_tp_comp[, , cycle]
    m_hs_int[cycle + 1,]  <- m_hs_int[cycle,] %*% a_tp_int[, , cycle]
    
    
  }
  
  # Calculate outcomes
  
  # Life years
  v_ly_comp <-  v_ly_int <- c("Well" = 1,
                 "Post-minor_stroke" = 1, 
                 "Post-major_stroke" = 1, 
                 "Post-MI" = 1, 
                 "Death_stroke" = 0, 
                 "Death_MI" = 0, 
                 "Death_other" = 0)
 
  ## Determine the number of life year gained over the cycles (reward at the end of the cycle!)
  v_t_ly_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_ly_comp
  v_t_ly_int  <- m_hs_int[2:nrow(m_hs_int),] %*% v_ly_int
  
  ## Determine the cumulative number of life year gained over the cycles (reward at the end of the cycle!)
  v_cum_ly_comp <- cumsum(v_t_ly_comp)
  v_cum_ly_int  <- cumsum(v_t_ly_int)
  
  ## Determine the total number of life year gained (sum of all cycles; reward at the end of the cycle!)
  n_t_ly_comp <- sum(v_t_ly_comp)
  n_t_ly_int <- sum(v_t_ly_int)
  
  # QALY's
  ## Determine the number of QALYs won by 1 individual during 1 cycle
  v_qaly_comp <- c("Well" = u_healthy,
                   "Post-minor_stroke" = u_post_minor_stroke, 
                   "Post-major_stroke" = u_post_major_stroke, 
                   "Post-MI" = u_post_mi, 
                   "Death_stroke" = 0, 
                   "Death_MI" = 0, 
                   "Death_other" = 0)
  
  v_qaly_int <- c("Well" = u_aspirin_use,
                  "Post-minor_stroke" = u_post_minor_stroke, 
                  "Post-major_stroke" = u_post_major_stroke, 
                  "Post-MI" = u_post_mi, 
                  "Death_stroke" = 0, 
                  "Death_MI" = 0, 
                  "Death_other" = 0)
  
  ## Determine the number of QALYs gained over the cycles (reward at the end of the cycle!)
  v_t_qaly_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_qaly_comp
  v_t_qaly_int  <- m_hs_int[2:nrow(m_hs_int),] %*% v_qaly_int
  
  ## Determine the cumulative number of QALYsr gained over the cycles (reward at the end of the cycle!)
  v_cum_qaly_comp <- cumsum(v_t_qaly_comp)
  v_cum_qaly_int <- cumsum(v_t_qaly_int)
  
  ## Determine the total number of QALYs gained (sum of all cycles; reward at the end of the cycle!)
  n_t_qaly_comp <- sum(v_t_qaly_comp)
  n_t_qaly_int  <- sum(v_t_qaly_int)
  
  # Costs
  ## Determine the costs accrued by 1 individual during 1 cycle
  v_c_comp <- c("Well" = 0,
                "Post-minor_stroke" = c_post_minor_stroke, 
                "Post-major_stroke" = c_post_major_stroke, 
                "Post-MI" = c_post_mi, 
                "Death_stroke" = 0, 
                "Death_MI" = 0, 
                "Death_other" = 0)
  
  v_c_int <- c("Well" = c_aspirin_use,
               "Post-minor_stroke" = c_post_minor_stroke, 
               "Post-major_stroke" = c_post_major_stroke, 
               "Post-MI" = c_post_mi, 
               "Death_stroke" = 0, 
               "Death_MI" = 0, 
               "Death_other" = 0)
  
  ## Determine the costs accrued over the cycles (reward at the end of the cycle!)
  v_t_c_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_c_comp
  v_t_c_int  <- m_hs_int[2:nrow(m_hs_int),] %*% v_c_int
  
  ## Determine the costs accrued over the cycles (reward at the end of the cycle!)
  v_cum_c_comp <- cumsum(v_t_c_comp)
  v_cum_c_int  <- cumsum(v_t_c_int)
  
  ## Determine the total costs accrued (sum of all cycles; reward at the end of the cycle!)
  n_t_c_comp <- sum(v_t_c_comp)
  n_t_c_int  <- sum(v_t_c_int)
  
  # Mean outcomes per individual
  df_res <- data.frame(cbind(Strategy = c("No aspirin", "Aspirin"),
        LY = round( c(n_t_ly_comp, n_t_ly_int) / n_ind, 3),
        QALY = round( c(n_t_qaly_comp, n_t_qaly_int) / n_ind, 3),
        COSTS = round( c(n_t_c_comp, n_t_c_int) / n_ind, 0),
        INC_QALY = c("-", round(((n_t_qaly_int - n_t_qaly_comp) / n_ind), 3)),
        INC_COSTS = c("-", round(((n_t_c_int - n_t_c_comp) / n_ind), 0)),
        ICER = c("-", round( ((n_t_c_int - n_t_c_comp) / n_ind) / ((n_t_qaly_int - n_t_qaly_comp) / n_ind), 0))
  ))
  
  # Calculate discounted results

  # Life years
  ## Define discount weights per cycle (years in this case)
  v_dw_e <- 1 / (1 + r_d_effects) ^ c(1:n_cycles)
  
  ## Total discounted life years, using matrix multiplication
  n_t_ly_comp_d <- t(v_t_ly_comp) %*% v_dw_e
  n_t_ly_int_d  <- t(v_t_ly_int) %*% v_dw_e
  
  # QALYs
  ## Total discounted life years, using matrix multiplication
  n_t_qaly_comp_d <- t(v_t_qaly_comp) %*% v_dw_e
  n_t_qaly_int_d  <- t(v_t_qaly_int) %*% v_dw_e
  
  # Costs
  ## Define discount weights per cycle (years in this case)
  v_dw_c <- 1 / (1 + r_d_costs) ^ c(1:n_cycles)
  
  ## Total discounted life years, using matrix multiplication
  n_t_c_comp_d <- t(v_t_c_comp) %*% v_dw_c
  n_t_c_int_d  <- t(v_t_c_int) %*% v_dw_c
  
  # Cumulative discounted effects and costs
  v_cum_ly_comp_d <- cumsum(v_t_ly_comp * v_dw_e)
  v_cum_ly_int_d <- cumsum(v_t_ly_int * v_dw_e)
  v_cum_qaly_comp_d <- cumsum(v_t_qaly_comp * v_dw_e)
  v_cum_qaly_int_d <- cumsum(v_t_qaly_int * v_dw_e)
  v_cum_c_comp_d <- cumsum(v_t_c_comp * v_dw_c)
  v_cum_c_int_d <- cumsum(v_t_c_int * v_dw_c)
  
  # Mean discounted outcomes per individual
  ## incrementals and ICER
  df_res_d <- data.frame(cbind(Strategy = c("No aspirin", "Aspirin"),
        LY = round( c(n_t_ly_comp_d, n_t_ly_int_d) / n_ind, 3),
        QALY = round( c(n_t_qaly_comp_d, n_t_qaly_int_d) / n_ind, 3),
        COSTS = round( c(n_t_c_comp_d, n_t_c_int_d) / n_ind, 0),
        INC_QALY = c("-", round(((n_t_qaly_int_d - n_t_qaly_comp_d) / n_ind), 3)),
        INC_COSTS = c("-", round(((n_t_c_int_d - n_t_c_comp_d) / n_ind), 0)),
        ICER = c("-", round( ((n_t_c_int_d - n_t_c_comp_d) / n_ind) / ((n_t_qaly_int_d - n_t_qaly_comp_d) / n_ind), 0))
  )
  )
  
  l_output <- list(n_ind = n_ind,
                   v_names_hs = v_names_hs,
                   m_hs_comp = m_hs_comp,
                   m_hs_int = m_hs_int,
                   v_cum_ly_comp_d = v_cum_ly_comp_d,
                   v_cum_ly_int_d = v_cum_ly_int_d,
                   v_cum_qaly_comp_d = v_cum_qaly_comp_d,
                   v_cum_qaly_int_d = v_cum_qaly_int_d,
                   v_cum_qaly_comp_d = v_cum_qaly_comp_d,
                   v_cum_qaly_int_d = v_cum_qaly_int_d,
                   v_cum_c_comp_d = v_cum_c_comp_d,
                   v_cum_c_int_d = v_cum_c_int_d,
                   v_cum_ly_comp = v_cum_ly_comp,
                   v_cum_ly_int = v_cum_ly_int,
                   v_cum_qaly_comp = v_cum_qaly_comp,
                   v_cum_qaly_int = v_cum_qaly_int,
                   v_cum_qaly_comp = v_cum_qaly_comp,
                   v_cum_qaly_int = v_cum_qaly_int,
                   v_cum_c_comp = v_cum_c_comp,
                   v_cum_c_int = v_cum_c_int,
                   df_res = df_res,
                   df_res_d = df_res_d)
  return(l_output)
  
}

output$plot_cohort_no_asp <- renderPlot({
  
  n_pt <- l_output_hstm()$n_ind
  v_names_hs <- l_output_hstm()$v_names_hs
  m_hs <- l_output_hstm()$m_hs_comp
  
  plot(y = m_hs[, "Well"]/ n_pt, 
       x = rownames(m_hs),
       type = 'l',
       main = 'Health state membership over model cycles',
       xlab = 'Cycle number',
       ylab = 'Proportion of individuals',
       ylim = c(0,1)
       )
  lines(y = m_hs[, "Post-minor_stroke"]/ n_pt, 
        x = rownames(m_hs), 
        col = 'red')
  lines(y = m_hs[, "Post-major_stroke"]/ n_pt, 
        x = rownames(m_hs), 
        col = 'blue')
  lines(y = m_hs[, "Post-MI"]/ n_pt, 
        x = rownames(m_hs), 
        col = 'orange')
  lines(y = m_hs[, "Death_stroke"]/ n_pt, 
        x = rownames(m_hs), 
        col = 'lightgrey')
  lines(y = m_hs[, "Death_MI"]/ n_pt, 
        x = rownames(m_hs), 
        col = 'lightblue')
  lines(y = m_hs[, "Death_other"]/ n_pt, 
        x = rownames(m_hs), 
        col = 'purple')
  legend("right",  
         legend = v_names_hs, 
         col = c('black', 'red', 'blue', 'orange', 'lightgrey', 'lightblue', 'purple'),
         lty = 1
  )
})

output$plot_cohort_asp <- renderPlot({
  
  n_pt <- l_output_hstm()$n_ind
  v_names_hs <- l_output_hstm()$v_names_hs
  m_hs <- l_output_hstm()$m_hs_int
  
  plot(y = m_hs[, "Well"]/ n_pt, 
       x = rownames(m_hs),
       type = 'l',
       main = 'Health state membership over model cycles',
       xlab = 'Cycle number',
       ylab = 'Proportion of individuals',
       ylim = c(0,1)
       )
  lines(y = m_hs[, "Post-minor_stroke"]/ n_pt, 
        x = rownames(m_hs), 
        col = 'red')
  lines(y = m_hs[, "Post-major_stroke"]/ n_pt, 
        x = rownames(m_hs), 
        col = 'blue')
  lines(y = m_hs[, "Post-MI"]/ n_pt, 
        x = rownames(m_hs), 
        col = 'orange')
  lines(y = m_hs[, "Death_stroke"]/ n_pt, 
        x = rownames(m_hs), 
        col = 'lightgrey')
  lines(y = m_hs[, "Death_MI"]/ n_pt, 
        x = rownames(m_hs), 
        col = 'lightblue')
  lines(y = m_hs[, "Death_other"]/ n_pt, 
        x = rownames(m_hs), 
        col = 'purple')
  legend("right",  
         legend = v_names_hs, 
         col = c('black', 'red', 'blue', 'orange', 'lightgrey', 'lightblue', 'purple'),
         lty = 1
  )
})
output$plot_cum_e <- renderPlot({
  
  n_pt <- l_output_hstm()$n_ind
  
  v_cum_ly_comp_d <- l_output_hstm()$v_cum_ly_comp_d
  v_cum_ly_int_d  <- l_output_hstm()$v_cum_ly_int_d
  v_cum_ly_comp_d <- l_output_hstm()$v_cum_ly_comp_d
  v_cum_ly_int_d  <- l_output_hstm()$v_cum_ly_int_d
  
  v_cum_qaly_comp_d <- l_output_hstm()$v_cum_qaly_comp_d
  v_cum_qaly_int_d  <- l_output_hstm()$v_cum_qaly_int_d
  v_cum_qaly_comp_d <- l_output_hstm()$v_cum_qaly_comp_d
  v_cum_qaly_int_d  <- l_output_hstm()$v_cum_qaly_int_d
  
  v_cycles <- 1:length(v_cum_qaly_comp_d)
  
  plot(y = v_cum_ly_comp_d / n_pt, 
       x = v_cycles,
       type = 'l',
       xlab = 'Cycle number',
       ylab = 'Cumulative discounted outcome'
  )
  lines(y = v_cum_qaly_comp_d / n_pt, 
        x = v_cycles,
        lty = 2)
  lines(y = v_cum_ly_int_d / n_pt, 
        x = v_cycles, 
        col = 'orange')
  lines(y = v_cum_qaly_comp_d / n_pt, 
        x = v_cycles, 
        col = 'orange',
        lty = 2)
  
  legend("right",  
         legend = c("Discounted LY - No Asp.",
                    "Discounted QALY - No Asp.",
                    "Discounted LY - Asp.",
                    "Discounted QALY - Asp."), 
         col = c('black', 'black', 'orange', 'orange'),
         lty = c(1, 2, 1, 2)
  )
  
})
output$plot_cum_c <- renderPlot({
  
  n_pt <- l_output_hstm()$n_ind
  
  v_cum_c_comp_d <- l_output_hstm()$v_cum_c_comp_d
  v_cum_c_int_d  <- l_output_hstm()$v_cum_c_int_d

  v_cycles <- 1:length(v_cum_c_comp_d)
  
  plot(y = v_cum_c_comp_d / n_pt, 
       x = v_cycles,
       type = 'l',
       xlab = 'Cycle number',
       ylab = 'Cumulative discounted outcome'
  )
  lines(y = v_cum_c_int_d / n_pt, 
        x = v_cycles, 
        col = 'orange')
  legend("right",  
         legend = c("Discounted Costs - No Asp.",
                    "Discounted Costs - Asp."), 
         col = c('black', 'orange'),
         lty = 1
  )
  
})
output$tbl_res <- renderTable({
  l_output_hstm()$df_res
  
})

output$tbl_res_d <- renderTable({
  l_output_hstm()$df_res_d
})

# Assignment HSTM 2C ----
# Perform PA 

l_res_pa <- eventReactive(input$do_pa, {
  
  # Generate probabilistic inputs
  df_inputs_pa <- generate_pa_inputs(n_sim = input$n_sim_pa, 
                                     mean_eff_mi = input$mean_eff,
                                     se_eff_mi = input$se_eff, # for shiny app --> variation around
                                     seed_num = input$seed_num_pa)

  # Perform PA
  ## Initialise matrix outcomes
  m_res_pa <- matrix(0, 
                     ncol = 4,
                     nrow = input$n_sim_pa,
                     dimnames = list(c(1:input$n_sim_pa),
                                     c("QALY_comp",
                                       "QALY_int",
                                       "Costs_comp",
                                       "Costs_int")))
  for(i in 1:input$n_sim_pa) {
    
    l_params_temp <- as.list(df_inputs_pa[i, ])
    
    m_res_pa[i, ] <- perform_simulation(l_params = l_params_temp, 
                                        start_age = input$n_start_age_pa,
                                        verbose = FALSE)
  }
  
  return(list(df_inputs_pa = df_inputs_pa,
              m_res_pa = m_res_pa))
  
}
)


output$inputs_det <- renderTable({
  df <- data.frame(
    Name = c("r_fatal_mi", 
             "r_fatal_stroke", 
             "r_inc_mi", 
             "r_inc_stroke", 
             "p_post_major_stroke",
             "eff_mi", 
             "eff_stroke", 
             "u_healthy", 
             "u_post_mi", 
             "u_post_minor_stroke", 
             "u_post_major_stroke", 
             "u_aspirin_use", 
             "c_aspirin_use", 
             "c_post_mi", 
             "c_post_minor_stroke", 
             "c_post_major_stroke" 
             ),
    Mean = c(0.25,
             0.3,
             400 / 100000,
             50 / 100000,
             235 / 1000,
             0.6,
             1.2,
             1,
             0.85,
             0.75,
             0.5,
             99,
             100,
             8000,
             2000,
             20000
             ),
    SE = c(round(sqrt((25 / 100 * (1 -  25 / 100)) / 100), 3),
           round(sqrt((30 / 100 * (1 -  30 / 100)) / 100), 3),
           round(sqrt((400 / 100000 * (1 - 400 / 100000)) / 100000), 3),
           round(sqrt((50 / 100000 * (1 - 50 / 100000)) / 100000), 3),
           round(sqrt((235 / 1000 * (1 -  235 / 1000)) / 1000), 3),
           0.15,
           0.1,
           0,
           0.85,
           0.75,
           0.5,
           0,
           round(100 * 0.25, 0),
           round(8000 * 0.25, 0),
           round(2000 * 0.25, 0),
           round(20000 * 0.25, 0)
           ),
    Distribution = c(rep("Beta", 5),
                     rep("Normal", 2),
                     "Fixed",
                     rep("Beta", 3),
                     "Fixed",
                     rep("Gamma", 4)
                     )
  )
  
  df
  
}, 
digits = 3)

output$inputs_pa_summary <- renderTable({
  df <- l_res_pa()$df_inputs_pa
  
  df_summary <- data.frame(
    Parameter = colnames(df),
    Mean = apply(df, 2, mean),
    SD = apply(df, 2, sd),
    Percentile_2.5th = apply(df, 2, function(x) quantile(x, 0.025)),
    Percentile_97.5th = apply(df, 2, function(x) quantile(x, 0.975)),
    Min = apply(df, 2, min),
    Max = apply(df, 2, max)
  )
  
  df_summary
  
},
digits = 3
)

output$inputs_pa <- renderTable({
  df <- l_res_pa()$df_inputs_pa
  head(df, 100)
},
digits = 3)

output$tbl_res_pa <- renderTable({
  m_res <- l_res_pa()$m_res_pa
  df <- as.data.frame(m_res)
  
  df$Incremental_QALY <- df$QALY_int - df$QALY_comp
  df$Incremental_Costs <- df$Costs_int - df$Costs_comp
  
  df_summary <- data.frame(
    Outcome = colnames(df),
    Mean = apply(df, 2, mean),
    SD = apply(df, 2, sd),
    Percentile_2.5th = apply(df, 2, function(x) quantile(x, 0.025)),
    Percentile_97.5th = apply(df, 2, function(x) quantile(x, 0.975)),
    Min = apply(df, 2, min),
    Max = apply(df, 2, max)
  )
  df_summary <- data.frame(rbind(df_summary,
        c("ICER_QALY", round(mean(df$Incremental_Costs)/ mean(df$Incremental_QALY), 0) , rep(NA, ncol(df_summary) - 2))
  )
  )
  df_summary[, 2:ncol(df_summary)] <- apply(df_summary[, 2:ncol(df_summary)], 2, function (x) as.numeric(as.character(x)))
  df_summary[, 2:ncol(df_summary)] <- apply(df_summary[, 2:ncol(df_summary)], 2, function (x) round(x, 3))
  df_summary
}, 
digits = 3)


output$ce_pa <- renderPlot({
  m_res <- l_res_pa()$m_res_pa
  
  df_res <- as.data.frame(m_res)
  
  df_res$Inc_QALY <- df_res$QALY_int - df_res$QALY_comp
  df_res$Inc_C <- df_res$Costs_int - df_res$Costs_comp
  
  ggplot(df_res) + 
    #ggtitle("Incremental cost-effectiveness plane") +
    geom_point(aes(x = QALY_comp, y = Costs_comp, colour = "No Aspirin"), shape = 1) + 
    geom_point(aes(x = QALY_int, y = Costs_int, colour = "Aspirin"), shape = 1) +
    xlab ("Total QALY") + 
    ylab("Total Costs") +
    geom_hline(yintercept = 0,  
               color = "black") +
    geom_vline(xintercept = 0,  
               color = "black") + 
    xlim(c(min(c(df_res$QALY_int, df_res$QALY_comp)), max(c(df_res$QALY_int, df_res$QALY_comp)))) +
    scale_colour_manual(name = "",
                        values = c(Aspirin = "orange", 
                                   `No Aspirin` = "grey"
                        )) +
    theme_bw() + 
    theme(legend.position="bottom") 
  
})

output$ice_pa <- renderPlot({
  m_res <- l_res_pa()$m_res_pa
  
  df_res <- as.data.frame(m_res)
  
  df_res$Inc_QALY <- df_res$QALY_int - df_res$QALY_comp
  df_res$Inc_C <- df_res$Costs_int - df_res$Costs_comp
  
  ggplot(df_res) + 
    #ggtitle("Incremental cost-effectiveness plane") +
    geom_point(aes(x = Inc_QALY, y = Inc_C, colour = "Incrementals"), shape = 1) + 
    geom_point(aes(x = mean(Inc_QALY), y = mean(Inc_C), colour = "Incrementals(mean)"), size = 2) +
    xlab ("Incremental QALY") + 
    ylab("Incremental costs") +
    geom_hline(yintercept = 0,  
               color = "black") +
    geom_vline(xintercept = 0,  
               color = "black") + 
    geom_abline(intercept = 0, slope = 20000, linetype= "dashed", 
                color = "black") + # 20,000 per QALY threshold line
    #xlim(c(0,1)) +
    scale_colour_manual(name = "",
                        values = c(Incrementals = "orange", 
                                   `Incrementals(mean)` = "darkblue"
                                   )) +
    theme_bw() + 
    theme(legend.position="bottom") 
  
})
output$ceac_pa <- renderPlot({
  m_res <- l_res_pa()$m_res_pa
  
  m_res_ceac <- calculate_CEAC(Q.trt = m_res[, "QALY_int"],
                               C.trt = m_res[, "Costs_int"],
                               Q.comp = m_res[, "QALY_comp"], 
                               C.comp = m_res[, "Costs_comp"],
                               v.wtp = seq(from = 0, to = 100000, by = 1000)
                               )
  
  df_CEAC <- as.data.frame(m_res_ceac)

  ggplot(data = df_CEAC, aes(x= WTP.threshold, y = Prob.trt)) + 
    #ggtitle("Cost-effectiveness acceptability curve") +
    geom_line(aes(colour = "Aspirin"), linejoin = "bevel") + ylim(c(0, 1)) +
    geom_line(aes(colour = "No Aspirin", x = WTP.threshold, y = 1- Prob.trt)) +
    scale_colour_manual(name = "",
                        values = c(Aspirin = "orange", 
                                   `No Aspirin` = "grey"
                        )) +
    xlab("Willingness to pay thresholds") + 
    #scale_x_continuous(labels = dollar_format(prefix = "\u20ac ", suffix = "")) +
    ylab("Probability of being cost effective") +
    theme_bw()
})


output$tbl_ice_pa <- renderTable({
  m_res <- l_res_pa()$m_res_pa
  
  df_res <- as.data.frame(m_res)
  
  df_res$Inc_QALY <- df_res$QALY_int - df_res$QALY_comp
  df_res$Inc_C <- df_res$Costs_int - df_res$Costs_comp
  
  df <- data.frame(
    Quadrant = c("NorthEast (more effective, more expensive)", "SouthEast (more effective, less expensive)",
                 "NorthWest (less effective, more expensive)", "SouthWest (less effective, less expensive)"),
    Percentage = c(paste(round(length(which(df_res$Inc_QALY > 0 & df_res$Inc_C > 0)) / nrow(df_res) * 100, 0), "%", sep = ""),
                   paste(round(length(which(df_res$Inc_QALY > 0 & df_res$Inc_C < 0)) / nrow(df_res) * 100, 0), "%", sep = ""),
                   paste(round(length(which(df_res$Inc_QALY < 0 & df_res$Inc_C > 0)) / nrow(df_res) * 100, 0), "%", sep = ""),
                   paste(round(length(which(df_res$Inc_QALY < 0 & df_res$Inc_C < 0)) / nrow(df_res) * 100, 0), "%", sep = "")
    )
  )
  
  df
})
output$tbl_ceac <- renderTable({
  
  m_res <- l_res_pa()$m_res_pa
  
  m_res_ceac <- calculate_CEAC(Q.trt = m_res[, "QALY_int"],
                               C.trt = m_res[, "Costs_int"],
                               Q.comp = m_res[, "QALY_comp"], 
                               C.comp = m_res[, "Costs_comp"],
                               v.wtp = seq(from = 0, to = 100000, by = 1000)
  )
  
  df_CEAC <- as.data.frame(m_res_ceac)
  
  names(df_CEAC) <- c("Willingness_to_pay", "Prob_Asp", "Prob_No_Asp")
  
  df_CEAC[, c(2,3)] <- apply(df_CEAC[, c(2,3)], 2, function(x) paste(round(x * 100, 0), "%", sep = ""))
  
  df_CEAC[which(df_CEAC[, 1] %in% seq(0, 100000, 5000)),]
  
},
digits = 0)

}

# Run the app ----
shinyApp(ui = ui, server = server)
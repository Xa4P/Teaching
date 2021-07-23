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

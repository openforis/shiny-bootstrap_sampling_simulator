################################################################
# BOOTSRAPPING SHINY APP
# Lauri Vesa, FAO. 1 March 2019
# updated LV: 9 April 2021
# the script takes random samples from 'sampleplots' data, with given interval ('s_step') and min/max limits
# and 'n_simulations' times at each case, and computes statistics (mean, variance, standard error, relative standard error, confidence intervals 95%)
# and makes graphs and non-linear model ('m1') of the form 
# 'c1*nplots^c2' where c1 and c2 are model parameters, nplots=number of samples.
# More about applied method at  http://www.fs.fed.us/emc/rig/Plot-GEM/
################################################################

library(ggplot2)
library(summarytools)
library(dplyr)

source("Run-bootstrap.R")


server = function(input, output, session) {
  
  #    library(dplyr)
  # https://stackoverflow.com/questions/47248534/dynamically-list-choices-for-selectinput-from-a-user-selected-column
  
  mydata <- ""
  
  resultvalues      <- reactiveValues(df_data = NULL)
  model_parameters  <- reactiveValues(c1 = 0, c2 = 0)
  
  clear_outputs <- function(){
    updateTextInput(session, "txtout1", value = NULL)
    updateTextInput(session, "txtout2", value = NULL)
    
    df <- data.frame()
    output$plot1 <- renderPlot({
      ggplot(df) + geom_point() + xlim(0, input$max_s_size) + ylim(0, 100) })
    output$plot2 <- renderPlot({
      ggplot(df) + geom_point() + xlim(0, input$max_s_size) + ylim(0, 100) })
    output$df_result_data <- NULL
    output$contents       <- NULL
    output$summarytable   <- NULL
    resultvalues$df_data  <- NULL
    model_parameters$c1   <- 0
    model_parameters$c2   <- 0
    
    updateSliderInput(session, "slider_error", value = 20)
    sResults              <- NULL

  }
  
  
  rawData <- reactive({
    infile <- req(input$file1)
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    clear_outputs()
    read.csv(infile$datapath, header = input$header, stringsAsFactors = FALSE)
#    a1 <- a1[, -1] # remove index column
  })
  
  ## update 'column' selector
  observeEvent( input$file1, {
    data_analysis <- rawData()
    if (is.null(data_analysis)) {
      # User has not uploaded a file yet
      return(NULL)
    }

    #      sel_col1 <- dplyr::select_if(rawData(), is.character() | is.factor())
    updateSelectInput(session, "sel_stratum", choices = c("<NONE>", names(data_analysis)))
    sel_col2 <- dplyr::select_if(data_analysis, is.numeric)
    updateSelectInput(session, "sel_variable", choices = names(sel_col2))
    
    max_n <- nrow(data_analysis)
    updateNumericInput(session, "max_s_size", value = max_n)
    step_n = trunc((max_n - input$min_s_size) / 20)
    if (step_n>0) {
      updateNumericInput(session, "s_step", value = step_n)
    }
    
    output$contents <- renderDataTable({
      M <- DT::datatable(data_analysis, options = list(scrollX = TRUE))
      M
    })
    
  })
  
  
  observeEvent(input$sel_stratum, {
    if(input$sel_stratum != "<NONE>") {
      data_analysis <- rawData()
      choise_list <- as.data.frame(data_analysis[,as.character(input$sel_stratum)])
      names(choise_list) <- "ch"
      ch_list <- sort(unique(as.character(choise_list$ch)))
      
      updateSelectInput(session, "sel_stratum_var", choices = c("",ch_list) )
    } else {
      updateSelectInput(session, "sel_stratum_var", choices ="")
    }
  })

      
  observeEvent(input$sel_stratum_var, {
      data_analysis <- rawData()
      # filter data by selected stratum
      if (input$sel_stratum_var !="" & !is.na(input$sel_stratum_var)) {
        # https://stackoverflow.com/questions/17075529/subset-based-on-variable-column-name
        data_analysis <- data_analysis[ data_analysis[[input$sel_stratum]] == input$sel_stratum_var , ]
      }
      
      max_n <- nrow(data_analysis)
      updateNumericInput(session, "max_s_size", value = max_n)
      step_n = trunc((max_n - input$min_s_size) / 20)
      if (step_n>0) { updateNumericInput(session, "s_step", value = step_n) }
  })

  
  observeEvent(input$action_plot, {
    
    data_analysis <-  reactive({
      data_analysis1 <- rawData()
      
      if (is.null(data_analysis1)) {
        # User has not uploaded a file yet
        return(NULL)
      }
      
      # filter data by selected stratum
      if (input$sel_stratum_var !="" & !is.na(input$sel_stratum_var)) {
        # https://stackoverflow.com/questions/17075529/subset-based-on-variable-column-name
        data_analysis1 <- data_analysis1[ data_analysis1[[input$sel_stratum]] == input$sel_stratum_var , ]
      }
      
      data_analysis1 <- as.data.frame(data_analysis1[,as.character(input$sel_variable)])
      names(data_analysis1) <- "input_var"
      data_analysis1 <- subset(data_analysis1, !is.na(input_var)) 

      return(data_analysis1)
    })
    
    
    output$summarytable <- renderTable({
      data_analysis1 <- data_analysis()
      mydata <- summarytools::descr(data_analysis1[,1], transpose=TRUE)
      head(mydata)
    }, rownames = FALSE)
    
    # call subcode
    sResults <- bootstrap_results(
      sampleplots   = data_analysis(), 
      min_s_size    = as.integer(input$min_s_size),
      max_s_size    = input$max_s_size,
      s_step        = as.integer(input$s_step),
      n_simulations = as.integer(input$n_simulations),
      ci_level      = as.numeric(input$ci_level)
    )

   
    if (exists("sResults")) { 
      
      err_target <- as.integer(input$slider_error)
      jono  <- as.list(sResults$a_error)
      n_error <- which(jono < err_target)[1]

      # fit a non-linear curve 
      m1 <- NULL
      try(m1 <- nls(a_error ~ I(c1*a_nplots^c2), data = sResults, start=c(c1=1.0,c2=-0.5), trace = T))
      if (!is.null(m1)){
        model_parameters$c1 <-  coef(m1)[1]
        model_parameters$c2 <-  coef(m1)[2]
      } else {
        model_parameters$c1 <-  NA
        model_parameters$c2 <-  NA
      }
      
      
    #  m1 <- nls(a_error ~ I(c1*a_nplots^c2), data = sResults, start=c(c1=1.0,c2=-0.5), trace = T)
    #  print(str(m1))
    #  model_parameters$c1 <-  coef(m1)[1]
    #  model_parameters$c2 <-  coef(m1)[2]
      
     # estimate along non-linear curve ('m1')
      n_estimated <- ifelse(!is.na(model_parameters$c1) & !is.na(model_parameters$c2) & model_parameters$c1 !=0, round(0.5 + (err_target/model_parameters$c1)^(1/model_parameters$c2)), NA)

      # Cochran (1977) formula
      ci_level <- as.numeric(input$ci_level)
      ci_level <- ci_level + (1 - ci_level)/2
      sResults$qt <- qt(ci_level, sResults$a_nplots)
      sResults$N_Cochran <- round(0.5 + ((sResults$qt * sqrt(sResults$a_var)/sResults$a_mean)/(err_target/100))^2)
 #     sResults$qt <- NULL
      
      
      n_needed <- ifelse(is.na(n_error) & !is.na(n_estimated), paste0("Not available inside range. Estimated: ", n_estimated),
        ifelse(is.na(n_error),"Not available",                 
         ifelse(n_error==1, "Less than min. limit", 
           round(0.5 + sResults$a_nplots[n_error] - abs(sResults$a_nplots[n_error] - sResults$a_nplots[n_error-1]) * abs(err_target - sResults$a_error[n_error])/ abs(sResults$a_error[n_error-1] - sResults$a_error[n_error]))
      )))
      
      output$txtout1 <- renderText({ paste0("Sample size needed to achieve acceptable error: ", as.character(n_needed)) })
      output$df_result_data <- renderTable(sResults)
      
      resultvalues$df_data <- sResults
    }
    
   
#   create graphs
    if (exists("sResults")) { 
     
      xx <-  seq(input$min_s_size, input$max_s_size, input$s_step)
      
      
      output$plot1 <- renderPlot( {
        
        # fit a non-linear curve for error chart
#        m1 <- nls(a_error ~ I(c1*a_nplots^c2), data = sResults, start=c(c1=1.0,c2=-0.5), trace = T)

        spline_error  <- as.data.frame(spline(sResults$a_nplots, sResults$a_error))
    #    sResults$modeled_error <- model_parameters$c1*sResults$a_nplots^model_parameters$c2
    #    modeled_error <- as.data.frame(spline(sResults$a_nplots, sResults$modeled_error))

        p1 <- ggplot(sResults, aes(x=a_nplots, y=a_error)) + geom_point(color="blue") +
          xlab("Sampling Intensity") + ylab("% error") +
          theme(text = element_text(size=14)) + scale_y_continuous(limits = c(0, NA)) + 
          theme(legend.position="none")
        if (input$curve_exact) {
          p1 <- p1 + geom_line(color="black")
        }
        
        if (input$curve_exact_smoothed) { 
          p1 <- p1 + geom_line(data = spline_error, aes(x = x, y = y), color="blue")
        }

        if (input$curve_error) { 
          p1 <- p1 + geom_hline(yintercept=input$slider_error, color = "red")
        } 
        print(p1)

        min_y <- trunc(min(sResults$ci_lower))

        output$plot2 <- renderPlot({
          # fit a non-linear curves for CI chart
          spline_ci1  <- as.data.frame(spline(sResults$a_nplots, sResults$ci_lower))
          spline_ci2  <- as.data.frame(spline(sResults$a_nplots, sResults$ci_upper))
          spline_mean <- as.data.frame(spline(sResults$a_nplots, sResults$a_mean))

          p2 <- ggplot(sResults, aes(x=a_nplots, y=ci_lower)) + geom_point(color="blue") +
            geom_point(aes(x=a_nplots, y=ci_upper), color="blue") +
            geom_point(aes(x=a_nplots, y=a_mean  ), color="black") +
            xlab("Sampling Intensity") + ylab("CI bounds") +
            theme(text = element_text(size=14)) + scale_y_continuous(limits = c(min_y, NA)) + 
            theme(legend.position="none")
          
          if (input$curve_exact) {
            p2 <- p2 + geom_line(aes(y=ci_lower), color="black")
            p2 <- p2 + geom_line(aes(y=ci_upper), color="black")
            p2 <- p2 + geom_line(aes(y=a_mean  ), color="black")
          }
          
          if (input$curve_exact_smoothed) { 
            p2 <- p2 + geom_line(data = spline_ci1,  aes(x = x, y = y), color="blue")
            p2 <- p2 + geom_line(data = spline_ci2,  aes(x = x, y = y), color="blue")
            p2 <- p2 + geom_line(data = spline_mean, aes(x = x, y = y), color="blue")
          }
          print(p2)
          })
      })
    }  
  })

  
  output$plot1 <- renderPlot({
    if (!exists("sResults")) {
      df <- data.frame()
      ggplot(df) + geom_point() + xlim(0, input$max_s_size) + ylim(0, 100)
    }
  })
  
  output$plot2 <- renderPlot({
    if (!exists("sResults")) {
      df <- data.frame()
      ggplot(df) + geom_point() + xlim(0, input$max_s_size) + ylim(0, 100)
    }
  })

  
  observeEvent(input$slider_error, {
    if (exists("resultvalues")) {
      sResults <- resultvalues$df_data
      err_target <- as.integer(input$slider_error)
      jono  <- as.list(sResults$a_error)
      n_error <- which(jono < err_target)[1]
      
      n_estimated <- ifelse(!is.na(model_parameters$c1) & !is.na(model_parameters$c2) & model_parameters$c1 !=0, round(0.5 + (err_target/model_parameters$c1)^(1/model_parameters$c2)), NA)
#      print(paste0("*Estimated: ",n_estimated))
      
      n_needed <- ifelse(is.na(n_error) & !is.na(n_estimated), paste0("Not available inside range. Estimated: ", n_estimated),
                         ifelse(is.na(n_error),"Not available",                 
                                ifelse(n_error==1, "Less than min. limit", 
                                       round(0.5 + sResults$a_nplots[n_error] - abs(sResults$a_nplots[n_error] - sResults$a_nplots[n_error-1]) * abs(err_target - sResults$a_error[n_error])/ abs(sResults$a_error[n_error-1] - sResults$a_error[n_error]))
      )))
      

      output$txtout1 <- renderText({ paste0("Sample size needed to achieve acceptable error: ", as.character(n_needed)) })  
    }
  })  
}

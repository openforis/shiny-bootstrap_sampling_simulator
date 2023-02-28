################################################################
# BOOTSRAPPING ALGORITM
# Lauri Vesa, FAO. 1 March 2019
# updated LV: 9 April 2021
# the script takes random samples from 'sampleplots' data, with given interval ('s_step') and min/max limits
# and 'n_simulations' times at each case, and computes statistics (mean, variance, standard error, relative standard error, confidence intervals 95%)
# and makes graphs and non-linear model ('m1') of the form 
# 'c1*nplots^c2' where c1 and c2 are model parameters, nplots=number of samples.
# More about applied method at  http://www.fs.fed.us/emc/rig/Plot-GEM/
################################################################

library('shinydashboard')
library('DT')
library('markdown')


ui <- function(request) {
  
  header <- dashboardHeader(
    title = "Bootstrap sampling simulation",
    titleWidth = 350
  )
  
  
  body <- dashboardBody(
    navbarPage(
      "",
      tabPanel("Application",
               sidebarPanel(
                 checkboxInput("header", "Headers in CSV", TRUE),
                 
                 fileInput("file1", "Upload CSV File",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                 ),
                selectInput("sel_variable", "Variable:", choices=""),
                fluidRow(
                   column(6,
                          selectInput("sel_stratum", "Stratum:", 
                                      choices="")),
                   column(6,
                          selectInput("sel_stratum_var", "Category in stratum:", 
                                      choices=""))
                 ),
                 splitLayout(
                   numericInput("min_s_size", "Min. sample:", 10),
                   numericInput("max_s_size", "Max. sample:", 100),
                   numericInput("s_step", "Step:", 13)),
                 fluidRow(
                   column(6,
                          numericInput("n_simulations", "Number of simulations:", 20)),
                   column(6,
                          selectInput("ci_level", "Confidence Level:", 
                                      choices=c("0.80","0.90","0.95","0.98","0.99"), selected ="0.95")
                   )),
                 sliderInput("slider_error", "Acceptable Percent Error:", 1, 100, 20),
                 tags$h5("Select curves:"),
                 checkboxInput("curve_exact", "Exact curve", FALSE),
                 checkboxInput("curve_exact_smoothed", "Exact smoothed curve", TRUE),
          #       checkboxInput("curve_modeled", "Modeled error curve", FALSE),
                 checkboxInput("curve_error", "Acceptable error", TRUE),
                 # tags$h5("actionButton with CSS class:"),
                 actionButton("action_plot", "Plot curves", class = "btn-primary")
               ),
               mainPanel(
                 tabsetPanel(
                    tabPanel("Results",
                            tags$br(),
                            splitLayout(
                              plotOutput("plot1"),
                              plotOutput("plot2")),
                            hr(),
                            tableOutput("summarytable"),
                            #   h4("Verbatim text output"),
                            verbatimTextOutput("txtout1"),
#                            p(tags$h5("Relative standard error (RSE) curve non-linear model parametes:")),
                            verbatimTextOutput("txtout2")
                            ),
                    tabPanel("Result table", 
                              tableOutput("df_result_data")
                    ),   
                    tabPanel("Data", 
                            tags$br(),
                            dataTableOutput("contents")
                   )
                 )
               )
      ),

      tabPanel("About", 
               includeMarkdown("www/releases.md")
               
      )
    ))
    
    dashboardPage(
      skin='green',
      header,
      dashboardSidebar(disable = TRUE),
      body
    )
}
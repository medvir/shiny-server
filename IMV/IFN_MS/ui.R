#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Extract interferon MS data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      checkboxInput("filter_p", "Filter on p-value"),
      conditionalPanel(
        condition = "input.filter_p == true",
        numericInput("pvalue",
                     HTML("p-value &le;"),
                     value = 0.05,
                     min = 0.0,
                     max = 1.0,
                     step =  0.05)
      ),
      checkboxInput("filter_q", "Filter on q-value"),
      conditionalPanel(
        condition = "input.filter_q == true",
        numericInput("qvalue",
                     HTML("q-value &le;"),
                     value = 0.1,
                     min = 0.0,
                     max = 1.0,
                     step =  0.05)
      ),
      checkboxInput("filter_fc", "Filter on log(FC)"),
      conditionalPanel(
        condition = "input.filter_fc == true",
        numericInput("log_fc",
                     HTML("abs(log FC) &ge;"),
                     value = 0.0,
                     min = 0.0,
                     step =  0.5)
      ),
      selectInput("time",
                  "Time point",
                  c('30min', '90min', '4h')),
      selectInput("treatment",
                  "Treatment",
                  c('alpha2', 'beta', 'omega', 'gamma', 'lambda1')),
      actionButton("goButton", "Plot!")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      dataTableOutput("genesTable"),
      downloadButton("genesDownload", label = "Download gene table", class = NULL),
      dataTableOutput("peptidesTable"),
      downloadButton("peptidesDownload", label = "Download full table", class = NULL),
      plotOutput('circosPlot'),
      downloadButton("plotDownload", label = "Download plot", class = NULL)
    )
  )
))

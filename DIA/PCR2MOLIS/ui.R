# PCR2MOLIS

library(shiny)
library(tidyverse)

shinyUI(fluidPage(
    titlePanel("PCR2MOLIS"),
    fluidRow(
        column(3,
               wellPanel(
                   #fileInput("pcr_file", "PCR raw data [.xlsx]:", accept = ".xlsx"),
                   uiOutput("folder_selection"),
                   uiOutput("file_selection"),
                   uiOutput("target_selection")
               )),
        column(1,
               wellPanel(
                   uiOutput("lin_log")
               )
        ),
        column(8,
               plotOutput("run_plot")
        )
    ),
    
    fluidRow(
        hr(),
        column(4,
               h3("Sample Plot"),
               uiOutput("sample_selection"),
               plotOutput("curve")
               ),
        column(8,
               h3("Samples"),
               dataTableOutput("results"),
               tableOutput("fit_data_table")
        )
    ),
    
    fluidRow(
        hr(),
        column(12,
               h5("Export"),
               downloadButton("pdf_export", "PDF Export"),
               h6(""),
               downloadButton("molis_export", "MOLIS Export")
        )
    )
))
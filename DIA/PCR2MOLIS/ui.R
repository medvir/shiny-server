# PCR2MOLIS

library(shiny)
library(tidyverse)
library(stringr)
library(readxl)
library(cowplot)
library(DT)

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
                   uiOutput("lin_log"),
                   uiOutput("threshold_selection")
               )
        ),
        column(8,
               plotOutput("run_plot")
        )
    ),
    
    fluidRow(
        hr(),
        column(4,
               uiOutput("sample_selection"),
               plotOutput("curve"),
               tableOutput("fit_data_table")
               ),
        column(8,
               dataTableOutput("results")
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
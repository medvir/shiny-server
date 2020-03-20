# SARS2MOLIS

library(shiny)
library(tidyverse)
library(stringr)
library(readxl)
library(cowplot)
library(DT)

shinyUI(fluidPage(
    titlePanel("PCR2MOLIS"),
    fluidRow(
        column(2,
               wellPanel(
                   fileInput("pcr_file", "PCR raw data [.xlsx]:", accept = c(".xls",".xlsx")),
                   #uiOutput("folder_selection"),
                   #uiOutput("file_selection"),
                   #fileInput("pcr_file_upload", "PCR Export Datei [.xlsx]:", accept = ".xls"),
                   uiOutput("target_selection")
               )),
        column(2,
               wellPanel(
                   uiOutput("lin_log_selection"),
                   uiOutput("threshold_selection"),
                   uiOutput("max_ct_selection"),
                   uiOutput("min_delta_Rn_selection")
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
               #downloadButton("pdf_export", "PDF Export"),
               #h6(""),
               downloadButton("molis_export", "MOLIS Export")
        )
    )
))

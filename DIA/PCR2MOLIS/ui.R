# PCR2MOLIS

library(shiny)
library(tidyverse)

shinyUI(fluidPage(
    titlePanel("PCR2MOLIS"),
    fluidRow(
        column(3,
               wellPanel(
                   #fileInput("pcr_file", "PCR raw data [.xlsx]:", accept = ".xlsx"),
                   uiOutput("folders"),
                   uiOutput("files")
               )),
        column(2,
               wellPanel(
                   uiOutput("targets")
               )),
        column(2,
               wellPanel(
                   uiOutput("lin_log")
               )),
        column(1,
               h5("Export"),
               downloadButton("pdf_export", "PDF Export"),
               h6(""),
               downloadButton("molis_export", "MOLIS Export")
        )
    ),
    fluidRow(
        column(12,
               h3("PCR Results"),
               plotOutput("plot")
        )
    ),
    fluidRow(
        hr(),
        column(6,
               h3("Samples"),
               dataTableOutput("results")
        ),
        column(6,
               h3("Sample Plot"),
               plotOutput("sample_plot"),
               plotOutput("curve")
               )
    ),
    fluidRow(
        hr(),
        column(6,
               h3("Raw data"),
               tableOutput("raw_data")
        )
    )
))
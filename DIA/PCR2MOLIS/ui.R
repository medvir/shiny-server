# PCR2MOLIS

library(shiny)
library(tidyverse)

shinyUI(fluidPage(
    titlePanel("PCR2MOLIS"),
    fluidRow(
        column(3,
               wellPanel(
                   fileInput("pcr_file", "PCR raw data [.xlsx]:", accept = ".xlsx")
               )),
        column(3,
               wellPanel(
                   uiOutput("targets")
               )),
        column(3,
               h5("Download"),
               downloadButton("results_file", "Download")
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
               plotOutput("sample_plot")
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
# SARS2MOLIS

library(shiny)
library(tidyverse)
library(stringr)
library(readxl)
library(cowplot)
library(DT)

### SETTINGS
Negativkontrollen <- c("NC")
Positivkontrollen <- c("pos DNA", "pos RNA", "pos. DNA", "pos. RNA", "runC+")

###
shinyUI(fluidPage(
    titlePanel("SARS2MOLIS"),
    fluidRow(
        column(2,
               wellPanel(
                   fileInput("pcr_file", "PCR raw data [.xls/.xlsx]:", accept = c(".xls",".xlsx")),
                   uiOutput("target_selection"),
                   uiOutput("lin_log_selection"),
                   checkboxGroupInput("neg_control", "Negativkontrollen",
                                      choices = Negativkontrollen,
                                      selected = Negativkontrollen),
                   checkboxGroupInput("pos_control", "Positivkontrollen",
                                      choices = Positivkontrollen,
                                      selected = Positivkontrollen),
                   uiOutput("max_ct_selection"),
                   #downloadButton("molis_export", "MOLIS Export"),
                   downloadButton("pdf_export", "PDF Export")
                   )
               ),
        column(10,
               plotOutput("run_plot"),
               dataTableOutput("results")
               )
        )
    ))
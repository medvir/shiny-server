# SARS2MOLIS

library(shiny)
library(tidyverse)
library(stringr)
library(readxl)
library(cowplot)
library(DT)

shinyUI(fluidPage(
    titlePanel("SARS2MOLIS"),
    fluidRow(
        column(2,
               wellPanel(
                   fileInput("pcr_file", "PCR raw data [.xls/.xlsx]:", accept = c(".xls",".xlsx")),
                   uiOutput("target_selection"),
                   textInput("neg_control", "Negativkontrolle", value = "NC"),
                   textInput("dna_pos_control", "DNA Positivkontrolle", value = "pos DNA"),
                   textInput("rna_pos_control", "RNA Positivkontrolle", value = "pos RNA"),
                   uiOutput("lin_log_selection"),
                   uiOutput("threshold_selection"),
                   uiOutput("max_ct_selection"),
                   #uiOutput("min_delta_Rn_selection"),
                   #downloadButton("molis_export", "MOLIS Export"),
                   downloadButton("pdf_export", "PDF Export")
                   )
               ),
        
        # column(2,
        #        wellPanel(
        #            uiOutput("lin_log_selection"),
        #            uiOutput("threshold_selection"),
        #            uiOutput("max_ct_selection"),
        #            uiOutput("min_delta_Rn_selection"),
        #            downloadButton("molis_export", "MOLIS Export"),
        #            downloadButton("pdf_export", "PDF Export")
        #            )
        #        ),
        
        column(10,
               plotOutput("run_plot"),
               dataTableOutput("results")
               )
        )
    
    # fluidRow(
    #     hr(),
    #     column(8,
    #            #plotOutput("run_plot")
    #            )
    #     )
    ))

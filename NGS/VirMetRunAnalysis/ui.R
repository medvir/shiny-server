library(shiny)
library(shinyFiles)
library(DT)

shinyUI(fluidPage(
        #theme = shinytheme("lumen"),
        titlePanel("VirMet Run Analysis"),
        
        fluidRow(
                column(4,
                       wellPanel(
                           shinyDirButton("dir", "Select virmet_output directory",
                                          "Please select a directory", FALSE,
                                          buttonType = "default"),
                           span(h4(textOutput("check_tsv")), style = "color:red")
                       )),
                column(4,
                       wellPanel(uiOutput("samples_found"))),
                column(4,
                       wellPanel(
                               downloadButton("report", "Generate report"),
                               span(h4(textOutput("check_sample")), style = "color:red")
                       ))
        ),
        hr(),
        fluidRow(column(
                6,
                h3("Number of raw and quality filtered reads"),
                plotOutput(outputId = "plot_run")
        ),
        column(
                6,
                h3("Distribution of sequencing reads into taxonomic categories"),
                plotOutput(outputId = "plot_domain")
        )),
        hr(),
        fluidRow(
                column(
                        6,
                        h3("Detected viruses on species level"),
                        DT::dataTableOutput("table_species")
                ),
                column(
                        6,
                        h3("Corresponding virus reference sequences"),
                        DT::dataTableOutput("table_ssciname")
                )
        )
))
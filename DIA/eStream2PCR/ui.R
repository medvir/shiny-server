library(shiny)
library(shinythemes)

shinyUI(fluidPage(
    #theme = shinytheme("lumen"),
    titlePanel("eStream2PCR"),
    
    fluidRow(
        column(6,
               wellPanel(
                   fileInput("export_file", "eStream Export Datei [.csv]:", accept = ".csv")
               )),
        column(6,
               wellPanel(
                   downloadButton("template", "Download")
               ))
    ),
    hr(),
    fluidRow(
        column(12,
               h3("PCR Template"),
               tableOutput(outputId = "template_table")
        )),
    hr(),
    fluidRow(
        column(12,
               h3("Export File"),
               tableOutput(outputId = "export_table")
        ))
))
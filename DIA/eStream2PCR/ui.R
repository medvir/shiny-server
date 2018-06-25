library(shiny)
library(DT)

shinyUI(fluidPage(
    titlePanel("eStream2PCR"),
    fluidRow(
        column(3,
               wellPanel(
                   fileInput("export_file", "eStream Export Datei [.csv]:", accept = ".csv")
               )),
        column(3,
               wellPanel(
                   uiOutput("targets")
               )),
        column(3,
               wellPanel(
                   uiOutput("colors")
               )),
        column(3,
               wellPanel(
                   uiOutput("cycler"),
                   downloadButton("template", "Download")
               ))
    ),
    fluidRow(
        column(12,
               h3("PCR Template"),
               DT::dataTableOutput(outputId = "template_table")
               )
        ))
)
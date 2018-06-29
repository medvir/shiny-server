library(shiny)
library(DT)

shinyUI(fluidPage(
    titlePanel("eStream2PCR"),
    fluidRow(
        column(3,
               wellPanel(
                   fileInput("eStream_file", "eStream Export Datei [.csv]:", accept = ".csv")
               )),
        column(2,
               wellPanel(
                   uiOutput("colors"),
                   uiOutput("saturation"),
                   uiOutput("value")
               )),
        column(2,
               wellPanel(
                   uiOutput("targets"),
                   actionButton("select_all_targets", "Select all")
               )),
        column(3,
               h5("Download"),
               downloadButton("template_file", "Download")
        )
    ),
    hr(),
    fluidRow(
        column(12,
               h3("PCR Template"),
               DT::dataTableOutput(outputId = "template_table")
        )
    ))
)
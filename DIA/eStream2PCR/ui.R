library(shiny)
library(DT)

shinyUI(fluidPage(
    titlePanel("eStream2PCR"),
    fluidRow(
        column(4,
               wellPanel(
                   fileInput("export_file", "eStream Export Datei [.csv]:", accept = ".csv")
               )),
        # column(2,
        #        wellPanel(
        #            uiOutput("targets")
        #        )),
        column(2,
               wellPanel(
                   uiOutput("colors")
               )),
        # column(2,
        #        wellPanel(
        #            uiOutput("cycler")
        #        )),
        column(2,
               h5("Download"),
               downloadButton("template", "Download")
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
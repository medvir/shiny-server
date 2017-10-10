library(shiny)
library(shinythemes)

shinyUI(fluidPage(
        theme = shinytheme("readable"),
        titlePanel("Quantitative PCR"),
        
        fluidRow(
                column(width = 3,
                       wellPanel(
                               uiOutput("manual_ct"),
                               fileInput("ct_file", "csv Datei hochladen", accept = ".csv"),
                               uiOutput("header"),
                               uiOutput("sample_col"),
                               uiOutput("ct_col"),
                               uiOutput("selection"),
                               uiOutput("info_col")
                               )
                       ),
                column(width =3,
                       wellPanel(
                               radioButtons("virus", "Virus", choices = c("Adeno", "CMV", "EBV"), selected = NULL),
                               radioButtons("cycler", "Instrument", choices = c("QuantStudio", "Veriti", "PCR3", "PCR4"), selected = NULL),
                               textOutput("formula"),
                               h4(""),
                               plotOutput("std_curve")
                               )
                       ),
                column(width = 6,
                       wellPanel(selectInput("visum", "Visum", width = 100, choices = c("MHu", "AB", "CD")),
                                downloadButton("report", "Report erstellen")
                                 ),
                       h4("Resultat"),
                       tableOutput(outputId = "result")
                       )
                )
        ))

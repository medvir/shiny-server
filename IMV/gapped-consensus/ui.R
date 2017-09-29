library(shiny)
library(shinyFiles)

shinyUI(fluidPage(
        
        titlePanel("Create gapped consensus"),
        
        fluidRow(
                column(width = 4,
                       wellPanel(
                               fileInput("cons_file", "upload consensus file (fasta)", accept = ".fasta"),
                               fileInput("depth_file", "upload samtools depth file (depth)", accept = ".depth"),
                               numericInput("min_cov", "minimal coverage", 1, min = 0, max = 100, step = 1),
                               textOutput("match"),
                               span(h4(textOutput("warning")), style="color:red"),
                               #actionButton("save", "Save"),
                               shinySaveButton("save", "Save fasta file", "Save file as ...", filetype = list(fasta = "fasta"))
                               )
                       ),
                column(width = 8,
                       plotOutput("cov_plot"),
                       textOutput("sequence")
                       )
                )
        ))

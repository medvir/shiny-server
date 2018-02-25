library(shiny)
library(DT)

shinyUI(fluidPage(
        #theme = shinytheme("lumen"),
        titlePanel("VirMet Run Analysis"),
        
        fluidRow(
                column(4,
                       wellPanel(
                               fileInput("reads_file", "Select run_reads_summary.tsv file:", accept = ".tsv"),
                               fileInput("orgs_file", "Select orgs_species_found.tsv file:", accept = ".tsv"),
                               span(h4(textOutput("check_tsv")), style="color:red")
                       )
                ),
                column(4,
                       wellPanel(
                               uiOutput("samples_found")
                       )
                ),
                column(4,
                       wellPanel(
                               downloadButton("report", "Generate report"),
                               span(h4(textOutput("check_sample")), style="color:red")
                       )
                )
        ),
        fluidRow(
                column(6,
                       h3("Number of raw and quality filtered reads"),
                       plotOutput(outputId = "plot_run")
                ),
                column(6,
                       h3("Distribution of sequencing reads into taxonomic categories"),
                       plotOutput(outputId = "plot_domain")
                )
        ),
        fluidRow(
                column(12,
                       h1(""),
                       h3("Detected virus species"),
                       DT::dataTableOutput(outputId = "table_species")
                )
        )
)
)

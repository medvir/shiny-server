library(shiny)
library(DT)
library(shinycustomloader)

shinyUI(fluidPage(
        #theme = shinytheme("lumen"),
        titlePanel("VirMet Run Analysis"),
        
        fluidRow(column(4,
                        wellPanel(
                                fileInput("reads_file", "Select run_reads_summary.tsv file:", accept = ".tsv"),
                                fileInput("orgs_file", "Select orgs_species_found.tsv file:", accept = ".tsv"),
                                span(h4(textOutput("check_tsv")), style = "color:red"))),
                 column(4,
                        wellPanel(uiOutput("samples_found"))),
                 column(4,
                        selectInput("user_name", "User name:", 
                                    choices = sort(c("",
                                                     "Verena Kufner",
                                                     "Maryam Zaheri",
                                                     "Gabriela Ziltener",
                                                     "Michael Huber",
                                                     "Stefan Schmutz"))),
                        numericInput("read_length", "Read length", value = 151)),
                 column(4,
                        wellPanel(
                                downloadButton("report", "Generate report"),
                                span(h4(textOutput("check_sample")), style = "color:red")))),
        
        hr(),
        
        fluidRow(column(6,
                        h3("Number of raw and quality filtered reads"),
                        withLoader(plotOutput(outputId = "plot_run"), type = "html", loader = "dnaspin")),
                 column(6,
                        h3("Distribution of sequencing reads into taxonomic categories"),
                        withLoader(plotOutput(outputId = "plot_domain"), type = "html", loader = "dnaspin"))),
        
        checkboxInput("checkbox_phages", label = "Hide phages", value = FALSE),
        
        checkboxInput("checkbox_blacklist", label = "Hide blacklisted viruses", value = FALSE),
        
        verbatimTextOutput("value"),
        
        #hr(),
        #fluidRow(column(3, verbatimTextOutput("value"))),
        
        hr(),
        
        fluidRow(column(6,
                        h3("Detected viruses on species level"),
                        DT::dataTableOutput("table_species")),
                column(6,
                       h3("Corresponding virus reference sequences"),
                       DT::dataTableOutput("table_ssciname")))
        ))
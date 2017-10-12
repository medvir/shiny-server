### HHV7app

library(shiny)

shinyUI(fluidPage(
        titlePanel("HHV7app"),
        sidebarLayout(
                
                sidebarPanel(
                        checkboxGroupInput("sample_type", "Sample Types", c("Blood", "Throat swab", "Urine"), selected = c("Blood", "Throat swab", "Urine")),
                        actionButton("select_all_sample_types", "Select all"),
                        hr(),
                        uiOutput("sample_selection"),
                        actionButton("select_all_samples", "Select all"),
                        hr(),
                        uiOutput("group_selection"),
                        actionButton("select_all_groups", "Select all"),
                        hr(),
                        uiOutput("student_selection"),
                        actionButton("select_all_students", "Select all"),
                        width = 2),
                
                mainPanel(
                        tabsetPanel(
                                tabPanel("Quantitative",
                                         verticalLayout(h2("HHV-7 concentrations in different sample types"),
                                                        plotOutput("plot_quant", height = 800)
                                                        )),
                                tabPanel("Qualitative",
                                         verticalLayout(h2("HHV-7 results in different sample types"),
                                                        plotOutput("plot_qual", height = 800)
                                         )),
                                tabPanel("Sex",
                                         verticalLayout(h2("HHV-7 concentrations in male and female individuals"),
                                                        plotOutput("plot_sex", height = 800)
                                         )),
                                tabPanel("Age",
                                         verticalLayout(h2("HHV-7 concentrations depending on age"),
                                                        plotOutput("plot_age", height = 800)
                                         )),
                                 tabPanel("Symptoms",
                                          verticalLayout(h2("HHV-7 concentrations depending on symptoms"),
                                                         plotOutput("plot_symptoms", height = 800)
                                          )),
                                tabPanel("Replicates",
                                         verticalLayout(h2("HHV-7 concentrations measured by different students"),
                                                        plotOutput("plot_repli", height = 800)
                                         ))
                                ),
                        hr(),
                        h2("Data table"),
                        tableOutput("data_table")
                        )
                )
))
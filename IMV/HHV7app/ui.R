### HHV7app

library(shiny)

plot_height = 800

shinyUI(fluidPage(
        #titlePanel("HHV7app"),
        sidebarLayout(
                
                sidebarPanel(
                        checkboxGroupInput("sample_type", "Sample Types", choiceValues = c("BE", "AR", "UR"), choiceNames = c("Blood", "Throat swab", "Urine"), selected = c("BE", "AR", "UR")),
                        actionButton("select_all_sample_types", "Select all"),
                        hr(),
                        uiOutput("sample_selection"),
                        actionButton("select_all_samples", "Select all"),
                        hr(),
                        # uiOutput("course_selection"),
                        # actionButton("select_all_courses", "Select all"),
                        # hr(),
                        uiOutput("student_selection"),
                        actionButton("select_all_students", "Select all"),
                        width = 1),
                
                mainPanel(
                        tabsetPanel(
                                tabPanel("Quantitative",
                                         verticalLayout(h2("HHV-7 concentrations in different sample types"),
                                                        plotOutput("plot_quant", height = plot_height)
                                                        )),
                                tabPanel("Qualitative",
                                         verticalLayout(h2("HHV-7 results in different sample types"),
                                                        plotOutput("plot_qual", height = plot_height)
                                         )),
                                tabPanel("Sex",
                                         verticalLayout(h2("HHV-7 concentrations in male and female individuals"),
                                                        plotOutput("plot_sex", height = plot_height)
                                         )),
                                tabPanel("Age",
                                         verticalLayout(h2("HHV-7 concentrations depending on age"),
                                                        plotOutput("plot_age", height = plot_height)
                                         )),
                                 tabPanel("Symptoms",
                                          verticalLayout(h2("HHV-7 concentrations depending on symptoms"),
                                                         plotOutput("plot_symptoms", height = plot_height)
                                          )),
                                tabPanel("Replicates",
                                         verticalLayout(h2("HHV-7 concentrations measured by different students"),
                                                        plotOutput("plot_repli", height = plot_height)
                                         ))
                                ),
                        hr(),
                        h2("Data table"),
                        tableOutput("data_table")
                        )
                )
))
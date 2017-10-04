### HHV7app

library(shiny)
library(shinythemes)

shinyUI(fluidPage(
        #theme = shinytheme("cerulean"),
        
        titlePanel("HHV7app"),
        sidebarLayout(
                
                sidebarPanel(
                        checkboxGroupInput("sample_type", "Sample Types", c("Blood", "Throat swab", "Urine"), selected = c("Blood", "Throat swab", "Urine")),
                        actionButton("select_all_sample_types", "Select all"),
                        hr(),
                        uiOutput("sample_selection"),
                        actionButton("select_all_samples", "Select all"),
                        hr(),
                        uiOutput("student_selection"),
                        actionButton("select_all_students", "Select all"),
                        width = 2),
                
                mainPanel(
                        tabsetPanel(
                                tabPanel("Sample",
                                         verticalLayout(h2("HHV-7 concentrations in different sample types"),
                                                        plotOutput("plot_sample", height = 800)
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
                                                        plotOutput("plot_symtpoms", height = 800)
                                         )),
                                tabPanel("Group",
                                         verticalLayout(h2("HHV-7 concentrations measured by the four student groups"),
                                                        plotOutput("plot_group", height = 800)
                                         ))
                                )
                        )
                )
))
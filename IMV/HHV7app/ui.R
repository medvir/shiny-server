library(shiny)
library(shinythemes)

shinyUI(fluidPage(
        theme = shinytheme("cerulean"),
        titlePanel("HHV7app"),
        fluidRow(
                column(2,
                       wellPanel(
                               checkboxGroupInput("sample_type", "Sample Type", c("Blood", "Throat swab", "Urine"), selected = c("Blood", "Throat swab", "Urine")),
                               actionButton("all_types", "Select all"),
                               hr(),
                               uiOutput("samples_found"),
                               actionButton("all_samples", "Select all"),
                               hr(),
                               uiOutput("students_found"),
                               actionButton("all_students", "Select all"),
                               hr(),
                               radioButtons("comparison", "Comparison", c("Sample Type" = "sample_type", "Sex" = "sex", "Age" = "age", "Group" = "group", "Symptoms" = "symptoms"), selected = "sample_type")
                               )
                       ),
                column(10,
                       plotOutput("plot", height = 800)
                        )
                )
        )
)

library(shiny)

shinyUI(fluidPage(
        titlePanel("tidyverse"),
        mainPanel(
                plotOutput("plot")
        )
))

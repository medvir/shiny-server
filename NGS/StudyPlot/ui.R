library(shiny)
library(shinythemes)

shinyUI(fluidPage(
        theme = shinytheme("united"),
        titlePanel("CRPP Transplant Study"),
        sidebarPanel(
                radioButtons("ids", "IDs", c("All", "Lung transplants", "Kidney Pairs"), selected = "All"),
                checkboxGroupInput("organs", "Organs", c("Lung", "Kidney", "Kidney and Pancreas"), selected = c("Lung", "Kidney", "Kidney and Pancreas")),
                checkboxGroupInput("scenario", "Scenario", c("routine", "symptomatic"), selected = c("routine", "symptomatic")),
                radioButtons("time", "Time", c("relative", "absolute"), selected = "relative"),
                radioButtons("sort_crit", "Sorting", c("Tpx Date" = "tpx_date", "ID" = "id"), selected = "tpx_date"),
                radioButtons("sort_dir", "Direction", c("descending" = "TRUE", "ascending" = "FALSE"), selected = "TRUE"),
                width = 2
                ),
        mainPanel(
                plotOutput('plot')
                )
        ))

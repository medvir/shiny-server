library(shiny)
library(shinythemes)

shinyUI(fluidPage(
        theme = shinytheme("united"),
        titlePanel("CRPP Transplant Study"),
        sidebarPanel(
                radioButtons("ids", "IDs", c("All", "Kidney Pairs"), selected = "All"),
                checkboxGroupInput("organs", "Organs", c("Lung", "Kidney", "Kidney and Pancreas"), selected = "Lung"),
                checkboxGroupInput("scenario", "Scenario", c("routine", "symptomatic"), selected = "routine"),
                radioButtons("time", "Time", c("relative", "absolute"), selected = "relative"),
                radioButtons("sort_crit", "Sorting", c("ID" = "id", "Tpx Date" = "tpx_date"), selected = "id"),
                radioButtons("sort_dir", "Direction", c("descending" = "TRUE", "ascending" = "FALSE"), selected = "TRUE"),
                width = 2
                ),
        mainPanel(
                plotOutput('plot')
                )
        ))
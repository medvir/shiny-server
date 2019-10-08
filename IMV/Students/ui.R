library(shiny)
library(shinythemes)

shinyUI(fluidPage(
        theme = shinytheme("united"),
        titlePanel("Students"),
        sidebarPanel(
                checkboxGroupInput("supervisor", "Supervisor",
                                   c("AT", "BGH", "SSt", "MH"),
                                   selected = c("AT", "BGH", "SSt", "MH")),
                
                checkboxGroupInput("event", "Event",
                                   c("Committee Meeting", "Progress Meeting", "Appraisal Meeting"),
                                   selected = c("Committee Meeting", "Progress Meeting", "Appraisal Meeting")),
                
                radioButtons("time", "Time", c("relative", "absolute"),
                             selected = "relative"),
                
                radioButtons("sort_crit", "Sorting",
                             c("Start Date" = "StartDate", "Name" = "Name"),
                             selected = "Name"),
                
                radioButtons("sort_dir", "Direction",
                             c("descending" = "TRUE", "ascending" = "FALSE"),
                             selected = "TRUE"),
                width = 2
                ),
        mainPanel(
                plotOutput('plot'),
                tableOutput('table')
                )
        ))

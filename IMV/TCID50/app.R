library(tidyverse)
library(shiny)

# UI 
ui <- fluidPage(
    titlePanel("TCID 50 calculator"),
    sidebarPanel(
        numericInput("init", "Initial sample dilution factor", min = 1, max = 10000, value = 1, step = 1),
        numericInput("vol", "Inoculation volume [ml]", min = 0.1, max = 10, value = 0.2, step = 0.1),
        numericInput("dil", "Titration dilution factor", min = 1, max = 50, value = 3, step = 1),
        sliderInput("rep", "Replicates", min = 4, max = 6, value = 4, step = 1, ticks = TRUE)
    ),
    mainPanel(
        fluidRow(
            h5("Check all positive wells"),
            column(1,
                   h5("1"),
                   textOutput("dil1"),
                   checkboxInput("a1", "", value = TRUE),
                   checkboxInput("b1", "", value = TRUE),
                   checkboxInput("c1", "", value = TRUE),
                   checkboxInput("d1", "", value = TRUE),
                   uiOutput("e1"),
                   uiOutput("f1")
            ),
            column(1,
                   h5("2"),
                   textOutput("dil2"),
                   checkboxInput("a2", "", value = TRUE),
                   checkboxInput("b2", "", value = TRUE),
                   checkboxInput("c2", "", value = TRUE),
                   checkboxInput("d2", "", value = TRUE),
                   uiOutput("e2"),
                   uiOutput("f2")
            ),
            column(1,
                   h5("3"),
                   textOutput("dil3"),
                   checkboxInput("a3", "", value = FALSE),
                   checkboxInput("b3", "", value = FALSE),
                   checkboxInput("c3", "", value = FALSE),
                   checkboxInput("d3", "", value = FALSE),
                   uiOutput("e3"),
                   uiOutput("f3")
            ),
            column(1,
                   h5("4"),
                   textOutput("dil4"),
                   checkboxInput("a4", "", value = FALSE),
                   checkboxInput("b4", "", value = FALSE),
                   checkboxInput("c4", "", value = FALSE),
                   checkboxInput("d4", "", value = FALSE),
                   uiOutput("e4"),
                   uiOutput("f4")
            ),
            column(1,
                   h5("5"),
                   textOutput("dil5"),
                   checkboxInput("a5", "", value = FALSE),
                   checkboxInput("b5", "", value = FALSE),
                   checkboxInput("c5", "", value = FALSE),
                   checkboxInput("d5", "", value = FALSE),
                   uiOutput("e5"),
                   uiOutput("f5")
            ),
            column(1,
                   h5("6"),
                   textOutput("dil6"),
                   checkboxInput("a6", "", value = FALSE),
                   checkboxInput("b6", "", value = FALSE),
                   checkboxInput("c6", "", value = FALSE),
                   checkboxInput("d6", "", value = FALSE),
                   uiOutput("e6"),
                   uiOutput("f6")
            ),
            column(1,
                   h5("7"),
                   textOutput("dil7"),
                   checkboxInput("a7", "", value = FALSE),
                   checkboxInput("b7", "", value = FALSE),
                   checkboxInput("c7", "", value = FALSE),
                   checkboxInput("d7", "", value = FALSE),
                   uiOutput("e7"),
                   uiOutput("f7")
            ),
            column(1,
                   h5("8"),
                   textOutput("dil8"),
                   checkboxInput("a8", "", value = FALSE),
                   checkboxInput("b8", "", value = FALSE),
                   checkboxInput("c8", "", value = FALSE),
                   checkboxInput("d8", "", value = FALSE),
                   uiOutput("e8"),
                   uiOutput("f8")
            )
        ),
        fluidRow(
            column(12,
                   textOutput("pos"),
                   hr(),
                   h3("Results"),
                   h4(textOutput("logtcid50")),
                   h4(textOutput("tcid50"))
            )
        )
    )
    )


# SERVER
server <- function(input, output) {
    
    pos = reactive({
        n = c(input$a1, input$b1, input$c1, input$d1, input$e1, input$f1,
              input$a2, input$b2, input$c2, input$d2, input$e2, input$f2,
              input$a3, input$b3, input$c3, input$d3, input$e3, input$f3,
              input$a4, input$b4, input$c4, input$d4, input$e4, input$f4,
              input$a5, input$b5, input$c5, input$d5, input$e5, input$f5,
              input$a6, input$b6, input$c6, input$d6, input$e6, input$f6,
              input$a7, input$b7, input$c7, input$d7, input$e7, input$f7,
              input$a8, input$b8, input$c8, input$d8, input$e8, input$f8)
        sum(n, na.rm = TRUE)
    })
    
    logtcid50 = reactive({
        log(input$init,10) + (pos()/input$rep - 0.5)*log(input$dil,10) + log(1/input$vol,10)
    })
    
    tcid50 = reactive({
        10^logtcid50()
    })
    
    # result output
    output$pos <- renderText({paste("Number of positive wells =", pos())})
    output$logtcid50 <- renderText({paste("log TCID50 =", round(logtcid50(),2))})
    output$tcid50 <- renderText({paste("TCID50 =", round(tcid50(),2))})
    
    # dilution labels
    output$dil1 <- renderText({input$init * input$dil^0})
    output$dil2 <- renderText({input$init * input$dil^1})
    output$dil3 <- renderText({input$init * input$dil^2})
    output$dil4 <- renderText({input$init * input$dil^3})
    output$dil5 <- renderText({input$init * input$dil^4})
    output$dil6 <- renderText({input$init * input$dil^5})
    output$dil7 <- renderText({input$init * input$dil^6})
    output$dil8 <- renderText({input$init * input$dil^7})
    
    # checkboxes > 4 replicates    
    output$e1 <- renderUI({if (input$rep > 4) {checkboxInput("e1", "", value = FALSE)}})
    output$e2 <- renderUI({if (input$rep > 4) {checkboxInput("e2", "", value = FALSE)}})
    output$e3 <- renderUI({if (input$rep > 4) {checkboxInput("e3", "", value = FALSE)}})
    output$e4 <- renderUI({if (input$rep > 4) {checkboxInput("e4", "", value = FALSE)}})
    output$e5 <- renderUI({if (input$rep > 4) {checkboxInput("e5", "", value = FALSE)}})
    output$e6 <- renderUI({if (input$rep > 4) {checkboxInput("e6", "", value = FALSE)}})
    output$e7 <- renderUI({if (input$rep > 4) {checkboxInput("e7", "", value = FALSE)}})
    output$e8 <- renderUI({if (input$rep > 4) {checkboxInput("e8", "", value = FALSE)}})
    
    # checkboxes > 5 replicates
    output$f1 <- renderUI({if (input$rep > 5) {checkboxInput("f1", "", value = FALSE)}})
    output$f2 <- renderUI({if (input$rep > 5) {checkboxInput("f2", "", value = FALSE)}})
    output$f3 <- renderUI({if (input$rep > 5) {checkboxInput("f3", "", value = FALSE)}})
    output$f4 <- renderUI({if (input$rep > 5) {checkboxInput("f4", "", value = FALSE)}})
    output$f5 <- renderUI({if (input$rep > 5) {checkboxInput("f5", "", value = FALSE)}})
    output$f6 <- renderUI({if (input$rep > 5) {checkboxInput("f6", "", value = FALSE)}})
    output$f7 <- renderUI({if (input$rep > 5) {checkboxInput("f7", "", value = FALSE)}})
    output$f8 <- renderUI({if (input$rep > 5) {checkboxInput("f8", "", value = FALSE)}})
}


# RUN APPLICATION
shinyApp(ui = ui, server = server)

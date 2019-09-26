library(tidyverse)
library(shiny)

# UI 
ui <- fluidPage(
    titlePanel("TCID 50 calculator"),
    fluidRow(
        h3("Assay setup"),
        column(3,
               numericInput("init", "Initial dilution factor", min = 1, max = 10000, value = 1)
        ),
        column(3,
               numericInput("dil", "Dilution factor", min = 1, max = 50, value = 3)
        ),
        column(3,
               sliderInput("rep", "Replicates", min = 4, max = 6, value = 4, step = 1, ticks = FALSE)
        )
    ),
    fluidRow(
        h3("Results"),
        h5("Check all positive wells"),
        column(1,
               h5("1"),
               checkboxInput("a1", "", value = TRUE),
               checkboxInput("b1", "", value = TRUE),
               checkboxInput("c1", "", value = TRUE),
               checkboxInput("d1", "", value = TRUE),
               uiOutput("e1"),
               uiOutput("f1")
        ),
        column(1,
               h5("2"),
               checkboxInput("a2", "", value = TRUE),
               checkboxInput("b2", "", value = TRUE),
               checkboxInput("c2", "", value = TRUE),
               checkboxInput("d2", "", value = TRUE),
               uiOutput("e2"),
               uiOutput("f2")
        ),
        column(1,
               h5("3"),
               checkboxInput("a3", "", value = FALSE),
               checkboxInput("b3", "", value = FALSE),
               checkboxInput("c3", "", value = FALSE),
               checkboxInput("d3", "", value = FALSE),
               uiOutput("e3"),
               uiOutput("f3")
        ),
        column(1,
               h5("4"),
               checkboxInput("a4", "", value = FALSE),
               checkboxInput("b4", "", value = FALSE),
               checkboxInput("c4", "", value = FALSE),
               checkboxInput("d4", "", value = FALSE),
               uiOutput("e4"),
               uiOutput("f4")
        ),
        column(1,
               h5("5"),
               checkboxInput("a5", "", value = FALSE),
               checkboxInput("b5", "", value = FALSE),
               checkboxInput("c5", "", value = FALSE),
               checkboxInput("d5", "", value = FALSE),
               uiOutput("e5"),
               uiOutput("f5")
        ),
        column(1,
               h5("6"),
               checkboxInput("a6", "", value = FALSE),
               checkboxInput("b6", "", value = FALSE),
               checkboxInput("c6", "", value = FALSE),
               checkboxInput("d6", "", value = FALSE),
               uiOutput("e6"),
               uiOutput("f6")
        )
    ),
    fluidRow(
        column(12,
               textOutput("pos"),
               h1(" "),
               h4("TCID Results"),
               textOutput("logtcid50"),
               textOutput("tcid50")
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
              input$a6, input$b6, input$c6, input$d6, input$e6, input$f6)
        sum(n, na.rm = TRUE)
    })
    
    logtcid50 = reactive({
        log(input$init,10) + (pos()/input$rep - 0.5)*log(input$dil,10)
    })
    
    tcid50 = reactive({
        10^logtcid50()
    })
    
    output$pos <- renderText({
        paste("Number of positive wells =", pos())
    })
    
    output$logtcid50 <- renderText({
        paste("log TCID 50 =", round(logtcid50(),2))
    })
    
    output$tcid50 <- renderText({
        paste("TCID 50 =", round(tcid50(),2))
    })
    
    # 5 replicates    
    output$e1 <- renderUI({
        if (input$rep > 4) {
            checkboxInput("e1", "", value = FALSE)
        }
    })
    
    output$e2 <- renderUI({
        if (input$rep > 4) {
            checkboxInput("e2", "", value = FALSE)
        }
    })
    
    output$e3 <- renderUI({
        if (input$rep > 4) {
            checkboxInput("e3", "", value = FALSE)
        }
    })
    
    output$e4 <- renderUI({
        if (input$rep > 4) {
            checkboxInput("e4", "", value = FALSE)
        }
    })
    
    output$e5 <- renderUI({
        if (input$rep > 4) {
            checkboxInput("e5", "", value = FALSE)
        }
    })
    
    output$e6 <- renderUI({
        if (input$rep > 4) {
            checkboxInput("e6", "", value = FALSE)
        }
    })
    
    # 6 replicates
    output$f1 <- renderUI({
        if (input$rep > 5) {
            checkboxInput("f1", "", value = FALSE)
        }
    })
    
    output$f2 <- renderUI({
        if (input$rep > 5) {
            checkboxInput("f2", "", value = FALSE)
        }
    })
    
    output$f3 <- renderUI({
        if (input$rep > 5) {
            checkboxInput("f3", "", value = FALSE)
        }
    })
    
    output$f4 <- renderUI({
        if (input$rep > 5) {
            checkboxInput("f4", "", value = FALSE)
        }
    })
    
    output$f5 <- renderUI({
        if (input$rep > 5) {
            checkboxInput("f5", "", value = FALSE)
        }
    })
    
    output$f6 <- renderUI({
        if (input$rep > 5) {
            checkboxInput("f6", "", value = FALSE)
        }
    })
}


# RUN
shinyApp(ui = ui, server = server)

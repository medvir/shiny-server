#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(circlize)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  selected_cols <- cols(
    time = col_character(),
    treatment = col_character(),
    full_id = col_character(),
    gene = col_character(),
    log_fc = col_double(),
    pvalue = col_double(),
    qvalue = col_double()
  )
  
  path = "appdata/"
  
  files <- reactive({
      list.files(path = path, pattern = ".csv")
  })
  
  output$fileselect <- renderUI({
      selectInput("datafile",
                  label = "Data File",
                  choices = files())
  })
  
  treatments <- reactive({
      selected() %>%
          pull(treatment) %>%
          unique()
  })
  
  output$treatmentselect <- renderUI({
      selectInput("treatment",
                  label = "Treatment",
                  choices = treatments())
  })
  
  selected <- reactive({
      read_csv(paste0(path, input$datafile), col_types = selected_cols)
  })

  p_threshold <- reactive({
    if (input$filter_p){
      input$pvalue
    }
    else{
      1.0
    }
  })
  
  q_threshold <- reactive({
    if (input$filter_q){
      input$qvalue
    }
    else{
      1.0
    }
  })
  
  fc_threshold <- reactive({
    if (input$filter_fc){
      input$log_fc
    }
    else{
      0.0
    }
  })

  all_treatments <- reactive({
    selected() %>%
      filter(time == input$time,
             abs(log_fc) >= fc_threshold(),
             pvalue <= p_threshold(),
             qvalue <= q_threshold())
  })  
  
  filtered_data <- reactive({
    all_treatments() %>%
      filter(treatment == input$treatment) %>%
      select(-c(time, treatment))
  })
  
  unique_genes <- reactive({
    filtered_data() %>%
      select(gene) %>%
      distinct(gene) %>%
      arrange(gene)
  })
  
  output$genesTable <- renderDataTable({
    unique_genes()},
    options = list(pageLength=4)
  )
  
  output$genesDownload <- downloadHandler(
    filename = function(){
      paste0("genes-", input$time, '-', input$treatment, ".csv")
    },
    content <- function(file){
      write_csv(unique_genes(), file)
    }
  )
  
  output$peptidesTable <- renderDataTable({
    filtered_data()},
    options = list(pageLength=4)
  )
  
  output$peptidesDownload <- downloadHandler(
    filename = function(){
      paste0("peptides-", input$time, '-', input$treatment, ".csv")
    },
    content <- function(file){
      write_csv(filtered_data(), file)
    }
  )
  
  
  output$zircosPlot <- renderPlot({
    if (input$goButton == 0)
      return()
    x <- isolate(rnorm(n = 100))
    hist(x)
  })
  
  plotInput <- function(){
    
    if (input$goButton == 0)
      return()
    
    df <- isolate({
      all_treatments() %>%
        select(c(treatment, gene, log_fc, qvalue, pvalue, full_id)) %>%
        mutate(colour = ifelse(log_fc > 0, '#E69F00', '#56B4E9')) %>%
        rbind(c('alpha2', NA, 0.0, 1, 1, 'fake_alpha2', '#FFFFFF')) %>%
        rbind(c('omega', NA, 0.0, 1, 1, 'fake_omega', '#FFFFFF')) %>%
        rbind(c('beta', NA, 0.0, 1, 1, 'fake_beta', '#FFFFFF')) %>%
        rbind(c('lambda1', NA, 0.0, 1, 1, 'fake_lambda1', '#FFFFFF')) %>%
        rbind(c('gamma', NA, 0.0, 1, 1, 'fake_gamma', '#FFFFFF')) %>%
        mutate(log_fc = as.numeric(log_fc), qvalue = as.numeric(qvalue))
    })
    
    cp <- df %>%
      full_join(df, by = 'full_id') %>%
      filter(treatment.x != treatment.y)
    
    # compute location of major ticks to fix an issue
    n_sectors <- length(unique(df$treatment))
    n_ticks <- 1 + ceiling(18 / n_sectors)
    if (n_ticks %% 2 == 0){
      n_ticks <- n_ticks + 1
    }
    x_max <- ceiling(max(abs(df$log_fc)))
    if (x_max %% 2){
      b <- x_max - 1
    }
    else{
      b <- x_max - 2
    }
    if(b == 2){
      b <- 3
      n_ticks <- 7
    }
    circos.clear()
    isolate({
    major_ticks <- round(seq(-b, b, length.out = n_ticks), 1)
    if(input$time == '4h'){
      circos.par("track.height" = 0.1, "canvas.ylim" = c(-1.25, 1.25))
    }
    else{
      circos.par("track.height" = 0.1, "canvas.ylim" = c(-1.25, 1.25))
    }
    circos.initialize(factors = df$treatment, x = df$log_fc, xlim = c(-x_max, x_max))
    
    circos.track(factors = df$treatment, y = df$qvalue,
                 panel.fun = function(x, y) {
                   circos.text(CELL_META$xcenter, CELL_META$cell.ylim[2] + uy(5, "mm"),
                               CELL_META$sector.index)
                   circos.axis(labels.cex = 0.9, minor.ticks = FALSE, major.at = major_ticks)
                 })
    
    circos.trackPoints(df$treatment, df$log_fc, df$qvalue, col = df$colour, pch = 16, cex = 0.75)
    # circos.text(-1, 0.5, "text", sector.index = "alpha2", track.index = 1) place text outside
    for(i in 1:nrow(cp)) {
      row <- cp[i, ]
      circos.link(row$treatment.x, row$log_fc.x, row$treatment.y, row$log_fc.y)
    }
    # legend("bottomleft", pch = 1, legend = "This is the legend")
    title(paste('time =', input$time))
    
  })
  }

  output$circosPlot <- renderPlot({
    print(plotInput())
  })
    
  output$plotDownload <- downloadHandler(
    filename = paste0('circos-', input$time, '.png'),
    content = function(file) {
      png(file, height = 1400, width = 1400)
      plotInput()
      dev.off()
    })    
  
})

library(shiny)
#library(tidyverse)
library(stringr)
library(seqinr)
library(cowplot)
library(shinyFiles)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)

shinyServer(function(input, output, session) {

        cons_data <- reactive({
                req(input$cons_file)
                data.frame(CONS = unlist(strsplit(readLines(input$cons_file$datapath)[-1], ""))) %>%
                        mutate(POS = seq.int(nrow(.)))
                })
        
        cons_name <- reactive({
                req(input$cons_file)
                unlist(readLines(input$cons_file$datapath)[1]) %>% sub(">", "", .)
                })
        
        depth_data <- reactive({
                req(input$depth_file)
                depth = read_delim(input$depth_file$datapath, "\t", col_names = FALSE, trim_ws = TRUE, col_types = "cii") %>% select(X3) %>% unlist()
                names(depth) = read_delim(input$depth_file$datapath, "\t", col_names = FALSE, trim_ws = TRUE) %>% select(X2) %>% unlist()
                data.frame(POS = 1:max(as.numeric(names(depth)))) %>%
                        mutate(COV = ifelse(is.na(depth[as.character(POS)]), 0, depth[as.character(POS)]))
                })
        
        depth_name <- reactive({
                req(input$depth_file)
                read_delim(input$depth_file$datapath, "\t", col_names = FALSE) %>% pull(X1) %>% unique()
                })
        
        sequence <- reactive({
                stopifnot(nrow(cons_data()) >= nrow(depth_data()))
                full_join(cons_data(), depth_data(), by = "POS") %>%
                        mutate(CONS = as.character(CONS)) %>%
                        mutate(COV = as.numeric(COV)) %>%
                        mutate(CONS = case_when(
                                COV < input$min_cov ~ "-",
                                is.na(COV) ~ "-",
                                TRUE ~ CONS
                                )) %>%
                        pull(CONS)
                })
        
        output$sequence <- renderText({
                sequence()
               })  

        output$cov_plot <- renderPlot({
                depth_data() %>%
                ggplot(aes(x=POS, y=COV)) +
                geom_line(size=.5) +
                xlab('genome position') +
                ylab('coverage (reads)') +
                scale_y_log10(breaks = c(1,10,100,1000,10000,100000)) +
                background_grid(major = "xy") +
                geom_hline(yintercept = input$min_cov, color = "red", size = 0.5) +
                theme(legend.position = "bottom")
                })
        
        filename <- reactive({
                sub(".fasta", "_covgaps.fasta", input$cons_file[1])
                })
        
        observe({
                volumes = c("UserFolder" = "/")
                shinyFileSave(input, "save", roots = volumes, session = session)
                fileinfo = parseSavePath(volumes, input$save)
                if (nrow(fileinfo) > 0) {write.fasta(sequence(), filename(), as.character(fileinfo$datapath), open = "w", nbchar = 60)}
                })
        
        output$warning <- renderText({
                if (cons_name() != depth_name()) {paste("References do not match!", cons_name(), depth_name())}     
                })
        
        output$match <- renderText({
                if (cons_name() == depth_name()) {paste("References match:", cons_name())}
                })
        
        output$filename <- renderText({
                req(input$cons_file)
                paste("File name:", filename())
                })
 })

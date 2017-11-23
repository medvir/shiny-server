### qPCR Parameters ############################################################

### slope for each virus and cycler (y = m*x + b)
adeno_m = list(QuantStudio = 1, Veriti = 1.5, PCR3 = 0, PCR4 = 0)
cmv_m =   list(QuantStudio = 2, Veriti = 2.5, PCR3 = 0, PCR4 = 0)
ebv_m =   list(QuantStudio = 3, Veriti = 3.5, PCR3 = 0, PCR4 = 0)

### y-intercept for each virus and cycler (y = m*x + b)
adeno_b = list(QuantStudio = 20, Veriti = 25, PCR3 = 0, PCR4 = 0)
cmv_b =   list(QuantStudio = 20, Veriti = 25, PCR3 = 0, PCR4 = 0)
ebv_b =   list(QuantStudio = 20, Veriti = 25, PCR3 = 0, PCR4 = 0)

### don't allow ct values < or > than min or max ct
min_ct = 5
max_ct = 50

### limits for positive/borderline/negative interpretaion (cp/ml)
cp_pos = 1000
cp_bl = 10

################################################################################

library(shiny)
library(tidyverse)
library(cowplot)
library(readxl)

ms = list(Adeno = adeno_m, CMV = cmv_m, EBV = ebv_m )
bs = list(Adeno = adeno_b, CMV = cmv_b, EBV = ebv_b)

today = Sys.Date() %>% format(., format="%y%m%d")

shinyServer(function(input, output) {
        
        output$manual_ct <- renderUI({
                req(is.null(input$ct_file))
                textAreaInput("cts", "ct Werte eingeben", value = "", rows = 5)
                })
        
        output$selection <- renderUI({
                req(input$ct_file)
                selectInput("selection", "Eine oder mehrere Proben auswaehlen", ct_file() %>% pull(input$sample_col) %>% unique(), selected = ct_file() %>% pull(input$sample_col) %>% unique(), multiple = TRUE, selectize = FALSE, size = 10)
                })
        
        output$header <- renderUI({
                req(input$ct_file)
                radioButtons("header", "Titelzeile", choices = c("ja" = TRUE, "nein" = FALSE), inline = TRUE)
                })
        
        output$sample_col <- renderUI({
                req(input$ct_file)
                selectInput("sample_col", "Auswahl Spalte", colnames(ct_file()), selectize = FALSE, selected = "Sample_Name")
                })
        
        output$ct_col <- renderUI({
                req(input$ct_file)
                selectInput("ct_col", "ct Wert Spalte", colnames(ct_file()), selectize = FALSE, selected = "CT")
                })
        
        output$info_col <- renderUI({
                req(input$ct_file)
                if (ncol(ct_file()) > 2) {
                        selectInput("info_col", "Info Spalte", colnames(ct_file()), multiple=TRUE, selectize=FALSE, selected = "Target_Name")
                        }
                })
        
        ct_manual <- reactive({
                unlist(strsplit(input$cts, split ="\n"))
                })
        
        ct_file <- reactive({
                req(input$ct_file)
                print(input$ct_file)
                if (grepl(".csv", input$ct_file)) {
                        read_csv(input$ct_file$datapath, col_names = as.logical(input$header))
                }
                else if (grepl(".xls", input$ct_file)) {
                        read_excel(input$ct_file$datapath, col_names = as.logical(input$header))
                }
                })
        
        m <- reactive({
                ms[[input$virus]][[input$cycler]]
                })
        
        b <- reactive({
                bs[[input$virus]][[input$cycler]]
                })
        
        results <- reactive({
                if (is.null(input$ct_file)) {
                        req(input$cts)
                        ct_values = data.frame(Sample = as.integer(seq(1, length(ct_manual()))), ct = ct_manual()) %>%
                                mutate(ct = as.character(ct))
                } else {
                        req(input$selection)
                        if (ncol(ct_file()) > 2) {
                                ct_values = ct_file() %>% 
                                        select(Sample = input$sample_col, ct = input$ct_col, input$info_col) %>%
                                        filter(Sample %in% input$selection)
                        } else {
                                ct_values = ct_file() %>% 
                                        select(Sample = input$sample_col, ct = input$ct_col) %>%
                                        filter(Sample %in% input$selection)
                                }
                }
                ct_values %>%
                        mutate(ct_num = as.numeric(ct)) %>%
                        mutate(cp = as.integer((m() * ct_num + b()) * 1000)) %>%  ### formula to calculate copies/ml
                        mutate(cp = ifelse(ct_num < min_ct, NA, ifelse(ct_num > max_ct, NA, cp))) %>%
                        select(-ct_num) %>%
                        mutate(Interpretation = case_when(
                                cp >= cp_pos ~ "positiv",
                                cp >= cp_bl ~ "grenzwertig",
                                cp < cp_bl ~ "negativ",
                                TRUE ~ "NA")) %>%
                        rename("ct Wert" = ct, "Kopien/ml" = cp)
                })
        
        output$result <- renderTable({
                results()
                }) 

        output$formula <- renderText({
                paste("Kopien/ml = 1000 * (", m(), "* ct +", b(),")")
                })
        
        output$std_curve <- renderPlot({
                results() %>%
                        mutate(ct_num = as.numeric(`ct Wert`),
                               cp = as.numeric(`Kopien/ml`)) %>%
                        ggplot(aes(x = ct_num, y = cp)) +
                        geom_point() + 
                        geom_line() +
                        xlab("ct Wert") +
                        ylab("Kopien/ml") +
                        geom_vline(xintercept = min_ct, color = "red", linetype = 3) +
                        geom_vline(xintercept = max_ct, color = "red", linetype = 3)
                })
        
        output$report <- downloadHandler(
                filename = function() {
                        paste0(today, "_", input$virus, ".pdf")
                },
                content = function(file) {
                        tempReport <- file.path(tempdir(), "report.Rmd")
                        file.copy("Report.Rmd", tempReport, overwrite = TRUE)
                        
                        params <- list(virus = input$virus,
                                       cycler = input$cycler,
                                       results = results(),
                                       visum = input$visum)
                        
                        rmarkdown::render(tempReport, output_file = file,
                                          params = params,
                                          envir = new.env(parent = globalenv()))
                })
})

library(shiny)
library(cowplot)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
#library(shinythemes)

shinyServer(function(input, output) {
        
        reads_data <- reactive({
                req(input$reads_file)
                read_delim(input$reads_file$datapath, "\t")  #, sep = '\t', header = TRUE)
                })
        
        orgs_data <- reactive({
                req(input$orgs_file)
                read_delim(input$orgs_file$datapath, "\t") #, sep = '\t', header = TRUE)
                })
       
        output$plot_run <- renderPlot({
                req(!(is.null(input$chosen_sample)))
                reads_data() %>%
                        filter(sample %in% input$chosen_sample) %>%
                        filter(category == "raw_reads" | category == "passing_filter") %>%
                        mutate(quality = factor(category, levels = c("raw_reads", "passing_filter"))) %>%
                        ### plot 
                        ggplot(aes(x = quality, y = reads, fill = quality)) +
                        geom_bar(stat="identity", position=position_dodge()) +
                        panel_border() + background_grid(major = "xy", minor = "") +
                        xlab("") + ylab("") +
                        facet_wrap( ~ sample) +
                        theme(legend.position="bottom") + theme(legend.title=element_blank()) +
                        theme(axis.text.x  = element_text(angle=90, vjust=0.4, hjust = 1)) +
                        scale_x_discrete(breaks=NULL)
                })
        
        output$plot_domain <- renderPlot({
                req(!(is.null(input$chosen_sample)))
                reads_data() %>%
                        filter(sample %in% input$chosen_sample) %>%
                        filter(!(category == "raw_reads" | category == "passing_filter" | category == "reads_to_blast")) %>%
                        mutate(domain = case_when(
                                category %in% c("matching_bact1", "matching_bact2", "matching_bact3") ~ "bacterial",
                                category %in% c("viral_reads") ~ "viral",
                                category %in% c("matching_humanGRCh38") ~ "human",
                                category %in% c("undetermined_reads") ~ "unknown",
                                category %in% c("matching_bt_ref", "matching_fungi1") ~ "other",
                                TRUE ~ "none"
                                )) %>%
                        filter(!(domain == "none")) %>%
                        group_by(sample) %>%
                        mutate(percent = reads/sum(reads)) %>%
                        ### plot        
                        ggplot(aes(x = domain, y = percent, fill = domain)) +
                        geom_bar(stat="identity", position=position_dodge()) +
                        panel_border() + background_grid(major = "xy", minor = "") +
                        xlab("") + ylab("") +
                        facet_wrap( ~ sample) +
                        theme(legend.position="bottom") + theme(legend.title=element_blank()) +
                        theme(axis.text.x  = element_text(angle=90, vjust=0.4, hjust = 1)) + 
                        scale_x_discrete(breaks=NULL)
                })

        output$table_species <- renderTable({
                req(!(is.null(input$chosen_sample)))
                orgs_data() %>%
                        filter(sample %in% input$chosen_sample) %>%
                        select(-run) %>%
                        spread(key = sample, value = reads) %>%
                        mutate(total = as.integer(rowSums(.[,-1], na.rm = TRUE))) %>%
                        mutate(percent = round(total/sum(total)*100, 3)) %>%
                        mutate(abundance = strrep("+", 1+round(percent/max(percent)*9))) %>%
                        mutate(abundance = ifelse(total < 20, "-", abundance)) %>%
                        mutate(occurence = rowSums(is.na(.))) %>%
                        arrange(desc(total)) %>%
                        arrange(occurence) %>%
                        select(-occurence)
                }) 
        
        output$report <- downloadHandler(
                filename = function() {
                        paste0(substr(input$chosen_sample[1], 1, 13), ".pdf")
                        },
                content = function(file) {
                        tempReport <- file.path(tempdir(), "report.Rmd")
                        file.copy("RunReport.Rmd", tempReport, overwrite = TRUE)
                        
                        params <- list(orgs_file = input$orgs_file$datapath,
                                       reads_file = input$reads_file$datapath,
                                       sample_name = input$chosen_sample)
                        
                        rmarkdown::render(tempReport, output_file = file,
                                          params = params,
                                          envir = new.env(parent = globalenv()))
                })
        
        output$samples_found <- renderUI({
          found_samples <- reads_data() %>% arrange(sample) %>% .$sample %>% unique() 
          selectInput("chosen_sample", "Choose one or more samples:", found_samples, multiple=TRUE, selectize=FALSE)
          })
        
        
        output$check_tsv <- renderText({
          if (reads_data()$run[1] != orgs_data()$run[1]) {"Run IDs do not match!"}
          })
        
        
        output$check_sample <- renderText({
          if (length(unique(substr(input$chosen_sample, 1, 10))) > 1) {"More than one sample selected!"}
          })
})


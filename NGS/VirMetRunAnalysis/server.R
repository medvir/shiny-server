library(shiny)
library(cowplot)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(DT)

shinyServer(function(input, output) {
    
    reads_data <- reactive({
        req(input$reads_file)
        read_delim(input$reads_file$datapath, "\t")
    })
    
    orgs_data <- reactive({
        req(input$orgs_file)
        orgs_data <-
          read.delim(input$orgs_file$datapath, sep = "\t") %>%
          mutate(covered_percent = 100 * covered_region / seq_len) %>%
          mutate(covered_exp = 100 * (1 - exp(-reads * 151 / seq_len))) %>%
          mutate(covered_score = round(100 * covered_percent / covered_exp, 1)) %>%
          mutate(covered_percent = round(covered_percent, 1))
        
        if (isTRUE(input$checkbox)) {
          orgs_data <-
            orgs_data %>%
            filter(grepl("phage", species) == FALSE, ignore.case = TRUE) %>%
            filter(grepl("phage", ssciname) == FALSE, ignore.case = TRUE)
        }
        orgs_data
    })
    
    output$plot_run <- renderPlot({
        req(!(is.null(input$chosen_sample)))
        reads_data() %>%
            filter(sample %in% input$chosen_sample) %>%
            filter(category == "raw_reads" | category == "passing_filter") %>%
            mutate(quality = factor(category, levels = c("raw_reads", "passing_filter"))) %>%
            
            ### plot 
            ggplot(aes(x = quality, y = reads, fill = quality)) +
            geom_col(colour = "black") +
            scale_fill_manual(name = "",
                              values = c("#D55E00", "#009E73")) +
            panel_border() + background_grid(major = "xy", minor = "") +
            xlab("") + ylab("") +
            facet_wrap( ~ sample) +
            theme(legend.position = "bottom") + theme(legend.title = element_blank()) +
            theme(axis.text.x  = element_text(angle = 90, vjust = 0.4, hjust = 1)) +
            scale_x_discrete(breaks = NULL)
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
                category %in% c("matching_bt_ref") ~ "bovine",
                category %in% c("matching_fungi1") ~ "fungal",
                TRUE ~ "none"
            )) %>%
            mutate(domain = factor(as.character(domain), levels = c("human", "bacterial", "fungal", "bovine", "viral", "unknown"))) %>%
            filter(!(domain == "none")) %>%
            group_by(sample) %>%
            mutate(percent = reads/sum(reads)) %>%
            
            ### plot        
            ggplot(aes(x = domain, y = percent, fill = domain)) +
            geom_col(colour = "black") +
            scale_fill_manual(name = "",
                              values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")) +
            panel_border() + background_grid(major = "xy", minor = "") +
            xlab("") + ylab("") +
            facet_wrap( ~ sample) +
            theme(legend.position = "bottom") + theme(legend.title = element_blank()) +
            theme(axis.text.x  = element_text(angle = 90, vjust = 0.4, hjust = 1)) + 
            scale_x_discrete(breaks = NULL)
    })
    
    output$table_ssciname <- DT::renderDataTable(
        filter = "top",
        rownames = FALSE,
        options = list(pageLength = 100, autoWidth = FALSE),
        {
            req(!(is.null(input$chosen_sample)))
            orgs_data() %>%
                filter(sample %in% input$chosen_sample) %>%
                filter(species %in% species_selected()) %>%
                separate(stitle, c('short_name', 'appendix'), sep = ', complete', extra = 'merge') %>%
                select(short_name, reads, covered_score, sample) %>%
                arrange(desc(reads))
        })
    
    
    table_species <- reactive({
        req(!(is.null(input$chosen_sample)))
        orgs_data() %>%
            filter(sample %in% input$chosen_sample) %>%
            select(species, reads, sample) %>%
            group_by(species, sample) %>%
            summarise(reads_sum = sum(reads)) %>%
            spread(key = sample, value = reads_sum) %>%
            ungroup() %>%
            mutate(reads_total = as.integer(rowSums(.[,-1], na.rm = TRUE))) %>%
            arrange(desc(reads_total))
    })
    
    species_selected <- reactive({
        table_species()[input$hoverIndexJS + 1, ] %>%
            pull(species)
    })
    
    output$table_species <- DT::renderDataTable(
        filter = "top",
        rownames = FALSE,
        options = list(pageLength = 100,
                       autoWidth = FALSE,
                       ordering = FALSE,
                       dom = 't',
                       rowCallback = JS('function(row, data) {
                                                  $(row).mouseenter(function(){
                                                  var hover_index = $(this)[0]._DT_RowIndex
                                                  /* console.log(hover_index); */
                                                  Shiny.onInputChange("hoverIndexJS", hover_index);
                                                  });
                                          }')
        ), 
        {
            table_species()
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
                           sample_name = input$chosen_sample,
                           rows_selected = input$table_species_rows_selected)
            
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv()))
        })
    
    output$samples_found <- renderUI({
        found_samples <- reads_data() %>% arrange(sample) %>% .$sample %>% unique() 
        selectInput("chosen_sample", "Choose one or more samples:", found_samples, multiple = TRUE,
                    selected = found_samples[1], selectize = FALSE, size = 10)
    })
    
    output$check_tsv <- renderText({
        if (reads_data()$run[1] != orgs_data()$run[1]) {"Run IDs do not match!"}
    })
    
    output$check_sample <- renderText({
        if (length(unique(substr(input$chosen_sample, 1, 10))) > 1) {"More than one sample selected!"}
    })
})

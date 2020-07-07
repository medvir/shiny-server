library(shiny)
library(cowplot)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(DT)
library(stringr)
library(tidyselect)


# define functions --------------------------------------------------------

read_orgs_data <- function(orgs_file, read_length, checkbox_phages, checkbox_blocklist, blocklist){
    # reads orgs_species_found.tsv, calculate the covered_score and returns the table
    # with optional removing phages and entries from the blocklist
    orgs_data <-
        read.delim(orgs_file$datapath, sep = "\t") %>%
        mutate(covered_percent = 100 * covered_region / seq_len) %>%
        mutate(covered_exp = 100 * (1 - exp(-reads * read_length / seq_len))) %>%
        mutate(covered_score = round(100 * covered_percent / covered_exp, 1)) %>%
        mutate(covered_percent = round(covered_percent, 1))
    
    if (isTRUE(checkbox_phages)) {
        phage_endogenous_retrovirus_pattern <-
            "phage|escherichia|streptococcus|staphylococcus|bacillus|actinomyces|ostreococcus|myoviridae|clostridium|shigella|haemophilus|endogenous retrovirus"
        
        orgs_data <-
            orgs_data %>%
            filter(grepl(phage_endogenous_retrovirus_pattern, species, ignore.case = TRUE) == FALSE) %>%
            filter(grepl(phage_endogenous_retrovirus_pattern, ssciname, ignore.case = TRUE) == FALSE)
    }
    
    if (isTRUE(checkbox_blocklist)) {
        orgs_data <-
            orgs_data %>%
            anti_join(blocklist, by = "stitle")
    }
    
    return(orgs_data)
    
}

read_table_species <- function(orgs_data, chosen_sample){
    # takes a orgs_data table and groups results by species
    table_species <-
        orgs_data %>%
        filter(sample %in% chosen_sample) %>%
        select(species, reads, sample) %>%
        group_by(species, sample) %>%
        summarise(reads_sum = sum(reads)) %>%
        spread(key = sample, value = reads_sum, fill = 0) %>%
        ungroup() %>%
        mutate(reads_total = as.integer(rowSums(.[,-1], na.rm = TRUE))) %>%
        arrange(desc(reads_total))
    
    return(table_species)
}


shinyServer(function(input, output) {


# loading data ------------------------------------------------------------

    reads_data <- reactive({
        req(input$reads_file)
        
        read_delim(input$reads_file$datapath, "\t")
        })
    
    reads_data_processed <- reactive({
        req(input$reads_file)
        
        reads_data() %>%
            filter(sample %in% input$chosen_sample) %>%
            mutate(category_grouped = case_when(
                category == "raw_reads" ~ "raw_reads",
                category == "passing_filter" ~ "quality_filtered_reads",
                category == "matching_humanGRCh38" ~ "human",
                str_detect(category, "matching_bact") ~ "bacterial",
                category == "matching_fungi1" ~ "fungal",
                category == "matching_bt_ref" ~ "bovine",
                category == "viral_reads" ~ "viral",
                category == "undetermined_reads" ~ "undetermined",
                TRUE ~ "none"
            )) %>%
            filter(!(category_grouped == "none"))
        })
    
    blocklist <-
        read_csv("data/blocklist.csv")
    
    
    orgs_data <- reactive({
        req(input$orgs_file)
        
        # returns table to show in the app
        read_orgs_data(input$orgs_file, input$read_length, input$checkbox_phages, input$checkbox_blocklist, blocklist)
        
        })
    
    orgs_data_complete <- reactive({
        req(input$orgs_file)
        
        # returns complete table (except blocklisted entries) to include in the report
        read_orgs_data(input$orgs_file, input$read_length, checkbox_phages=FALSE, input$checkbox_blocklist, blocklist)
        
    })
    
    table_species <- reactive({
        req(!(is.null(input$chosen_sample)))
        
        read_table_species(orgs_data(), input$chosen_sample)
        
        })
    
    species_reported <- reactive({
        table_species()[input$table_species_rows_selected, ] %>%
        pull(species)
        
        })


# data checks -------------------------------------------------------------

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


# create plots ------------------------------------------------------------

    output$plot_run <- renderPlot({
        req(!(is.null(input$chosen_sample)))
        reads_data_processed() %>%
            filter(category == "raw_reads" | category == "passing_filter") %>%
            mutate(quality = factor(category, levels = c("raw_reads", "passing_filter"))) %>%
            
            ### plot 
            ggplot(aes(x = quality, y = reads, fill = quality)) +
            geom_col(colour = "black") +
            geom_text(aes(label = reads), vjust = -0.5) +
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
        reads_data_processed() %>%
            filter(!(category == "raw_reads" | category == "passing_filter")) %>%
            mutate(category_grouped = factor(as.character(category_grouped), levels = c("human", "bacterial", "fungal", "bovine", "viral", "undetermined"))) %>%
            group_by(sample) %>%
            mutate(percent = reads/sum(reads)) %>%
            
            ### plot
            ggplot(aes(x = category_grouped, y = percent, fill = category_grouped)) +
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


# create tables -----------------------------------------------------------

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


# internal control --------------------------------------------------------

    MS2_reads <- reactive({
        req(!(is.null(input$chosen_sample)))
        
        read_orgs_data(input$orgs_file, input$read_length, checkbox_phages=FALSE, input$checkbox_blocklist, blocklist) %>%
            filter(sample %in% input$chosen_sample,
                   str_detect(sample, "RNA"),
                   str_detect(species, "MS2")) %>%
            pull(reads) %>%
            sum()
    })
    
    filtered_RNA_reads <- reactive({
        req(!(is.null(input$chosen_sample)))
        reads_data() %>%
            filter(sample %in% input$chosen_sample) %>%
            filter(category == "passing_filter",
                   str_detect(sample, "RNA")) %>%
            pull(reads) %>%
            sum()
    })
    
    MS2_RPM <- reactive({
        req(!(is.null(input$chosen_sample)))
        round(MS2_reads() / (filtered_RNA_reads()/1E6), digits = 0)
    })
    
    output$MS2_internal_control <- renderText({ 
        paste0("the RNA sample contains ", MS2_reads(), " reads assigned to MS2 \n
               this equals ", MS2_RPM(), " reads per million filtered reads (RPM)")
    })


# create files for download -----------------------------------------------

    output$report <- downloadHandler(
        filename = function() {
            paste0(substr(input$chosen_sample[1], 1, 13), ".pdf")
        },
        content = function(file) {
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("RunReport.Rmd", tempReport, overwrite = TRUE)
            
            params <- list(orgs_data_complete = orgs_data_complete(),
                           reads_data = reads_data(),
                           sample_name = input$chosen_sample,
                           user_name = input$user_name,
                           species_reported = species_reported(),
                           blocklist = blocklist,
                           checkbox_blocklist = input$checkbox_blocklist,
                           MS2_RPM = MS2_RPM()
                           )
            
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv()))
        })
    
    output$csv_table <- downloadHandler(
        filename = function() {
            paste0(substr(input$chosen_sample[1], 1, 13), ".csv")
        },
        content = function(file) {
            req(input$orgs_file)
            req(!(is.null(input$chosen_sample)))
            
            read_count_dna <-
                reads_data_processed() %>%
                filter(sample == str_subset(input$chosen_sample, "DNA"))
            
            read_count_rna <-
                reads_data_processed() %>%
                filter(sample == str_subset(input$chosen_sample, "RNA"))
            
            read_table_species(orgs_data_complete(), input$chosen_sample) %>%
                select(-reads_total) %>%
                rename(species_reads_dna = contains("DNA"),
                       species_reads_rna = contains("RNA")) %>%
                mutate(reported = if_else(species %in% species_reported(), TRUE, FALSE),
                       run_name = reads_data()$run[1],

                       molis_nr = unique(str_extract(input$chosen_sample, "\\d{10,}")),

                       dna_sample_name = str_subset(input$chosen_sample, "DNA"),
                       rna_sample_name = str_subset(input$chosen_sample, "RNA"),

                       dna_raw_reads = filter(read_count_dna, category_grouped == "raw_reads")$reads,
                       rna_raw_reads = filter(read_count_rna, category_grouped == "raw_reads")$reads,

                       dna_quality_filtered_reads = filter(read_count_dna, category_grouped == "quality_filtered_reads")$reads,
                       rna_quality_filtered_reads = filter(read_count_rna, category_grouped == "quality_filtered_reads")$reads,

                       dna_human_reads = filter(read_count_dna, category_grouped == "human")$reads,
                       rna_human_reads = filter(read_count_rna, category_grouped == "human")$reads,

                       dna_bacterial_reads = sum(filter(read_count_dna, category_grouped == "bacterial")$reads),
                       rna_bacterial_reads = sum(filter(read_count_rna, category_grouped == "bacterial")$reads),

                       dna_fungal_reads = filter(read_count_dna, category_grouped == "fungal")$reads,
                       rna_fungal_reads = filter(read_count_rna, category_grouped == "fungal")$reads,

                       dna_bovine_reads = filter(read_count_dna, category_grouped == "bovine")$reads,
                       rna_bovine_reads = filter(read_count_rna, category_grouped == "bovine")$reads,

                       dna_viral_reads = filter(read_count_dna, category_grouped == "viral")$reads,
                       rna_viral_reads = filter(read_count_rna, category_grouped == "viral")$reads,

                       dna_undetermined_reads = filter(read_count_dna, category_grouped == "undetermined")$reads,
                       rna_undetermined_reads = filter(read_count_rna, category_grouped == "undetermined")$reads,

                       rna_internal_control_reads = MS2_reads()) %>%
                write_csv(file)
        })
})

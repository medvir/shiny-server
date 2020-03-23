# SARS2MOLIS

library(shiny)
library(tidyverse)
library(stringr)
library(readxl)
library(cowplot)
library(DT)

### SETTINGS
SARS_threshold <- 39
GAPDH_threshold <- 30
MS2_threshold <- 40

###
shinyServer(function(input, output, session) {
    
    ### function to determine if result is valid or not
    is_valid <- function(sample_name, SARS_ct, GAPDH_ct, MS2_ct) {
        case_when(
            
            # for all negative control samples
            (sample_name %in% input$neg_control
                 & is.na(SARS_ct)
                 & is.na(GAPDH_ct)
                 & MS2_ct < MS2_threshold) ~ TRUE,
            
            # for all positive control samples
            (sample_name %in% input$pos_control
                & SARS_ct < input$max_ct_SARS) ~ TRUE,
        
            # for all samples with a nr. as samplename
            (!is.na(as.numeric(sample_name))
                 & GAPDH_ct < GAPDH_threshold
                 & MS2_ct < MS2_threshold
                 & !is.na(GAPDH_ct)
                 & !is.na(MS2_ct)) ~ TRUE,
        
            # for all other cases return is_valid() = FALSE
            TRUE ~ FALSE
        )
    }
    

    ### read raw data from different cyclers, join amp and res data sheets
    raw_data <- reactive({
        pcr_file = input$pcr_file$datapath
        
        first_row_amp = match("Well", read_excel(pcr_file, sheet = "Amplification Data") %>% pull('Block Type'))
        amp = read_excel(pcr_file, sheet = "Amplification Data", skip = first_row_amp) %>%
            rename(well = "Well") %>%
            rename(cycle = "Cycle") %>%
            rename(target = "Target Name") %>%
            rename(delta_Rn = "Delta Rn") %>%
            select(well, cycle, target, Rn, delta_Rn)
            
        first_row_res = match("Well", read_excel(pcr_file, sheet = "Results") %>% pull('Block Type'))
        res = read_excel(pcr_file, sheet = "Results", skip = first_row_res) %>%
            rename(well = "Well") %>%
            rename(well_pos = "Well Position") %>%
            rename(target = "Target Name") %>%
            rename(sample_name = "Sample Name") %>%
            rename(ct_export = "CT") %>%
            rename(threshold = "Ct Threshold") %>%
            group_by(sample_name, target) %>%
            mutate(replicate = row_number()) %>%
            mutate(sample_name_replicate = paste0(sample_name, "_", replicate)) %>%
            select(well, well_pos, target, sample_name, ct_export, threshold, replicate, sample_name_replicate) 

        left_join(res, amp, by = c("well", "target"))
    })
    
    cycler_nr_out <- reactive({
        pcr_file = input$pcr_file$datapath
        
        read_excel(pcr_file, col_names = FALSE) %>%
            select(`...1`, `...2`) %>%
            filter(`...1` == "Instrument Serial Number") %>%
            pull(`...2`)
    })
    
    end_time_out <- reactive({
        pcr_file = input$pcr_file$datapath
        
        read_excel(pcr_file, col_names = FALSE) %>%
            select(`...1`, `...2`) %>%
            filter(`...1` == "Experiment Run End Time") %>%
            pull(`...2`)
    })
    
    
    
    ### target selection and threshold by target
    available_targets <- reactive({
        raw_data() %>%
            pull(target) %>%
            unique()
    })
    
    output$target_selection <- renderUI({
        req(input$pcr_file)
        radioButtons("selected_target", "Targets", choices = available_targets(), selected = available_targets()[1])
    })
    
    target_data <- reactive({
        raw_data() %>%
            filter(target == input$selected_target)
    })
    
    ### Parameter selection
    output$lin_log_selection <- renderUI({
        radioButtons("lin_log", "Log/Lin", choiceNames = c("logarithmic", "linear"), choiceValues = c("log", "lin"), selected = "log")
    })
    
    target_threshold <- reactive({
        target_data() %>% pull(threshold) %>% unique()
    })
    
    output$max_ct_selection <- renderUI({
        numericInput("max_ct_SARS", "Maximal Ct SARS", value = SARS_threshold, step = 1, min = 30, max = 45)
    })

    ### plot
    plot <- reactive({
        req(input$pcr_file)
        if (input$lin_log == "log") {
            target_data() %>%
                ggplot(aes(x = cycle, y = (log10(delta_Rn)), color = sample_name, group = well_pos)) +
                geom_line(size = .75, show.legend = FALSE) +
                geom_hline(yintercept = log10(target_threshold()), size = 0.5, linetype="dashed") +
                geom_vline(xintercept = input$max_ct_SARS, size = 0.5, linetype="dashed") +
                geom_text(aes(label = ifelse(cycle == 50, as.character(sample_name), "")), hjust = -.1, vjust = -.1, size = 3, show.legend = FALSE) +
                xlim(0, 50) +
                ylab("log10 delta Rn") +
                xlab("cycles") +
                panel_border() +
                background_grid(major = "xy", minor = "xy") +
                theme_bw() #+ facet_wrap(~sample_name, ncol = 4)
        } else {
            target_data() %>%
                ggplot(aes(x = cycle, y = delta_Rn, color = sample_name, group = well_pos)) +
                geom_line(size = .75, show.legend = FALSE) +
                geom_hline(yintercept = target_threshold(), size = 0.5, linetype="dashed") +
                geom_vline(xintercept = input$max_ct_SARS, size = 0.5, linetype="dashed") +
                geom_text(aes(label = ifelse(cycle == 50, as.character(sample_name), "")), hjust = -.1, vjust = -.1, size = 3, show.legend = FALSE) +
                ylim(-0.1, NA) +
                xlim(0, 50) +
                ylab("delta Rn") +
                xlab("cycles") +
                panel_border() +
                background_grid(major = "xy", minor = "xy") +
                theme_bw() #+ facet_wrap(~sample_name, ncol = 4)
        }
    })
    
    
    ### show plot
    output$run_plot <- renderPlot({
        plot()
    }
    #, height = 1400, width = 1000
    )
    
    
    ### table
    table <- reactive({
        req(input$pcr_file)
        raw_data() %>%
            group_by(sample_name, target) %>%
            sample_n(1) %>%
            ungroup() %>%
            select(sample_name, target, ct_export) %>%
            mutate(ct_export = round(as.numeric(ct_export), digits = 1)) %>%
            pivot_wider(names_from = target, values_from = ct_export) %>%
            arrange(sample_name) %>%
            rename(GAPDH_ct = GAPDH,
                   MS2_ct = `MS-2`,
                   SARS_ct = `CoV Wuhan E`) %>%
            mutate(valid = if_else(is_valid(sample_name, SARS_ct, GAPDH_ct, MS2_ct), true = "yes", false = "no")) %>%
            mutate(result = case_when(
                
                # valid samples ct < input$max_ct_SARS
                SARS_ct < input$max_ct_SARS & !is.na(SARS_ct) & valid == "yes" ~ "pos",
                
                # valid samples ct >= input$max_ct_SARS
                SARS_ct >= input$max_ct_SARS & !is.na(SARS_ct) & valid == "yes" ~ "gw",
                
                # valid samples ct undetermined
                is.na(SARS_ct) & valid == "yes" ~ "n",
                
                # invalid samples
                TRUE ~ ""))
    })
    
    
    ### show table
    output$results <- DT::renderDataTable(
        filter = "none",
        rownames = FALSE,
        options = list(pageLength = 100,
                       ordering = FALSE,
                       autoWidth = TRUE,
                       dom = 't',
                       rowCallback = JS('function(row, data) {
                                        $(row).mouseenter(function(){
                                        var hover_index = $(this)[0]._DT_RowIndex
                                        /* console.log(hover_index); */
                                        Shiny.onInputChange("hoverIndexJS", hover_index);
                                        });
                        }')
         ), {table()}
    )

    
    ### Export
    molis_out <- reactive({
        table()
    })
    
    molis_min <- reactive({
        table() %>%
            mutate(sample_name_num = as.numeric(sample_name)) %>%
            pull(sample_name_num)
            # somehow doesn't work with min,
            # but giving a vector into past0 below takes the first element
            #min(sample_name_num, na.rm = TRUE)
    })
    
    molis_max <- reactive({
        table() %>%
            mutate(sample_name_num = as.numeric(sample_name)) %>%
            arrange(desc(sample_name_num)) %>%
            pull(sample_name_num)
            # somehow doesn't work with max,
            # but giving a vector into past0 below takes the first element
            #max(sample_name_num, na.rm = TRUE)
    })

    plot_out <- reactive({
        plot() + facet_wrap(~sample_name, ncol = 4)
    })
    
    ### Download
    output$molis_export <- downloadHandler(
        
        filename = function() {
            paste0("molis-", Sys.Date(), ".txt")
        },
        
        content = function(file) {
            write.table(molis_out(),
                        file,
                        quote = FALSE,
                        sep ='\t',
                        row.names = FALSE,
                        eol = "\r\n",
                        append = FALSE)
        })
    
    output$pdf_export <- downloadHandler(
        
        filename = function() {
            paste0(molis_min(), "_to_", molis_max(), ".pdf")
        },
        
        content = function(file) {
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            params <- list(end_time = end_time_out(),
                           cycler_nr = cycler_nr_out(),
                           plot = plot_out(),
                           molis_out_table = molis_out())
            
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv()))
        })
})

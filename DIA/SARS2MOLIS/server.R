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

# MS2 range will now be defined by +/- 2s of mean
#MS2_threshold <- 40

###
shinyServer(function(input, output, session) {


    ### function to determine if flag or not, output doesn't differentiate between GAPDH and MS2 flag
    flag <- function(sample_name, MS2_ct_dbl, MS2_mean, MS2_sd, GAPDH_ct_dbl, MS2_ct, GAPDH_ct, SARS_ct) {
        
        MS2_lower <- MS2_mean - (2*MS2_sd)
        MS2_upper <- MS2_mean + (2*MS2_sd)
        
        case_when(
            # flag if not all targets are present/tested
            (!(sample_name %in% input$neg_control)
             & !(sample_name %in% input$pos_control)
             & (is.na(MS2_ct) | is.na(GAPDH_ct) | is.na(SARS_ct))) ~ "not all targets!",
            
            # GAPDH and MS2 flag
            (!(sample_name %in% input$neg_control)
             & !(sample_name %in% input$pos_control)
             & (GAPDH_ct_dbl > GAPDH_threshold | is.na(GAPDH_ct_dbl))
             & (MS2_ct_dbl > MS2_upper | is.na(MS2_ct_dbl))) ~ "GAPDH and MS2 ct high", # only warning for ct higher mean + 2s
            
            # GAPDH flag
            (!(sample_name %in% input$neg_control)
             & !(sample_name %in% input$pos_control)
             & (GAPDH_ct_dbl > GAPDH_threshold | is.na(GAPDH_ct_dbl))) ~ "GAPDH ct high",
            
            # MS2 flag
            (!(sample_name %in% input$neg_control)
             & !(sample_name %in% input$pos_control)
             & (MS2_ct_dbl > MS2_upper | is.na(MS2_ct_dbl))) ~ "MS2 ct high", # only warning for ct higher mean + 2s
            
            TRUE ~ NA_character_
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
                xlim(0, 45) +
                ylim(3, NA) +
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
                xlim(0, 45) +
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
            mutate(ct_export = round(as.numeric(ct_export), digits = 1),
                   ct_export = replace_na(ct_export, "undet.")) %>%
            pivot_wider(names_from = target, values_from = ct_export) %>%
            arrange(sample_name) %>%
            rename(GAPDH_ct = GAPDH,
                   MS2_ct = `MS-2`,
                   SARS_ct = `CoV Wuhan E`) %>%
            mutate(GAPDH_ct_dbl = as.numeric(GAPDH_ct),
                   MS2_ct_dbl = as.numeric(MS2_ct),
                   SARS_ct_dbl = as.numeric(SARS_ct)) %>%
            mutate(result = case_when(
                
                # samples ct < input$max_ct_SARS
                SARS_ct_dbl < input$max_ct_SARS & !is.na(SARS_ct_dbl) == TRUE ~ "pos",
                
                # samples ct >= input$max_ct_SARS
                SARS_ct_dbl >= input$max_ct_SARS & !is.na(SARS_ct_dbl) == TRUE ~ "gw",
                
                # samples ct undetermined
                is.na(SARS_ct_dbl) == TRUE ~ "n",
                
                # invalid samples
                TRUE ~ "")) %>%
            
            mutate(flag = flag(sample_name, MS2_ct_dbl, mean(MS2_ct_dbl, na.rm = TRUE), sd(MS2_ct_dbl, na.rm = TRUE), GAPDH_ct_dbl, MS2_ct, GAPDH_ct, SARS_ct))
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
         ), {table() %>% select(!ends_with("_dbl"))}
    )

    ### Export
    molis_out <- reactive({
        table() %>% select(!ends_with("_dbl"))
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
            from_molis <- if_else(molis_min() < 1000000000, true = molis_min()+1000000000, false = molis_min())
            to_molis <- if_else(molis_max() < 1000000000, true = molis_max()+1000000000, false = molis_max())
            
            paste0(from_molis, "_to_", to_molis, ".pdf")
        },
        
        content = function(file) {
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            params <- list(filename = input$pcr_file$name,
                           end_time = end_time_out(),
                           cycler_nr = cycler_nr_out(),
                           plot = plot_out(),
                           raw_data = raw_data(),
                           molis_out_table = molis_out(),
                           MS2_mean = round(mean(table()$MS2_ct_dbl, na.rm = TRUE), digits = 1),
                           MS2_sd = round(sd(table()$MS2_ct_dbl, na.rm = TRUE), digits = 3))
            
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv()))
        })
})

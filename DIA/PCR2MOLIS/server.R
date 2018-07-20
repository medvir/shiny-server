# PCR2MOLIS

library(shiny)
library(tidyverse)
library(stringr)
library(readxl)
library(cowplot)
library(DT)

path = "/Volumes/Home$/Repositories/shiny-server/DIA/PCR2MOLIS/data/"

### Shiny Server
shinyServer(function(input, output, session) {
    

    ### file selection
    available_folders = c("/Volumes/Home$/Repositories/shiny-server/DIA/PCR2MOLIS/data/", "/Volumes/Diagnostic/")
    
    output$folder_selection <- renderUI({
        selectInput("folders", "Folder", choices = available_folders, selected = available_folders[1], selectize = FALSE)
    })
    
    available_files <- reactive({
        list.files(input$folders, pattern = ".xls")
    })
    
    output$file_selection <- renderUI({
        selectInput("pcr_file", "File", choices = available_files(), selected = available_files()[1], selectize = FALSE)
    })
    
    
    ### read raw data from different cyclers, join amp and res data sheets
    raw_data <- reactive({
        req(input$pcr_file)
        pcr_file = paste0(path, input$pcr_file)
        cycler = as.character(read_excel(pcr_file) %>% unlist)
        ### QuantStudio
        if ("278870036" %in% cycler | "272322000" %in% cycler) { 
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
                rename(ct = "CT") %>%
                rename(threshold = "Ct Threshold") %>%
                select(well, well_pos, target, sample_name, ct, threshold) 
            
        } else {
            print("cycler not recognized")
            quit()
        }
        
        left_join(res, amp, by = c('well', 'target'))
    })
    
    
    ### target selection and threshold by target
    available_targets <- reactive({
        raw_data() %>%
            pull(target) %>%
            unique()
        })
    
    output$target_selection <- renderUI({
        radioButtons("selected_target", "Targets", choices = available_targets(), selected = available_targets()[1])
    })
    
    target_data <- reactive({
        raw_data() %>%
            filter(target == input$selected_target)
    })
    
    threshold <- reactive({
        target_data() %>%
            pull(threshold) %>%
            unique()
    })
    
    ### samples
    
    available_samples <- reactive({
        target_data() %>%
            pull(sample_name) %>%
            unique()})
    
    output$sample_selection <- renderUI({
        selectInput("samples_selected", "Samples", choices = available_samples(), selected = available_samples()[1], selectize = FALSE)
    })
    
    
    ### lin log
    output$lin_log <- renderUI({
        radioButtons("lin_log", "Lin/Log", choiceNames = c("logarithmic", "linear"), choiceValues = c("log", "lin"), selected = "log")
    })
    

    
    
    
    ### curve fit
    fit_data = reactive({
        fit_data = data.frame()
        for (i in available_samples()) {
            dat = target_data() %>%
                filter(sample_name == i)
            x = dat$cycle
            y = dat$delta_Rn
            
            try = class(try(nls(y ~ p1 + (p2-p1)/(1 + 10^((p3-x)*p4)), #sigmoidal dose response with variable slope
                                start = list(p1 = 0, p2 = 2.5, p3 = 30, p4 = 0.1)))) 
            
            if (try == "nls") { 
                fit = nls(y ~ p1 + (p2-p1)/(1 + 10^((p3-x)*p4)), #sigmoidal dose response with variable slope
                          start = list(p1 = 0, p2 = 2.5, p3 = 30, p4 = 0.1))
                fit_data = rbind(fit_data, data.frame(sample_name = i,
                                          p1 = summary(fit)$coefficients[1,1],
                                          p2 = summary(fit)$coefficients[2,1],
                                          p3 = summary(fit)$coefficients[3,1],
                                          p4 = summary(fit)$coefficients[4,1]))
            } 
        }
        return(fit_data)
    })
    
    
    
    
    ### results
    results <- reactive({
        target_data() %>%
            left_join(. , fit_data(), by = "sample_name") %>%
            select(sample_name, well_pos, target, ct, p1, p2, p3, p4) %>%
            group_by(sample_name, well_pos) %>%
            sample_n(1) %>%
            mutate(interpretation = ifelse(p3 <= 45 & p2 >= 2, "positive", "negative"))
    })
    
    
    
    ### plot curve fit
    output$curve <- renderPlot({
    
        
        dat = target_data() %>%
            #filter(sample_name %in% samples_selected())
            filter(sample_name == input$samples_selected)
        x = dat$cycle
        y = dat$delta_Rn
        fit = nls(y ~ p1 + (p2-p1)/(1 + 10^((p3-x)*p4)), start = list(p1 = 0, p2 = 2.5, p3 = 30, p4 = 0.1)) #sigmoidal dose response with variable slope
        df_fit = data.frame(delta_Rn_pred = predict(fit, dat), cycle = dat$cycle)
        
        if (input$lin_log == "lin") {
            ggplot(dat, aes(x = dat$cycle, y = dat$delta_Rn)) + 
                geom_point() +
                geom_line(color = "red", data = df_fit, aes(y = delta_Rn_pred, x = cycle)) +
                ylab("delta Rn") +
                xlab("cycles") +
                panel_border() +
                background_grid(major = "xy", minor = "xy") +
                theme(legend.title=element_blank())
        } else {
            ggplot(dat, aes(x = dat$cycle, y = log10(dat$delta_Rn))) + 
                geom_point() +
                geom_line(color = "red", data = df_fit, aes(y = log10(delta_Rn_pred), x = cycle)) +
                ylab("log10 delta Rn") +
                xlab("cycles") +
                panel_border() +
                background_grid(major = "xy", minor = "xy") +
                theme(legend.title=element_blank())
        }
    })
    
    ### Output Plots
    output$run_plot <- renderPlot({
        if (input$lin_log == "log") {
            target_data() %>%
                ggplot(aes(x = cycle, y = (log10(delta_Rn)), color = sample_name, group = well_pos)) +
                #geom_point() +
                geom_line(size = .75) +
                geom_hline(yintercept = threshold(), size = 0.5, linetype="dashed") +
                geom_text(aes(label = ifelse(cycle == 50, as.character(sample_name), "")), hjust = -.1, vjust = -.1, size = 3, show.legend = FALSE) +
                xlim(0, 50) +
                ylab("log10 delta Rn") +
                xlab("cycles") +
                panel_border() +
                background_grid(major = "xy", minor = "xy") +
                theme(legend.title=element_blank())
        } else {
            target_data() %>%
                ggplot(aes(x = cycle, y = delta_Rn, color = sample_name, group = well_pos)) +
                #geom_point() +
                geom_line(size = .75) +
                geom_hline(yintercept = threshold(), size = 0.5, linetype="dashed") +
                geom_text(aes(label = ifelse(cycle == 50, as.character(sample_name), "")), hjust = -.1, vjust = -.1, size = 3, show.legend = FALSE) +
                ylim(-0.1, NA) +
                xlim(0, 50) +
                ylab("delta Rn") +
                xlab("cycles") +
                panel_border() +
                background_grid(major = "xy", minor = "xy") +
                theme(legend.title=element_blank())
        }
    })
    
    # output$sample_plot <- renderPlot({
    #     if (input$lin_log == "log") {
    #         target_data() %>%
    #             #filter(sample_name %in% samples_selected()) %>%
    #             filter(sample_name == input$samples_selected) %>%
    #             ggplot(aes(x = cycle, y = (log10(delta_Rn)), color = "black", group = well_pos)) +
    #             geom_line(size = .75) +
    #             geom_hline(yintercept = threshold(), size = 0.5, linetype="dashed") +
    #             geom_text(aes(label = ifelse(cycle == 50, as.character(sample_name), "")), hjust = -.1, vjust = -.1, size = 3, show.legend = FALSE) +
    #             xlim(0, 50) +
    #             ylab("log10 delta Rn") +
    #             xlab("cycles") +
    #             panel_border() +
    #             background_grid(major = "xy", minor = "xy") +
    #             theme(legend.title=element_blank())
    #     } else {
    #         target_data() %>%
    #             #filter(sample_name %in% samples_selected()) %>%
    #             filter(sample_name == input$samples_selected) %>%
    #             ggplot(aes(x = cycle, y = delta_Rn, color = "black", group = well_pos)) +
    #             geom_line(size = .75) +
    #             geom_hline(yintercept = threshold(), size = 0.5, linetype="dashed") +
    #             geom_text(aes(label = ifelse(cycle == 50, as.character(sample_name), "")), hjust = -.1, vjust = -.1, size = 3, show.legend = FALSE) +
    #             ylim(-0.1, NA) +
    #             xlim(0, 50) +
    #             ylab("delta Rn") +
    #             xlab("cycles") +
    #             panel_border() +
    #             background_grid(major = "xy", minor = "xy") +
    #             theme(legend.title=element_blank())
    #     }
    # })
    
    
    ### Output tables
    output$results <- DT::renderDataTable(
        filter = "none",
        rownames = FALSE,
        options = list(pageLength = 100,
                       ordering = FALSE,
                       autoWidth = FALSE,
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
            results()
        })

    # samples_selected <- reactive({
    #     results()[input$hoverIndexJS + 1, ] %>%
    #         pull(sample_name) 
    # }) 
    
    
    ### Export
    molis_out <- reactive({
        results()[input$results_rows_selected, ] %>%
            group_by(sample_name, target) %>%
            mutate(mean = mean(as.numeric(ct))) %>%
            sample_n(1) %>%
            select(sample_name, target, mean, interpretation)
    })
    
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
    
    # output$report <- downloadHandler(
    #     filename = function() {
    #         paste0(substr(input$chosen_sample[1], 1, 13), ".pdf")
    #     },
    #     content = function(file) {
    #         tempReport <- file.path(tempdir(), "report.Rmd")
    #         file.copy("RunReport.Rmd", tempReport, overwrite = TRUE)
    #         
    #         params <- list(orgs_file = input$orgs_file$datapath,
    #                        reads_file = input$reads_file$datapath,
    #                        sample_name = input$chosen_sample,
    #                        rows_selected = input$table_species_rows_selected)
    #         
    #         rmarkdown::render(tempReport, output_file = file,
    #                           params = params,
    #                           envir = new.env(parent = globalenv()))
    #     })

})
# SARS2MOLIS

library(shiny)
library(tidyverse)
library(stringr)
library(readxl)
library(cowplot)
library(DT)

# # # # # # #
# FUNCTIONS #
# # # # # # #

# curve_fit <- function(x,y) {
#     #sigmoidal dose response with variable slope
#     nls(y ~ p1 + (p2-p1)/(1 + 10^((p3-x)*p4)), start = list(p1 = 0, p2 = max(y), p3 = 30, p4 = 0.2))
#     
#     # 7 parameter Knobel
#     # p1 = O? (1)
#     # p2 = Baseline Drift (0)
#     # p3 = Saturation Line (-4.5)
#     # p4 = Exponential Slope (0.04)
#     # p5 = Growth Infliction (30)
#     # p6 = Saturation Slope (-0.4)
#     # p7 = Saturation Infliction (38)
#     
#     #nls(y ~ p1 * (1 + p2*x + (p3 / ((1 + exp(-p4 * (x - p5)))*(1 + exp(-p6 * (x - p7)))))),
#     #start = list(p1 = 0, p2 = 0.5, p3 = -4.5, p4 = 0.04, p5 = 30, p6 = -0.4, p7 = 38),
#     #start = list(p1 = 0.98, p2 = 0.4, p3 = -4.6, p4 = 0.04, p5 = 30, p6 = -0.49, p7 = 38), #works for 405266
#     #start = list(p1 = 0.9, p2 = 0.4, p3 = -4.6, p4 = 0.04, p5 = 30, p6 = -0.49, p7 = 38),
#     #lower = c( 0, -1, -6, 0,  0, -1,  0),
#     #upper = c( 2,  1,  0, 1, 50,  0, 50),
#     #control = list(maxiter = 10000))
# }


# # # # # # # # #
# Shiny Server  #
# # # # # # # # #

shinyServer(function(input, output, session) {
    
    ### file selection
    
    # available_folders = c("/Volumes/Home$/Repositories/shiny-server/DIA/PCR2MOLIS/data/",
    #                       "/Volumes/huber.michael/Repositories/shiny-server/DIA/PCR2MOLIS/data/",
    #                       "/Volumes/Diagnostic/")
    # 
    # output$folder_selection <- renderUI({
    #     selectInput("folder", "Folder", choices = available_folders, selected = available_folders[1], selectize = FALSE)
    # })
    # 
    # available_files <- reactive({
    #     list.files(input$folder, pattern = ".xls")
    # })
    # 
    # output$file_selection <- renderUI({
    #     selectInput("pcr_file", "File", choices = available_files(), selected = available_files()[1], selectize = FALSE)
    # })
    
    
    ### read raw data from different cyclers, join amp and res data sheets
    raw_data <- reactive({
    #     if (is.null( input$pcr_file_upload)) {
    #         pcr_file = paste0(input$folder, input$pcr_file)
    #     } else {
    #         pcr_file = input$pcr_file_upload$datapath
    #     }
        
    
    pcr_file = input$pcr_file$datapath
    
        print(pcr_file)
        
        cycler = as.character(read_excel(pcr_file) %>% unlist)
        ### QuantStudio
        if ("278870036" %in% cycler | "272322000" %in% cycler | "278880634" %in% cycler) { 
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
    

    ### samples
    available_samples <- reactive({
        target_data() %>%
            pull(sample_name) %>%
            unique()})
    
    available_sample_name_replicates <- reactive({
        target_data() %>%
            pull(sample_name_replicate) %>%
            unique()})
    
    
    output$sample_selection <- renderUI({
        selectInput("samples_selected", "Samples", choices = available_samples(), selected = available_samples()[1], selectize = FALSE)
    })
    
    
    ### Parameter selection
    output$lin_log_selection <- renderUI({
        radioButtons("lin_log", "Lin/Log", choiceNames = c("logarithmic", "linear"), choiceValues = c("log", "lin"), selected = "log")
    })
    
    suggested_threshold <- reactive({
        target_data() %>%
            pull(threshold) %>%
            unique()
    })
    
    output$threshold_selection <- renderUI({
        numericInput("threshold", "Threshold", value = suggested_threshold(), step = 0.01)
    })
    
    output$max_ct_selection <- renderUI({
        numericInput("max_ct", "Minimal Ct", value = 39, step = 1)
    })
    
    output$min_delta_Rn_selection <- renderUI({
     numericInput("min_delta_Rn", "Minimal delta Rn", value = 2, step = 0.1)
    })

    

    ### curve fit
    # fit_data = reactive({
    #     fit_data = data.frame()
    #     for (i in available_sample_name_replicates()) {
    #         df = target_data() %>%
    #             filter(sample_name_replicate == i)
    #         try = class(try(curve_fit(df$cycle, df$delta_Rn)))
    #         if (try == "nls") { 
    #             fit = curve_fit(df$cycle, df$delta_Rn)
    #             print(fit)
    #             fit_data = rbind(fit_data, data.frame(sample_name_replicate = i,
    #                                       p1 = round(summary(fit)$coefficients[1,1],3),
    #                                       p2 = round(summary(fit)$coefficients[2,1],3),
    #                                       p3 = round(summary(fit)$coefficients[3,1],1),
    #                                       p4 = round(summary(fit)$coefficients[4,1],3),
    #                                       #p5 = round(summary(fit)$coefficients[5,1],3),
    #                                       #p6 = round(summary(fit)$coefficients[6,1],3),
    #                                       #p7 = round(summary(fit)$coefficients[7,1],3),
    #                                       ct = round(approx(x = fitted.values(fit), y = df$cycle, xout = input$threshold)$y, 1)
    #                                       ))
    #         } 
    #     }
    #     return(fit_data)
    # })
    
    
    ### results
    results <- reactive({
        raw_data() %>%
            group_by(sample_name, target) %>%
            sample_n(1) %>%
            ungroup()
        #target_data() %>%
            #left_join(. , fit_data(), by = "sample_name_replicate") %>%
            #group_by(sample_name, well_pos) %>%
            #sample_n(1) %>%
            #group_by(sample_name) %>%
            #mutate(ct_mean = mean(ct)) %>%
            #mutate(interpretation = case_when(
            #    ct <= input$max_ct & p2 >= input$min_delta_Rn ~ "positive",
            #    TRUE ~" negative"
            #    )) %>%
            #ungroup()
    })
    
    
    ### plot curve fit
    # output$curve <- renderPlot({
    # 
    #     dat = target_data() %>%
    #         filter(sample_name == input$samples_selected)
    #     
    #     dat1 = dat %>% filter(replicate == 1)
    #     try1 = class(try(curve_fit(dat1$cycle, dat1$delta_Rn)))
    #     if (try1 == "nls") {
    #         fit = curve_fit(dat1$cycle, dat1$delta_Rn)
    #         df_fit_1 = data.frame(delta_Rn_pred = predict(fit, dat), cycle = dat$cycle)
    #     }
    # 
    #     dat2 = dat %>% filter(replicate == 2)
    #     try2 = class(try(curve_fit(dat2$cycle, dat2$delta_Rn)))
    #     if (try2 == "nls") {
    #         fit = curve_fit(dat2$cycle, dat2$delta_Rn)
    #         df_fit_2 = data.frame(delta_Rn_pred = predict(fit, dat), cycle = dat$cycle)
    #     }
    # 
    #     if (input$lin_log == "lin") {
    #         p = ggplot(dat, aes(x = dat$cycle, y = dat$delta_Rn, color = as.factor(replicate))) + 
    #             geom_point() +
    #             geom_hline(yintercept = input$threshold, size = 0.5, linetype="dashed") +
    #             geom_hline(yintercept = input$min_delta_Rn, size = 0.5, linetype="dashed") +
    #             geom_vline(xintercept = input$max_ct, size = 0.5, linetype="dashed") +
    #             ylab("delta Rn") +
    #             xlab("cycles") +
    #             panel_border() +
    #             background_grid(major = "xy", minor = "xy") +
    #             theme(legend.title=element_blank())
    #         if (try1 == "nls") {
    #             p = p +
    #                 geom_line(color = "red", data = df_fit_1, aes(y = delta_Rn_pred, x = cycle))
    #         }
    #         if (try2 == "nls") {
    #             p = p +
    #                 geom_line(color = "red", data = df_fit_2, aes(y = delta_Rn_pred, x = cycle))
    #         }
    #     } else {
    #         p = ggplot(dat, aes(x = dat$cycle, y = log10(dat$delta_Rn), color = as.factor(replicate))) + 
    #             geom_point() +
    #             geom_hline(yintercept = log10(input$threshold), size = 0.5, linetype="dashed") +
    #             geom_hline(yintercept = log10(input$min_delta_Rn), size = 0.5, linetype="dashed") +
    #             geom_vline(xintercept = input$max_ct, size = 0.5, linetype="dashed") +
    #             ylab("log10 delta Rn") +
    #             xlab("cycles") +
    #             panel_border() +
    #             background_grid(major = "xy", minor = "xy") +
    #             theme(legend.title=element_blank())
    #         if (try1 == "nls") {
    #             p = p +
    #                 geom_line(color = "red", data = df_fit_1, aes(y = log10(delta_Rn_pred), x = cycle))
    #         }
    #         if (try2 == "nls") {
    #             p = p +
    #                 geom_line(color = "red", data = df_fit_2, aes(y = log10(delta_Rn_pred), x = cycle))
    #         }
    #     }
    #     p
    # })
    
    
    ### run plot
    output$run_plot <- renderPlot({
        if (input$lin_log == "log") {
            target_data() %>%
                ggplot(aes(x = cycle, y = (log10(delta_Rn)), color = sample_name, group = well_pos)) +
                geom_line(size = .75) +
                geom_hline(yintercept = log10(input$threshold), size = 0.5, linetype="dashed") +
                geom_hline(yintercept = log10(input$min_delta_Rn), size = 0.5, linetype="dashed") +
                geom_vline(xintercept = input$max_ct, size = 0.5, linetype="dashed") +
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
                geom_line(size = .75) +
                geom_hline(yintercept = input$threshold, size = 0.5, linetype="dashed") +
                geom_hline(yintercept = input$min_delta_Rn, size = 0.5, linetype="dashed") +
                geom_vline(xintercept = input$max_ct, size = 0.5, linetype="dashed") +
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
    
    
    ### table
    # output$fit_data_table <- renderTable({
    #     results() %>%
    #         filter(sample_name == input$samples_selected) %>%
    #         select(replicate, ct_export, p1, p2, p3, p4, ct)
    # })
    
    
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
            results() %>%
                #select(sample_name, replicate, well_pos, target, ct_export, p1, p2, p3, p4, ct, ct_mean, interpretation)
                select(sample_name, target, ct_export) %>%
                mutate(ct_export = round(as.numeric(ct_export), digits = 1)) %>%
                pivot_wider(names_from = target, values_from = ct_export) %>%
                arrange(sample_name) %>%
                rename(GAPDH_ct = GAPDH,
                       MS2_ct = `MS-2`,
                       SARS_ct = `CoV Wuhan E`) %>%
                mutate(valid = if_else(GAPDH_ct < 26 & MS2_ct < 36 & !is.na(GAPDH_ct) & !is.na(MS2_ct), true = "yes", false = "no"),
                       result = if_else(SARS_ct < 40 & !is.na(SARS_ct), true = "pos", false = "n"))

        })

    # samples_selected <- reactive({
    #     results()[input$hoverIndexJS + 1, ] %>%
    #         pull(sample_name) 
    # }) 
    
    
    ### Export
    molis_out <- reactive({
        results() %>%
            #select(sample_name, replicate, well_pos, target, ct_export, p1, p2, p3, p4, ct, ct_mean, interpretation)
            select(sample_name, target, ct_export) %>%
            mutate(ct_export = round(as.numeric(ct_export), digits = 1)) %>%
            pivot_wider(names_from = target, values_from = ct_export) %>%
            arrange(sample_name) %>%
            rename(GAPDH_ct = GAPDH,
                   MS2_ct = `MS-2`,
                   SARS_ct = `CoV Wuhan E`) %>%
            mutate(valid = if_else(GAPDH_ct < 26 & MS2_ct < 36 & !is.na(GAPDH_ct) & !is.na(MS2_ct), true = "yes", false = "no"),
                   result = if_else(SARS_ct < 40 & !is.na(SARS_ct), true = "p", false = "n"))
        # results() %>%
        # group_by(sample_name, target) %>%
        # mutate(mean = mean(as.numeric(ct))) %>%
        # sample_n(1) %>%
        # select(sample_name, target, mean, interpretation)
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

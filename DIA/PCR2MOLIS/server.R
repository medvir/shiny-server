# PCR2MOLIS

library(shiny)
library(tidyverse)
library(stringr)
library(readxl)
library(cowplot)
library(DT)


### Shiny Server
shinyServer(function(input, output, session) {
    
    ### read raw data from different cyclers
    pcr_data <- reactive({
        req(input$pcr_file)
        
        cycler = as.character(read_excel(input$pcr_file$datapath) %>% unlist)
        ### QuantStudio
        if ("278870036" %in% cycler | "272322000" %in% cycler) { 
            first_row_amp = match("Well", read_excel(input$pcr_file$datapath, sheet = "Amplification Data") %>% pull('Block Type'))
            amp = read_excel(input$pcr_file$datapath, sheet = "Amplification Data", skip = first_row_amp) %>%
                rename(well = "Well") %>%
                rename(cycle = "Cycle") %>%
                rename(target = "Target Name") %>%
                rename(delta_Rn = "Delta Rn") %>%
                select(well, cycle, target, Rn, delta_Rn)
            
            first_row_res = match("Well", read_excel(input$pcr_file$datapath, sheet = "Results") %>% pull('Block Type'))
            res = read_excel(input$pcr_file$datapath, sheet = "Results", skip = first_row_res) %>%
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
    
    targets <- reactive({pcr_data() %>% pull(target) %>% unique()})
    
    output$targets <- renderUI({
        radioButtons("selected_targets", "Targets", choices = targets(), selected = targets()[1])
    })
    
    threshold <- reactive({
        pcr_data() %>%
            pull(threshold) %>%
            unique()
    })
    

    output$plot <- renderPlot({
        pcr_data() %>%
            ggplot(aes(x = cycle, y = (log10(delta_Rn)), color = sample_name)) +
            geom_line(size = .75) +
            geom_hline(yintercept = threshold(), size = 0.5, linetype="dashed") +
            #geom_text(aes(label = ifelse(cycle == 45, as.character(sample), "")), hjust = -.1, vjust = -.1, size = 3, show.legend = FALSE) +
            #ylim(-0.1, NA) +
            #xlim(0, 45) +
            ylab("log delta Rn") +
            xlab("cycles") +
            #facet_wrap( ~ sample_name) +
            panel_border() +
            background_grid(major = "xy", minor = "xy") +
            theme(legend.title=element_blank())
    })
    
    
    output$sample_plot <- renderPlot({
        pcr_data() %>%
            filter(sample_name %in% samples_selected()) %>%
            ggplot(aes(x = cycle, y = (log10(delta_Rn)), color = sample_name)) +
            geom_line(size = .75) +
            geom_hline(yintercept = threshold(), size = 0.5, linetype="dashed") +
            #geom_text(aes(label = ifelse(cycle == 45, as.character(sample), "")), hjust = -.1, vjust = -.1, size = 3, show.legend = FALSE) +
            #ylim(-0.1, NA) +
            #xlim(0, 45) +
            ylab("log delta Rn") +
            xlab("cycles") +
            #facet_wrap( ~ sample_name) +
            panel_border() +
            background_grid(major = "xy", minor = "xy") +
            theme(legend.title=element_blank())
    })
    
    

    output$raw_data <- renderTable({
        pcr_data()
    })
    
    
    results <- reactive({
        pcr_data() %>% select(sample_name, target, ct, threshold) %>%
            filter(target %in% input$selected_targets) %>%
            group_by(sample_name) %>%
            sample_n(1)
    })

    output$results <- DT::renderDataTable(
        filter = "none",
        rownames = FALSE,
        options = list(pageLength = 100,
                       autoWidth = FALSE,
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

    
    samples_selected <- reactive({
        results()[input$hoverIndexJS + 1, ] %>%
            pull(sample_name) 
    })  
    
    
      
 
    # all_targets = reactive({
    #     eStream_data() %>% pull(`Target Name`) %>% unique()
    # })
    # 
    # 
    # output$targets <- renderUI({
    #     checkboxGroupInput("targets", "Targets", choices = all_targets(), selected = all_targets())
    # })
    # 
    # 
    # observeEvent(input$select_all_targets, {
    #     updateSelectInput(session = session, "targets", selected = all_targets())
    #     })
    # 
    # 
    # 
    # output$template_file <- downloadHandler(
    #     filename = function() {
    #         paste("template-", Sys.Date(), ".txt", sep = "")
    #     },
    #     content = function(file) {
    #         writeLines("[Sample Setup]", file)
    #         write.table(template_data() %>%
    #                         mutate(`Sample Color` = paste0("\"", `Sample Color`, "\"")) %>%
    #                         mutate(`Target Color` = paste0("\"", `Target Color`, "\"")),
    #                     file,
    #                     quote = FALSE,
    #                     sep ='\t',
    #                     row.names = FALSE,
    #                     eol = "\r\n",
    #                     append = TRUE)
    # })
})
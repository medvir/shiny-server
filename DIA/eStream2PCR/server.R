#eStream2PCR

library(shiny)
library(tidyverse)
library(stringr)
library(DT)
library(readr)

### setup
setup = read_csv("setup.csv")

reporter = setup$reporter
names(reporter) = setup$target

quencher = setup$quencher
names(quencher) = setup$target

well_dict = 1:96
names(well_dict) = c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12",
                     "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11", "B12",
                     "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12",
                     "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12",
                     "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", "E11", "E12",
                     "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12",
                     "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", "G10", "G11", "G12",
                     "H1", "H2", "H3", "H4", "H5",  "H6", "H7","H8", "H9","H10", "H11", "H12")


### color functions
rgb2hex <- function(v) {
    sapply(strsplit(as.character(v), "RGB\\(|,|)"), function(x) rgb(x[2], x[3], x[4], maxColorValue=255))
}


rgb_color_rainbow <- function(n, s, v) {
    col = rainbow(n, s, v, start = 0, end = max(1, n - 1)/n)
    rgb = sapply(col, function(x) paste0("RGB(", col2rgb(x)[1], ",", col2rgb(x)[2], ",", col2rgb(x)[3], ")"))
    names(rgb) = 1:n
    return(rgb)
}


rgb_color_repeat <- function(n) {
    col = c("RGB(230,159,0)", "RGB(86,180,233)", "RGB(0,158,115)", "RGB(240,228,66)", "RGB(0,114,178)", "RGB(213,94,0)", "RGB(204,121,167)", "RGB(153,153,153)")
    rgb = rep(col, length.out = n)
    names(rgb) = 1:n
    return(rgb)
}


rgb_color_random <- function(n, s, v) {
    rgb = vector(length = n)
    for (i in 1:n) {
        col = hsv(runif(1), s, v)
        rgb[i] = paste0("RGB(", col2rgb(col)[1], ",", col2rgb(col)[2], ",", col2rgb(col)[3], ")")
    }
    names(rgb) = 1:n
    return(rgb)
}


### Shiny Server
shinyServer(function(input, output, session) {
    
    eStream_data <- reactive({
        req(input$eStream_file)
        read_csv(input$eStream_file$datapath) %>%
            mutate(Lambda_pat = ifelse(`GAPDH-Lambda Patientenproben PCR` == 1, 1, 0)) %>%
            mutate(Lambda_contr = ifelse(`GAPDH-Lambda Kontrollen PCR` == 1, 1, 0)) %>%
            select(-`Source plate-ID`, -`Target plate ID`, -Id, -Conc.) %>%
            rename(`Sample Name` = Name) %>%
            
            gather(key = target, value = value, -Well, -`Sample Name`) %>%
            filter(value == 1) %>%
            select(-value) %>%
            
            mutate(`Target Name` = case_when(
                grepl("CMV", target) ~ "CMV",
                grepl("EBV", target) ~ "EBV",
                grepl("BK", target) ~ "BK",
                grepl("GAPDH", target) ~ "GAPDH",
                grepl("Lambda", target) ~ "Lambda",
                grepl("HHV-6", target) ~ "HHV-6",
                grepl("VZV", target) ~ "VZV",
                grepl("HSV-1", target) ~ "HSV-1",
                grepl("HSV-2", target) ~ "HSV-2",
                grepl("Parvo", target) ~ "Parvo",
                grepl("Adeno", target) ~ "Adeno",
                grepl("JC", target) ~ "JC",
                TRUE ~ "unknown"
            )) %>%
            
            separate(Well, into = c("pos", "Well Position"), sep = "\\.") %>%
            select(-pos) %>%
            mutate(Well = well_dict[`Well Position`]) %>%
            mutate(Reporter	= reporter[`Target Name`]) %>%
            mutate(Quencher = quencher[`Target Name`])
    })
    
    
    template_data <- reactive({
        
        dat = eStream_data() %>%
            filter(`Target Name` %in% input$targets) %>%
            mutate(sample_color_id = group_indices(.,`Sample Name`)) %>%
            mutate(target_color_id = group_indices(.,`Target Name`))
        
        if (input$colors == "repeat") {
            sample_colors = rgb_color_repeat(max(dat$sample_color_id))
            target_colors = rgb_color_repeat(max(dat$target_color_id))
        } else if (input$colors == "random") {
            sample_colors = rgb_color_random(max(dat$sample_color_id), input$saturation, input$value)
            target_colors = rgb_color_random(max(dat$target_color_id), input$saturation, input$value)
        } else {
            sample_colors = rgb_color_rainbow(max(dat$sample_color_id), input$saturation, input$value)
            target_colors = rgb_color_rainbow(max(dat$target_color_id), input$saturation, input$value)
        } 
        
        dat %>%
            mutate(`Sample Color` = sample_colors[sample_color_id]) %>%
            mutate(`Target Color` = target_colors[target_color_id]) %>%
            mutate(Task = "UNKNOWN", 'Biogroup Name' = "", 'Biogroup Color' = "", Quantity = "", Comments = "") %>%
            arrange(Well) %>%
            select(Well, `Well Position`, `Sample Name`, `Sample Color`, `Biogroup Name`, `Biogroup Color`, `Target Name`, `Target Color`, Task, Reporter, Quencher, Quantity, Comments)
    })
    
    
    output$colors <- renderUI({
        radioButtons("colors", "Colors", choices = c("repeat", "rainbow", "random"), selected = "repeat")
    })

    
    output$saturation <- renderUI({
        req(input$colors %in% c("random", "rainbow"))
        sliderInput("saturation", "Saturation", 0, 1, value = .4, step = .1, ticks = FALSE)
    })
    
    
    output$value <- renderUI({
        req(input$colors %in% c("random", "rainbow"))
        sliderInput("value", "Value", 0, 1, value = .9, step = .1, ticks = FALSE)
    })
    
    
    all_targets = reactive({
        eStream_data() %>% pull(`Target Name`) %>% unique()
    })

    
    output$targets <- renderUI({
        checkboxGroupInput("targets", "Targets", choices = all_targets(), selected = all_targets())
    })
    
    
    observeEvent(input$select_all_targets, {
        updateSelectInput(session = session, "targets", selected = all_targets())
        })
    
    
    output$template_table <- DT::renderDataTable({
        t_col_names = template_data() %>% pull('Target Color')
        t_col_hex = template_data() %>% pull('Target Color') %>% sapply(., function(x) rgb2hex(x)) %>% unname()
        s_col_names = template_data() %>% pull('Sample Color')
        s_col_hex = template_data() %>% pull('Sample Color') %>% sapply(., function(x) rgb2hex(x)) %>% unname()
        
        datatable(template_data(), rownames = FALSE, options = list(pageLength = 100)) %>%
            formatStyle('Target Color', backgroundColor = styleEqual(t_col_names, t_col_hex)) %>%
            formatStyle('Sample Color', backgroundColor = styleEqual(s_col_names, s_col_hex))
    })
    
    
    output$template_file <- downloadHandler(
        filename = function() {
            paste("template-", Sys.Date(), ".txt", sep = "")
        },
        content = function(file) {
            writeLines("[Sample Setup]", file)
            write.table(template_data() %>%
                            mutate(`Sample Color` = paste0("\"", `Sample Color`, "\"")) %>%
                            mutate(`Target Color` = paste0("\"", `Target Color`, "\"")),
                        file,
                        quote = FALSE,
                        sep ='\t',
                        row.names = FALSE,
                        eol = "\r\n",
                        append = TRUE)
    })
})
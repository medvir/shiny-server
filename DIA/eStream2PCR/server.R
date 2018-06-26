library(shiny)
library(tidyverse)
library(stringr)
library(DT)

well_dict = 1:96
names(well_dict) = c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12",
                     "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11", "B12",
                     "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12",
                     "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12",
                     "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", "E11", "E12",
                     "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12",
                     "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", "G10", "G11", "G12",
                     "H1", "H2", "H3", "H4", "H5",  "H6", "H7","H8", "H9","H10", "H11", "H12")

rgb2hex <- function(v) {
    sapply(strsplit(as.character(v), "RGB\\(|,|)"), function(x) rgb(x[2], x[3], x[4], maxColorValue=255))
}

rgb_color_rain <- function(n) {
    col = rainbow(n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 1)
    rgb = sapply(col, function(x) paste0("RGB(", col2rgb(x, alpha = FALSE)[1], ",", col2rgb(x, alpha = FALSE)[2], ",", col2rgb(x, alpha = FALSE)[3], ")"))
    names(rgb) = 1:n
    return(rgb)
}

rgb_color_rep <- function(n) {
    col = c("RGB(230,159,0)", "RGB(86,180,233)", "RGB(0,158,115)", "RGB(240,228,66)", "RGB(0,114,178)", "RGB(213,94,0)", "RGB(204,121,167)", "RGB(153,153,153)")
    rgb = rep(col, length.out = n)
    names(rgb) = 1:n
    return(rgb)
}

rgb_color_ran <- function(n) {
    rgb = vector(length = n)
    for (i in 1:n) {
        rgb[i] = paste0("RGB(", sample(25:255, 1), ",", sample(0:255, 1), ",", sample(50:255, 1), ")")
    }
    names(rgb) = 1:n
    return(rgb)
}


### Shiny Server
shinyServer(function(input, output) {
    
    export_data <- reactive({
        req(input$export_file)
        read_csv(input$export_file$datapath)
    })
    
    
    template_data <- reactive({
        #sample_colors = (nrow(export_data()))
        
        dat = export_data() %>%
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
                TRUE ~ "unknown"
            )) %>%
            
            separate(Well, into = c("pos", "Well Position"), sep = "\\.") %>%
            select(-pos) %>%
            mutate(Well = well_dict[`Well Position`]) %>%
            
            mutate(Reporter	= case_when(
                `Target Name` == "CMV" ~ "FAM",
                `Target Name` == "EBV" ~ "FAM",
                `Target Name` == "BK" ~ "FAM",
                `Target Name` == "GAPDH" ~ "VIC",
                `Target Name` == "Lambda" ~ "FAM",
                TRUE ~ "unknown"
            )) %>%
            
            mutate(`Target Color` = case_when(
                `Target Name` == "CMV" ~ "RGB(230,159,0)",
                `Target Name` == "EBV" ~ "RGB(86,180,233)",
                `Target Name` == "BK" ~ "RGB(0,158,115)",
                `Target Name` == "GAPDH" ~ "RGB(213,94,0)",
                `Target Name` == "Lambda" ~ "RGB(204,121,167)",
                TRUE ~ "RGB(0,0,0)"
            ))
        
        if (input$colors == "repeat") {
            sample_colors = rgb_color_rep(max(dat$Well))
        } else if (input$colors == "random") {
            sample_colors = rgb_color_ran(max(dat$Well))
        } else {
            sample_colors = rgb_color_rain(max(dat$Well))
        } 
        
        dat = dat %>%
            mutate(`Sample Color` = sample_colors[Well]) %>%
            mutate(Quencher = "TAMRA") %>%
            mutate(Task = "UNKNOWN") %>%
            mutate('Biogroup Name' = "", 'Biogroup Color' = "", Quantity = "", Comments = "") %>%
            arrange(Well) %>%
            select(Well, `Well Position`, `Sample Name`, `Sample Color`, `Biogroup Name`, `Biogroup Color`, `Target Name`, `Target Color`, Task,	Reporter, Quencher, Quantity, Comments)
        
        return(dat)
    })
    
    output$colors <- renderUI({
        radioButtons("colors", "Sample Colors", choices = c("repeat", "rainbow", "random"), selected = "repeat")
    })

    output$cycler <- renderUI({
        radioButtons("cycler", "Cycler", choices = c("QuantStudio 3", "QuantStudio 5"), selected = "QuantStudio 3")
    })
    
        
    all_targets = reactive({
        template_data() %>% pull(`Target Name`) %>% unique()
    })

    output$targets <- renderUI({
        checkboxGroupInput("targets", "Targets", #multiple = TRUE, selectize = FALSE,
                    choices = all_targets(),
                    selected = all_targets())
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
    
    output$template <- downloadHandler(
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
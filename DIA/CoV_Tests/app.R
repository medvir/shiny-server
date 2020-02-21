# CoV Nachtestungen

library(tidyverse)
library(shiny)
library(readxl)
library(DT)

ui <- fluidPage(
    titlePanel("SARS-CoV-2 Nachtestungen"),
    sidebarLayout(
        sidebarPanel(
            fileInput("files", "Upload (multiple) export files", multiple = TRUE, accept = "xlsx"),
            #selectInput("path", "Pfad", choices = c("C:/Users/VIRO/Desktop/Corona-NTIKC-export", "/Users/huber.michael/Desktop")),
            numericInput("ct", "Max Ct for positive result", value = 45)
        ),
        mainPanel(
            h4("Summary"),
            tableOutput("summary"),
            h4("Results"),
            DT::dataTableOutput("results")
        )
    )
)

server <- function(input, output) {
    # files = reactive({
    #     list.files(path = input$path, pattern = "export.xls", full.names = TRUE)
    # })
    
    raw_data = reactive({
        res = data.frame()
        for (i in input$files$datapath) {
        #for (i in files()) {
            skip_n = match("Well", read_excel(i, sheet = "Results") %>% pull('Instrument Name'))
            
            res_i = read_excel(i, sheet = "Results", skip = skip_n) %>%
                rename(well = "Well") %>%
                rename(well_pos = "Well Position") %>%
                rename(target = "Target Name") %>%
                rename(sample_name = "Sample Name") %>%
                rename(ct = "CT") %>%
                rename(threshold = "Ct Threshold") %>%
                select(well, well_pos, target, sample_name, ct, threshold)
            res = rbind(res, res_i)
        }
        return(res)
    })
    
    results = reactive({
        raw_data() %>%
            select(sample_name, target, ct) %>%
            filter(!(sample_name %in% c("run Ko", "NC", "pos. DNA", "pos. RNA"))) %>%
            filter(!(grepl("pos", sample_name, ignore.case = TRUE))) %>%
            filter(!(grepl("neg", sample_name, ignore.case = TRUE))) %>%
            mutate(result = case_when(
                ct == "Undetermined" ~ "negative",
                as.numeric(ct) <= input$ct ~ "positive",
                TRUE ~ "??"))
    })
    
    summary = reactive({
        results() %>%
            group_by(target) %>%
            mutate(posneg = ifelse(result == "positive", 1, 0)) %>%
            mutate(positive = sum(posneg)) %>%
            mutate(negative = n() - positive) %>%
            sample_n(1) %>%
            select(target, positive, negative)
    })
    
    output$results = DT::renderDataTable(
        options = list(pageLength = 100, autoWidth = FALSE),{
        req(input$files)
        results()
    })
    
    output$summary = renderTable({
        req(input$files)
        summary()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
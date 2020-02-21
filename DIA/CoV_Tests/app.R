# CoV Nachtestungen

library(tidyverse)
library(shiny)
library(readxl)
library(DT)


#C:\Users\VIRO\Desktop\Corona-NTIKC-export

# Define UI
ui <- fluidPage(

    titlePanel("SARS-CoV-2 Nachtestungen"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("files", "Upload (multiple) export files", multiple = TRUE, accept = "xlsx"),
            #selectInput("path", "Pfad", choices = c("C:/Users/VIRO/Desktop/Corona-NTIKC-export", "/Users/huber.michael/Desktop")),
            textOutput("n_files")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h4("Summary"),
            tableOutput("summary"),
            h4("Results"),
            DT::dataTableOutput("results")
        )
    )
)

# Define server logic required to draw a histogram
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
            mutate(result = case_when(
                ct == "Undetermined" ~ "neg",
                as.numeric(ct) < 43 ~ "pos",
                TRUE ~ "??"))
    })
    
    summary = reactive({
        results() %>%
            group_by(target) %>%
            mutate(posneg = ifelse(result == "pos", 1, 0)) %>%
            mutate(positive = sum(posneg)) %>%
            mutate(negative = n() - positive) %>%
            sample_n(1) %>%
            select(target, positive, negative)
    })
    
    output$results = DT::renderDataTable({
        req(input$files)
        results()
    })
    
    output$summary = renderTable({
        req(input$files)
        summary()
    })
    
    output$n_files = renderText({
        paste("Number of files:", length(input$files))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

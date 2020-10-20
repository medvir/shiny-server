# Statistik

library(shiny)
library(tidyverse)
library(readxl)
library(cowplot)
library(shinythemes)
library(lubridate)

master = read.csv("master_table.csv")
options(shiny.maxRequestSize = 100*1024^2)

### UI ###
ui <- fluidPage(
        theme = shinytheme("yeti"),
        titlePanel("Diagnostik Statistik"),
        sidebarLayout(
                sidebarPanel(
                        fileInput("molis_file", "MOLIS Export [xls, xlsx oder csv]", accept = c(".xls", ".xlsx", ".csv")),
                        dateRangeInput("date", label = "Date range", start = floor_date(Sys.Date(), 'year'), end = Sys.Date(), max = Sys.Date(), weekstart = 1),
                        h1(),
                        plotOutput(outputId = "Summary"),
                        h5(),
                        downloadButton("downloadBereich", "Download Statistik Arbeitsbereich"),
                        h5(),
                        downloadButton("downloadVerfahren", "Download Statistik Verfahren"),
                        h5(),
                        downloadButton("downloadMaster", "Download Mastertabelle")
                ),
                mainPanel(
                        h3("Arbeitsbereiche"),
                        tableOutput(outputId = "Bereich"),
                        h3("Analysen"),
                        tableOutput(outputId = "Verfahren")
                )
        )
)

### SERVER ###
server <- function(input, output) {
        data = reactive({
                req(input$molis_file)
                if (endsWith(input$molis_file$datapath, ".csv")) {
                        raw_data = read_csv(input$molis_file$datapath)
                }
                else {
                        raw_data = read_excel(input$molis_file$datapath)
                }
                
                #try(raw_data %>% select(Einsender, MC))
                raw_data %>%
                        select(Einsender, MC, Eingangsdatum) %>%
                        mutate(Eingangsdatum = as.Date(Eingangsdatum, "%d.%m.%Y")) %>%
                        rename(Code = MC) %>%
                        left_join(. , master) %>%
                        filter(!(is.na(Analyse))) %>%
                        
                        ### assign new code to anonymous HIV tests
                        mutate(Code = ifelse(Einsender == "HIVANO" & Code == "H06USD", "XXX", Code)) %>%
                        mutate(Code = ifelse(Einsender == "HIVANO" & Code == "H06UST", "XXX", Code)) %>%
                
                        ### assign new code to SARS PCR C23F*
                        mutate(Code = ifelse(Code %in% c("C23FK", "C23FKP", "C23GK", "C23GKA", "C23GKU", "C23FPU"), "C23GK", Code)) #%>%
                        
                        ### filter out SARS
                        #filter(!Code %in% c("C23FK", "C23RS", "C23PS", "C23FKP", "C23GK", "C23GKA", "C23GKU"))
        })
        
        data_filter = reactive({
                #print(data() %>% pull(Code) %>% unique())
                data() %>%
                        filter(Eingangsdatum >= input$date[1] & Eingangsdatum <= input$date[2]) %>%
                        select(-Eingangsdatum)
        })
       
        
        Bereich = reactive({
                data_filter() %>%
                        mutate(type = paste(Arbeitsbereich, Unterbereich, Weitere_Unterteilung)) %>%
                        group_by(type) %>%
                        mutate(Subtotal = n()) %>%
                        sample_n(1) %>%
                        group_by(Arbeitsbereich) %>%
                        mutate(Total = sum(Subtotal)) %>%
                        ungroup() %>%
                        mutate(Gesamttotal = sum(Subtotal)) %>%
                        select(-Einsender, -type, -Code, -Pathogen, -Analyse)
        })
        
        Verfahren = reactive({
                data_filter() %>%
                        group_by(Code) %>%
                        mutate(Total = n()) %>%
                        sample_n(1) %>%
                        ungroup() %>%
                        arrange(Pathogen) %>%
                        select(Pathogen, Analyse, Total)
        })
        
        
        output$Bereich = renderTable({
                req(input$molis_file)
                Bereich()
        })
        
        output$Verfahren = renderTable({
                req(input$molis_file)
                Verfahren()
        })
        
        output$Summary = renderPlot({
                req(input$molis_file)
                Bereich() %>%
                        mutate(Arbeitsbereich = case_when(
                                Arbeitsbereich == "PCR-basierte Analytik" ~ "PCR",
                                Arbeitsbereich == "Sequenzbasierte Analytik" ~ "Sequenz",
                                TRUE ~ as.character(Arbeitsbereich)
                                )) %>%
                        ggplot(aes(x = Unterbereich, y = Subtotal, fill = Arbeitsbereich)) +
                        geom_bar(stat="identity") +
                        facet_grid(Arbeitsbereich ~ ., scales = "free") +
                        panel_border() +
                        background_grid(major = "xy") +
                        xlab("") + ylab("") +
                        theme(legend.position="none") +
                        coord_flip() +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
        })
        
        output$downloadBereich <- downloadHandler(
                filename = function() {
                        paste(sub("(.*?)\\..*$", "\\1", input$molis_file), "_Bereich.csv", sep = "")
                },
                content = function(file) {
                        write.csv(Bereich(), file, row.names = FALSE)
                })
        
        output$downloadVerfahren <- downloadHandler(
                filename = function() {
                        paste(sub("(.*?)\\..*$", "\\1", input$molis_file), "_Verfahren.csv", sep = "")
                },
                content = function(file) {
                        write.csv(Verfahren(), file, row.names = FALSE)
                })
        
        output$downloadMaster <- downloadHandler(
                filename = function() {
                        paste("Mastertabelle_Analysestatistik.csv", sep = "")
                },
                content = function(file) {
                        write.csv(master, file, row.names = FALSE)
                })
}

### RUN APP ###
shinyApp(ui = ui, server = server)
library(shiny)
library(tidyverse)
library(readxl)

master = read_excel("Mastertabelle_Analysestatistik.xlsx")
options(shiny.maxRequestSize = 100*1024^2)

### UI ###
ui <- fluidPage(
        
        titlePanel("Diagnostik Statistik"),
        sidebarLayout(
                sidebarPanel(
                        fileInput("molis_file", "MOLIS Export")
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
                read_excel(input$molis_file$datapath) %>%
                        select(Einsender, MC) %>%
                        rename(Code = MC) %>%
                        full_join(. , master) %>%
                        filter(!(is.na(Analyse))) %>%
                        
                        ### assign new code to anonymous HIV tests
                        mutate(Code = ifelse(Einsender == "HIVANO" & Code == "H06USD", "XXX", Code)) %>%
                        mutate(Code = ifelse(Einsender == "HIVANO" & Code == "H06UST", "XXX", Code))
        })
        
        output$Bereich = renderTable({
                req(input$molis_file)
                print(data())
                data() %>%
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
        
        output$Verfahren = renderTable({
                req(input$molis_file)
                data() %>%
                        group_by(Code) %>%
                        mutate(Total = n()) %>%
                        sample_n(1) %>%
                        ungroup() %>%
                        select(-Einsender, -Code, -Arbeitsbereich, -Unterbereich, -Weitere_Unterteilung)
        })
}

### RUN APP ###
shinyApp(ui = ui, server = server)
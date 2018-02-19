library(shiny)
library(tidyverse)
library(readxl)
library(cowplot)
library(shinythemes)

### UI ###
ui <- fluidPage(
        theme = shinytheme("yeti"),
        titlePanel("Diagnostik Statistik"),
        sidebarLayout(
                sidebarPanel(
                        fileInput("master_file", "Mastertabelle", accept = c(".xls", ".xlsx", ".csv")),
                        fileInput("results_files", "Resultate", multiple = TRUE, accept = c(".xls", ".xlsx", ".csv")),
                        fileInput("massnahmen_file", "Massnahmen", accept = c(".xls", ".xlsx", ".csv")),
                        uiOutput(outputId = "Anbieter"),
                        uiOutput(outputId = "Panel"),
                        uiOutput(outputId = "Erreger"),
                        dateRangeInput("daterange", "Datum", start = "2018-01-01", separator = " bis "),
                        uiOutput(outputId = "Technologie"),
                        h3(),
                        downloadButton("downloadTabelle", "Download Tabelle")
                ),
                mainPanel(
                        fluidRow(
                                column(width = 4,
                                        h3("Overview"),
                                        tableOutput(outputId = "Overview")
                                        ),
                                column(width = 4,
                                        h3("Resultate"),
                                        tableOutput(outputId = "Resultate")
                                       )
                                ),
                        fluidRow(
                                column(width = 12,
                                        h3("Tabelle"),
                                        tableOutput(outputId = "Tabelle"),
                                        h3("Ausstehende Panels"),
                                        tableOutput(outputId = "Missing")
                                       )
                                )
                )
        )
)

### Functions
challenge = function(v) { ### input has to be sorted!
        v = duplicated(v)
        c = c()
        ci = 0
        for (i in 1:length(v)) {
                if (!(v[i])) {ci = ci + 1}
                c = append(c, ci)
        }
        return(c)
}

### SERVER ###
server <- function(input, output) {
        
        ### read files
        master = reactive({
                req(input$master_file)
                dat = read_excel(input$master_file$datapath)
                
                ### replicate panels with Frequenz > 1
                data.frame(dat[rep(seq_len(dim(dat)[1]), dat$Frequenz), , drop = FALSE], row.names=NULL) %>%
                        arrange(Erreger) %>%
                        group_by(Erreger, Technologiebereich, QK_Anbieter, Panel) %>%
                        mutate(Challenge = row_number()) %>%
                        ungroup() %>%
                        select(-Frequenz)
        })
        
        massnahmen = reactive({
                req(input$massnahmen_file)
                read_excel(input$massnahmen_file$datapath)
        })
        
        results = reactive({
                req(input$results_files)
                dat = lapply(input$results_files$datapath, read_excel)
                do.call("rbind", dat) %>%
                        arrange(Datum) %>%
                        group_by(Panel) %>%
                        mutate(Challenge = challenge(Datum)) %>%
                        mutate(Datum = as.character(Datum))
        })
        
        ### data join and filter
        raw_data = reactive({
                if (is.null(input$results_files)) {
                        master()
                }
                else if (is.null(input$massnahmen_file)) {
                        full_join(master(), results(), by = c("Panel", "Challenge"))
                } else {
                        full_join(master(), results(), by = c("Panel", "Challenge")) %>%
                                full_join(. , massnahmen(), by = c("Distribution Nr.", "Specimen ID"))
                }
        })
        
        data = reactive({
                raw_data() %>%
                        mutate(Challenge = as.integer(Challenge)) %>%
                        filter(QK_Anbieter %in% input$anbieter) %>%
                        filter(Technologiebereich %in% input$technologie) %>%
                        filter(Erreger %in% input$erreger) %>%
                        filter(Panel %in% input$panel) #%>%
                        #filter(Datum %in% input$daterange)
        })
        
        ### ui outputs
        output$Anbieter = renderUI({
                req(input$master_file)
                choices = master() %>%
                        pull(QK_Anbieter) %>%
                        unique()
                checkboxGroupInput("anbieter", "Anbieter", choices, selected = choices)
        })
        
        output$Technologie = renderUI({
                req(input$master_file)
                choices = master() %>%
                        pull(Technologiebereich) %>%
                        unique()
                checkboxGroupInput("technologie", "Technologie ", choices, selected = choices)
        })
        
        output$Erreger = renderUI({
                req(input$master_file)
                choices = master() %>%
                        arrange(Erreger) %>%
                        filter(QK_Anbieter %in% input$anbieter) %>%
                        filter(Panel %in% input$panel) %>%
                        pull(Erreger) %>%
                        unique()
                selectInput("erreger", "Erreger ", choices, selected = choices, multiple = TRUE, selectize = FALSE)
        })
        
        output$Panel = renderUI({
                req(input$master_file)
                choices = master() %>%
                        arrange(Panel) %>%
                        filter(QK_Anbieter %in% input$anbieter) %>%
                        pull(Panel) %>%
                        unique()
                selectInput("panel", "Panel ", choices, selected = choices, multiple = TRUE, selectize = FALSE)
        })
        
        ### data outputs
        output$Tabelle = renderTable({
                req(input$master_file)
                data() 
        })
        
        output$Overview = renderTable({
                req(input$master_file)
                req(input$results_files)
                data() %>%
                        mutate(Panels = !(is.na(Resultat))) %>%
                        group_by(Panel, Challenge) %>%
                        sample_n(1) %>%
                        group_by(Panels) %>%
                        mutate(Anzahl = n()) %>%
                        sample_n(1) %>%
                        select(Panels, Anzahl)
        })
        
        output$Resultate = renderTable({
                req(input$master_file)
                req(input$results_files)
                data() %>%
                        mutate(Resultate = case_when(
                                Score == 2 ~ "richtig",
                                is.na(Score) ~ "NA",
                                TRUE ~ "falsch"
                                )) %>%
                        group_by(Resultate) %>%
                        mutate(Anzahl = n()) %>%
                        sample_n(1) %>%
                        select(Resultate, Anzahl)
        })
        
        output$Missing = renderTable({
                req(input$master_file)
                req(input$results_files)
                data() %>%
                        mutate(result = !(is.na(Resultat))) %>%
                        filter(result == FALSE) %>%
                        select(QK_Anbieter, Panel, Challenge)
        })
        
        # output$Summary = renderPlot({
        #         req(input$master_file)
        #         req(input$results_files)
        #         data() %>%
        #                 ggplot(aes(x = Technologiebereich, y = n_specimen, fill = Technologiebereich)) +
        #                 geom_bar(stat="identity") +
        #                 facet_grid(QK_Anbieter ~ ., scales = "free") +
        #                 panel_border() +
        #                 background_grid(major = "xy") +
        #                 xlab("") + ylab("") +
        #                 theme(legend.position="none") +
        #                 coord_flip() +
        #                 theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
        # })
        
        ### downlowds 
        output$downloadTabelle <- downloadHandler(
                filename = function() {
                        "Tabelle_Statistik.csv"
                },
                content = function(file) {
                        write.csv(data(), file, row.names = FALSE)
                })
}

### RUN APP ###
shinyApp(ui = ui, server = server)

library(shiny)
library(tidyverse)
library(readxl)
library(cowplot)
library(plotly)
library(shinythemes)

date.range.years = 10

ui <- fluidPage(
    theme = shinytheme("yeti"),

    titlePanel("IQC"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "PCR-Kontrollen", accept = "xlsx"),
            selectInput("sheet", "Sheet", c("DNA ViiA7"), selected = "DNA ViiA7"),
            uiOutput("target"),
            uiOutput("lot"),
            dateRangeInput("dateRange",
                           label = "Date range input: yyyy-mm-dd",
                           start = Sys.Date() - (date.range.years*365), end = Sys.Date(),
                           weekstart = 1),
            numericInput("n.ref", "N reference", value = 100, min = 1, step = 10),
            tableOutput("text")
        ),

        mainPanel(
            plotlyOutput("plot")
        )
    )
)

server <- function(input, output) {
    
    raw_data = reactive({read_excel(input$file$datapath, sheet = input$sheet)[1:14] })
    
    lot_data = reactive({
        data = raw_data() %>%
            rename(date = 1) %>%
            filter(!date %in% c("Mittelwert", "Standardabw", "2s", "oberer GW", "unterer GW", "Lot:", "Datum")) %>%
            mutate(lot = ifelse(as.numeric(`HSV 2`) > 40000, `HSV 2`, NA)) %>%
            mutate(lot = ifelse(is.na(lot), lag(lot), lot))
        
        for (n in 2:nrow(data)) {
            if (is.na(data$lot[n]) | data$lot[n] == "") {
                data$lot[n] = data$lot[n-1]
            }
        }
        return(data)
    })
    
    tidy_data = reactive({
        lot_data() %>%
        filter(!(is.na(date))) %>%
        gather(key = "target", value = "value", -date, -lot) %>%
        mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>%
        mutate(lot = as.Date(as.numeric(lot), origin = "1899-12-30")) %>%
        mutate(value = as.numeric(value))
        })
    
    targets = reactive({ tidy_data() %>% pull(target) %>% unique() })
    output$target = renderUI({
        req(input$file)
        selectInput("target", "Target", targets(), selectize = FALSE)
        })
    
    lots = reactive({ tidy_data() %>% pull(lot) %>% unique() })
    output$lot = renderUI({
        req(input$file)
        selectInput("lot", "Lot", lots(), multiple = TRUE, selected = lots(), selectize = FALSE)
        })
    
    target_data = reactive({
        tidy_data() %>%
            filter(target == input$target) %>%
            filter(!(is.na(value))) %>%
            filter(as.character(lot) %in% input$lot) %>%
            filter(date > input$dateRange[1]) %>% ###
            filter(date < input$dateRange[2])     ###
    })

    n = reactive({ if (input$n.ref >= nrow(target_data())) {nrow(target_data())} else {input$n.ref} })
    mean = reactive({ target_data() %>% pull(value) %>% .[1:n()] %>% base::mean() })
    sd = reactive({ target_data() %>% pull(value) %>% .[1:n()] %>% stats::sd() })
    
    plot_data = reactive({
        target_data() %>%
            filter(date > input$dateRange[1]) %>%
            filter(date < input$dateRange[2])
    })

    plot = reactive({
        df = plot_data() %>%
            mutate(category = case_when( ### Levey Jennings Plot
                value <= mean() + 1*sd() & value >= mean() - 1*sd() ~ "0",
                value <= mean() + 2*sd() & value >= mean() - 2*sd() ~ "1S",
                value <= mean() + 3*sd() & value >= mean() - 3*sd() ~ "2S",
                TRUE ~ "3S")) %>%
            mutate(m.above = 0, m.below = 0, s1.above = 0, s1.below = 0, s2.above = 0, s2.below = 0, s3.above = 0, s3.below = 0, r4 = 0)
        
        for (i in 2:nrow(df)) {
            if (df$value[i] > mean()) {df$m.above[i] = df$m.above[i-1] + 1}  ### above mean
            if (df$value[i] < mean()) {df$m.below[i] = df$m.below[i-1] + 1}  ### below mean
            if (df$value[i] > mean() + 1*sd()) {df$s1.above[i] = df$s1.above[i-1] + 1}  ### above 1s
            if (df$value[i] < mean() - 1*sd()) {df$s1.below[i] = df$s1.below[i-1] + 1}  ### below 1s
            if (df$value[i] > mean() + 2*sd()) {df$s2.above[i] = df$s2.above[i-1] + 1}  ### above 2s
            if (df$value[i] < mean() - 2*sd()) {df$s2.below[i] = df$s2.below[i-1] + 1}  ### below 2s
            if (df$value[i] > mean() + 3*sd()) {df$s3.above[i] = df$s3.above[i-1] + 1}  ### above 3s
            if (df$value[i] < mean() - 3*sd()) {df$s3.below[i] = df$s3.below[i-1] + 1}  ### below 3s
            if (df$s2.above[i-1] >= 1 & df$s2.below[i] >= 1) {df$r4[i] = 1} ### above 2s --> below 2s
            if (df$s2.below[i-1] >= 1 & df$s2.above[i] >= 1) {df$r4[i] = 1} ### bewlo 2s --> above 2s
        }
        
        df %>%
            mutate(rule = case_when( ### Westgard Rules
                r4 >= 1 ~ "R-4s",  ### R-4s rule
                s3.above >= 1 ~ "1-3s", ### 1-3s rule
                s3.below >= 1 ~ "1-3s", ### 1-3s rule
                s2.above >= 2 ~ "2-2s", ### 2-2s rule
                s2.above >= 2 ~ "2-2s", ### 2-2s rule
                s2.above >= 1 ~ "1-2s", ### 1-2s alarm
                s2.above >= 1 ~ "1-2s", ### 1-2s alarm
                s1.above >= 3 ~ "3-1s", ### 3-1s alarm
                s1.above >= 3 ~ "3-1s", ### 3-1s alarm
                m.above >= 10 ~ "10x", ### 10 above mean alarm
                m.below >= 10 ~ "10x", ### 10 below mean alarm
                TRUE ~ "ok"
            )) %>%
            
            ggplot(aes(x = date, y = value, color = rule)) +
            #facet_grid(. ~ lot, scales = "free") +

            scale_colour_manual(values = c("ok" = "#228B22", ### grün
                                           "10x"= "#FFA500", ### orange
                                           "1-2s"= "#FFA500", ### orange
                                           "3-1s"= "#FFA500", ### orange
                                           "R-4s" = "#0000FF", ### blau
                                           "2-2s" = "#FF0000", ### rot
                                           "1-3s" = "#FF0000")) + ### rot
            geom_hline(yintercept = mean(), color = "black", linetype = "dotted", size = 0.25) +
            geom_hline(yintercept = mean() + 1*sd(), color = "#228B22", linetype = "dotted", size = 0.25) +
            geom_hline(yintercept = mean() - 1*sd(), color = "#228B22", linetype = "dotted", size = 0.25) +
            geom_hline(yintercept = mean() + 2*sd(), color = "#FFA500", linetype = "dashed") +
            geom_hline(yintercept = mean() - 2*sd(), color = "#FFA500", linetype = "dashed") +
            geom_hline(yintercept = mean() + 3*sd(), color = "#FF0000") +
            geom_hline(yintercept = mean() - 3*sd(), color = "#FF0000") +

            geom_point(size = 3, shape = 16) +
            panel_border() +
            background_grid(major = "xy", minor = "xy") +
            theme(legend.position = "none") +
            ylab("Ct value") +
            xlab("Date") +
            theme_set(theme_cowplot()) +
            ggtitle(paste0(input$target, " (n = ", nrow(plot_data()), ")")) +
            theme(legend.position = "none") +
            panel_border() + background_grid(major = "xy", minor = "xy")
    })
    
    output$plot = renderPlotly({
        req(input$file)
        ggplotly(plot())
        })
}

# Run the applicategoryion 
shinyApp(ui = ui, server = server)


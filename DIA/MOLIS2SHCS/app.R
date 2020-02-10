library(shiny, lib.loc = "C:/RStudio/lib")
library(shinythemes, lib.loc = "C:/RStudio/lib")
library(shinyWidgets, lib.loc = "C:/RStudio/lib")
library(tidyverse, lib.loc = "C:/RStudio/lib")
library(readxl, lib.loc = "C:/RStudio/lib")
library(lubridate, lib.loc = "C:/RStudio/lib")

#library(shiny)
#library(tidyverse)
#library(readxl)
#library(shinythemes)
#library(shinyWidgets)
#library(lubridate)

fields = c("Anforderungsnr.", "Entnahmedatum", "Eingangsdatum", "MC", "RES", "Patient",
           "Geschl.", "Geburtsdatum", "Externe Patientennummer", "Entn.Zt.", "Patientennummer", "Eins.")
MCs = c("PLASMA", "ZELLE", "DNA", "URIN", "PTF", "ZTF", "PSIZE", "ZSIZE", "ZENTRL", "VERARB")

# Define UI
ui <- fluidPage(
    theme = shinytheme("yeti"),
    titlePanel("MOLIS 2 SHCS"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "MOLIS Export", accept = "xlsx"),
            airMonthpickerInput(
                inputId = "month",
                label = "Select a month:",
                value = Sys.Date() - 30,
                clearButton = TRUE),
            actionButton("export", "Export")
            ),
        mainPanel(
            tableOutput("raw"),
            tableOutput("tidy"),
            textOutput("month"),
            tableOutput("shcs")
        )
    )
)


# Define server
server <- function(input, output) {
    
    options(shiny.maxRequestSize = 100*1024^2)
    
    raw_molis = reactive({
        read_excel(input$file$datapath) %>%
        filter(MC %in% MCs) %>%
        select(fields)
    })
    
    tidy_molis = reactive({
        raw_molis() %>%
            mutate(Entnahmedatum = as.Date(Entnahmedatum, "%d.%m.%Y")) %>%
            mutate(Eingangsdatum = as.Date(Eingangsdatum, "%d.%m.%Y")) %>%
            #mutate(Entnahmedatum = as.Date(ifelse(is.na(Entnahmedatum), Eingangsdatum, Entnahmedatum), origin = "1970-01-01")) %>%
            mutate(S_TYPE = case_when(
                MC %in% c("PLASMA", "PTF", "PSIZE") ~ "P",
                MC %in% c("ZELLE", "ZTF", "ZSIZE") ~ "C",
                MC %in% c("DNA") ~ "D",
                MC %in% c("URIN") ~ "U",
                TRUE ~ "")) %>%
            group_by(Anforderungsnr.) %>%
            mutate(ZENTRL = RES[match("ZENTRL", MC)]) %>%
            mutate(ZSIZE = RES[match("ZSIZE", MC)]) %>%
            mutate(ZTF = RES[match("ZTF", MC)]) %>%
            mutate(VERARB = RES[match("VERARB", MC)]) %>%
            filter(MC != "ZENTRL") %>%
            filter(MC != "ZSIZE") %>%
            filter(MC != "ZTF") %>%
            filter(MC != "VERARB") %>%
            spread(key = MC, value = RES) %>%
            mutate(VERARB = as.Date(VERARB, "%d.%m.%y"))
    })
    
    shcs_molis = reactive({
        tidy_molis() %>%
            rename(LAB_DATE = Eingangsdatum) %>%
            rename(SAMPLE_DATE = Entnahmedatum) %>%
            rename(SAMPLE_TIME = Entn.Zt.) %>%
            rename(NAME = Patient) %>%
            rename(S_ID = Anforderungsnr.) %>%
            rename(SENDER = Eins.) %>%
            rename(EXT_ID = `Externe Patientennummer`) %>%
            rename(IMV_ID = Patientennummer) %>%
            mutate(S_NRAL = case_when(
                S_TYPE == "P" ~ PLASMA,
                S_TYPE == "C" ~ ZELLE,
                S_TYPE == "D" ~ DNA,
                TRUE ~ S_TYPE)) %>%
            mutate(S_TIME = ifelse(S_TYPE == "P", PTF, ZTF)) %>%
            mutate(S_SIZE = ifelse(S_TYPE == "P", PSIZE, ifelse(S_TYPE == "C", ZSIZE, NA))) %>%
            mutate(FREEZING_DATE = ifelse(S_TYPE == "P" | S_TYPE == "C", VERARB, NA)) %>%
            mutate(FREEZING_DATE = as.Date(FREEZING_DATE, origin = "1970-01-01")) %>%
            mutate(TUBE = ifelse(S_TYPE == "P", "E", "E")) %>% ### TUBE always E ?!
            mutate(LAB_STOCK = 11) %>%                         ### LAB_STOCK always 11 ?!
            mutate(S_TIME = sub("\\.", ":", S_TIME)) %>%
            mutate(ZENTRL = sub("\\.", ":", ZENTRL)) %>%
            select(S_ID, NAME, SENDER, IMV_ID, EXT_ID, SAMPLE_DATE, SAMPLE_TIME, LAB_DATE, S_TYPE, TUBE, S_NRAL, S_SIZE, FREEZING_DATE, S_TIME, LAB_STOCK) %>%
            filter(LAB_DATE >= start.date() & LAB_DATE <= end.date())
    })
    
    output$month <- renderText({
        paste("Export SHCS from", start.date(), "to", end.date())
    })
    
    start.date = reactive({input$month})
    end.date = reactive({as.Date(input$month) + months(1) - days(1)})
    
    output$raw <- renderTable({
        req(input$file)
        head(raw_molis(), 10)
    })
    
    output$tidy <- renderTable({
        req(input$file)
        head(tidy_molis(), 10)
    })
    
    output$shcs <- renderTable({
        req(input$file)
        shcs_molis()
    })
    
    observeEvent(input$export, {
        write.csv(shcs_molis(), file = paste0("SHCS_Export_", start.date(), "-", end.date(), ".csv"))
    })
}

# Run the application
setwd("P:/Desktop/MOLIS2SHCS")
shinyApp(ui = ui, server = server)

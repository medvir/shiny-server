library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)
library(shinyWidgets)
library(lubridate)
library(DT)


######################## FUNCTIONS ########################
replicate_material <- function(x, y){
    mat_idx <- which(x == 'MATD')
    materials = y[mat_idx]
    mat_idx <- c(mat_idx, length(x) + 1)
    rep_mats = rep(materials, diff(mat_idx))
    return(rep_mats)
}

move_bewertung <- function(x, y){
    mask1 <- which(x == 'Bewertung') - 1
    mask2 <- which(grepl(', qn \\(', x))
    tst <- all(mask1 %in% mask2)
    assertthat::are_equal(tst, TRUE)
    ### copy the Bewertung one row up
    new_col <- vector("character", length = length(x))
    new_col[mask1] <- y[mask1 + 1]
    return(new_col)
}

block_discount <- function(MC, TP) { 
    for (i in 1:length(MC)) {
        
        ### Blocks of tests as list: ((Tests that are part of the block), (Test that are charged)) -----------------------------------    
        FLURSV = list(c("I08FKP", "I04FKP", "R01FKP"),
                      c("I08FKP"))
        # EPLEX = list(c("A01DPU", "C03FPU", "C20FPU", "C04FPU", "C05FPU", "C21FPU", "B01DPU", "M03FPU", "R09FPU",
        #                "I08FPU", "I09FPU", "I07FPU", "I10FPU","I04FPU", "P02FPU", "P03FPU", "P04FPU", "P05FPU",
        #                "R02FPU", "R03FPU", "B02DPU", "C22DPU", "L01DPU", "M06DPU"),
        #              c("A01DPU", "C03FPU", "C20FPU"))
        
        ### Rules on discount for block analyses -------------------------------------------------------------------------------------
        if (MC[i] %in% FLURSV[[1]] & all(FLURSV[[1]] %in% MC) & !MC[i] %in% FLURSV[[2]]) {TP[i] = 0}   ### FLURSV
        #if (MC[i] %in% EPLEX[[1]] & all(EPLEX[[1]] %in% MC) & !MC[i] %in% EPLEX[[2]]) {TP[i] = 0}   ### EPLEX
        
        ### --------------------------------------------------------------------------------------------------------------------------
    }
    return(TP)
}

######################## SETUP ########################
fields = c("Anforderungsnr.", "Entnahmedatum", "Eingangsdatum", "MC", "NAME", "RES", "Einsender", "Kontrakt", "Herkunft", "Stationärer Patient", "Abrechnungsstatus")

abrvb.dict = read_csv("abrvb.table.csv") %>% pull(ABRVB)
names(abrvb.dict) = read_csv("abrvb.table.csv") %>% pull(KURZNAME)

tax.dict = read_csv("tax.table.csv") %>% pull(CHF)
names(tax.dict) = read_csv("tax.table.csv") %>% pull(ABRVB)

TP.dict = read_csv("anaverf.table.csv") %>% pull(TP)
names(TP.dict) = read_csv("anaverf.table.csv") %>% pull(MC)

discount.dict = read_csv("discount.table.csv") %>% pull(PERCENT)
names(discount.dict) = read_csv("discount.table.csv") %>% pull(SENDER)


######################## UI ######################## 
ui <- fluidPage(
    theme = shinytheme("yeti"),
    titlePanel("MOLIS 2 accounting"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "MOLIS Export", accept = "xlsx"),
            # airMonthpickerInput(
            #     inputId = "month",
            #     label = "Select a month:",
            #     value = Sys.Date() - 30,
            #     clearButton = TRUE),
            dateRangeInput("date",
                           label = "Date range",
                           start = Sys.Date() - 3*360, end = Sys.Date(), max = Sys.Date(), weekstart = 1),
                           #separator = " - ", format = "dd/mm/yy", startview = 'year'
            uiOutput("status"),
            actionButton("export", "Export"),
            h4("Anzahl Aufträge"),
            textOutput("n"),
            h4("Summe"),
            textOutput("sum"),
            tableOutput("summary")
        ),
        mainPanel(
            DT::dataTableOutput("total"),
            DT::dataTableOutput("selected"),
            h6("******************"),
            tableOutput("raw"),
            tableOutput("tidy"),
            tableOutput("cost"),
        )
    )
)


######################## SERVER ######################## 
server <- function(input, output) {
    options(shiny.maxRequestSize = 100*1024^2)
    
    raw_molis = reactive({
        read_excel(input$file$datapath) %>%
            select(fields)
    })
    

    tidy_molis = reactive({
        raw_molis() %>%
            filter(!is.na(MC)) %>%
            group_by(Anforderungsnr.) %>%
            filter(any(MC == "MATD")) %>% ### some MOLIS_nb have no material, these are used to store cohort samples, remove them ?!?!?
            slice((which(MC == "MATD")[1]):n()) %>%
            ungroup() %>%
            group_by(Anforderungsnr.) %>%
            mutate(Material = replicate_material(MC, RES), Interpretation = move_bewertung(NAME, RES)) %>% ### functions replicate_material and move_bewertung
            filter(!MC == "MATD", !is.na(NAME), NAME != 'Bewertung') %>%
            mutate(Entnahmedatum = as.Date(Entnahmedatum, "%d.%m.%Y")) %>%
            mutate(Eingangsdatum = as.Date(Eingangsdatum, "%d.%m.%Y")) %>%
            mutate(Entnahmedatum = as.Date(ifelse(is.na(Entnahmedatum), Eingangsdatum, Entnahmedatum), origin = "1970-01-01"))
    })
    

    cost_molis = reactive({
        tidy_molis() %>%
            mutate(TP = TP.dict[MC]) %>%
            mutate(Rechnungsempfänger = ifelse(Herkunft == "USZ", paste0(Herkunft, `Stationärer Patient`), Kontrakt)) %>%
            group_by(Anforderungsnr.) %>%
            mutate(counter = row_number(Anforderungsnr.)) %>%
            mutate(Auftragspauschale = case_when(
                counter == 1 & Rechnungsempfänger == "USZJ"~ 22,
                counter == 1 & Rechnungsempfänger == "USZN"~ 33,
                counter == 1 ~ tax.dict[abrvb.dict[Einsender]],
                TRUE ~ 0
            )) %>%
            mutate(TP = block_discount(MC, TP)) %>%  ### funtion block_discount
            mutate(Subtotal = ifelse(counter == 1, sum(TP, na.rm = TRUE), 0)) %>%
            select(Anforderungsnr., Eingangsdatum, counter, MC, NAME, RES, Einsender, Material, TP, Subtotal, Auftragspauschale, Abrechnungsstatus)
    })
    

    total_molis = reactive({
        cost_molis() %>%
            filter(counter == 1) %>%
            #sample_n(1) %>%
            mutate(Discount = discount.dict[Einsender]) %>%
            mutate(Discount = ifelse(is.na(Discount), 0, Discount)) %>%
            mutate(Total = (Subtotal + Auftragspauschale) * (1 - Discount/100)) %>%
            select(Anforderungsnr., Eingangsdatum, Einsender, Subtotal, Auftragspauschale, Discount, Total, Abrechnungsstatus)
    })
    
    filter_molis = reactive({
        total_molis() %>%
            filter(Eingangsdatum >= input$date[1] & Eingangsdatum <= input$date[2]) %>%
            filter(Abrechnungsstatus %in% input$status)
    })
    
    summary = reactive({
        filter_molis() %>%
            select(Total, Abrechnungsstatus) %>%
            group_by(Abrechnungsstatus) %>%
            summarise(Summe = sum(Total, na.rm = TRUE), Anzahl = n())
    })
    
    ids_selected = reactive({
        filter_molis() %>% pull(Anforderungsnr.) %>% .[input$total_rows_selected]
    })
    
    selected = reactive({
        cost_molis() %>%
            filter(Anforderungsnr. %in% ids_selected())
    })
    
    
    output$sum <- renderText({
        req(input$file)
        sum(filter_molis()$Total, na.rm = TRUE)
            })
    
    output$n <- renderText({
        req(input$file)
        nrow(filter_molis())
    })
    
    output$raw <- renderTable({
        req(input$file)
        head(raw_molis())
    })
    
    output$tidy <- renderTable({
        req(input$file)
        head(tidy_molis())
    })
    
    output$cost <- renderTable({
        req(input$file)
        head(cost_molis())
    })
    
    output$total <- DT::renderDataTable(
        filter = "top",
        rownames = FALSE,
        options = list(pageLength = 10, autoWidth = FALSE),{
        req(input$file)
        filter_molis()
    })
    
    output$summary <- renderTable({
        req(input$file)
        summary()
    })
    
    output$selected <- DT::renderDataTable(
        filter = "top",
        rownames = FALSE,
        options = list(pageLength = 100, autoWidth = FALSE),{
        req(input$file)
        selected() %>%
            select(Anforderungsnr., Eingangsdatum, MC, NAME, Einsender, TP)
        })
    
    # output$month <- renderText({
    #     paste("Export from", start.date(), "to", end.date())
    # })
    # 
    # start.date = reactive({input$month})
    # end.date = reactive({as.Date(input$month) + months(1) - days(1)})
    
    output$status <- renderUI({
        req(input$file)
        checkboxGroupInput("status", "Abrechnungsstatus", choices = all_status(), selected = all_status(), inline = TRUE)
    })
    
    all_status = reactive({
        cost_molis() %>% pull(Abrechnungsstatus) %>% unique()
    })
    
}

######################## RUN ########################
shinyApp(ui = ui, server = server)

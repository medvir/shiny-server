library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)
library(lubridate)
library(DT)

# setwd("P:/Desktop/MOLIS2accounting")
# library(shiny, lib.loc = "C:/RStudio/lib")
# library(tidyverse, lib.loc = "C:/RStudio/lib")
# library(readxl, lib.loc = "C:/RStudio/lib")
# library(shinythemes, lib.loc = "C:/RStudio/lib")
# library(lubridate, lib.loc = "C:/RStudio/lib")
# library(DT, lib.loc = "C:/RStudio/lib")



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
fields = c("Periode", "Anforderungsnr.", "Entnahmedatum", "Eingangsdatum", "MC", "NAME", "Einsender", "Kontrakt", "Herkunft", "Abrechnungsstatus", "TAR_IND", "Patientennummer")  #"Stationarer Patient",

abrvb.dict = read_csv("abrvb.table.csv") %>% pull(ABRVB)
names(abrvb.dict) = read_csv("abrvb.table.csv") %>% pull(KURZNAME)

tax.dict = read_csv("tax.table.csv") %>% pull(CHF)
names(tax.dict) = read_csv("tax.table.csv") %>% pull(ABRVB)

TP.dict = read_csv("anaverf.table.csv") %>% pull(TP)
names(TP.dict) = read_csv("anaverf.table.csv") %>% pull(MC)

######################## UI ######################## 
ui <- fluidPage(
    theme = shinytheme("yeti"),
    titlePanel("MOLIS 2 accounting"),
    fluidRow(column(3,
                    #panel(
                        fileInput("file", "MOLIS Export", accept = "xlsx"),
                        dateRangeInput("date", label = "Date range", start = floor_date(Sys.Date(), 'year') - 2 * 365, end = Sys.Date(), max = Sys.Date(), weekstart = 1),
                        uiOutput("status")
                    ),
             column(3,
                    #panel(
                        h4("Anzahl AuftrÃ¤ge"),
                        textOutput("n"),
                        h4("Summe"),
                        textOutput("sum")
                    ),
             column(3,
                    #panel(
                        tableOutput("summary")
                    ),
             column(3,
                    #panel(
                    tableOutput("monthly")
             )),
    fluidRow(
        sidebarLayout(
            sidebarPanel(h2("Subtotal"),
                         tableOutput("selected")),
            mainPanel(h2("Total"),
                      DT::dataTableOutput("total"),
                      tableOutput("cost")
                      ),
                      position = "right", fluid = TRUE
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
            #mutate(Material = replicate_material(MC, RES), Interpretation = move_bewertung(NAME, RES)) %>% ### functions replicate_material and move_bewertung
            #mutate(Material = replicate_material(MC, RES)) %>% ### functions replicate_material
            filter(!MC == "MATD", !is.na(NAME), NAME != 'Bewertung') %>%
            mutate(Entnahmedatum = as.Date(Entnahmedatum, "%d.%m.%Y")) %>%
            mutate(Eingangsdatum = as.Date(Eingangsdatum, "%d.%m.%Y")) %>%
            mutate(Entnahmedatum = as.Date(ifelse(is.na(Entnahmedatum), Eingangsdatum, Entnahmedatum), origin = "1970-01-01"))
    })
    

    cost_molis = reactive({
        tidy_molis() %>%
            mutate(TP = ifelse(TAR_IND == "0", TP.dict[MC], 0)) %>%
            #mutate(Rechnungsempfaenger = ifelse(Herkunft == "USZ", paste0(Herkunft, `Stationaerer Patient`), Kontrakt)) %>%
            group_by(Anforderungsnr.) %>%
            mutate(counter = row_number()) %>%
            mutate(Subtotal = ifelse(counter == 1, sum(TP, na.rm = TRUE), 0)) %>%
            ### Block Discount ###
            mutate(TP = block_discount(MC, TP)) %>%
            
            group_by(Patientennummer, Eingangsdatum) %>%
            mutate(counter.day = row_number()) %>%
            
            ### Auftragspauschale ###
            mutate(Auftragspauschale = case_when(
                Subtotal == 0 ~ 0,
                counter.day == 1 & Einsender %in% names(abrvb.dict) ~ tax.dict[abrvb.dict[Einsender]],
                counter.day == 1 ~ 24,
                TRUE ~ 0)) %>%
            
            group_by(Anforderungsnr.) %>%
            select(Periode, Anforderungsnr., Eingangsdatum, counter, MC, NAME, Einsender, TP, Subtotal, Auftragspauschale, Abrechnungsstatus, TAR_IND, Kontrakt)
    })
    
    total_molis = reactive({
        cost_molis() %>%
            filter(counter == 1) %>%
            
            ### Discount ###
            mutate(Discount = case_when(
                Kontrakt == "IMV" ~ 100,
                Kontrakt == "USZ" ~ 22.5,
                TRUE ~ 0)) %>%
            mutate(Total = sum(c(Subtotal, Auftragspauschale), na.rm = TRUE) * (1 - Discount/100)) %>%
            select(Periode, Anforderungsnr., Eingangsdatum, Einsender, Subtotal, Auftragspauschale, Discount, Total, Abrechnungsstatus)
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
    
    monthly = reactive({
        filter_molis() %>%
            select(Periode, Total) %>%
            group_by(Periode) %>%
            summarise(Summe = sum(Total, na.rm = TRUE), Anzahl = n())
    })
    
    ids_selected = reactive({
        filter_molis() %>% pull(Anforderungsnr.) %>% .[input$total_rows_selected]
        #filter_molis()[input$hoverIndexJS + 1, ] %>% pull(Anforderungsnr.) 
    })
    
    selected = reactive({
        cost_molis() %>%
            filter(Anforderungsnr. %in% ids_selected())
    })
    
    
    ### OUTPUT ###
    
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
        head(cost_molis(), 10000)
    })
    
    output$total <- DT::renderDataTable(
        filter = "top",
        rownames = FALSE,
        options = list(pageLength = 100, autoWidth = FALSE,
                       #dom = 't',
                       rowCallback = JS('function(row, data) {
                                                  $(row).mouseenter(function(){
                                                  var hover_index = $(this)[0]._DT_RowIndex
                                                  /* console.log(hover_index); */
                                                  Shiny.onInputChange("hoverIndexJS", hover_index);
                                                  });
                                          }')
        ),{
        req(input$file)
        filter_molis()
    })
    
    output$summary <- renderTable({
        req(input$file)
        summary()
    })
    
    output$monthly <- renderTable({
        req(input$file)
        monthly()
    })
    
    output$selected <- renderTable({
            req(input$file)
            selected() %>%
                select(Anforderungsnr., MC, NAME, TP)
        })
    
    # output$month <- renderText({
    #     paste("Export from", start.date(), "to", end.date())
    # })
    # 
    # start.date = reactive({input$month})
    # end.date = reactive({as.Date(input$month) + months(1) - days(1)})
    
    
    ### UI OUTPUT ###
    
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

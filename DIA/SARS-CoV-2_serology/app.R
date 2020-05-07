library(shiny)
library(tidyverse)
library(readxl)
library(gt)

source("app-functions.R", local = TRUE)


# shiny ui ----------------------------------------------------------------

ui <- fluidPage(
    titlePanel("SARS-CoV-2 serology"),
    tags$head(tags$style(HTML("hr {border-top: 1px solid #949998;}"))),
    fluidRow(
        column(2,
               wellPanel(
                   fileInput("all_in_one_file", "Luminex (all-in-one) output [.csv]:", accept = c(".csv")),
                   hr(),
                   fileInput("igg_file", "Luminex IgG output [.csv]:", accept = c(".csv")),
                   fileInput("iga_file", "Luminex IgA output [.csv]:", accept = c(".csv")),
                   fileInput("igm_file", "Luminex IgM output [.csv]:", accept = c(".csv")),
                   hr(),
                   fileInput("barcodes_file", "Barcodes file [.xlsx]:", accept = c(".xlsx")),
                   br(),
                   downloadButton("export_png", "Save as png"),
                   downloadButton("export_pdf", "Save as pdf"),
                   downloadButton("export_csv", "Save as csv")
                   )
               ),

        column(10,
               gt_output(outputId = "table")
               )
    )
)


# shiny server ------------------------------------------------------------

server <- function(input, output, session) {
    
    table <- reactive({
        
        all_in_one_path = input$all_in_one_file$datapath
        all_in_one_name = input$all_in_one_file$name

        igg_path = input$igg_file$datapath
        igg_name = input$igg_file$name
        iga_path = input$iga_file$datapath
        iga_name = input$iga_file$name
        igm_path = input$igm_file$datapath
        igm_name = input$igm_file$name

        barcodes_path = input$barcodes_file$datapath

        req(input$barcodes_file)
        
        # Use fixed path for debugging, uncomment following lines and comment "req"
        # barcodes_path <- "data/200420_BARCODES.xlsx"
        # igg_path <- "data/200420_plate_1_IgG_20200420_121133.csv"
        # iga_path <- "data/200420_plate_1_IgA_20200420_122957.csv"
        # igm_path <- "data/200420_plate_1_IgM_20200420_125709.csv"
        
        

        # check input -------------------------------------------------------------
        
        
        if (!is.null(input$all_in_one_file$datapath)) {
            date_all_in_one <- strsplit(all_in_one_name, "_")[[1]][1]
            date_barcodes <- strsplit(input$barcodes_file$name, "_")[[1]][1]
            
            if (date_all_in_one != date_barcodes) {
                stop("Check uploaded Files (dates do not match)")
            }
        } else {
            date_igg <- strsplit(igg_name, "_")[[1]][1]
            plate_igg <- paste0(strsplit(igg_name, "_")[[1]][2:3], collapse = " ")
            
            if (isFALSE(str_detect(igg_name, pattern = regex("igg", ignore_case = TRUE)))){
                stop("Check Luminex output for IgG ('IgG' is not part of the filename)")
            }
            
            
            date_iga <- strsplit(input$iga_file$name, "_")[[1]][1]
            plate_iga <- paste0(strsplit(input$iga_file$name, "_")[[1]][2:3], collapse = " ")
            
            if (isFALSE(str_detect(iga_name, pattern = regex("iga", ignore_case = TRUE)))){
                stop("Check Luminex output for IgA ('IgA' is not part of the filename)")
            }
            
            
            date_igm <- strsplit(input$igm_file$name, "_")[[1]][1]
            plate_igm <- paste0(strsplit(input$igm_file$name, "_")[[1]][2:3], collapse = " ")
            
            if (isFALSE(str_detect(igm_name, pattern = regex("igm", ignore_case = TRUE)))){
                stop("Check Luminex output for IgM ('IgM' is not part of the filename)")
            }
            
            date_barcodes <- strsplit(input$barcodes_file$name, "_")[[1]][1]
            
            
            date = c(date_igg, date_iga, date_igm, date_barcodes)
            plate = c(plate_igg, plate_iga, plate_igm)
            
            if (length(unique(date)) != 1){
                stop("Check uploaded Files (not all dates are equal)")
            }
            
            if (length(unique(plate)) != 1){
                stop("Check uploaded Files (not all plate numbers are equal)")
            }
        }

        
        # Prepare Barcodes --------------------------------------------------------
        
        barcodes <- read_barcodes(barcodes_path)
        
        # if isotypes are specified in barcodes document (e.g. multiple isotypes on one plate)
        isotypes <- read_isotypes(barcodes_path)
        barcodes_isotypes <- join_barcodes_isotypes(barcodes, isotypes)
        
        
        # Prepare Luminex output --------------------------------------------------
        
        if (!is.null(input$all_in_one_file$datapath)) {
            count <- get_count(all_in_one_path, barcodes_isotypes)
            
            net_mfi_foc <-
                get_net_mfi(all_in_one_path, barcodes_isotypes) %>%
                left_join(count, by = c("Sample"), suffix = c("", "_count"))

        } else {
            count <-
                get_count(igg_path, barcodes_isotypes, "IgG") %>%
                left_join(get_count(iga_path, barcodes_isotypes, "IgA"), by = c("Sample")) %>%
                left_join(get_count(igm_path, barcodes_isotypes, "IgM"), by = c("Sample"))
            
            net_mfi_foc <-
                get_net_mfi(igg_path, barcodes_isotypes, "IgG") %>%
                left_join(get_net_mfi(iga_path, barcodes_isotypes, "IgA"), by = c("Sample")) %>%
                left_join(get_net_mfi(igm_path, barcodes_isotypes, "IgM"), by = c("Sample")) %>%
                left_join(count, by = c("Sample"), suffix = c("", "_count"))
        }


        # set flag ----------------------------------------------------------------
        
        # minimal count for each measurment has to be following value, everything below gets flagged
        min_count <- 20

        # empty FOC (fold over cutoff) max. following value, everything above gets flagged
        above_cutoff <- 1

        net_mfi_foc <- set_flag(net_mfi_foc, min_count, above_cutoff)


        # Test Resultat -------------------------------------------------------------
        
        net_mfi_foc <- test_result(net_mfi_foc)
        
        net_mfi_foc_clean <-
            net_mfi_foc %>%
            select(Sample,
                   Serokonversion,
                   IgG_Resultat, IgG_NP, IgG_S2, IgG_S1,
                   IgM_Resultat, IgM_NP, IgM_S2, IgM_S1,
                   IgA_Resultat, IgA_NP, IgA_S2, IgA_S1,
                   Kommentar,
                   Fehler_count, Fehler_empty) %>%
            mutate(IgG_Resultat = if_else(IgG_Resultat, "pos", "neg"),
                   IgM_Resultat = if_else(IgM_Resultat, "pos", "neg"),
                   IgA_Resultat = if_else(IgA_Resultat, "pos", "neg")) %>%
            arrange(Sample)
        
        net_mfi_foc_clean
    })
    
    assay_date <- reactive({
        if (!is.null(input$all_in_one_file$datapath)) {
            strsplit(input$all_in_one_file$name, "_")[[1]][1]
        } else {
            strsplit(input$igg_file$name, "_")[[1]][1]
        }
    })
    
    plate_number <- reactive({
        if (!is.null(input$all_in_one_file$datapath)) {
            paste0(strsplit(input$all_in_one_file$name, "_")[[1]][2:3], collapse = "_")
        } else {
            paste0(strsplit(input$igg_file$name, "_")[[1]][2:3], collapse = "_")
        }
    })
    
    
    table_gt <- reactive({
            create_gt(table())
    })

    
    output$table <- render_gt(expr = table_gt())
    
    
    output$export_png <- downloadHandler(
        filename = function() {
            paste0(assay_date(), "_", plate_number(), ".png")
        },
        content = function(file) {
            gtsave(table_gt(), file, vwidth = 1500)
        }
    )
    
    output$export_pdf <- downloadHandler(
        filename = function() {
            paste0(assay_date(), "_", plate_number(), ".pdf")
        },
        content = function(file) {
            tempReport <- file.path(tempdir(), "SARS-CoV-2_serology.Rmd")
            file.copy("SARS-CoV-2_serology.Rmd", tempReport, overwrite = TRUE)
            
            params <- list(table = table(),
                           assay_date = assay_date(),
                           plate_number = plate_number())
            
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv()))
        }
    )
    
    output$export_csv <- downloadHandler(
        filename = function() {
            paste0(assay_date(), "_", plate_number(), ".csv")
        },
        content = function(file) {
            table() %>%
                mutate(assay_date = assay_date(),
                       plate_number = plate_number()) %>%
                write_csv(file)
        }
    )
    

}



# Run the application 
shinyApp(ui = ui, server = server)

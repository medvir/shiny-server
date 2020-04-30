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
                   downloadButton("export_png", "Save Table (png)"),
                   downloadButton("export_pdf", "Save Table (pdf)"),
                   downloadButton("export_csv", "Save Table (csv)")
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

        igg_path = input$igg_file$datapath
        iga_path = input$iga_file$datapath
        igm_path = input$igm_file$datapath

        barcodes_path = input$barcodes_file$datapath

        req(input$barcodes_file)
        
        # Use fixed path for debugging, uncomment following lines and comment "req"
        # barcodes_path <- "data/200420_BARCODES.xlsx"
        # igg_path <- "data/200420_plate_1_IgG_20200420_121133.csv"
        # iga_path <- "data/200420_plate_1_IgA_20200420_122957.csv"
        # igm_path <- "data/200420_plate_1_IgM_20200420_125709.csv"
        
        
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
                   Interpretation,
                   IgG_Resultat, IgG_NP, IgG_S2, IgG_S1,
                   IgA_Resultat, IgA_NP, IgA_S2, IgA_S1,
                   IgM_Resultat, IgM_NP, IgM_S2, IgM_S1,
                   Kommentar,
                   Fehler_count, Fehler_empty) %>%
            mutate(IgG_Resultat = if_else(IgG_Resultat, "pos", "neg"),
                   IgA_Resultat = if_else(IgA_Resultat, "pos", "neg"),
                   IgM_Resultat = if_else(IgM_Resultat, "pos", "neg"))
        
        net_mfi_foc_clean
        

    })
    
    
        
        table_gt <- reactive({
            create_gt(table())
    })

    output$table <- render_gt(
        expr = table_gt()
    )
    
    output$export_png <- downloadHandler(
        filename = function() {
            "output.png"
        },
        content = function(file) {
            gtsave(table_gt(), file, vwidth = 1500)
            
        }
    )
    
    output$export_pdf <- downloadHandler(
        filename = function() {
            "output.pdf"
        },
        content = function(file) {
            tempReport <- file.path(tempdir(), "SARS-CoV-2_serology.Rmd")
            file.copy("SARS-CoV-2_serology.Rmd", tempReport, overwrite = TRUE)
            
            params <- list(table = table())
            
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv()))
        }
    )
    
    output$export_csv <- downloadHandler(
        filename = function() {
            "output.csv"
        },
        content = function(file) {
            write_csv(table(), file)
            
        }
    )
    

}



# Run the application 
shinyApp(ui = ui, server = server)

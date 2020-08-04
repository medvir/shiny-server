library(shiny)
library(tidyverse)
library(readxl)
library(gt)

source("app-functions.R", local = TRUE)


# shiny ui ----------------------------------------------------------------

ui <- fluidPage(
    titlePanel("SARS-CoV-2 serology - Version 2"),
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
    
    table_raw <- reactive({
        
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
        # barcodes_path <- "data/200706_BARCODES_Ciao_4.xlsx"
        # igg_path <- "data/200706_Ciao_4_IgG_20200707_104333.csv"
        # iga_path <- "data/200706_Ciao_4_IgA_20200707_141245.csv"
        # igm_path <- "data/200706_Ciao_4_IgM_20200707_164242.csv"
        
        

        # check input -------------------------------------------------------------
        
        
        if (!is.null(input$all_in_one_file$datapath)) {
            date_all_in_one <- strsplit(all_in_one_name, "_")[[1]][1]
            date_barcodes <- strsplit(input$barcodes_file$name, "_")[[1]][1]
            
            validate(
                need(date_all_in_one == date_barcodes,
                     "Check uploaded Files (dates do not match)")
            )
        } else {
            date_igg <- strsplit(igg_name, "_")[[1]][1]
            plate_igg <- paste0(strsplit(igg_name, "_")[[1]][2:3], collapse = " ")
            
            validate(
                need(isTRUE(str_detect(igg_name, pattern = regex("igg", ignore_case = TRUE))),
                     "Check Luminex output for IgG ('IgG' is not part of the filename)")
            )
            
            
            date_iga <- strsplit(input$iga_file$name, "_")[[1]][1]
            plate_iga <- paste0(strsplit(input$iga_file$name, "_")[[1]][2:3], collapse = " ")
            
            validate(
                need(isTRUE(str_detect(iga_name, pattern = regex("iga", ignore_case = TRUE))),
                     "Check Luminex output for IgA ('IgA' is not part of the filename)")
            )
            
            
            date_igm <- strsplit(input$igm_file$name, "_")[[1]][1]
            plate_igm <- paste0(strsplit(input$igm_file$name, "_")[[1]][2:3], collapse = " ")
            
            validate(
                need(isTRUE(str_detect(igm_name, pattern = regex("igm", ignore_case = TRUE))),
                     "Check Luminex output for IgM ('IgM' is not part of the filename)")
            )
            
            date_barcodes <- strsplit(input$barcodes_file$name, "_")[[1]][1]
            
            
            date = c(date_igg, date_iga, date_igm, date_barcodes)
            plate = c(plate_igg, plate_iga, plate_igm)
            
            validate(
                need(length(unique(date)) == 1,
                     "Check uploaded Files (not all dates are equal)")
            )
            
            validate(
                need(length(unique(plate)) == 1,
                     "Check uploaded Files (not all plate numbers are equal)")
            )
        }
        
        # Prepare Barcodes --------------------------------------------------------
        
        barcodes <- read_barcodes(barcodes_path)
        
        # if isotypes are specified in barcodes document (e.g. multiple isotypes on one plate)
        isotypes <- read_isotypes(barcodes_path)
        barcodes_isotypes <- join_barcodes_isotypes(barcodes, isotypes)
        
        
        # Prepare Luminex output --------------------------------------------------
        
        if (!is.null(input$all_in_one_file$datapath)) {
            count <- get_count(all_in_one_path, barcodes_isotypes)
            
            net_mfi_soc <-
                get_net_mfi(all_in_one_path, barcodes_isotypes) %>%
                left_join(count, by = c("Sample"))

        } else {
            count <-
                get_count(igg_path, barcodes_isotypes, "IgG") %>%
                left_join(get_count(iga_path, barcodes_isotypes, "IgA"), by = c("Sample")) %>%
                left_join(get_count(igm_path, barcodes_isotypes, "IgM"), by = c("Sample"))
            
            net_mfi_soc <-
                get_net_mfi(igg_path, barcodes_isotypes, "IgG") %>%
                left_join(get_net_mfi(iga_path, barcodes_isotypes, "IgA"), by = c("Sample")) %>%
                left_join(get_net_mfi(igm_path, barcodes_isotypes, "IgM"), by = c("Sample")) %>%
                left_join(count, by = c("Sample"))
        }


        # set flag ----------------------------------------------------------------
        
        # minimal count for each measurment has to be following value, everything below gets flagged
        min_count <- 20

        # empty bead net MFI max. following values, everything above gets flagged
        above_cutoff_IgG <- 40.05
        above_cutoff_IgA <- 55.26
        above_cutoff_IgM <- 539.74

        net_mfi_soc <- set_flag(net_mfi_soc, min_count, above_cutoff_IgG, above_cutoff_IgA, above_cutoff_IgM)


        # Test Resultat -------------------------------------------------------------
        
        # define thresholds for some gw (grenzwertig) results
        IgG_NP_gw <- 0.884
        IgG_RBD_gw <- 0.714
        IgG_S1_gw <- 0.895
        
        net_mfi_soc <-
            test_result(net_mfi_soc, IgG_NP_gw, IgG_RBD_gw, IgG_S1_gw) %>%
            arrange(Sample)
        
        net_mfi_soc
    })
        

    # Clean table -------------------------------------------------------------
    table <- reactive({
        
        # only rename and select columns, no transformation/calculations should be done here
        
        if (sum(str_detect(names(table_raw()), "RBD")) > 0){
            net_mfi_soc_clean <-
                table_raw() %>%
                rename(IgG_NP = IgG_NP_net_mfi_soc, IgG_S2 = IgG_S2_net_mfi_soc, IgG_S1 = IgG_S1_net_mfi_soc, IgG_RBD = IgG_RBD_net_mfi_soc,
                       IgM_NP = IgM_NP_net_mfi_soc, IgM_S2 = IgM_S2_net_mfi_soc, IgM_S1 = IgM_S1_net_mfi_soc, IgM_RBD = IgM_RBD_net_mfi_soc,
                       IgA_NP = IgA_NP_net_mfi_soc, IgA_S2 = IgA_S2_net_mfi_soc, IgA_S1 = IgA_S1_net_mfi_soc, IgA_RBD = IgA_RBD_net_mfi_soc,) %>%
                select(Sample,
                       Serokonversion,
                       IgG_NP, IgG_S2, IgG_S1, IgG_RBD,
                       IgM_NP, IgM_S2, IgM_S1, IgM_RBD,
                       IgA_NP, IgA_S2, IgA_S1, IgA_RBD,
                       Fehler_count, Fehler_empty)
        } else{
            net_mfi_soc_clean <-
                table_raw() %>%
                rename(IgG_NP = IgG_NP_net_mfi_soc, IgG_S2 = IgG_S2_net_mfi_soc, IgG_S1 = IgG_S1_net_mfi_soc,
                       IgM_NP = IgM_NP_net_mfi_soc, IgM_S2 = IgM_S2_net_mfi_soc, IgM_S1 = IgM_S1_net_mfi_soc,
                       IgA_NP = IgA_NP_net_mfi_soc, IgA_S2 = IgA_S2_net_mfi_soc, IgA_S1 = IgA_S1_net_mfi_soc) %>%
                select(Sample,
                       Serokonversion,
                       IgG_NP, IgG_S2, IgG_S1,
                       IgM_NP, IgM_S2, IgM_S1,
                       IgA_NP, IgA_S2, IgA_S1,
                       Fehler_count, Fehler_empty) 
        }
        
        
        net_mfi_soc_clean
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
            if (sum(str_detect(names(table_raw()), "HKU")) > 0){
                table_raw() %>%
                    select(-starts_with("IgG_Resultat_"),
                           -starts_with("IgM_Resultat_"),
                           -starts_with("IgA_Resultat_"),
                           -Resultat_sum, -Resultat_sum_IgG, -Resultat_n_pos) %>%
                    rename(IgG_HKU_net_mfi = IgG_HKU1,
                           IgA_HKU_net_mfi = IgA_HKU1,
                           IgM_HKU_net_mfi = IgM_HKU1,
                           IgG_Empty_net_mfi = IgG_Empty,
                           IgA_Empty_net_mfi = IgA_Empty,
                           IgM_Empty_net_mfi = IgM_Empty) %>%
                    mutate(assay_date = assay_date(),
                           plate_number = plate_number(),
                           interpretation_version = "2.0.0") %>%
                    write_csv(file)
            } else{
                table_raw() %>%
                    select(-starts_with("IgG_Resultat_"),
                           -starts_with("IgM_Resultat_"),
                           -starts_with("IgA_Resultat_"),
                           -Resultat_sum, -Resultat_sum_IgG, -Resultat_n_pos) %>%
                    rename(IgG_Empty_net_mfi = IgG_Empty,
                           IgA_Empty_net_mfi = IgA_Empty,
                           IgM_Empty_net_mfi = IgM_Empty) %>%
                    mutate(assay_date = assay_date(),
                           plate_number = plate_number(),
                           interpretation_version = "2.0.0") %>%
                    write_csv(file)
            }
        }
    )
    

}



# Run the application 
shinyApp(ui = ui, server = server)

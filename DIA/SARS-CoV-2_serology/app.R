library(shiny)
library(tidyverse)
library(readxl)
library(gt)


# shiny ui ----------------------------------------------------------------

ui <- fluidPage(
    titlePanel("SARS-CoV-2 serology"),
    fluidRow(
        column(2,
               wellPanel(
                   fileInput("barcodes_file", "Barcodes file [.xlsx]:", accept = c(".xlsx")),
                   fileInput("igg_file", "Luminex IgG output [.csv]:", accept = c(".csv")),
                   fileInput("iga_file", "Luminex IgA output [.csv]:", accept = c(".csv")),
                   fileInput("igm_file", "Luminex IgM output [.csv]:", accept = c(".csv")),
                   downloadButton("export_png", "Save Table (png)")
               )),
        column(10,
               gt_output(outputId = "table"))
    )
)



# shiny server ------------------------------------------------------------

server <- function(input, output, session) {
    
    table <- reactive({
        
        barcodes_path = input$barcodes_file$datapath
        igg_path = input$igg_file$datapath
        iga_path = input$iga_file$datapath
        igm_path = input$igm_file$datapath
        
        req(input$barcodes_file, input$igg_file, input$iga_file, input$igm_file)
        
        #Use fixed path for debugging, uncomment following lines and comment "req"
        # barcodes_path <- "data/200420_BARCODES.xlsx"
        # igg_path <- "data/200420_plate_1_IgG_20200420_121133.csv"
        # iga_path <- "data/200420_plate_1_IgA_20200420_122957.csv"
        # igm_path <- "data/200420_plate_1_IgM_20200420_125709.csv"
        
        
        # Prepare Barcodes --------------------------------------------------------
        
        barcodes_sheets <- excel_sheets(barcodes_path)
        
        # Replace with INPUT from shiny app
        barcodes_sheets_selected <- barcodes_sheets[1]
        
        barcodes <-
            read_excel(barcodes_path,
                       sheet = barcodes_sheets_selected,
                       skip = 3,
                       col_names = FALSE)
        
        colnames(barcodes) <- c("row", 1:12)
        
        barcodes <-
            barcodes %>%
            pivot_longer(-row, names_to = "column", values_to = "Sample") %>%
            filter(!is.na(Sample)) %>%
            arrange(column) %>%
            mutate(sample_nr = 1,
                   sample_nr = cumsum(sample_nr),
                   Location = paste0(sample_nr, "(1,", row, column, ")")) %>%
            select(Location, Sample)
        
        
        
        # Prepare Luminex output --------------------------------------------------
        
        skip_rows <-
            function(filepath, match){
                count_row <-
                    read_csv(filepath, skip = 46, skip_empty_rows = FALSE) %>%
                    pull(2) %>%
                    match(match, .)
                
                return(count_row+46-1)
            }
        
        # IgG
        ## count
        igg_count <-
            read_csv(igg_path, skip = skip_rows(igg_path, "Count"), skip_empty_rows = FALSE) %>%
            head(length(barcodes$Sample)) %>%
            rename(IgG_NP = NP,
                   IgG_S2 = S2,
                   IgG_S1 = S1,
                   IgG_empty = empty,
                   Total_Events_IgG = `Total Events`) %>%
            select(-Sample)
        
        ## net_mfi
        igg_net_mfi <-
            read_csv(igg_path, skip = skip_rows(igg_path, "Net MFI")+1, skip_empty_rows = FALSE) %>%
            head(length(barcodes$Sample)) %>%
            select(-Sample, -`Total Events`) %>%
            left_join(barcodes, by = c("Location")) %>%
            select(-Location) 
        
        igg_net_mfi_neg <-
            igg_net_mfi %>%
            filter(Sample == "neg control") %>%
            pivot_longer(-Sample, names_to = "target", values_to = "igg_net_mfi_neg") %>%
            select(-Sample)
        
        igg_net_mfi <-
            igg_net_mfi %>%  
            pivot_longer(-Sample, names_to = "target", values_to = "igg_net_mfi") %>%
            left_join(igg_net_mfi_neg, by = c("target")) %>%
            mutate(target = paste0("IgG_", target),
                   net_mfi_foc = as.numeric(igg_net_mfi)/(3*as.numeric(igg_net_mfi_neg))) %>%
            select(Sample, target, net_mfi_foc) %>%
            pivot_wider(names_from = target, values_from = net_mfi_foc)
        
        
        # IgA
        ## count
        iga_count <-
            read_csv(iga_path, skip = skip_rows(iga_path, "Count"), skip_empty_rows = FALSE) %>%
            head(length(barcodes$Sample)) %>%
            rename(IgA_NP = NP,
                   IgA_S2 = S2,
                   IgA_S1 = S1,
                   IgA_empty = empty,
                   Total_Events_IgA = `Total Events`) %>%
            select(-Sample)
        
        ## net_mfi
        iga_net_mfi <-
            read_csv(iga_path, skip = skip_rows(iga_path, "Net MFI")+1, skip_empty_rows = FALSE) %>%
            head(length(barcodes$Sample)) %>%
            select(-Sample, -`Total Events`) %>%
            left_join(barcodes, by = c("Location")) %>%
            select(-Location) 
        
        iga_net_mfi_neg <-
            iga_net_mfi %>%
            filter(Sample == "neg control") %>%
            pivot_longer(-Sample, names_to = "target", values_to = "iga_net_mfi_neg") %>%
            select(-Sample)
        
        iga_net_mfi <-
            iga_net_mfi %>%  
            pivot_longer(-Sample, names_to = "target", values_to = "iga_net_mfi") %>%
            left_join(iga_net_mfi_neg, by = c("target")) %>%
            mutate(target = paste0("IgA_", target),
                   net_mfi_foc = as.numeric(iga_net_mfi)/(3*as.numeric(iga_net_mfi_neg))) %>%
            select(Sample, target, net_mfi_foc) %>%
            pivot_wider(names_from = target, values_from = net_mfi_foc)
        
        
        # IgM
        ## count
        igm_count <-
            read_csv(igm_path, skip = skip_rows(igm_path, "Count"), skip_empty_rows = FALSE) %>%
            head(length(barcodes$Sample)) %>%
            rename(IgM_NP = NP,
                   IgM_S2 = S2,
                   IgM_S1 = S1,
                   IgM_empty = empty,
                   Total_Events_IgM = `Total Events`) %>%
            select(-Sample)
        
        ## net_mfi
        igm_net_mfi <-
            read_csv(igm_path, skip = skip_rows(igm_path, "Net MFI")+1, skip_empty_rows = FALSE) %>%
            head(length(barcodes$Sample)) %>%
            select(-Sample, -`Total Events`) %>%
            left_join(barcodes, by = c("Location")) %>%
            select(-Location) 
        
        igm_net_mfi_neg <-
            igm_net_mfi %>%
            filter(Sample == "neg control") %>%
            pivot_longer(-Sample, names_to = "target", values_to = "igm_net_mfi_neg") %>%
            select(-Sample)
        
        igm_net_mfi <-
            igm_net_mfi %>%  
            pivot_longer(-Sample, names_to = "target", values_to = "igm_net_mfi") %>%
            left_join(igm_net_mfi_neg, by = c("target")) %>%
            mutate(target = paste0("IgM_", target),
                   net_mfi_foc = as.numeric(igm_net_mfi)/(3*as.numeric(igm_net_mfi_neg))) %>%
            select(Sample, target, net_mfi_foc) %>%
            pivot_wider(names_from = target, values_from = net_mfi_foc)
        
        
        
        # Join Luminex output -----------------------------------------------------
        
        count <-
            barcodes %>%
            left_join(igg_count, by = c("Location")) %>%
            left_join(iga_count, by = c("Location")) %>%
            left_join(igm_count, by = c("Location")) %>%
            select(-Location)
        
        net_mfi_foc <-
            igg_net_mfi %>%
            left_join(iga_net_mfi, by = c("Sample")) %>%
            left_join(igm_net_mfi, by = c("Sample")) %>%
            left_join(count, by = c("Sample"), suffix = c("", "_count"))
        
        
        
        # set flag ----------------------------------------------------------------
        
        # minimal count for each measurment has to be following value, everything below gets flagged
        min_count <- 20
        
        # empty FOC (fold over cutoff) max. following value, everything above gets flagged
        above_cutoff <- 1
        
        net_mfi_foc <-
            net_mfi_foc %>%
            mutate(IgG_NP_count_flag = if_else(as.numeric(IgG_NP_count) < min_count, "IgG NP", NULL),
                   IgG_S2_count_flag = if_else(as.numeric(IgG_S2_count) < min_count, "IgG S2", NULL),
                   IgG_S1_count_flag = if_else(as.numeric(IgG_S1_count) < min_count, "IgG S1", NULL),
                   IgA_NP_count_flag = if_else(as.numeric(IgA_NP_count) < min_count, "IgA NP", NULL),
                   IgA_S2_count_flag = if_else(as.numeric(IgA_S2_count) < min_count, "IgA S2", NULL),
                   IgA_S1_count_flag = if_else(as.numeric(IgA_S1_count) < min_count, "IgA S1", NULL),
                   IgM_NP_count_flag = if_else(as.numeric(IgM_NP_count) < min_count, "IgM NP", NULL),
                   IgM_S2_count_flag = if_else(as.numeric(IgM_S2_count) < min_count, "IgM S2", NULL),
                   IgM_S1_count_flag = if_else(as.numeric(IgM_S1_count) < min_count, "IgM S1", NULL)) %>%
            unite(Fehler_count, ends_with("count_flag"), na.rm=TRUE, sep = ", ") %>%
            mutate(IgG_empty_flag = if_else(IgG_empty > above_cutoff, "IgG", NULL),
                   IgA_empty_flag = if_else(IgA_empty > above_cutoff, "IgA", NULL),
                   IgM_empty_flag = if_else(IgM_empty > above_cutoff, "IgM", NULL)) %>%
            unite(Fehler_empty, ends_with("empty_flag"), na.rm=TRUE, sep = ", ")
        
        
        # Test Resultat -------------------------------------------------------------
        
        net_mfi_foc <-
            net_mfi_foc %>%
            mutate(IgG_Resultat_S1 = IgG_S1 > 1,
                   IgA_Resultat_S1 = IgA_S1 > 1,
                   IgM_Resultat_S1 = IgM_S1 > 1) %>%
            mutate(IgG_Resultat_S2 = IgG_S2 > 1 & (IgG_Resultat_S1 + IgA_Resultat_S1 + IgM_Resultat_S1 > 0),
                   IgA_Resultat_S2 = IgA_S2 > 1 & (IgG_Resultat_S1 + IgA_Resultat_S1 + IgM_Resultat_S1 > 0),
                   IgM_Resultat_S2 = IgM_S2 > 1 & (IgG_Resultat_S1 + IgA_Resultat_S1 + IgM_Resultat_S1 > 0)) %>%
            mutate(IgG_Resultat_NP = IgG_NP > 1 & (IgG_Resultat_S1 + IgA_Resultat_S1 + IgM_Resultat_S1 > 0),
                   IgA_Resultat_NP = IgA_NP > 1 & (IgG_Resultat_S1 + IgA_Resultat_S1 + IgM_Resultat_S1 > 0),
                   IgM_Resultat_NP = IgM_NP > 1 & (IgG_Resultat_S1 + IgA_Resultat_S1 + IgM_Resultat_S1 > 0)) %>%
            mutate(IgG_Resultat = IgG_Resultat_S1 | IgG_Resultat_S2 | IgG_Resultat_NP,
                   IgA_Resultat = IgA_Resultat_S1 | IgA_Resultat_S2 | IgA_Resultat_NP,
                   IgM_Resultat = IgM_Resultat_S1 | IgM_Resultat_S2 | IgM_Resultat_NP) %>%
            mutate(Interpretation = case_when(
                IgG_Resultat_S1 == 1 ~ "Serokonversion fortgeschritten",
                IgA_Resultat_S1 + IgM_Resultat_S1 > 0 ~ "Serokonversion partiell",
                IgG_Resultat_S1 + IgA_Resultat_S1 + IgM_Resultat_S1 < 3 ~ "keine SARS-CoV-2 Serokonversion"
            )) %>%
            mutate(Kommentar = case_when(
                (Interpretation == "keine SARS-CoV-2 Serokonversion") & (IgG_S1 > 1 | IgA_S1 > 1 | IgM_S1 > 1 |
                                                                             IgG_S2 > 1 | IgG_S2 > 1 | IgM_S2 > 1 |
                                                                             IgG_NP > 1 | IgA_NP > 1 | IgM_NP > 1) ~ "Kreuzreaktivität mit anderem Coronavirus wahrscheinlich",
                (Interpretation == "keine SARS-CoV-2 Serokonversion") & (IgG_S1 >= 0.9 | IgA_S1 >= 0.9 | IgM_S1 >= 0.9) ~ "S1 Reaktivität grenzwertig, bitte Verlaufsprobe einsenden",
                TRUE ~ ""
            ))
        
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
        
        
        
        # export csv --------------------------------------------------------------
        
        #write_csv(net_mfi_foc_clean, "data/200420_plate_1_example_output.csv")
        
        
        # gt ----------------------------------------------------------------------
        
        net_mfi_foc_clean_gt <-
            net_mfi_foc_clean %>%
            gt(rowname_col = "Sample") %>%
            tab_spanner_delim(delim = "_") %>%
            
            tab_style(
                style = cell_fill(color = "#FFE74C"),
                locations = cells_body(
                    columns = vars(IgG_NP),
                    rows = IgG_NP >= 0.9)
            ) %>%
            tab_style(
                style = cell_fill(color = "#06AED5"),
                locations = cells_body(
                    columns = vars(IgG_NP),
                    rows = IgG_NP > 1)
            ) %>%
            tab_style(
                style = cell_fill(color = "#FFE74C"),
                locations = cells_body(
                    columns = vars(IgG_S2),
                    rows = IgG_S2 >= 0.9)
            ) %>%
            tab_style(
                style = cell_fill(color = "#06AED5"),
                locations = cells_body(
                    columns = vars(IgG_S2),
                    rows = IgG_S2 > 1)
            ) %>%
            tab_style(
                style = cell_fill(color = "#FFE74C"),
                locations = cells_body(
                    columns = vars(IgG_S1),
                    rows = IgG_S1 >= 0.9)
            ) %>%
            tab_style(
                style = cell_fill(color = "#06AED5"),
                locations = cells_body(
                    columns = vars(IgG_S1),
                    rows = IgG_S1 > 1)
            ) %>%
            tab_style(
                style = cell_fill(color = "#FFE74C"),
                locations = cells_body(
                    columns = vars(IgA_NP),
                    rows = IgA_NP >= 0.9)
            ) %>%
            tab_style(
                style = cell_fill(color = "#06AED5"),
                locations = cells_body(
                    columns = vars(IgA_NP),
                    rows = IgA_NP > 1)
            ) %>%
            tab_style(
                style = cell_fill(color = "#FFE74C"),
                locations = cells_body(
                    columns = vars(IgA_S2),
                    rows = IgA_S2 >= 0.9)
            ) %>%
            tab_style(
                style = cell_fill(color = "#06AED5"),
                locations = cells_body(
                    columns = vars(IgA_S2),
                    rows = IgA_S2 > 1)
            ) %>%
            tab_style(
                style = cell_fill(color = "#FFE74C"),
                locations = cells_body(
                    columns = vars(IgA_S1),
                    rows = IgA_S1 >= 0.9)
            ) %>%
            tab_style(
                style = cell_fill(color = "#06AED5"),
                locations = cells_body(
                    columns = vars(IgA_S1),
                    rows = IgA_S1 > 1)
            ) %>%
            tab_style(
                style = cell_fill(color = "#FFE74C"),
                locations = cells_body(
                    columns = vars(IgM_NP),
                    rows = IgM_NP >= 0.9)
            ) %>%
            tab_style(
                style = cell_fill(color = "#06AED5"),
                locations = cells_body(
                    columns = vars(IgM_NP),
                    rows = IgM_NP > 1)
            ) %>%
            tab_style(
                style = cell_fill(color = "#FFE74C"),
                locations = cells_body(
                    columns = vars(IgM_S2),
                    rows = IgM_S2 >= 0.9)
            ) %>%
            tab_style(
                style = cell_fill(color = "#06AED5"),
                locations = cells_body(
                    columns = vars(IgM_S2),
                    rows = IgM_S2 > 1)
            ) %>%
            tab_style(
                style = cell_fill(color = "#FFE74C"),
                locations = cells_body(
                    columns = vars(IgM_S1),
                    rows = IgM_S1 >= 0.9)
            ) %>%
            tab_style(
                style = cell_fill(color = "#06AED5"),
                locations = cells_body(
                    columns = vars(IgM_S1),
                    rows = IgM_S1 > 1)
            ) %>%
            
            tab_style(
                style = cell_fill(color = "#bfbfbf"),
                locations = cells_body(
                    columns = vars(IgG_Resultat),
                    rows = IgG_Resultat == "pos")
            ) %>%
            tab_style(
                style = cell_fill(color = "#bfbfbf"),
                locations = cells_body(
                    columns = vars(IgA_Resultat),
                    rows = IgA_Resultat == "pos")
            ) %>%
            tab_style(
                style = cell_fill(color = "#bfbfbf"),
                locations = cells_body(
                    columns = vars(IgM_Resultat),
                    rows = IgM_Resultat == "pos")
            ) %>%
            
            fmt_number(
                columns = c(ends_with("NP"),
                            ends_with("S2"),
                            ends_with("S1")),
                decimals = 1
            )
        
        net_mfi_foc_clean_gt
        
    })

    output$table <- render_gt(
        expr = table()
    )
    
    output$export_png <- downloadHandler(
        filename = function() {
            "output.png"
        },
        content = function(file) {
            gtsave(table(), file, vwidth = 1500)
            
        }
    )
    

}



# Run the application 
shinyApp(ui = ui, server = server)

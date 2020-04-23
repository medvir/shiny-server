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
                   downloadButton("export", "Save Table")
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
                   net_mfi_fob = as.numeric(igg_net_mfi)/as.numeric(igg_net_mfi_neg)) %>%
            select(Sample, target, net_mfi_fob) %>%
            pivot_wider(names_from = target, values_from = net_mfi_fob)
        
        
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
                   net_mfi_fob = as.numeric(iga_net_mfi)/as.numeric(iga_net_mfi_neg)) %>%
            select(Sample, target, net_mfi_fob) %>%
            pivot_wider(names_from = target, values_from = net_mfi_fob)
        
        
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
                   net_mfi_fob = as.numeric(igm_net_mfi)/as.numeric(igm_net_mfi_neg)) %>%
            select(Sample, target, net_mfi_fob) %>%
            pivot_wider(names_from = target, values_from = net_mfi_fob)
        
        
        
        # Join Luminex output -----------------------------------------------------
        
        count <-
            barcodes %>%
            left_join(igg_count, by = c("Location")) %>%
            left_join(iga_count, by = c("Location")) %>%
            left_join(igm_count, by = c("Location")) %>%
            select(-Location)
        
        net_mfi_fob <-
            igg_net_mfi %>%
            left_join(iga_net_mfi, by = c("Sample")) %>%
            left_join(igm_net_mfi, by = c("Sample"))
        
        
        # Test Resultat -------------------------------------------------------------
        
        net_mfi_fob <-
            net_mfi_fob %>%
            mutate(Resultat_IgG_S1 = IgG_S1 > 3,
                   Resultat_IgA_S1 = IgA_S1 > 3,
                   Resultat_IgM_S1 = IgM_S1 > 3) %>%
            mutate(Resultat_IgG_S2 = IgG_S2 > 3 & (Resultat_IgG_S1 + Resultat_IgA_S1 + Resultat_IgM_S1 > 0),
                   Resultat_IgA_S2 = IgA_S2 > 3 & (Resultat_IgG_S1 + Resultat_IgA_S1 + Resultat_IgM_S1 > 0),
                   Resultat_IgM_S2 = IgM_S2 > 3 & (Resultat_IgG_S1 + Resultat_IgA_S1 + Resultat_IgM_S1 > 0)) %>%
            mutate(Resultat_IgG_NP = IgG_NP > 3 & (Resultat_IgG_S1 + Resultat_IgA_S1 + Resultat_IgM_S1 > 0),
                   Resultat_IgA_NP = IgA_NP > 3 & (Resultat_IgG_S1 + Resultat_IgA_S1 + Resultat_IgM_S1 > 0),
                   Resultat_IgM_NP = IgM_NP > 3 & (Resultat_IgG_S1 + Resultat_IgA_S1 + Resultat_IgM_S1 > 0)) %>%
            mutate(Resultat_IgG = Resultat_IgG_S1 | Resultat_IgG_S2 | Resultat_IgG_NP,
                   Resultat_IgA = Resultat_IgA_S1 | Resultat_IgA_S2 | Resultat_IgA_NP,
                   Resultat_IgM = Resultat_IgM_S1 | Resultat_IgM_S2 | Resultat_IgM_NP) %>%
            mutate(Interpretation = case_when(
                Resultat_IgG_S1 == 1 ~ "Serokonversion fortgeschritten",
                Resultat_IgA_S1 + Resultat_IgM_S1 > 0 ~ "Serokonversion partiell",
                Resultat_IgG_S1 + Resultat_IgA_S1 + Resultat_IgM_S1 < 3 ~ "keine SARS-CoV-2 Serokonversion"
            ))
        
        net_mfi_fob_clean <-
            net_mfi_fob %>%
            select(-Resultat_IgG_S1, -Resultat_IgA_S1, -Resultat_IgM_S1,
                   -Resultat_IgG_S2, -Resultat_IgA_S2, -Resultat_IgM_S2,
                   -Resultat_IgG_NP, -Resultat_IgA_NP, -Resultat_IgM_NP,) %>%
            mutate(Resultat_IgG = if_else(Resultat_IgG, "pos", "neg"),
                   Resultat_IgA = if_else(Resultat_IgA, "pos", "neg"),
                   Resultat_IgM = if_else(Resultat_IgM, "pos", "neg"))
        
        net_mfi_fob_clean_gt <-
            net_mfi_fob_clean %>%
            gt(rowname_col = "Sample") %>%
            tab_spanner_delim(delim = "_") %>%
            
            tab_style(
                style = cell_fill(color = "#c5d9f1"),
                locations = cells_body(
                    columns = vars(IgG_NP),
                    rows = IgG_NP >= 3)
            ) %>%
            tab_style(
                style = cell_fill(color = "#538dd5"),
                locations = cells_body(
                    columns = vars(IgG_NP),
                    rows = IgG_NP >= 10)
            ) %>%
            tab_style(
                style = cell_fill(color = "#c5d9f1"),
                locations = cells_body(
                    columns = vars(IgG_S2),
                    rows = IgG_S2 >= 3)
            ) %>%
            tab_style(
                style = cell_fill(color = "#538dd5"),
                locations = cells_body(
                    columns = vars(IgG_S2),
                    rows = IgG_S2 >= 10)
            ) %>%
            tab_style(
                style = cell_fill(color = "#c5d9f1"),
                locations = cells_body(
                    columns = vars(IgG_S1),
                    rows = IgG_S1 >= 3)
            ) %>%
            tab_style(
                style = cell_fill(color = "#538dd5"),
                locations = cells_body(
                    columns = vars(IgG_S1),
                    rows = IgG_S1 >= 10)
            ) %>%
            tab_style(
                style = cell_fill(color = "#c5d9f1"),
                locations = cells_body(
                    columns = vars(IgG_empty),
                    rows = IgG_empty >= 3)
            ) %>%
            tab_style(
                style = cell_fill(color = "#538dd5"),
                locations = cells_body(
                    columns = vars(IgA_NP),
                    rows = IgA_NP >= 10)
            ) %>%
            tab_style(
                style = cell_fill(color = "#c5d9f1"),
                locations = cells_body(
                    columns = vars(IgA_S2),
                    rows = IgA_S2 >= 3)
            ) %>%
            tab_style(
                style = cell_fill(color = "#538dd5"),
                locations = cells_body(
                    columns = vars(IgA_S2),
                    rows = IgA_S2 >= 10)
            ) %>%
            tab_style(
                style = cell_fill(color = "#c5d9f1"),
                locations = cells_body(
                    columns = vars(IgA_S1),
                    rows = IgA_S1 >= 3)
            ) %>%
            tab_style(
                style = cell_fill(color = "#538dd5"),
                locations = cells_body(
                    columns = vars(IgA_S1),
                    rows = IgA_S1 >= 10)
            ) %>%
            tab_style(
                style = cell_fill(color = "#c5d9f1"),
                locations = cells_body(
                    columns = vars(IgA_empty),
                    rows = IgA_empty >= 3)
            ) %>%
            tab_style(
                style = cell_fill(color = "#538dd5"),
                locations = cells_body(
                    columns = vars(IgA_empty),
                    rows = IgA_empty >= 10)
            ) %>%
            tab_style(
                style = cell_fill(color = "#c5d9f1"),
                locations = cells_body(
                    columns = vars(IgM_NP),
                    rows = IgM_NP >= 3)
            ) %>%
            tab_style(
                style = cell_fill(color = "#538dd5"),
                locations = cells_body(
                    columns = vars(IgM_NP),
                    rows = IgM_NP >= 10)
            ) %>%
            tab_style(
                style = cell_fill(color = "#c5d9f1"),
                locations = cells_body(
                    columns = vars(IgM_S2),
                    rows = IgM_S2 >= 3)
            ) %>%
            tab_style(
                style = cell_fill(color = "#538dd5"),
                locations = cells_body(
                    columns = vars(IgM_S2),
                    rows = IgM_S2 >= 10)
            ) %>%
            tab_style(
                style = cell_fill(color = "#c5d9f1"),
                locations = cells_body(
                    columns = vars(IgM_S1),
                    rows = IgM_S1 >= 3)
            ) %>%
            tab_style(
                style = cell_fill(color = "#538dd5"),
                locations = cells_body(
                    columns = vars(IgM_S1),
                    rows = IgM_S1 >= 10)
            ) %>%
            tab_style(
                style = cell_fill(color = "#c5d9f1"),
                locations = cells_body(
                    columns = vars(IgM_empty),
                    rows = IgM_empty >= 3)
            ) %>%
            tab_style(
                style = cell_fill(color = "#538dd5"),
                locations = cells_body(
                    columns = vars(IgM_empty),
                    rows = IgM_empty >= 10)
            ) %>%
            
            tab_style(
                style = cell_fill(color = "#bfbfbf"),
                locations = cells_body(
                    columns = vars(Resultat_IgG),
                    rows = Resultat_IgG == "pos")
            ) %>%
            tab_style(
                style = cell_fill(color = "#bfbfbf"),
                locations = cells_body(
                    columns = vars(Resultat_IgA),
                    rows = Resultat_IgA == "pos")
            ) %>%
            tab_style(
                style = cell_fill(color = "#bfbfbf"),
                locations = cells_body(
                    columns = vars(Resultat_IgM),
                    rows = Resultat_IgM == "pos")
            ) %>%
            
            fmt_number(
                columns = c(ends_with("NP"),
                            ends_with("S2"),
                            ends_with("S1"),
                            ends_with("empty")),
                decimals = 1
            )
        
        net_mfi_fob_clean_gt
        
    })

    output$table <- render_gt(
        expr = table()
    )
    
    output$export <- downloadHandler(
        filename = function() {
            "output.png"
        },
        content = function(file) {
            gtsave(table(), file)
            
        }
    )
    

}



# Run the application 
shinyApp(ui = ui, server = server)

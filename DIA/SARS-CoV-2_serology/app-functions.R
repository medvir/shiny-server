
# read barcodes -----------------------------------------------------------

read_barcodes <- function(barcodes_path){
  
  barcodes_sheets <- excel_sheets(barcodes_path)
  
  # Replace with INPUT from shiny app or assume it's the first sheet
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
    arrange(as.numeric(column)) %>%
    mutate(sample_nr = 1,
           sample_nr = cumsum(sample_nr),
           Location = paste0(sample_nr, "(1,", row, column, ")")) %>%
    filter(!is.na(Sample)) %>%
    select(Location, Sample)
  
  return(barcodes)
  
}

# barcodes1 <- read_barcodes("data/200420_BARCODES.xlsx")
# barcodes2 <- read_barcodes("data/200423_plate_2_BARCODES.xlsx")



# read isotypes -----------------------------------------------------------

read_isotypes <- function(barcodes_path){
  
  # prepare a full plate to join later with isotypes
  sample_nr <- 1:96
  row <- c("A", "B", "C", "D",
           "E", "F", "G", "H")
  column <- 1:12
  
  full_plate <-
    crossing(column, row) %>%
    bind_cols(sample_nr = sample_nr) %>%
    mutate(Location = paste0(sample_nr, "(1,", row, column, ")")) %>%
    select(Location, column)
  

  # read isotypes from first row of excel document
  # 1st row on 1st sheet is read, if information is not there, it will fail
  barcodes_sheets <- excel_sheets(barcodes_path)
  
  # Replace with INPUT from shiny app or assume it's the first sheet
  barcodes_sheets_selected <- barcodes_sheets[1]
  
  barcodes_structure <-
    read_excel(barcodes_path,
               sheet = barcodes_sheets_selected,
               col_names = FALSE) %>%
    slice(c(1,3))
  
  isotype_list <- as.character(unlist(barcodes_structure[1,]))
  column_list <- as.numeric(barcodes_structure[2,])
  
  isotypes <-
    tibble(isotype = isotype_list, column = column_list) %>%
    filter(!is.na(column)) %>%
    fill(isotype) %>%
    left_join(full_plate, by = "column") %>%
    select(Location, isotype)

  
  return(isotypes)
  
}

# isotypes1 <- read_isotypes("data/200420_BARCODES.xlsx")
# isotypes2 <- read_isotypes("data/200423_plate_2_BARCODES.xlsx")


# join barcodes and isotypes ----------------------------------------------

join_barcodes_isotypes <- function(barcodes, isotypes){
  return(left_join(barcodes, isotypes, by = "Location"))
}

# barcodes_isotypes1 <- join_barcodes_isotypes(barcodes1, isotypes1)
# barcodes_isotypes2 <- join_barcodes_isotypes(barcodes2, isotypes2)


# create gt table ---------------------------------------------------------

create_gt <- function(table){
  
  net_mfi_foc_clean_gt <-
    table %>%
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
  
  return(net_mfi_foc_clean_gt)

}

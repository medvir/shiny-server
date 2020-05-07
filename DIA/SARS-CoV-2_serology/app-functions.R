
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

isotypes1 <- read_isotypes("data/200420_BARCODES.xlsx")
isotypes2 <- read_isotypes("data/200423_plate_2_BARCODES.xlsx")


# join barcodes and isotypes ----------------------------------------------

join_barcodes_isotypes <- function(barcodes, isotypes){
  return(left_join(barcodes, isotypes, by = "Location"))
}

# barcodes_isotypes1 <- join_barcodes_isotypes(barcodes1, isotypes1)
# barcodes_isotypes2 <- join_barcodes_isotypes(barcodes2, isotypes2)


# how many rows to skip ---------------------------------------------------

skip_rows <- function(filepath, match){
  count_row <-
    read_csv(filepath, skip = 46, skip_empty_rows = FALSE) %>%
    pull(2) %>%
    match(match, .)
    
  return(count_row+46-1)
}


# get count data ----------------------------------------------------------

get_count <- function(filepath, barcodes_isotypes, isotype_given=NA){
  
  barcodes_isotypes <-
    barcodes_isotypes %>%
    mutate(isotype = replace_na(isotype, isotype_given))
  
  n_samples <- length(barcodes_isotypes$Sample)

  count <-
    read_csv(filepath, skip = skip_rows(filepath, "Count"), skip_empty_rows = FALSE) %>%
    janitor::clean_names(case = "title", abbreviations = "NP") %>%
    head(n_samples) %>%
    select(-Sample, -`Total Events`) %>%
    mutate(NP = as.numeric(NP),
           S2 = as.numeric(S2),
           S1 = as.numeric(S1),
           Empty = as.numeric(Empty)) %>%
    pivot_longer(-Location, names_to = "target", values_to = "count") %>%
    left_join(barcodes_isotypes, by = "Location") %>%
    mutate(isotype_target = paste0(isotype, "_", target, "_count")) %>%
    select(-Location, -target, -isotype) %>%
    pivot_wider(names_from = isotype_target, values_from = count)
  
  return(count)
}

# igg_count1 <- get_count("data/200420_plate_1_IgG_20200420_121133.csv", barcodes_isotypes1, "IgG")
# count2 <- get_count("data/200423_plate_2_20200423_142012.csv", barcodes_isotypes2)


# get net mfi data --------------------------------------------------------

get_net_mfi <- function(filepath, barcodes_isotypes, isotype_given=NA){
  barcodes_isotypes <-
    barcodes_isotypes %>%
    mutate(isotype = replace_na(isotype, isotype_given))
  
  n_samples <- length(barcodes_isotypes$Sample)
  
  net_mfi <-
    read_csv(filepath, skip = skip_rows(filepath, "Net MFI")+1, skip_empty_rows = FALSE) %>%
    janitor::clean_names(case = "title", abbreviations = "NP") %>%
    head(n_samples) %>%
    select(-Sample, -`Total Events`) %>%
    mutate(NP = as.numeric(NP),
           S2 = as.numeric(S2),
           S1 = as.numeric(S1),
           Empty = as.numeric(Empty)) %>%
    pivot_longer(-Location, names_to = "target", values_to = "count") %>%
    left_join(barcodes_isotypes, by = "Location") %>%
    mutate(isotype_target = paste0(isotype, "_", target)) %>%
    select(-Location, -target, -isotype) %>%
    pivot_wider(names_from = isotype_target, values_from = count)
  
  net_mfi_neg <-
    net_mfi %>%
    filter(str_detect(Sample, pattern = regex("neg", ignore_case = TRUE))) %>%
    pivot_longer(-Sample, names_to = "target", values_to = "net_mfi_neg") %>%
    select(-Sample)
  
  # foc = fold over cutoff
  cutoff = 3
  
  net_mfi_foc <-
    net_mfi %>%  
    pivot_longer(-Sample, names_to = "target", values_to = "net_mfi") %>%
    left_join(net_mfi_neg, by = c("target")) %>%
    mutate(net_mfi_foc = net_mfi/(cutoff*net_mfi_neg)) %>%
    select(Sample, target, net_mfi_foc) %>%
    pivot_wider(names_from = target, values_from = net_mfi_foc)
  
  net_mfi_full <- 
    net_mfi %>%
    left_join(net_mfi_foc, by = c("Sample"), suffix = c("_net_mfi", "_net_mfi_foc"))
  
  return(net_mfi_full)
}

# igg_net_mfi1 <- get_net_mfi("data/200420_plate_1_IgG_20200420_121133.csv", barcodes_isotypes1, "IgG")
# net_mfi2 <- get_net_mfi("data/200423_plate_2_20200423_142012.csv", barcodes_isotypes2)


# set flag ----------------------------------------------------------------

set_flag <- function(net_mfi_foc, min_count, above_cutoff){
  
  net_mfi_foc_flagged <-
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
    mutate(IgG_Empty_flag = if_else(IgG_Empty_net_mfi_foc > above_cutoff, "IgG", NULL),
           IgA_Empty_flag = if_else(IgA_Empty_net_mfi_foc > above_cutoff, "IgA", NULL),
           IgM_Empty_flag = if_else(IgM_Empty_net_mfi_foc > above_cutoff, "IgM", NULL)) %>%
    unite(Fehler_empty, ends_with("empty_flag"), na.rm=TRUE, sep = ", ")
  
  return(net_mfi_foc_flagged)
}

# net_mfi2 <- left_join(net_mfi2, count2, by = c("Sample"))
# net_mfi2_flagged <- set_flag(net_mfi2, 20, 1)


# test result -------------------------------------------------------------

test_result <- function(net_mfi_foc){
  
  net_mfi_foc_result <-
    net_mfi_foc %>%
    mutate(IgG_Resultat_S1 = IgG_S1_net_mfi_foc > 1,
           IgA_Resultat_S1 = IgA_S1_net_mfi_foc > 1,
           IgM_Resultat_S1 = IgM_S1_net_mfi_foc > 1) %>%
    mutate(IgG_Resultat_S2 = IgG_S2_net_mfi_foc > 1 & (IgG_Resultat_S1 + IgA_Resultat_S1 + IgM_Resultat_S1 > 0),
           IgA_Resultat_S2 = IgA_S2_net_mfi_foc > 1 & (IgG_Resultat_S1 + IgA_Resultat_S1 + IgM_Resultat_S1 > 0),
           IgM_Resultat_S2 = IgM_S2_net_mfi_foc > 1 & (IgG_Resultat_S1 + IgA_Resultat_S1 + IgM_Resultat_S1 > 0)) %>%
    mutate(IgG_Resultat_NP = IgG_NP_net_mfi_foc > 1 & (IgG_Resultat_S1 + IgA_Resultat_S1 + IgM_Resultat_S1 > 0),
           IgA_Resultat_NP = IgA_NP_net_mfi_foc > 1 & (IgG_Resultat_S1 + IgA_Resultat_S1 + IgM_Resultat_S1 > 0),
           IgM_Resultat_NP = IgM_NP_net_mfi_foc > 1 & (IgG_Resultat_S1 + IgA_Resultat_S1 + IgM_Resultat_S1 > 0)) %>%
    mutate(IgG_Resultat = IgG_Resultat_S1 | IgG_Resultat_S2 | IgG_Resultat_NP,
           IgA_Resultat = IgA_Resultat_S1 | IgA_Resultat_S2 | IgA_Resultat_NP,
           IgM_Resultat = IgM_Resultat_S1 | IgM_Resultat_S2 | IgM_Resultat_NP) %>%
    mutate(Serokonversion = case_when(
      IgG_Resultat_S1 == 1 ~ "fs (fortgeschritten)",
      IgA_Resultat_S1 + IgM_Resultat_S1 > 0 ~ "ps (partiell)",
      IgG_Resultat_S1 + IgA_Resultat_S1 + IgM_Resultat_S1 < 3 ~ "ks (keine)"
    )) %>%
    mutate(Kommentar = case_when(
      (Serokonversion == "ks (keine)") & (IgG_S1_net_mfi_foc > 1 | IgA_S1_net_mfi_foc > 1 | IgM_S1_net_mfi_foc > 1 |
                                          IgG_S2_net_mfi_foc > 1 | IgA_S2_net_mfi_foc > 1 | IgM_S2_net_mfi_foc > 1 |
                                          IgG_NP_net_mfi_foc > 1 | IgA_NP_net_mfi_foc > 1 | IgM_NP_net_mfi_foc > 1) ~ "*sarsk (Kreuzrkt whs)",
      (Serokonversion == "ks (keine)") & (IgG_S1_net_mfi_foc >= 0.9 | IgA_S1_net_mfi_foc >= 0.9 | IgM_S1_net_mfi_foc >= 0.9) ~ "S1 Reaktivität grenzwertig, bitte Verlaufsprobe einsenden",
      TRUE ~ ""
    ))
  
  return(net_mfi_foc_result)
}

# net_mfi2_flagged_result <- test_result(net_mfi2_flagged)

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


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
    mutate_all(as.character) %>%
    pivot_longer(-row, names_to = "column", values_to = "Sample") %>%
    arrange(as.numeric(column)) %>%
    mutate(sample_nr = 1,
           sample_nr = cumsum(sample_nr),
           Location = paste0(sample_nr, "(1,", row, column, ")")) %>%
    filter(!is.na(Sample)) %>%
    select(Location, Sample)
  
  return(barcodes)
  
}

barcodes1 <- read_barcodes("data/200706_BARCODES_Ciao_4.xlsx")


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

isotypes1 <- read_isotypes("data/200706_BARCODES_Ciao_4.xlsx")


# join barcodes and isotypes ----------------------------------------------

join_barcodes_isotypes <- function(barcodes, isotypes){
  return(left_join(barcodes, isotypes, by = "Location"))
}

barcodes_isotypes1 <- join_barcodes_isotypes(barcodes1, isotypes1)


# how many rows to skip ---------------------------------------------------

skip_rows <- function(filepath, match){
  
  rows_until_data <-
    read_csv(filepath, col_names = FALSE, skip_empty_rows = FALSE) %>%
    pull(1) %>%
    match("DataType:", .) - 6 # empty rows are somehow read twice that's why 6 are subtracted
  
  count_row <-
    read_csv(filepath, skip = rows_until_data, skip_empty_rows = FALSE) %>%
    pull(2) %>%
    match(match, .)
    
  return(count_row+rows_until_data-1)
}


# get count data ----------------------------------------------------------

get_count <- function(filepath, barcodes_isotypes, isotype_given=NA){
  
  barcodes_isotypes <-
    barcodes_isotypes %>%
    mutate(isotype = replace_na(isotype, isotype_given))
  
  n_samples <- length(barcodes_isotypes$Sample)

  count <-
    read_csv(filepath, skip = skip_rows(filepath, "Count"), skip_empty_rows = FALSE) %>%
    janitor::clean_names(case = "title", abbreviations = c("NP", "RBD", "HKU1")) %>%
    head(n_samples) %>%
    select(-Sample, -`Total Events`) %>%
    mutate(NP = as.numeric(NP),
           S2 = as.numeric(S2),
           S1 = as.numeric(S1),
           Empty = as.numeric(Empty)) %>%
    mutate_at(vars(one_of("RBD")), as.numeric) %>%
    mutate_at(vars(one_of("HKU1")), as.numeric) %>%
    pivot_longer(-Location, names_to = "target", values_to = "count") %>%
    left_join(barcodes_isotypes, by = "Location") %>%
    mutate(isotype_target = paste0(isotype, "_", target, "_count")) %>%
    select(-Location, -target, -isotype) %>%
    pivot_wider(names_from = isotype_target, values_from = count)
  
  return(count)
}

igg_count1 <- get_count("data/200706_Ciao_4_IgG_20200707_104333.csv", barcodes_isotypes1, "IgG")
iga_count1 <- get_count("data/200706_Ciao_4_IgA_20200707_141245.csv", barcodes_isotypes1, "IgA")
igm_count1 <- get_count("data/200706_Ciao_4_IgM_20200707_164242.csv", barcodes_isotypes1, "IgM")

count1 <-
  igg_count1 %>%
  left_join(iga_count1, by = c("Sample")) %>%
  left_join(igm_count1, by = c("Sample"))


# get net mfi data --------------------------------------------------------

get_net_mfi <- function(filepath, barcodes_isotypes, isotype_given=NA){
  barcodes_isotypes <-
    barcodes_isotypes %>%
    mutate(isotype = replace_na(isotype, isotype_given))
  
  n_samples <- length(barcodes_isotypes$Sample)
  
  net_mfi <-
    read_csv(filepath, skip = skip_rows(filepath, "Net MFI")+1, skip_empty_rows = FALSE) %>%
    janitor::clean_names(case = "title", abbreviations = c("NP", "RBD", "HKU1")) %>%
    head(n_samples) %>%
    select(-Sample, -`Total Events`) %>%
    mutate(NP = as.numeric(NP),
           S2 = as.numeric(S2),
           S1 = as.numeric(S1),
           Empty = as.numeric(Empty)) %>%
    mutate_at(vars(one_of("RBD")), as.numeric) %>%
    mutate_at(vars(one_of("HKU1")), as.numeric) %>%
    pivot_longer(-Location, names_to = "target", values_to = "count") %>%
    left_join(barcodes_isotypes, by = "Location") %>%
    mutate(isotype_target = paste0(isotype, "_", target)) %>%
    select(-Location, -target, -isotype) %>%
    pivot_wider(names_from = isotype_target, values_from = count)
  
  net_mfi_empty <-
    net_mfi %>%
    select(Sample, empty = ends_with("_Empty"))

  # soc = signal over cutoff
  # cutoff fold over empty beads
  # defined by: mean + some factor * standard deviation
  cutoff <-
    tribble(
      ~target, ~cutoff,
      "IgG_NP", 7.7, # mean + 6*sd
      "IgG_RBD", 3.5, # mean + 4*sd
      "IgG_S1", 2.3, # mean + 3*sd
      "IgG_S2", 62.8, # mean + 6*sd
      
      "IgA_NP", 27.7, # mean + 6*sd
      "IgA_RBD", 13.9, # mean + 6*sd
      "IgA_S1", 6.7, # mean + 6*sd
      "IgA_S2", 35.8, # mean + 6*sd
      
      "IgM_NP", 49.2, # mean + 4*sd
      "IgM_RBD", 66.8, # mean + 4*sd
      "IgM_S1", 22.9, # mean + 4*sd
      "IgM_S2", 25.2, # mean + 4*sd
    )
  
  # calculate foe (fold over empty beads)
  net_mfi_soc <-
    net_mfi %>%
    pivot_longer(-Sample, names_to = "target", values_to = "net_mfi") %>%
    left_join(net_mfi_empty, by = c("Sample")) %>%
    left_join(cutoff, by = c("target")) %>%
    mutate(net_mfi_soc = net_mfi/(cutoff*empty)) %>%
    filter(!is.na(net_mfi_soc)) %>%
    select(Sample, target, net_mfi_soc) %>%
    pivot_wider(names_from = target, values_from = net_mfi_soc)
  
  net_mfi_full <-
    net_mfi %>%
    left_join(net_mfi_soc, by = c("Sample"), suffix = c("_net_mfi", "_net_mfi_soc"))
  
  return(net_mfi_full)
}

igg_net_mfi1 <- get_net_mfi("data/200706_Ciao_4_IgG_20200707_104333.csv", barcodes_isotypes1, "IgG")
iga_net_mfi1 <- get_net_mfi("data/200706_Ciao_4_IgA_20200707_141245.csv", barcodes_isotypes1, "IgA")
igm_net_mfi1 <- get_net_mfi("data/200706_Ciao_4_IgM_20200707_164242.csv", barcodes_isotypes1, "IgM")

net_mfi1 <-
  igg_net_mfi1 %>%
  left_join(iga_net_mfi1, by = c("Sample")) %>%
  left_join(igm_net_mfi1, by = c("Sample"))


# set flag ----------------------------------------------------------------

set_flag <- function(net_mfi_soc, min_count, above_cutoff_IgG, above_cutoff_IgA, above_cutoff_IgM){
  
  if (sum(str_detect(names(net_mfi_soc), "RBD")) > 0){
    net_mfi_soc_flagged <-
      net_mfi_soc %>%
      mutate(IgG_NP_count_flag = if_else(as.numeric(IgG_NP_count) < min_count, "IgG NP", NULL),
             IgG_S2_count_flag = if_else(as.numeric(IgG_S2_count) < min_count, "IgG S2", NULL),
             IgG_S1_count_flag = if_else(as.numeric(IgG_S1_count) < min_count, "IgG S1", NULL),
             IgG_RBD_count_flag = if_else(as.numeric(IgG_RBD_count) < min_count, "IgG RBD", NULL),
             IgG_HKU1_count_flag = if_else(as.numeric(IgG_HKU1_count) < min_count, "IgG HKU1", NULL),
             IgA_NP_count_flag = if_else(as.numeric(IgA_NP_count) < min_count, "IgA NP", NULL),
             IgA_S2_count_flag = if_else(as.numeric(IgA_S2_count) < min_count, "IgA S2", NULL),
             IgA_S1_count_flag = if_else(as.numeric(IgA_S1_count) < min_count, "IgA S1", NULL),
             IgA_RBD_count_flag = if_else(as.numeric(IgA_RBD_count) < min_count, "IgA RBD", NULL),
             IgA_HKU1_count_flag = if_else(as.numeric(IgA_HKU1_count) < min_count, "IgA HKU1", NULL),
             IgM_NP_count_flag = if_else(as.numeric(IgM_NP_count) < min_count, "IgM NP", NULL),
             IgM_S2_count_flag = if_else(as.numeric(IgM_S2_count) < min_count, "IgM S2", NULL),
             IgM_S1_count_flag = if_else(as.numeric(IgM_S1_count) < min_count, "IgM S1", NULL),
             IgM_RBD_count_flag = if_else(as.numeric(IgM_RBD_count) < min_count, "IgM RBD", NULL),
             IgM_HKU1_count_flag = if_else(as.numeric(IgM_HKU1_count) < min_count, "IgM HKU1", NULL)) %>%
      unite(Fehler_count, ends_with("count_flag"), na.rm=TRUE, sep = ", ") %>%
      mutate(IgG_Empty_flag = if_else(IgG_Empty > above_cutoff_IgG, "IgG", NULL),
             IgA_Empty_flag = if_else(IgA_Empty > above_cutoff_IgA, "IgA", NULL),
             IgM_Empty_flag = if_else(IgM_Empty > above_cutoff_IgM, "IgM", NULL)) %>%
      unite(Fehler_empty, ends_with("empty_flag"), na.rm=TRUE, sep = ", ")
  } else{
    net_mfi_soc_flagged <-
      net_mfi_soc %>%
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
      mutate(IgG_Empty_flag = if_else(IgG_Empty > above_cutoff_IgG, "IgG", NULL),
             IgA_Empty_flag = if_else(IgA_Empty > above_cutoff_IgA, "IgA", NULL),
             IgM_Empty_flag = if_else(IgM_Empty > above_cutoff_IgM, "IgM", NULL)) %>%
      unite(Fehler_empty, ends_with("empty_flag"), na.rm=TRUE, sep = ", ")
  }
  
  
  
  return(net_mfi_soc_flagged)
}

net_mfi1 <- left_join(net_mfi1, count1, by = c("Sample"))
net_mfi1_flagged <- set_flag(net_mfi1, 20, 40.05, 55.26, 539.74)


# test result -------------------------------------------------------------

test_result <- function(net_mfi_soc, IgG_NP_gw, IgG_RBD_gw, IgG_S1_gw){
  
  # the result of each of the 12 parameter can either be
  # positive (1), negative (0) or intermediate (0.5)
  
  net_mfi_soc_result <-
    net_mfi_soc %>%
    mutate(IgG_Resultat_S1 = if_else(IgG_S1_net_mfi_soc > 1, 1, if_else(IgG_S1_net_mfi_soc > IgG_S1_gw, 0.5, 0)),
           IgA_Resultat_S1 = if_else(IgA_S1_net_mfi_soc > 1, 1, 0),
           IgM_Resultat_S1 = if_else(IgM_S1_net_mfi_soc > 1, 1, 0)) %>%
    mutate(IgG_Resultat_S2 = if_else(IgG_S2_net_mfi_soc > 1, 1, 0),
           IgA_Resultat_S2 = if_else(IgA_S2_net_mfi_soc > 1, 1, 0),
           IgM_Resultat_S2 = if_else(IgM_S2_net_mfi_soc > 1, 1, 0)) %>%
    mutate(IgG_Resultat_NP = if_else(IgG_NP_net_mfi_soc > 1, 1, if_else(IgG_NP_net_mfi_soc > IgG_NP_gw, 0.5, 0)),
           IgA_Resultat_NP = if_else(IgA_NP_net_mfi_soc > 1, 1, 0),
           IgM_Resultat_NP = if_else(IgM_NP_net_mfi_soc > 1, 1, 0)) %>%
    mutate(IgG_Resultat_RBD = if_else(IgG_RBD_net_mfi_soc > 1, 1, if_else(IgG_RBD_net_mfi_soc > IgG_RBD_gw, 0.5, 0)),
           IgA_Resultat_RBD = if_else(IgA_RBD_net_mfi_soc > 1, 1, 0),
           IgM_Resultat_RBD = if_else(IgM_RBD_net_mfi_soc > 1, 1, 0)) %>%
    mutate(Resultat_sum = IgG_Resultat_S1+IgA_Resultat_S1+IgM_Resultat_S1+
                          IgG_Resultat_S2+IgA_Resultat_S2+IgM_Resultat_S2+
                          IgG_Resultat_NP+IgA_Resultat_NP+IgM_Resultat_NP+
                          IgG_Resultat_RBD+IgA_Resultat_RBD+IgM_Resultat_RBD,
           Resultat_sum_IgG = IgG_Resultat_S1+IgG_Resultat_S2+IgG_Resultat_NP+IgG_Resultat_RBD,
           Resultat_n_pos = (IgG_Resultat_S1==1)+(IgA_Resultat_S1==1)+(IgM_Resultat_S1==1)+
                            (IgG_Resultat_S2==1)+(IgA_Resultat_S2==1)+(IgM_Resultat_S2==1)+
                            (IgG_Resultat_NP==1)+(IgA_Resultat_NP==1)+(IgM_Resultat_NP==1)+
                            (IgG_Resultat_RBD==1)+(IgA_Resultat_RBD==1)+(IgM_Resultat_RBD==1)) %>%
    mutate(Serokonversion = case_when(
      (Resultat_sum >= 2) & (Resultat_sum_IgG >= 0.5) ~ "Positiv, fortgeschritten",
      (Resultat_sum >= 2) & (Resultat_sum_IgG == 0) ~ "Positiv, partiell",
      (IgG_Resultat_S1 == 1 | IgG_Resultat_NP == 1 | IgG_Resultat_RBD == 1) | (Resultat_sum_IgG >= 0.5 & Resultat_n_pos == 1) ~ "Schwach reaktiv",
      Resultat_n_pos == 1 ~ "Indeterminat",
      TRUE ~ "Negativ"
    ))
  
  return(net_mfi_soc_result)
}

net_mfi1_flagged_result <- test_result(net_mfi1_flagged, 0.884, 0.714, 0.895)


# create gt table ---------------------------------------------------------

create_gt <- function(table){
  
  if (sum(str_detect(names(table), "RBD")) > 0){
    net_mfi_soc_clean_gt <-
      table %>%
      gt(rowname_col = "Sample") %>%
      tab_spanner_delim(delim = "_") %>%
      
      tab_style(
        style = cell_fill(color = "#FFE74C"),
        locations = cells_body(
          columns = vars(IgG_NP),
          rows = IgG_NP >= 0.884)
      ) %>%
      tab_style(
        style = cell_fill(color = "#06AED5"),
        locations = cells_body(
          columns = vars(IgG_NP),
          rows = IgG_NP > 1)
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
          rows = IgG_S1 >= 0.895)
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
          columns = vars(IgG_RBD),
          rows = IgG_RBD >= 0.714)
      ) %>%
      tab_style(
        style = cell_fill(color = "#06AED5"),
        locations = cells_body(
          columns = vars(IgG_RBD),
          rows = IgG_RBD > 1)
      ) %>%
      
      
      tab_style(
        style = cell_fill(color = "#06AED5"),
        locations = cells_body(
          columns = vars(IgA_NP),
          rows = IgA_NP > 1)
      ) %>%
      
      tab_style(
        style = cell_fill(color = "#06AED5"),
        locations = cells_body(
          columns = vars(IgA_S2),
          rows = IgA_S2 > 1)
      ) %>%
      
      tab_style(
        style = cell_fill(color = "#06AED5"),
        locations = cells_body(
          columns = vars(IgA_S1),
          rows = IgA_S1 > 1)
      ) %>%
      
      tab_style(
        style = cell_fill(color = "#06AED5"),
        locations = cells_body(
          columns = vars(IgA_RBD),
          rows = IgA_RBD > 1)
      ) %>%
      
      
      tab_style(
        style = cell_fill(color = "#06AED5"),
        locations = cells_body(
          columns = vars(IgM_NP),
          rows = IgM_NP > 1)
      ) %>%
      
      tab_style(
        style = cell_fill(color = "#06AED5"),
        locations = cells_body(
          columns = vars(IgM_S2),
          rows = IgM_S2 > 1)
      ) %>%
      
      tab_style(
        style = cell_fill(color = "#06AED5"),
        locations = cells_body(
          columns = vars(IgM_S1),
          rows = IgM_S1 > 1)
      ) %>%
      
      tab_style(
        style = cell_fill(color = "#06AED5"),
        locations = cells_body(
          columns = vars(IgM_RBD),
          rows = IgM_RBD > 1)
      ) %>%
      
      
      fmt_number(
        columns = c(ends_with("NP"),
                    ends_with("S2"),
                    ends_with("S1"),
                    ends_with("RBD")),
        decimals = 1
      )
  } else{
    net_mfi_soc_clean_gt <-
      table %>%
      gt(rowname_col = "Sample") %>%
      tab_spanner_delim(delim = "_") %>%
      
      tab_style(
        style = cell_fill(color = "#FFE74C"),
        locations = cells_body(
          columns = vars(IgG_NP),
          rows = IgG_NP >= 0.884)
      ) %>%
      tab_style(
        style = cell_fill(color = "#06AED5"),
        locations = cells_body(
          columns = vars(IgG_NP),
          rows = IgG_NP > 1)
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
          rows = IgG_S1 >= 0.895)
      ) %>%
      tab_style(
        style = cell_fill(color = "#06AED5"),
        locations = cells_body(
          columns = vars(IgG_S1),
          rows = IgG_S1 > 1)
      ) %>%
      
      
      tab_style(
        style = cell_fill(color = "#06AED5"),
        locations = cells_body(
          columns = vars(IgA_NP),
          rows = IgA_NP > 1)
      ) %>%
      
      tab_style(
        style = cell_fill(color = "#06AED5"),
        locations = cells_body(
          columns = vars(IgA_S2),
          rows = IgA_S2 > 1)
      ) %>%
      
      tab_style(
        style = cell_fill(color = "#06AED5"),
        locations = cells_body(
          columns = vars(IgA_S1),
          rows = IgA_S1 > 1)
      ) %>%
      
      
      tab_style(
        style = cell_fill(color = "#06AED5"),
        locations = cells_body(
          columns = vars(IgM_NP),
          rows = IgM_NP > 1)
      ) %>%
      
      tab_style(
        style = cell_fill(color = "#06AED5"),
        locations = cells_body(
          columns = vars(IgM_S2),
          rows = IgM_S2 > 1)
      ) %>%
      
      tab_style(
        style = cell_fill(color = "#06AED5"),
        locations = cells_body(
          columns = vars(IgM_S1),
          rows = IgM_S1 > 1)
      ) %>%
      
      
      fmt_number(
        columns = c(ends_with("NP"),
                    ends_with("S2"),
                    ends_with("S1")),
        decimals = 1
      )
  }
  
  return(net_mfi_soc_clean_gt)

}

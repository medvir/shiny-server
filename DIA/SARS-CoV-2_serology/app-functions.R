
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

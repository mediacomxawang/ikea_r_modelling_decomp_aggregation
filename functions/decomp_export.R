# export decomp output as csv
decomp_csv <- function(decomp_output, path = NULL, brand_product_channel = NULL){
  
  if (!is.data.frame(decomp_output)) {
    decomp_df <- decomp_output@DecompedData
  } else {
    decomp_df <- decomp_output
  }
  
  output_df <- decomp_df %>% 
    dplyr::select(date, modelled_vars, contributions) %>%
    dplyr::mutate(modelled_vars = factor(modelled_vars, levels = unique(modelled_vars))) %>% 
    tidyr::spread(modelled_vars, contributions)
  
  if (!is.null(path)) {
    readr::write_csv(output_df, 
                     glue::glue('{path}/{brand_product_channel}.csv'))
  } else {
    readr::write_csv(output_df, 'decomp_output.csv')
  }
  
}

# export decomp output with the groups above column names
decomp_export <- function(decomp_output, mapping_tbl, file_name = "decomp_export_table"){
  
  output_var <- decomp_ex_var(decomp_output, mapping_tbl)
  output_group <- decomp_ex_group(decomp_output)
  output_kpi <- decomp_ex_kpi(decomp_output)
  
  output_df_final <- output_var %>% 
    left_join(output_kpi, by = "id") %>% 
    left_join(output_group, by = "id") %>% 
    mutate(empty = NA, .after = actual) %>% 
    mutate(id = NA)
  
  readr::write_csv(output_df_final,
                   col_names = FALSE,
                   na = "",
                   paste(file_name, ".csv", sep = ""))
  
  
}


decomp_ex_var <- function(decomp_output, mapping_tbl){
  
  decomp_df <-  decomp_output@DecompedData %>% 
    dplyr::mutate(modelled_vars = factor(modelled_vars, levels = unique(modelled_vars)))
  
  mapping_table <- mapping_tbl %>% 
    select(actual.vars, Group_Modeller,Group_Base_Media_Price,Group_Base_MediaBreakDown_Price,Group_BaseBreakDown_MediaBreakDown_Price)
  
  decomp_df <- decomp_df %>% 
    mutate(actual.vars = clean_modelled_vars(modelled_vars)) %>% 
    left_join(mapping_table, by = "actual.vars")
  
  output_df <- decomp_df %>% 
    dplyr::select(date, modelled_vars, contributions) %>%
    dplyr::mutate(modelled_vars = factor(modelled_vars, levels = unique(modelled_vars))) %>% 
    tidyr::spread(modelled_vars, contributions) %>% 
    dplyr::mutate(quarter = lubridate::quarter(date,with_year=TRUE,fiscal_start = 9)) %>% 
    dplyr::mutate(fy = paste0("fy_",stringr::str_sub(as.character(quarter),3,4))) %>% 
    dplyr::select(-quarter) %>% 
    dplyr::mutate(across(everything(), ~as.character(.x)))
  
  con_by_year <- decomp_df %>% 
    dplyr::mutate(quarter = lubridate::quarter(date,with_year=TRUE,fiscal_start = 9)) %>% 
    dplyr::mutate(fy = paste0("fy_",stringr::str_sub(as.character(quarter),3,4))) %>% 
    dplyr::select(fy,modelled_vars,contributions) %>% 
    dplyr::group_by(fy, modelled_vars) %>% 
    dplyr::summarise(contributions = format(round(sum(contributions)), big.mark = ",")) %>% 
    dplyr::ungroup() %>% 
    tidyr::spread(modelled_vars, contributions) %>% 
    dplyr::mutate(across(everything(), ~as.character(.x)))
  
  top_rows <- decomp_df %>% 
    dplyr::select(modelled_vars, group, Group_Base_Media_Price, contributions) %>%
    dplyr::group_by(modelled_vars, group, Group_Base_Media_Price) %>% 
    dplyr::summarise(contributions = sum(contributions)) %>% 
    dplyr::mutate(percent = (contributions/sum(decomp_df$contributions))*100) %>% 
    dplyr::mutate(percent = round(percent, 2)) %>% 
    ungroup()
  
  
  group_row <- top_rows %>% 
    dplyr::select(modelled_vars, group) %>% 
    tidyr::spread(modelled_vars, group)
  
  group_base_media_price_row <- top_rows %>% 
    dplyr::select(modelled_vars, Group_Base_Media_Price) %>% 
    tidyr::spread(modelled_vars, Group_Base_Media_Price)
  
  percent_row <- top_rows %>% 
    dplyr::select(modelled_vars, percent) %>% 
    dplyr::mutate(percent = as.character(percent)) %>%
    dplyr::mutate(percent = paste(percent, "%", sep = "")) %>% 
    tidyr::spread(modelled_vars, percent)
  
  value_row <- top_rows %>% 
    dplyr::select(modelled_vars, contributions) %>% 
    dplyr::mutate(contributions = format(round(contributions), big.mark = ",")) %>% 
    dplyr::mutate(contributions = as.character(contributions)) %>%
    tidyr::spread(modelled_vars, contributions)
  
  var_row <- data.frame(vars_name = colnames(output_df), vars = colnames(output_df)) %>% 
    tidyr::spread(vars_name, vars)
  
  top_row <- dplyr::bind_rows(con_by_year, value_row, percent_row, group_row, group_base_media_price_row, var_row) %>% 
    relocate(fy, date)
  
  output_t <- bind_rows(top_row, output_df) %>% 
    mutate(id = row_number())
  
  return(output_t)
}

decomp_ex_group <- function(decomp_output){
  
  decomp_df <-  decomp_output@DecompedData %>% 
    dplyr::mutate(modelled_vars = factor(modelled_vars, levels = unique(modelled_vars)))
  
  output_df_group <- decomp_df %>% 
    dplyr::select(date, group, contributions) %>%
    dplyr::group_by(date, group) %>% 
    dplyr::summarise(contributions = sum(contributions)) %>% 
    dplyr::ungroup() %>% 
    tidyr::spread(group, contributions) %>% 
    dplyr::mutate(quarter = lubridate::quarter(date,with_year=TRUE,fiscal_start = 9)) %>% 
    dplyr::mutate(fy = paste0("fy_",stringr::str_sub(as.character(quarter),3,4))) %>% 
    dplyr::select(-quarter) %>% 
    dplyr::mutate(across(everything(), ~as.character(.x)))
  
  con_by_year_group <- decomp_df %>% 
    dplyr::mutate(quarter = lubridate::quarter(date,with_year=TRUE,fiscal_start = 9)) %>% 
    dplyr::mutate(fy = paste0("fy_",stringr::str_sub(as.character(quarter),3,4)))%>% 
    dplyr::select(fy,group,contributions) %>% 
    dplyr::group_by(fy, group) %>% 
    dplyr::summarise(contributions = format(round(sum(contributions)), big.mark = ",")) %>% 
    dplyr::ungroup() %>% 
    tidyr::spread(group, contributions) %>% 
    dplyr::mutate(across(everything(), ~as.character(.x)))
  
  top_rows_group <- decomp_df %>% 
    dplyr::select(modelled_vars, group, contributions) %>%
    dplyr::mutate(modelled_vars = group) %>% 
    dplyr::group_by(modelled_vars, group) %>% 
    dplyr::summarise(contributions = sum(contributions)) %>% 
    dplyr::mutate(percent = (contributions/sum(decomp_df$contributions))*100) %>% 
    dplyr::mutate(percent = round(percent, 2))
  
  
  group_row_group <- top_rows_group %>% 
    dplyr::select(modelled_vars, group) %>% 
    tidyr::spread(modelled_vars, group)
  
  percent_row_group <- top_rows_group %>% 
    dplyr::select(modelled_vars, percent) %>% 
    dplyr::mutate(percent = as.character(percent)) %>%
    dplyr::mutate(percent = paste(percent, "%", sep = "")) %>% 
    tidyr::spread(modelled_vars, percent)
  
  value_row_group <- top_rows_group %>% 
    dplyr::select(modelled_vars, contributions) %>% 
    dplyr::mutate(contributions = format(round(contributions), big.mark = ",")) %>% 
    dplyr::mutate(contributions = as.character(contributions)) %>%
    tidyr::spread(modelled_vars, contributions)
  
  var_row_group <- data.frame(vars_name = colnames(output_df_group), vars = colnames(output_df_group)) %>% 
    tidyr::spread(vars_name, vars)
  
  top_row_group <- dplyr::bind_rows(con_by_year_group, value_row_group, percent_row_group, group_row_group, group_row_group, var_row_group) %>% 
    relocate(date)
  
  output_t_group <- bind_rows(top_row_group, output_df_group) %>% 
    mutate(id = row_number())
  
  return(output_t_group)
}

decomp_ex_kpi <- function(decomp_output){
 
  decomp_df <-  decomp_output@DecompedData %>% 
    dplyr::mutate(modelled_vars = factor(modelled_vars, levels = unique(modelled_vars)))
  
  output_df_kpi <- decomp_df %>% 
    select(date, contributions) %>% 
    group_by(date) %>% 
    summarise(fitted = sum(contributions)) %>% 
    ungroup() %>% 
    left_join(decomp_output@KPIData, by = "date") %>% 
    mutate(actual = exp(y)) %>% 
    select(-y) 
  
  con_by_year_kpi <- output_df_kpi %>% 
    dplyr::mutate(quarter = lubridate::quarter(date,with_year=TRUE,fiscal_start = 9)) %>% 
    dplyr::mutate(date = paste0("fy_",stringr::str_sub(as.character(quarter),3,4)))%>% 
    dplyr::select(date,fitted,actual) %>%  
    dplyr::group_by(date) %>% 
    dplyr::summarise(fitted = sum(fitted),
                     actual = sum(actual)) %>% 
    ungroup() %>% 
    janitor::adorn_totals("row") %>% 
    mutate(across(!starts_with("date"), ~format(round(.x), big.mark = ","))) %>% 
    mutate(across(everything(), ~as.character(.x))) %>% 
    add_row(date = NA, fitted = NA, actual = NA) %>% 
    add_row(date = NA, fitted = "KPI", actual = "KPI") %>% 
    add_row(date = NA, fitted = "KPI", actual = "KPI") %>% 
    add_row(date = "date", fitted = "fitted", actual = "actual")
  
  output_df_kpi <- output_df_kpi %>% 
    dplyr::mutate(across(everything(), ~as.character(.x)))
  
  output_t_kpi <- bind_rows(con_by_year_kpi, output_df_kpi) %>% 
    mutate(id = row_number())
  
  return(output_t_kpi)
}












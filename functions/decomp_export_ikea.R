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
  
  output_var <- decomp_ex_actual_var(decomp_output, mapping_tbl)
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

decomp_ex_actual_var <- function(decomp_output, mapping_tbl){
  
  decomp_df <-  decomp_output@DecompedData %>% 
    dplyr::mutate(modelled_vars = factor(modelled_vars, levels = unique(modelled_vars))) %>% 
    dplyr::mutate(quarter = lubridate::quarter(date,with_year=TRUE,fiscal_start = 9)) %>% 
    dplyr::mutate(fy = paste0("fy_",stringr::str_sub(as.character(quarter),3,4))) %>% 
    dplyr::select(-quarter)
  
  mapping_table <- mapping_tbl %>% 
    select(actual.vars, Group_Modeller,Group_Base_Media_Price,Group_Base_MediaBreakDown_Price,Group_BaseBreakDown_MediaBreakDown_Price)
  
  decomp_df <- decomp_df %>% 
    mutate(actual.vars = clean_modelled_vars(modelled_vars)) %>% 
    left_join(mapping_table, by = "actual.vars")
  
  output_df <- decomp_df %>% 
    dplyr::select(date, fy,Group_Modeller, contributions) %>%
    dplyr::mutate(Group_Modeller = factor(Group_Modeller, levels = unique(Group_Modeller))) %>% 
    dplyr::group_by(date,fy,Group_Modeller) %>% 
    dplyr::summarise(contributions = format(round(sum(contributions)), big.mark = ",")) %>% 
    tidyr::spread(Group_Modeller, contributions) %>% 
    dplyr::mutate(across(everything(), ~as.character(.x))) %>% 
    dplyr::mutate(date = as.character(date))
  
  con_by_year <- decomp_df %>% 
    dplyr::select(fy,Group_Modeller,contributions) %>% 
    dplyr::group_by(fy, Group_Modeller) %>% 
    dplyr::summarise(contributions = sum(contributions)) %>% 
    dplyr::ungroup() %>% 
    tidyr::spread(Group_Modeller, contributions) %>% 
    janitor::adorn_totals("row") %>% 
    dplyr::mutate(across(everything(), ~as.character(.x)))
  
  top_rows <- decomp_df %>% 
    dplyr::select(Group_Modeller, group, Group_Base_Media_Price, contributions) %>%
    dplyr::group_by(Group_Modeller, group, Group_Base_Media_Price) %>% 
    dplyr::summarise(contributions = sum(contributions)) %>% 
    dplyr::mutate(percent = (contributions/sum(decomp_df$contributions))*100) %>% 
    dplyr::mutate(percent = round(percent, 2)) %>% 
    ungroup()
  
  group_row <- top_rows %>% 
    dplyr::select(Group_Modeller, group) %>% 
    tidyr::spread(Group_Modeller, group)%>% 
    dplyr::mutate(fy='Group Modeller') %>% 
    dplyr::mutate(across(everything(), ~as.character(.x)))
  
  group_base_media_price_row <- top_rows %>% 
    dplyr::select(Group_Modeller, Group_Base_Media_Price) %>% 
    tidyr::spread(Group_Modeller, Group_Base_Media_Price)%>% 
    dplyr::mutate(fy='Group_Base_Media_Brand') %>% 
    dplyr::mutate(across(everything(), ~as.character(.x)))
  
  percent_row <- top_rows %>% 
    dplyr::select(Group_Modeller, percent) %>% 
    dplyr::mutate(percent = as.character(percent)) %>%
    dplyr::mutate(percent = paste(percent, "%", sep = "")) %>% 
    tidyr::spread(Group_Modeller, percent)%>% 
    dplyr::mutate(fy='% Contri') %>% 
    dplyr::mutate(across(everything(), ~as.character(.x)))
  

  var_row <- data.frame(vars_name = colnames(output_df), vars = colnames(output_df)) %>% 
    tidyr::spread(vars_name, vars)
  
  top_row <- dplyr::bind_rows(con_by_year, percent_row, group_row, group_base_media_price_row, var_row) %>% 
    relocate(fy, date)
  
  output_t <- bind_rows(top_row, output_df) %>% 
    mutate(id = row_number())
  
  return(output_t)
}



#################################### EXCEL VIEWS #####################################

#add parameter with output folder

decomp_export_output<-function(decomp_output,mapping_tbl,file_name = "decomp_export_table"){
  
  library(writexl)
  library(openxlsx)
  library(janitor)
  
  
  #VIEW1 - Weekly decomp
  decomp_df <-  decomp_output@DecompedData %>% 
    dplyr::mutate(modelled_vars = factor(modelled_vars, levels = unique(modelled_vars))) %>% 
    dplyr::mutate(quarter = lubridate::quarter(date,with_year=TRUE,fiscal_start = 9)) %>% 
    dplyr::mutate(fy = paste0("fy_",stringr::str_sub(as.character(quarter),3,4))) %>% 
    dplyr::select(-quarter)
  
  mapping_table <- mapping_tbl %>% 
    select(actual.vars, Group_Modeller,Group_Base_Media_Price,Group_Base_MediaBreakDown_Price,Group_BaseBreakDown_MediaBreakDown_Price)
  
  
  decomp_df <- decomp_df %>% 
    mutate(actual.vars = clean_modelled_vars(modelled_vars)) %>% 
    left_join(mapping_table, by = "actual.vars")
  
  
  output_df <- decomp_df %>%
    dplyr::select(date, Group_Modeller, contributions) %>%
    group_by(date,Group_Modeller)%>%
    summarise(contributions=sum(contributions))%>%
    tidyr::spread(Group_Modeller, contributions)
  
  
  
  #VIEW2 - contributions by FY and Tertials
  
  tertials<-wcsun_fy_tertial(min_date,max_date)%>%
    rename(date="week")
  
  tertial_decomp_df<-left_join(decomp_df,tertials,by="date")%>%
    mutate(fy_t=paste(fy.y,tertial))
  
  
  #Contributions by fy and Tertial
  
  con_by_year <- decomp_df %>% 
    dplyr::select(fy,Group_Modeller,contributions) %>% 
    dplyr::group_by(fy, Group_Modeller) %>% 
    dplyr::summarise(contributions = sum(contributions)) %>% 
    dplyr::ungroup() %>% 
    tidyr::spread(Group_Modeller, contributions) %>% 
    janitor::adorn_totals("row") %>% 
    dplyr::mutate(across(everything(), ~as.character(.x)))
  
  
  con_by_tertial<- tertial_decomp_df %>% 
    dplyr::select(fy_t,Group_Modeller,contributions) %>% 
    dplyr::group_by(fy_t, Group_Modeller) %>% 
    dplyr::summarise(contributions = sum(contributions)) %>% 
    dplyr::ungroup() %>% 
    tidyr::spread(Group_Modeller, contributions) %>% 
    janitor::adorn_totals("row") %>% 
    dplyr::mutate(across(everything(), ~as.character(.x)))%>%
    rename(fy="fy_t")
  
  
  tot_con_fy_t<-dplyr::bind_rows(con_by_year, con_by_tertial)
  
  
  ##################### VIEW 3  ##########################
  
  
  view3<- decomp_df%>%
    select(date,group,fy,contributions)%>%
    group_by(date,fy,group)%>%
    summarise(contributions = sum((contributions)))
  
  final_view3<- spread(view3,group,contributions)
  
  
  ############################# VIEW 4 ####################
  
  fy_view4<- view3%>%
    select(date,group,fy,contributions)%>%
    group_by(fy,group)%>%
    summarise(contributions = sum((contributions)))
  
  final_fy_view4<-fy_view4%>%tidyr::spread(group, contributions)
  
  
  
  tertial_view4_df<-left_join(view3,tertials,by="date")%>%
    mutate(fy_t=paste(fy.y,tertial))
  
  tertial_view4<- tertial_view4_df%>%
    select(date,group,fy_t,contributions)%>%
    group_by(fy_t,group)%>%
    summarise(contributions = sum((contributions)))%>%
    rename(fy="fy_t")
  
  final_tertial_view4<-tertial_view4%>%tidyr::spread(group, contributions)
  
  
  final_view4<-dplyr::bind_rows(final_fy_view4,final_tertial_view4)
  
  
  ############################## VIEW 5 ###################
  
  tot_fy<- fy_view4%>%
    select(fy,contributions)%>%
    group_by(fy)%>%
    summarise(total = sum((contributions)))%>%
    select(-fy)
  
  tot_tertial<-tertial_view4%>%
    select(fy,contributions)%>%
    group_by(fy)%>%
    summarise(total = sum((contributions)))%>%
    select(-fy)
  
  final_fy_t5<-dplyr::bind_rows(tot_fy,tot_tertial)
  
  view5<- cbind(final_view4,final_fy_t5)
  
  final_view5<-view5%>%mutate(across(everything(),.fns=~./total))
  
  
  ###################### VIEW 6 ####################
  
  fy_view6<- decomp_df%>%
    select(date,Group_Base_Media_Price,fy,contributions)%>%
    group_by(fy,Group_Base_Media_Price)%>%
    summarise(contributions = sum((contributions)))
  
  
  final_fy_view6<- spread(fy_view6,Group_Base_Media_Price,contributions)
  
  
  tertial_view6<- decomp_df%>%
    select(date,Group_Base_Media_Price,fy,contributions)
  
  tertial_view6_df<-left_join(tertial_view6,tertials,by="date")%>%
    mutate(fy_t=paste(fy.y,tertial))
  
  tertial_view6<-tertial_view6_df%>%select(fy_t,Group_Base_Media_Price,contributions)%>%
    group_by(fy_t,Group_Base_Media_Price)%>%
    summarise(contributions = sum((contributions)))%>%
    rename(fy="fy_t")
  
  
  final_tertial_view6<-spread(tertial_view6,Group_Base_Media_Price,contributions)
  
  
  final_view6<-dplyr::bind_rows(final_fy_view6,final_tertial_view6)
  
  
  ###################### VIEW 7 ####################
  
  tot_fy7<- fy_view6%>%
    select(fy,contributions)%>%
    group_by(fy)%>%
    summarise(total = sum((contributions)))%>%
    select(-fy)
  
  fy_view7<- cbind(final_fy_view6,tot_fy7)
  
  
  
  tot_tertial7<- tertial_view6%>%
    select(fy,contributions)%>%
    group_by(fy)%>%
    summarise(total = sum((contributions)))%>%
    select(-fy)
  
  tertial_view7<- cbind(final_tertial_view6,tot_tertial7)
  
  
  final_view7<-dplyr::bind_rows(fy_view7,tertial_view7)
  final_view7<-final_view7%>%mutate(across(everything(),.fns=~./total))
  
  ################## VIEW 8 - fitted vs Actual ##################
  
  #actual
  
  actual <- decomp_output@KPIData %>% 
    mutate(actual = exp(y))%>%
    select(-y)
  
  #fitted    
  fitted<- decomp_df%>%
    select(date,contributions)%>%
    group_by(date)%>%
    summarise(fitted=sum(contributions))
  
  
  actual_fitted <- left_join(actual,fitted, by = "date") %>% 
    mutate(date = as.Date(date))
  
  
  ############# EXPORT ALL VIEWS TO EXCEL ###############
  #excel file with multiple sheets
  wb<- createWorkbook()
  addWorksheet(wb,"weekly_decomp")
  writeData(wb,"weekly_decomp",output_df)
  addWorksheet(wb,"group_modeller_sum_by_FY")
  writeData(wb,"group_modeller_sum_by_FY",tot_con_fy_t)
  addWorksheet(wb,"weekly_decomp_by_group")
  writeData(wb,"weekly_decomp_by_group",final_view3)
  addWorksheet(wb,"group_sum_by_FY")
  writeData(wb,"group_sum_by_FY",final_view4)
  addWorksheet(wb,"group_sum_by_FY_percent")
  writeData(wb,"group_sum_by_FY_percent",final_view5)
  addWorksheet(wb,"Base_brand_media_by_FY")
  writeData(wb,"Base_brand_media_by_FY",final_view6)
  addWorksheet(wb,"Base_brand_media_by_FY_percent")
  writeData(wb,"Base_brand_media_by_FY_percent",final_view7)
  addWorksheet(wb,"actual_fitted_KPI")
  writeData(wb,"actual_fitted_KPI",actual_fitted)
  saveWorkbook(wb,file_name,overwrite = TRUE)
  
  
  
  return("Decomped output exported")
}

##################################################################




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












make_display_names <- function(name){
  
  name <- gsub('_', ' ', name)
  
  name <- stringr::str_to_title(name)
  
  name <- trimws(name)
  
  return(name)
  
}

add_missing_vars_to_map_tbl <- function(tbl_data_raw, tbl_map) {
  
  unmapped_vars <- tibble(actual.vars = names(tbl_data_raw)[-1]) %>% 
    left_join(tbl_map, by = 'actual.vars') %>% 
    filter(is.na(group)) %>% 
    pull(actual.vars)
  
  map_seasonality_vars <- tibble(actual.vars = grep('^payday_|^bh_|^d_|^dummy_', unmapped_vars, value = TRUE)) %>% 
    filter(!grepl("covid", actual.vars)) %>%
    mutate(Group_Base_Media_Price = 'Base',
           Group_Base_MediaBreakDown_Price = 'Base',
           Group_BaseBreakDown_MediaBreakDown_Price = 'Seasonality',
           Group_Modeller = Group_BaseBreakDown_MediaBreakDown_Price,
           Group_StackedBar = "Base",
           group = 'Base',
           ref_points = 'None',
           agg_method = 'Sum',
           variable_display = make_display_names(actual.vars),
           isforecastable = 'yes')
  
  # exception for covid dummies
  map_covid_vars <- tibble(actual.vars = grep('covid', unmapped_vars, value = TRUE)) %>% 
    mutate(Group_Base_Media_Price = 'Base',
           Group_Base_MediaBreakDown_Price = 'Base',
           Group_BaseBreakDown_MediaBreakDown_Price = 'Covid',
           Group_Modeller = 'Covid',
           Group_StackedBar = "Covid",
           group = 'Covid',
           ref_points = 'None',
           agg_method = 'Sum',
           variable_display = make_display_names(actual.vars),
           isforecastable = 'yes')
  
  map_weather_vars <- tibble(actual.vars = grep('^ww_', unmapped_vars, value = TRUE)) %>% 
    mutate(Group_Base_Media_Price = 'Base',
           Group_Base_MediaBreakDown_Price = 'Base',
           Group_BaseBreakDown_MediaBreakDown_Price = 'Weather',
           Group_Modeller = Group_BaseBreakDown_MediaBreakDown_Price,
           Group_StackedBar = "Base",
           group = 'Base',
           ref_points = 'None',
           agg_method = 'Sum',
           variable_display = make_display_names(actual.vars),
           isforecastable = 'yes')
  
  tbl_map_new <- tbl_map %>% 
    bind_rows(map_seasonality_vars, map_weather_vars, map_covid_vars)
  
  warn_missing_vars <- tibble(actual.vars = names(tbl_data_raw)[-1]) %>% 
    left_join(tbl_map_new, by = 'actual.vars') %>% 
    filter(is.na(group)) %>% 
    pull(actual.vars) 
  
  if (length(warn_missing_vars) > 0) {
    warning(glue::glue('Following variables are missing from the mapping table:
                       {paste(warn_missing_vars, collapse = "\n")}'))
  }
  
  return(tbl_map_new)
  
}



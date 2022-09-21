# get data for cico

get_data_for_cico <- function(decomp_output){
  
  decomped_data <- decomp_output@DecompedData
  
  adstock_rates <- decomped_data %>% 
    distinct(modelled_vars) %>% 
    mutate(clean_var = clean_modelled_vars(modelled_vars, keep_adstock = TRUE)) %>% 
    filter(substr(clean_var, 1, 2) == 'm_') %>% 
    mutate(value = gsub('[a-z]|_', '', clean_var),
           value = substr(value, nchar(value)-1, nchar(value)),
           value = as.numeric(value)/100,
           variable_name = clean_modelled_vars(clean_var)) %>% 
    select(variable_name, value) %>% 
    filter(!is.na(value))
  
  media_decomp <- decomped_data %>% 
    left_join(select(decomp_output@SetupData, actual.vars, modelled_vars), by = 'modelled_vars') %>% 
    filter(substr(actual.vars, 1, 2) == 'm_') %>% 
    select(date, actual.vars, contributions) %>% 
    spread(actual.vars, contributions) %>% 
    rename(Date = date)
  
  date_mapping <- decomped_data %>% 
    select(date) %>% 
    distinct() %>% 
    rename(start_date = date) %>% 
    mutate(end_date = start_date,
           #period = paste("Week", row_number(), sep = " "), .before = start_date
           period = paste('wc', start_date), .before = start_date
           ) %>% 
    slice(-1)
  
  return(list(media_decomp = media_decomp,
              adstock_rates = adstock_rates,
              date_mapping = date_mapping))
  
}

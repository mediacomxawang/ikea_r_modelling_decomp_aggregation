get_sales_decomp <- function(decomp_output, tbl_q2s, dep_var_name) {
  
  decomp_output_sales <- decomp_output
  
  decomp_output_data <- decomp_output@DecompedData
  
  # get brand product channel info from dep var name (for q2s calcs)
  dep_var_name <- gsub('^(q|s)_|_(q|s)$', '', dep_var_name)
  
  tbl_q2s <- tbl_q2s %>% 
    filter(brand_product_ch == dep_var_name) %>% 
    filter(!is.na(value))
  
  decomp_output_sales_data <- decomp_output_data %>% 
    left_join(tbl_q2s, by = "date") %>% 
    mutate(contributions = contributions * value,
           date = as.Date(date)) %>% 
    select(-brand_product_ch, value)
  
  decomp_output_sales@DecompedData <- decomp_output_sales_data
  
  decomp_output_sales@DecompedDataList <- list()
  
  decomp_output_sales@DecompGraph <- list()
  
  decomp_output_sales@DecompGroupProps <- tibble()
  
  return(decomp_output_sales)
  
}




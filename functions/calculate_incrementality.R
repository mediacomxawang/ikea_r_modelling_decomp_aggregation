


#' calculate_incrementality
#'
#' @param decomped_data <data.frame> long-format contributions from the decomp@DecompedData output
#' @param raw_data <data.frame> raw data in wide format
#' @param var_pattern <string> pattern to match in datasets - i.e for affiliates m_.*_aff_
#' @param time_period <string> if by_year, will calculate the incrementality by year, if 'total' then will calculate over the entire time period
#' @param metric_pattern <string> the pattern which designates the volume in the raw dataset which we wnat to check - i.e for tracked sales '_ts$'
#'
#' @return <data.frame> containing the total modelled and raw volumes and incrementality
#' @export
#'
#' @examples
#' 
#' calculate_incrementality(decomp_with_cico@DecompedData, tbl_model, 'm_.*aff_', time_period = 'by_year', metric_pattern = '_ts')
#' 
calculate_incrementality <- function(decomped_data, raw_data, var_pattern = NULL, time_period = 'by_year', metric_pattern = '_ts'){
  
  # Filter the decomp data to get the specific media variable we want to check the incrementality of
  decomped_data <- decomped_data %>% 
    dplyr::filter(grepl(var_pattern, modelled_vars))
    
  # Check there are actually variables which match the pattern
  if(nrow(decomped_data) == 0){
    stop(glue::glue('There are no variables in the model which match {var_pattern}'))
  }
  
  # Check the time period has been specified correctly
  if(!any(time_period %in% c('by_year', 'total'))){
    stop(glue::glue("Invalid time period of {time_period}.\nPlease specify one of 'by_year' or 'total'"))
  }
  
  # Clean the variable to remove adstocks, atan etc
  decomped_data <- decomped_data %>% 
    dplyr::mutate(modelled_vars = clean_modelled_vars(modelled_vars),
                  modelled_vars = gsub('_[a-z]+$', metric_pattern, modelled_vars))
  
  # Find the distinct variables which match the pattern
  cleaned_modelled_var_name <- unique(decomped_data$modelled_vars)
  
  # Filter the raw data for the variable we are checking the incrementality for
  raw_data <- raw_data %>% 
    dplyr::select(all_of(c('date', cleaned_modelled_var_name))) %>% 
    tidyr::gather('var_name', 'raw_values', -date)
  
  # Join the 2 datasets together so we can calculate the differences
  combined_data <- dplyr::left_join(
    decomped_data,
    raw_data,
    by = c('date', 'modelled_vars' = 'var_name')
  )
  
  if(time_period == 'by_year'){
    
    combined_data <- combined_data %>% 
      dplyr::mutate(date = lubridate::year(date)) %>% 
      dplyr::group_by(date, modelled_vars) %>% 
      dplyr::summarise(modelled_contributions = sum(contributions, na.rm = TRUE),
                raw_values = sum(raw_values, na.rm = TRUE)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(incrementality = scales::percent(modelled_contributions / raw_values))
  }
  
  else if(time_period == 'total'){
    
    min_date_with_data <- combined_data %>% 
      filter(raw_values > 0) %>% 
      pull(date) %>% 
      min()
    
    combined_data <- combined_data %>% 
      filter(date >= min_date_with_data) %>% 
      dplyr::group_by(modelled_vars) %>% 
      dplyr::summarise(modelled_contributions = sum(contributions, na.rm = TRUE),
                       raw_values = sum(raw_values, na.rm = TRUE)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(incrementality = scales::percent(modelled_contributions / raw_values))
  }
  
  combined_data
  
}


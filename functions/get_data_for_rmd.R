get_data_for_rmd <- function(update_folder_name, dep_var_name, model, model_data, 
                             decomp_output, decomp_output_with_cico, 
                             tbl_media_spend, tbl_cico, tbl_map) {
  
  # save only relevant records from tbl_map
  tbl_map <- tbl_map %>% 
    filter(actual.vars %in% clean_modelled_vars(unique(decomp_output@DecompedData$modelled_vars)))
  
  # save only relevant records from tbl_media_spend
  spend_vars <- distinct(decomp_output@DecompedData, modelled_vars) %>% 
    mutate(modelled_vars = clean_modelled_vars(modelled_vars)) %>% 
    filter(substr(modelled_vars, 1, 2) == "m_") %>% 
    mutate(spend_var = gsub("_[a-z]+$", "", modelled_vars)) %>% 
    pull(spend_var)
  
  tbl_media_spend <- tbl_media_spend %>% 
    mutate(variable = gsub("_sp", "", variable)) %>% 
    filter(variable %in% spend_vars)
  
  # select only the data needed
  model_data <- model_data %>% 
    select(c("date", clean_modelled_vars(unique(decomp_output@DecompedData$modelled_vars))[-1]))
  
  model_info <- list(dep_var_name = dep_var_name,
                     model = model,
                     model_data = model_data,
                     decomp_output = decomp_output,
                     decomp_output_with_cico = decomp_output_with_cico,
                     tbl_media_spend = tbl_media_spend,
                     #tbl_q2s = tbl_q2s,
                     tbl_cico = tbl_cico,
                     tbl_map = tbl_map)
  
  if (!dir.exists(glue::glue('updates/{update_folder_name}/outputs/model rds'))){
    dir.create(glue::glue('updates/{update_folder_name}/outputs/model rds'))
  }
  
  write_rds(model_info, glue::glue('updates/{update_folder_name}/outputs/model rds/{dep_var_name}_info.rds'))
  
}

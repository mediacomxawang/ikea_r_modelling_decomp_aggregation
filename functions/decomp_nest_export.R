decomp_nest_export <- function(main_model, nest_model, path_to_mapping_tbl){
  
  # decomp main model
  decomp_input <- get_data_for_decomp(main_model, path_to_mapping_tbl)
  
  main_model_decomp <- LogLinearDecomper(decomp_input$CoeffsData, 
                                         decomp_input$ModelData.wide, 
                                         decomp_input$VarSetup,
                                         decomp_input$kpiData)
  
  # decomp nested model
  decomp_input <- get_data_for_decomp(nest_model, path_to_mapping_tbl)
  
  nest_model_decomp <- LogLinearDecomper(decomp_input$CoeffsData, 
                                         decomp_input$ModelData.wide, 
                                         decomp_input$VarSetup,
                                         decomp_input$kpiData)
  
  # format main model
  main_model_format <- decomp_nest_format(main_model_decomp, deparse(substitute(main_model)))
  
  # retrieve contributions variable 
  nest_var <- main_model_decomp@DecompedData %>% 
    select(date, modelled_vars, contributions) %>%
    spread(modelled_vars, contributions) %>% 
    select(date, nest_model$terms[[2]][[2]])
  
  # format nested model 
  nest_model_format <- decomp_nest_format(nest_model_decomp, deparse(substitute(nest_model)), "nest", pull(nest_var[2]))
  
  # combine main model and nested model decomp formatted dataframes
  output <- main_model_format %>% 
    left_join(nest_model_format, by = "date")
  
  # export the output in csv
  utils::write.table(output, 
                     file = "decomp_nest_output_table.csv", 
                     quote = FALSE,
                     na = "",
                     row.names = FALSE,
                     col.names = FALSE,
                     sep=", "
  )
  
  return(output)
  
  
}

decomp_nest_format <- function(decomp_output, model_name = "no_name", model = "main", nest_var = NULL){
  
  decomp_df <-  decomp_output@DecompedData
  
  output_df <- decomp_df %>% 
    select(date, modelled_vars, contributions) %>%
    mutate(modelled_vars = factor(modelled_vars, levels = unique(modelled_vars))) %>% 
    spread(modelled_vars, contributions)
  
  if(model == "main"){
    
    output_df <- output_df %>% 
      mutate(year = format(date, "%Y"), .before = date)
  }
  else if(model == "nest"){
    
    output_df <- output_df %>% 
      mutate(across(!starts_with("date"), ~.x/rowSums(output_df[-1]))) %>% 
      mutate(across(!starts_with("date"), ~.x*nest_var))
    
  }
  
  output_df <- output_df %>% 
    mutate(across(everything(), ~as.character(.x)))
  
  top_names <- decomp_df %>% 
    select(modelled_vars, group) %>% 
    mutate(modelled_vars = factor(modelled_vars, levels = unique(modelled_vars))) %>% 
    group_by(modelled_vars, group) %>% 
    summarise() %>%
    ungroup() %>% 
    mutate(vars = modelled_vars) %>% 
    mutate(model = model_name, .before = group)
  
  top_names_t = setNames(data.frame(t(top_names[,-1])), top_names[,1])
  names(top_names_t) <- top_names$modelled_vars
  
  
  output_t <- bind_rows(output_df, top_names_t)
  
  r_end <- nrow(output_t)
  r_start <- r_end-nrow(top_names_t)+1
  
  output_t <- bind_rows(output_t[r_start:r_end, ], output_t[-c(r_start:r_end), ]) %>% 
    mutate(date = if_else(is.na(date), as.character(row_number()), date))
  
  return(output_t)
  
}
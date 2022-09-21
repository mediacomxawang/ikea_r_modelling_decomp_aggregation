
decomp_nest_multi <- function(main_model, nest_models, nest_model_names, path_to_mapping_tbl, sep_media = FALSE){
  
  # retrieves the name of main model object
  main_model_name <- deparse(substitute(main_model))
  
  # decomp the main model
  decomp_input <- get_data_for_decomp(main_model, path_to_mapping_tbl)
  
  main_model_decomp <- LogLinearDecomper(decomp_input$CoeffsData, 
                                         decomp_input$ModelData.wide, 
                                         decomp_input$VarSetup,
                                         decomp_input$kpiData)
  
  # creates a column for model name 
  main_decomp_df <-  main_model_decomp@DecompedData %>% 
    mutate(model = main_model_name, .before = contributions) 
  
  # decomp and appened the nested models to the output dataframe, replacing the variables there were modelled
  output_df <- main_decomp_df
  for(i in seq_along(nest_models)){
    
    # selects the variable in the main model that the nested model is modelling
    var_nested <- deparse(nest_models[[i]]$terms[[2]][[2]]) 
    
    # decomp nested model and redistribute its contribution in the main model across its regressors 
    nest_decomp_df <- decomp_nest_only(main_model_decomp, nest_models[[i]], nest_model_names[i], var_nested, path_to_mapping_tbl)
    
    # remove the variable that was modelled by nested model
    output_df <-  output_df %>% 
      filter(modelled_vars != var_nested)
    
    output_df <- bind_rows(output_df, nest_decomp_df)
    
  }
  
  if(sep_media){
    
    output_df <- output_df %>% 
      mutate(group = if_else(group == "media", paste("media", model, sep = "_"), group))
  } 
  
  return(output_df)
  
  
}

decomp_nest_only <- function(main_model_decomp, nest_model, nest_model_name, var_nested, path_to_mapping_tbl){
  
  # decomp the nested model
  decomp_input <- get_data_for_decomp(nest_model, path_to_mapping_tbl)
  
  nest_model_decomp <- LogLinearDecomper(decomp_input$CoeffsData, 
                                         decomp_input$ModelData.wide, 
                                         decomp_input$VarSetup,
                                         decomp_input$kpiData)
  
  # get the contribution of the nested from the main model
  nest_var <- main_model_decomp@DecompedData %>% 
    filter(modelled_vars == var_nested) %>% 
    select(date, contributions) %>% 
    rename("nest_con" = contributions)
  
  # redistribute the contribution of nested variable across its regressors 
  nest_decomp_df <-  nest_model_decomp@DecompedData %>% 
    mutate(model = nest_model_name, .before = contributions) %>% 
    left_join(nest_var, by = "date") %>% 
    group_by(date) %>% 
    mutate(total = sum(contributions)) %>% 
    ungroup() %>% 
    mutate(contributions = (contributions/total)*nest_con) %>% 
    select(-total, -nest_con)
  
  return(nest_decomp_df)
  
}

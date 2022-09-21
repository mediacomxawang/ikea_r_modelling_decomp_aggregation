decomp_nest <- function(main_model, nest_model, path_to_mapping_tbl, sep_media = FALSE){
  
  main_model_name <- deparse(substitute(main_model))
  nest_model_name <- deparse(substitute(nest_model))
  var_nested <- deparse(nest_model$terms[[2]][[2]])
  
  decomp_input <- get_data_for_decomp(main_model, path_to_mapping_tbl)
  
  main_model_decomp <- LogLinearDecomper(decomp_input$CoeffsData, 
                                         decomp_input$ModelData.wide, 
                                         decomp_input$VarSetup,
                                         decomp_input$kpiData)
  
  
  decomp_input <- get_data_for_decomp(nest_model, path_to_mapping_tbl)
  
  nest_model_decomp <- LogLinearDecomper(decomp_input$CoeffsData, 
                                         decomp_input$ModelData.wide, 
                                         decomp_input$VarSetup,
                                         decomp_input$kpiData)
  
  
  main_decomp_df <-  main_model_decomp@DecompedData %>% 
    mutate(model = main_model_name, .before = contributions) %>% 
    filter(modelled_vars != var_nested) 
  
  nest_var <- main_model_decomp@DecompedData %>% 
    filter(modelled_vars == var_nested) %>% 
    select(date, contributions) %>% 
    rename("nest_con" = contributions)
  
  
  nest_decomp_df <-  nest_model_decomp@DecompedData %>% 
    mutate(model = nest_model_name, .before = contributions) %>% 
    left_join(nest_var, by = "date") %>% 
    group_by(date) %>% 
    mutate(total = sum(contributions)) %>% 
    ungroup() %>% 
    mutate(contributions = (contributions/total)*nest_con) %>% 
    select(-total, -nest_con)
  
  output_df <- bind_rows(main_decomp_df, nest_decomp_df)
  
  if(sep_media){
    
    output_df <- output_df %>% 
      mutate(group = if_else(group == "media", paste("media", model, sep = "_"), group))
  } 
  
  return(output_df)
  
  
}
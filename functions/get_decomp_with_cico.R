get_decomp_with_cico <- function(decomp_output, decomp_input, tbl_cico, tbl_map) {
  
  decomp_with_cico_data <- decomp_group_by_custom_dates(decomp_output, tbl_cico) %>% 
    decomp_group_by_grouping_column(tbl_map, "modelled_vars") %>% 
    left_join(distinct(decomp_input$VarSetup, modelled_vars, group, decomp_group), by = "modelled_vars")
  
  decomp_with_cico <- decomp_output
  
  decomp_with_cico@DecompedData <- decomp_with_cico_data
  
  return(decomp_with_cico)
  
}
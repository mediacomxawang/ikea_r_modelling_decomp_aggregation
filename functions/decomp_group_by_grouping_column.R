decomp_group_by_grouping_column <- function(decomp_output, mapping_tbl, grouping_column) {
  
  if (class(decomp_output)[1] == "LogLinearDecompCalculator") {
    decomp_output <- decomp_output@DecompedData
  }
  
  decomp_df <- decomp_output %>%
    mutate(actual.vars = clean_modelled_vars(modelled_vars)) %>% 
    left_join(mapping_tbl, by = "actual.vars") %>% 
    group_by_at(c("date", grouping_column)) %>% 
    summarise(contributions = sum(contributions)) %>% 
    ungroup()
  
}

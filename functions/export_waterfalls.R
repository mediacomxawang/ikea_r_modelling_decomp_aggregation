export_waterfalls <- function(decomp_output, mapping_tbl, export_to) {
  
  roll_by <- c("rolling52wks", "rolling26wks")
  grouping_column <- c("Group_Base_Media_Price", "Group_Base_MediaBreakDown_Price")
  
  decomp_by_period <- map(roll_by, function(roll_by){
    decomp_group_by_custom_dates(decomp_output, roll_by = roll_by)
  })
  
  sheet_names <- do.call(paste, expand.grid(gsub("Group_", "", grouping_column), gsub("rolling", "", roll_by)))
  
  decomp_by_period_group <- map(decomp_by_period, function(x){
    map(grouping_column, function(grouping_column) {decomp_group_by_grouping_column(x, mapping_tbl, grouping_column)})
  }) %>% purrr::flatten()
  
  a <- map2(decomp_by_period_group, rep(roll_by, each = length(grouping_column)), function(decomp_df, roll_by) {
    
    period_lookup <- distinct(decomp_df, date) %>% 
      mutate(recency = n():1)
    
    period_clean_name <- if (roll_by == "rolling52wks") {
      "52w"
    } else if (roll_by == "rolling26wks") {
      "26w"
    } else {roll_by}
    
    decomp_df <- decomp_df %>% 
      left_join(period_lookup, by = 'date') %>% 
      filter(recency < 3)
    
    start_point <- decomp_df %>% filter(recency == 2) %>% summarise(sum(contributions)) %>% pull()
    start_point_name <- period_lookup %>% filter(recency == 2) %>% pull(date)
    start_point_name <- gsub("^(.*?)to\\s", glue::glue("{period_clean_name} to "), start_point_name)
    
    end_point <- decomp_df %>% filter(recency == 1) %>% summarise(sum(contributions)) %>% pull() 
    end_point_name <- period_lookup %>% filter(recency == 1) %>% pull(date)
    end_point_name <- gsub("^(.*?)to\\s", glue::glue("{period_clean_name} to "), end_point_name)
    
    # decomp_df <- decomp_df %>% 
    #   filter(recency == 1) %>% 
    #   dplyr::arrange(desc(contributions))
    
    colnames(decomp_df)[2] <- "group"
    
    # calculate percentage change of each group
    decomp_df <- decomp_df %>% 
      mutate(recency = if_else(recency == 1, "new", "old")) %>% 
      select(-date) %>% 
      spread(recency, contributions) %>% 
      mutate(contributions = new-old, .after = group) %>% 
      dplyr::arrange(desc(contributions)) %>% 
      mutate(text = signif(contributions, 1)) %>%
      mutate(text = if_else(abs(text) >= 0, paste(text/1000, "k", sep = ""), as.character(text)))
    
    x <- c(start_point_name, decomp_df$group, end_point_name) 
    measure <- c("relative", rep("relative", length(decomp_df$group)), "total") 
    y <- c(start_point, decomp_df$contributions, end_point)
    start_point_text <- if_else(abs(signif(start_point, 2)) >= 1000, 
                                paste(signif(start_point, 2)/1000, "k", sep = ""), 
                                as.character(signif(start_point, 2)))
    end_point_text <- if_else(abs(signif(end_point, 2)) >= 1000, 
                              paste(signif(end_point, 2)/1000, "k", sep = ""), 
                              as.character(signif(end_point, 2)))
    text <- c(start_point_text, decomp_df$text, end_point_text)
    data <- data.frame(x=factor(x,levels=x),measure,text,y)
    
  })
  
  list_of_datasets <- a
  names(list_of_datasets) <- sheet_names
  openxlsx::write.xlsx(list_of_datasets, file = export_to)
  
  
}


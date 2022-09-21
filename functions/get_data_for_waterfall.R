get_data_for_waterfall <- function(decomp_df, roll_by) {
  
  period_lookup <- distinct(decomp_df, date) %>% 
    mutate(recency = 1:n())
  
  period_clean_name <- if (roll_by == "rolling52wks") {
    "52 weeks"
  } else if (roll_by == "rolling26wks") {
    "26 weeks"
  } else {roll_by}
  
  decomp_df <- decomp_df %>% 
    left_join(period_lookup, by = 'date') %>% 
    filter(recency < 3)
  
  start_point <- decomp_df %>% filter(recency == 2) %>% summarise(sum(contributions)) %>% pull()
  # start_point_name <- period_lookup %>% filter(recency == 2) %>% pull(date)
  # start_point_name <- gsub("^(.*?)to\\s", glue::glue("{period_clean_name} to "), start_point_name)
  start_point_name <- paste("Previous", period_clean_name)
  
  end_point <- decomp_df %>% filter(recency == 1) %>% summarise(sum(contributions)) %>% pull() 
  # end_point_name <- period_lookup %>% filter(recency == 1) %>% pull(date)
  # end_point_name <- gsub("^(.*?)to\\s", glue::glue("{period_clean_name} to "), end_point_name)
  end_point_name <- paste("Last", period_clean_name)
  
  colnames(decomp_df)[2] <- "group"
  
  # calculate percentage change of each group
  decomp_df <- decomp_df %>% 
    mutate(recency = if_else(recency == 1, "new", "old")) %>% 
    select(-date) %>% 
    spread(recency, contributions) %>% 
    mutate(contributions = new-old, .after = group) %>% 
    dplyr::arrange(desc(contributions)) %>% 
    mutate(text = round(contributions)) %>%
    mutate(text = if_else(abs(text) >= 0, paste(round(text/1000, 1), "k", sep = ""), as.character(text)))
  
  x <- c(start_point_name, decomp_df$group, end_point_name) 
  measure <- c("relative", rep("relative", length(decomp_df$group)), "total") 
  y <- c(start_point, decomp_df$contributions, end_point)
  start_point_text <- if_else(abs(round(start_point)) >= 1000, 
                              paste(round(start_point/1000), "k", sep = ""), 
                              as.character(round(start_point)))
  end_point_text <- if_else(abs(round(end_point)) >= 1000, 
                            paste(round(end_point/1000), "k", sep = ""), 
                            as.character(round(end_point)))
  text <- c(start_point_text, decomp_df$text, end_point_text)
  data <- data.frame(x=factor(x,levels=x),measure,text,y)
  
  return(data)
  
}
decomp_group_by_custom_dates <- function(decomp_output, tbl_cico = NULL, roll_by = "original", FY_start = 4) {
  
  if (!is.data.frame(decomp_output)) {
    decomp_output <- decomp_output@DecompedData
  }
  
  if (!is.null(tbl_cico)) {
    # adjusts everything to cico values
    # needs cico output
    
    # replace adstocked media vars with their in-period CiCo values
    cico_vars <- tbl_cico %>% distinct(Variable) %>% pull()
    
    cico <- tbl_cico %>% 
      mutate(date = gsub('wc ', '', period),
             date = as.Date(date)) %>% 
      group_by(date, Variable) %>% 
      summarise(cico_contr = sum(final_total)) %>%
      ungroup()
    
    cico_dates <- cico %>% distinct(date) %>% pull()
    
    decomp_output <- decomp_output %>% 
      mutate(clean_variable = clean_modelled_vars(modelled_vars)) %>% 
      left_join(cico, by = c('date' = 'date', 'clean_variable' = 'Variable')) %>% 
      # cico will drop first datestamp 
      filter(date %in% cico_dates) %>% # filter to match cico dates
      mutate(contributions = ifelse(is.na(cico_contr), contributions, cico_contr))
  }
  
  decomp_df <- decomp_output
    
  if (roll_by == "week") {
    
    #decomp_df <- decomp_df %>% 
      #mutate(date = wc_mon(date))
    
  } else if (roll_by == "month") {
    
    decomp_df <- decomp_df %>% 
      mutate(date = format(date, "%Y-%m"),
             date = as.Date(paste0(date, "-01")))
    
  } else if (roll_by == "year") {
    
    decomp_df <- decomp_df %>% 
      mutate(date = format(date, "%Y")) 
    
  } else if (roll_by == "FY") {
    
    decomp_df <- decomp_df %>% 
      mutate(date = as.integer(zoo::as.yearmon(date) - (FY_start-1)/12 + 1),
             date = paste0("FY ", date))
    
  } else if (roll_by == "HY") {
    
    decomp_df <- decomp_df %>% 
      mutate(date = paste0(lubridate::year(date), " H", ceiling(as.integer(lubridate::month(date))/12 + 0.5))) 
    
  } else if (roll_by == "rolling52wks" | roll_by == "rolling26wks") {
    
    no_of_weeks <- readr::parse_number(roll_by)
    
    decomp_df <- decomp_df %>% 
      #mutate(date =wc_mon(date)) %>% 
      group_by(date, modelled_vars) %>% 
      summarise(contributions = sum(contributions)) %>% 
      ungroup() %>% 
      spread(modelled_vars, contributions) %>% 
      dplyr::arrange(desc(date))
    
    no_of_periods <- round(nrow(decomp_df)/no_of_weeks)+1
    rollingXwk_periods <- rep(1:no_of_periods, times=1, each=no_of_weeks)[1:nrow(decomp_df)]
    
    decomp_df <- decomp_df %>% 
      mutate(rollingXwk_periods = rollingXwk_periods) %>%
      filter(rollingXwk_periods <= 2) %>% 
      mutate(date = case_when(rollingXwk_periods == 1 ~ paste("Last", no_of_weeks, "weeks"),
                              rollingXwk_periods == 2 ~ paste("Previous", no_of_weeks, "weeks"))) %>% 
      # group_by(rollingXwk_periods) %>% 
      # mutate(date = paste0(no_of_weeks, "wks to ", max(date))) %>% 
      # ungroup() %>% 
      gather(modelled_vars, contributions, -date, -rollingXwk_periods)
    
  } else {
    decomp_df <- decomp_df
  }
  
  decomp_df <- decomp_df %>% 
      group_by(date, modelled_vars) %>% 
      summarise(contributions = sum(contributions)) %>% 
      ungroup()
  
  return(decomp_df)
  
}

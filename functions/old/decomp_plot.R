
decomp_plot <- function(decomp_output, roll_by = "original", type = "raw", FY_start = 4, gridlines = FALSE) {
  
  decomp_df <- plot_decomp_raw(decomp_output, roll_by, FY_start, gridlines)
  
  if(roll_by == "rolling52wks" | roll_by == "year" | roll_by == "FY"){
    if(type == "percent"){
      
      decomp_df <- decomp_df %>% 
        group_by(date) %>% 
        mutate(total = sum(contributions)) %>% 
        ungroup() %>% 
        mutate(contributions = round((contributions/total)*100, 2))
      
      d <- plot_ly(decomp_df, x = ~date, y = ~contributions, type = 'bar', 
                   name = ~group, color = ~group,
                   text = ~paste(format(contributions, nsmall = 2), "%", sep = ""), 
                   textfont = list(color = 'black', size = 24)) %>% 
        layout(yaxis = list(title = 'Contribution', ticksuffix="%"), barmode = 'relative')
      
    } else{
      
      d <- plot_ly(decomp_df, x = ~date, y = ~contributions, type = 'bar',
                   name = ~group, color = ~group) %>%
        layout(yaxis = list(title = 'Contribution'), barmode = 'relative')
    }
    
  }
  else{
    
    d <- dygraphs::dygraph(decomp_df, main = "Contributions") %>%
      dyStackedBarGroup(rev(names(decomp_df))) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"), drawGrid = gridlines)
    
  }
  return(d)
}


plot_decomp_raw <- function(decomp_output, roll_by = "original", FY_start = 4, gridlines = FALSE) {
  
  wc_mon <- function(datevar) {
    
    dow <- as.POSIXlt(datevar)$wday -1
    dow[dow== -1] <- 6
    wcm <- datevar - dow
    wcm
    
  }
  
  if (roll_by == "week") {
    
    decomp_df <- decomp_output@DecompedData %>% 
      mutate(date = wc_mon(date))
    
  } else if (roll_by == "month") {
    
    decomp_df <- decomp_output@DecompedData %>% 
      mutate(date = format(date, "%Y-%m"),
             date = as.Date(paste0(date, "-01")))
    
  } else if (roll_by == "year") {
    
    decomp_df <- decomp_output@DecompedData %>% 
      mutate(date = format(date, "%Y")) 
    
  } else if (roll_by == "FY") {
    
    decomp_df <- decomp_output@DecompedData %>% 
      mutate(date = as.integer(zoo::as.yearmon(date) - (FY_start-1)/12 + 1),
             date = paste0("FY ", date))
    
  } else if (roll_by == "rolling52wks") {
    
    decomp_df <- decomp_output@DecompedData %>% 
      mutate(date = wc_mon(date)) %>% 
      group_by(date, group) %>% 
      summarise(contributions = sum(contributions)) %>% 
      ungroup() %>% 
      spread(group, contributions) %>% 
      dplyr::arrange(desc(date))
    
    no_of_years <- round(nrow(decomp_df)/52)+1
    rolling52wk_periods <- rep(1:no_of_years, times=1, each=52)[1:nrow(decomp_df)]
    
    decomp_df <- decomp_df %>% 
      mutate(rolling52wk_periods = rolling52wk_periods) %>%
      group_by(rolling52wk_periods) %>% 
      mutate(date = paste0(min(date), " to ", max(date))) %>% 
      ungroup() %>% 
      gather(group, contributions, -date, -rolling52wk_periods)
    
  } else {
    decomp_df <- decomp_output@DecompedData
  }
  
  if (roll_by == "rolling52wks" | roll_by == "year" | roll_by == "FY") {
    
    decomp_df <- decomp_df %>% 
      group_by(date, group) %>% 
      summarise(contributions = sum(contributions)) %>% 
      ungroup()
    
  } else {
    
    decomp_df <- decomp_df %>% 
      group_by(date, group) %>% 
      summarise(contributions = sum(contributions)) %>% 
      ungroup() %>% 
      spread(group, contributions)
    
    # convert them to time series object
    decomp_df <- xts::xts(decomp_df[-1], order.by = decomp_df$date) 
    
  }
  
  return(decomp_df)
  
}

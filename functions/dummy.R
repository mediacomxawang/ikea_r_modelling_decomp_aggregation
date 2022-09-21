

dummy_weekly_data <- function(df, dummy_name , start_date, end_date, step_size = 1, type = "seasonal"){
  
  dummy_df <- NULL
  
  date_col <- df %>% 
    select(date) %>% 
    mutate(range = row_number())
  
  df_start_date <- as.Date(df$date[1])
  df_end_date <- as.Date(df$date[nrow(df)])
  
  dummy_df <-data.frame(date = seq.Date(df_start_date, df_end_date + 6, by = "days"))
  
  
  if(type == "custom"){
    dummy_df <- dummy_custom(dummy_df, dummy_name, start_date, end_date)
  }
  
  if(type == "ramp"){
    
    df <- df %>% 
      select(date)
    
    dummy_df <- dummy_ramp(df, dummy_name, start_date, end_date, step_size)
    
    return(dummy_df)
  }
  
  if(type == "seasonal"){
    
    dummy_df <- dummy_year(dummy_df)
    dummy_df  <- dummy_month(dummy_df)
    dummy_df  <- dummy_week(dummy_df)
    dummy_df  <- dummy_fy(dummy_df)
    dummy_df  <- dummy_tertial(dummy_df)
    
  }
  
  dummy_df <- dummy_df %>% 
    gather("dummy", "value", -date)
  
  tmp_df <- dummy_df %>% 
    left_join(date_col, by = "date") %>% 
    mutate(range = zoo::na.locf(range)) %>% 
    group_by(range, dummy) %>% 
    summarise(value = sum(value)/7) %>% 
    ungroup() %>% 
    spread(dummy, value)
  
  dummy_df <- date_col %>% 
    left_join(tmp_df, by = "range") %>% 
    select(-range)
  
  return(dummy_df)
  
}  

dummy <- function(df, dummy_name, start_date, end_date, step_size = 1, type = "month"){
  
  if(type == "custom"){
    df <- dummy_custom(df, dummy_name, start_date, end_date)
  }  
  
  if(type == "ramp"){
    df <- dummy_ramp(df, dummy_name, start_date, end_date, step_size)
  }
  
  if(type == "year"){
    df <- dummy_year(df)
  }
  
  if(type == "month"){
    df <- dummy_month(df)
  }
  
  if(type == "week"){
    df <- dummy_week(df)
  }
  
  if(type == "day"){
    df <- dummy_day(df)
  }
  
  return(df)
  
}  


dummy_custom<- function(df, dummy_name, start_date, end_date){
  
  output_df <- df %>% 
    mutate(dummy = if_else(date >= start_date & date <= end_date, 1, 0))
  
  names(output_df)[names(output_df) == "dummy"] <- dummy_name
  
  return(output_df)
  
}

dummy_ramp <- function(df, dummy_name, start_date, end_date, step_size = 1){
  
  ramp <- df %>% 
    select(date) %>% 
    filter(date >= start_date & date <= end_date) %>% 
    mutate(dummy = row_number()*step_size)
  
  output_df <- df %>% 
    left_join(ramp, by = "date") %>% 
    mutate(dummy = if_else(is.na(dummy), 0, dummy))
  
  names(output_df)[names(output_df) == "dummy"] <- dummy_name
  
  return(output_df)
  
}


dummy_day <- function(df){
  
  days_df <- df %>% 
    select(date) %>% 
    mutate(day = weekdays(date)) %>% 
    mutate(dummy = if_else(weekdays(date) == day, 1, 0)) %>% 
    spread(day, dummy) %>% 
    mutate(across(where(is.numeric), ~if_else(is.na(.x), 0, .x)))
  
  names(days_df)[-1] <- tolower(names(days_df)[-1])
  names(days_df)[-1] <- paste( "d_day", substr(names(days_df[-1]), 1, 3), sep = "_")
  
  output_df <- df %>% 
    left_join(days_df, by = "date")
  
  return(output_df)
  
}

dummy_week <- function(df){
  
  weeks_df <- df %>% 
    select(date) %>% 
    mutate(week = lubridate::week(date)) %>% 
    mutate(dummy = if_else(lubridate::week(date) == week, 1, 0)) %>% 
    spread(week, dummy) %>% 
    mutate(across(where(is.numeric), ~if_else(is.na(.x), 0, .x)))
  
  names(weeks_df)[-1] <- paste0( "d_week_", names(weeks_df[-1]))
  
  output_df <- df %>% 
    left_join(weeks_df, by = "date")
  
  return(output_df)
  
}


dummy_month <- function(df){
  
  months_df <- df %>% 
    select(date) %>% 
    mutate(month = months(date)) %>% 
    mutate(dummy = if_else(months(date) == month, 1, 0)) %>% 
    spread(month, dummy) %>% 
    mutate(across(where(is.numeric), ~if_else(is.na(.x), 0, .x)))
  
  names(months_df)[-1] <- tolower(names(months_df)[-1])
  names(months_df)[-1] <- paste( "d_month", substr(names(months_df[-1]), 1, 3), sep = "_")
  
  output_df <- df %>% 
    left_join(months_df, by = "date")
  
  return(output_df)
  
}

dummy_year <- function(df){
  
  years_df <- df %>% 
    select(date) %>% 
    mutate(year = lubridate::year(date)) %>% 
    mutate(dummy = if_else(lubridate::year(date) == year, 1, 0)) %>% 
    spread(year, dummy) %>% 
    mutate(across(where(is.numeric), ~if_else(is.na(.x), 0, .x)))
  
  names(years_df)[-1] <- paste0( "d_year_", names(years_df[-1]))
  
  output_df <- df %>% 
    left_join(years_df, by = "date")
  
  return(output_df)
  
}

dummy_fy <- function(df){
  
  fisyears_df <- df %>% 
    select(date) %>% 
    mutate(quarter = lubridate::quarter(date,with_year=TRUE,fiscal_start = 9)) %>% 
    mutate(fy = paste0("d_fy_",stringr::str_sub(as.character(quarter),3,4))) %>% 
    mutate(dummy = 1) %>% 
    select(date,fy,dummy) %>% 
    spread(fy,dummy) %>% 
    mutate(across(where(is.numeric), ~if_else(is.na(.x), 0, .x)))
  
  output_df <- df %>% 
    left_join(fisyears_df, by = "date")
  
  return(output_df)
  
}

dummy_tertial <- function(df){
  
  tertial_df <- df %>% 
    select(date) %>% 
    mutate(month = lubridate::month(date)) %>% 
    mutate(tertial = if_else(month >= 9 & month <= 12, "T1",
                             if_else(month >=1 & month <=4,"T2","T3"))) %>% 
    mutate(tertial = paste0("d_tertial_",tertial)) %>% 
    mutate(dummy = 1) %>% 
    select(date,tertial,dummy) %>% 
    spread(tertial,dummy) %>% 
    mutate(across(where(is.numeric), ~if_else(is.na(.x), 0, .x)))
 
  output_df <- df %>% 
    left_join(tertial_df, by = "date")
  
  return(output_df)
  
}

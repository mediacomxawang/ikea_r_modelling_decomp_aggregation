wcmon_fy_tertial <- function(min_date,max_date){
  
  days_between <- as.data.frame(seq(as.Date(min_date),as.Date(max_date),by=1))
  colnames(days_between) <- 'date'
  
  fy_tertial_df <- days_between %>% 
    select(date) %>% 
    mutate(month = lubridate::month(date)) %>% 
    mutate(tertial = if_else(month >= 9 & month <= 12, "T1",
                             if_else(month >=1 & month <=4,"T2","T3"))) %>% 
    mutate(quarter = lubridate::quarter(date,with_year=TRUE,fiscal_start = 9)) %>% 
    mutate(fy = paste0("FY_",stringr::str_sub(as.character(quarter),3,4))) %>%
    mutate(week = floor_date(date,"week,1")+1) %>% 
    mutate(dummy = 1) %>% 
    select(week,fy,tertial,dummy) %>% 
    group_by(week,fy,tertial) %>% 
    summarise(dummy = sum(dummy)) %>% 
    group_by(week) %>% 
    top_n(1, abs(dummy)) %>% 
    select(-dummy)
  
  return(fy_tertial_df)
  
}

wcsun_fy_tertial <- function(min_date,max_date){
  
  days_between <- as.data.frame(seq(as.Date(min_date),as.Date(max_date),by=1))
  colnames(days_between) <- 'date'
  
  fy_tertial_df <- days_between %>% 
    select(date) %>% 
    mutate(month = lubridate::month(date)) %>% 
    mutate(tertial = if_else(month >= 9 & month <= 12, "T1",
                             if_else(month >=1 & month <=4,"T2","T3"))) %>% 
    mutate(quarter = lubridate::quarter(date,with_year=TRUE,fiscal_start = 9)) %>% 
    mutate(fy = paste0("FY_",stringr::str_sub(as.character(quarter),3,4))) %>%
    mutate(week = floor_date(date,"week,1")) %>% 
    mutate(dummy = 1) %>% 
    select(week,fy,tertial,dummy) %>% 
    group_by(week,fy,tertial) %>% 
    summarise(dummy = sum(dummy)) %>% 
    group_by(week) %>% 
    top_n(1, abs(dummy)) %>% 
    select(-dummy)
  
  return(fy_tertial_df)
  
}

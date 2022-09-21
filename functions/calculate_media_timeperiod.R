# known as cico
calculate_media_timeperiod <- function(media_decomp,
                                       adstock_rates,
                                       start_date = NULL,
                                       end_date = NULL,
                                       date_mapping = NULL,
                                       freq = "week"){
  # browser()
  #create a date mapping table if it doesn't already exist
  if(is.null(date_mapping)){
    date_sequence <- data.frame(date = seq.Date(as.Date(start_date),as.Date(end_date),by = freq),
                                period = "InPeriod",
                                stringsAsFactors = F)
  }else{
    date_sequence <- purrr::pmap_df(list(date_mapping$period,date_mapping$start_date,date_mapping$end_date),
                                    function(period_name,start,end){
                                      
                                      data.frame(date = seq.Date(as.Date(start, origin = '1970-01-01'),
                                                                 as.Date(end, origin = '1970-01-01'),by = freq),
                                                 period = period_name,
                                                 stringsAsFactors = F)
                                    })
  }
  
  purrr::map_df(unique(date_sequence$period),function(period_name){
    #browser()
    #get the media part of the decomp
    media_decomp <- media_decomp %>% 
      left_join(date_sequence %>% filter(period == period_name),by = c("Date"="date"))
    
    #remove date and period variables
    media_vars <- colnames(media_decomp)[-c(1,ncol(media_decomp))] 
    
    #sort decomp into same order as adstocks
    media_decomp <- media_decomp %>% 
      select(Date,period,one_of(adstock_rates$variable_name))
    
    #find the first and last occurence of each period name
    first_rows <- media_decomp %>% 
      tibble::rownames_to_column("row") %>% 
      dplyr::group_by(period) %>%  
      dplyr::summarise(row = min(as.numeric(row))) %>% 
      drop_na()
    
    last_rows <- media_decomp %>% 
      tibble::rownames_to_column("row") %>% 
      dplyr::group_by(period) %>%  
      dplyr::summarise(row = max(as.numeric(row))) %>% 
      drop_na()
    
    #calculate carry in - value in period before first occurence * adstock - ends when period definition ends
    carry_in <- purrr::map2_df(adstock_rates$variable_name,adstock_rates$value,function(var,adstock){
      
      #this year
      #browser()
      this_start_loc <- as.numeric(first_rows %>% filter(period == period_name) %>% pull(row)) - 1
      this_start_value <- as.data.frame(media_decomp[this_start_loc,var]) %>% pull()
      
      #create a new vector and adstock
      temp_carryin_this <- this_start_value/(1-adstock) - this_start_value # subtract the starting value as this gets included when dividing by adstock
      
      
      final_carryin <- base::data.frame(period = period_name,
                                        variable_name = var,
                                        carry_in = c(temp_carryin_this),
                                        stringsAsFactors = F)
      return(final_carryin)
    })
    
    #calculate carry out - value in last date of period  * adstock - ends after 52 weeks
    carry_out <- purrr::map2_df(adstock_rates$variable_name,adstock_rates$value,function(var,adstock){
      
      this_end_loc <- as.numeric(last_rows %>% filter(period == period_name) %>% pull(row))
      this_end_value <- as.data.frame(media_decomp[this_end_loc,var]) %>% pull()
      
      #create a new vector and compute adstock
      temp_carryout_this <- this_end_value/(1-adstock) - this_end_value # subtract the starting value as this gets included when dividing by adstock and is already counted in in_period column 
      
      
      final_carryout <- base::data.frame(period = period_name,
                                         variable_name = var,
                                         carry_out = c(temp_carryout_this),
                                         stringsAsFactors = F)
      return(final_carryout)
    })
    
    inperiod_data <- media_decomp %>% 
      gather(key = Variable,value = Value,-Date,-period) %>% 
      filter(period == period_name) %>% 
      dplyr::group_by(Variable,period) %>% 
      dplyr::summarise(in_period = sum(Value)) %>% 
      ungroup() %>% 
      tidyr::replace_na(replace = list(in_period = 0)) %>% 
      #filter(value > 1) %>% 
      left_join(carry_in,by = c("Variable"="variable_name","period"="period")) %>% 
      left_join(carry_out,by = c("Variable"="variable_name","period"="period")) %>% 
      #mutate(in_period = in_period-carry_in) %>% 
      mutate(final_total = (in_period+carry_out)-carry_in) %>% 
      select(Variable,period,carry_in,in_period,carry_out,final_total)
    
    return(inperiod_data)
  })
}
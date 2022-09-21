
varmap <- function(data, var_to_test, model, tbl_calc_adstock_from, type = "atan"){
  
  if(type == "atan"){
    return(varmap_atan(data, var_to_test, model, tbl_calc_adstock_from))
  }
  else if(type == "ma"){
    return(varmap_ma(data, var_to_test, model))
  }
  else if(type == "both"){
    return(list(varmap_atan(data, var_to_test, model, tbl_calc_adstock_from),
                varmap_ma(data, var_to_test, model)))
  }
}

atan_divisor <- function(max){
  
  j <- c(1:20)
  max <- max/2
  t <- log10(max)
  
  atan <- j[FALSE]
  
  for(i in seq_along(j)){
    
    tmp <- (max*(j[i]/20))/(10^(t-2))
    atan[i] <- round(tmp)*(10^(t-2))
    
  }
  
  rounding <- floor(log10(atan[1]))
  atan <- round(atan, -rounding)
  
  return(atan) 
  
}

varmap_atan <- function(data, var_to_test, model, tbl_calc_adstock_from){
  
  media_var <- tbl_calc_adstock_from %>%
    select(1, all_of(var_to_test)) # select timestamp and var_to_test
  
  adstock_rate <- seq(0.05, 0.95, by = 0.05)
  
  # create adstocks
  adstocks <- adstock(media_var, date_var = names(media_var)[1], var_to_test, adstock_rate)
  
  # df of var_to_test and it's adstocks
  media_var <- media_var %>%
    filter(date %in% data$date) %>% 
    left_join(adstocks) %>%
    select(-1) # drop date
  
  # create vector of divisors
  divisors <- c(0, atan_divisor(max(media_var)))
  
  placeholder_tbl <- map(c(0,adstock_rate), ~tibble(. = numeric())) %>% bind_cols() %>% setNames(c("0", 100*adstock_rate))
  
  varmap_atan <- bind_cols(divisor = NA, placeholder_tbl)
  
  for(i in seq_along(divisors)){
    
    # atan the selected media variable
    if(divisors[i] == 0){
      atan_df <- data
    }
    else{
      
      media_var_atan <- media_var %>%
        mutate_all(~atan(./divisors[i]))
      
      atan_df <- data %>%
        select(-all_of(names(media_var_atan))) %>%
        bind_cols(media_var_atan)
      
    }
    
    # calculate t stat
    ccalc_df <- ccalc(atan_df, colnames(media_var), model) %>%
      select(term, statistic) %>%
      mutate(divisor = divisors[i],
             term = stringi::stri_sub(term, -2),
             term = ifelse(is.na(as.numeric(term)), "0", term)) %>% 
      spread(term, statistic)
    
    # add t-stat
    varmap_atan <- bind_rows(varmap_atan, ccalc_df)
    
  }
  
  return(varmap_atan)
  
}

varmap_ma <- function(data, var_to_test, model){
  
  media_var <- data %>%
    select(all_of(var_to_test)) # select timestamp and var_to_test
  
  # period range of moving averages 
  period <- c(1:11)
  
  placeholder_tbl <- map(c(0:9), ~tibble(. = numeric())) %>% bind_cols() %>% setNames(c(0:9))
  
  varmap_ma <- bind_cols(movavc = NA, placeholder_tbl)
  
  for(i in seq_along(period)){
    
    media_var_ma <- media_var %>%
      mutate_all(~rollmean(., k = i, fill = "extend"))
    
    ma_df <- data %>%
      select(-all_of(names(media_var_ma))) %>%
      bind_cols(media_var_ma)
    
    # calculate t stat
    ccalc_df <- ccalc(ma_df, colnames(media_var), model, c(1:9)) %>%
      select(term, statistic) %>%
      mutate(movavc = period[i],
             term = stringi::stri_sub(term, -1),
             term = ifelse(is.na(as.numeric(term)), "0", term)) %>%
      spread(term, statistic)
    
    # add t-stat
    varmap_ma <- bind_rows(varmap_ma, ccalc_df)
    
  }
  
  return(varmap_ma)
}


show_varmap <- function(varmap_output) {
  
  DT::datatable(varmap_output, class = "compact - hover",
                options = list(dom = 't', scrollY = FALSE, pageLength = 5000,
                               columnDefs = list(list(
                                 targets = 0,
                                 render = JS(
                                   "function(data, type, row, meta) {",
                                   "return type === 'display' && data.length > 60 ?",
                                   "'<span title=\"' + data + '\">' + data.substr(0, 60) + '...</span>' : data;",
                                   "}")
                               ))), rownames = FALSE, callback = JS('table.page(1).draw(false);')) %>% 
    DT::formatRound(colnames(varmap_output)[-1], 2) %>% 
    DT::formatStyle(colnames(varmap_output)[-1],
                    color = DT::styleInterval(c(-2, -1, 1, 2), 
                                              c("red", "orange", "yellow", "lightgreen", "green")))
  
}
calc_cpa <- function(decomp_output, tbl_media_spend, tbl_q2s, tbl_map, dep_var_name, 
                     penalty_factor = NULL, var_to_penalise = NULL, mode = "total"){
  
  if (mode == "total") {
    return(calc_cpa_tot(decomp_output, tbl_media_spend, tbl_q2s, tbl_map, dep_var_name, 
                                    penalty_factor, var_to_penalise))
  } else if (mode == "26w") {
    return(calc_cpa_26wks(decomp_output, tbl_media_spend, tbl_q2s, tbl_map, dep_var_name, 
                                      penalty_factor, var_to_penalise))
  } else {
    stop("Argument mode needs to be either 'total' or '26w' for a rolling 26 week CPA")
  }
  
}

calc_cpa_tot <- function(decomp_output, tbl_media_spend, tbl_q2s, tbl_map, dep_var_name, 
                     penalty_factor = NULL, var_to_penalise = NULL){
  
  # get rid of '_sp'
  media_spend <- tbl_media_spend %>% 
    mutate(variable = gsub("_[a-z]+$", "", variable)) #%>% distinct(variable) %>% pull()
  
  tbl_map <- tbl_map %>% 
    select(actual.vars, `brand/category_a`, `product/category_b`, `channel/category_c`)
  
  # get brand product channel info from dep var name (for q2s calcs)
  dep_var_name <- gsub('^(q|s)_|_(q|s)$', '', dep_var_name)
  
  tbl_q2s <- tbl_q2s %>% 
    filter(brand_product_ch == dep_var_name) %>% 
    filter(!is.na(value))
  
  brand_product <- gsub('^(pcw_)*([a-z0-9]+_[a-z]+).*', '\\2', unique(tbl_q2s$brand_product_ch))
  
  decomp_df <- decomp_output@DecompedData %>%
    mutate(Variable = clean_modelled_vars(modelled_vars)) %>%
    # filter(!Variable %in% cico_vars) %>%
    # bind_rows(cico) %>% #distinct(Variable) %>% pull()
    # filter(date %in% cico_dates) %>% # cico will drop first datestamp
    left_join(tbl_map, by = c('Variable' = 'actual.vars')) %>%
    mutate(Variable = gsub("_[a-z]+$", "", Variable)) %>% # drop spend/tvr/vol part of name
    group_by(date, Variable, `brand/category_a`, `product/category_b`, `channel/category_c`) %>%
    summarise(contr = sum(contributions)) %>%
    ungroup() %>%
    left_join(tbl_q2s, by = "date") %>%
    mutate(sales = contr * value) # multiple by q2s factor
  
  if (!is.null(penalty_factor)) {
    # apply penalty factor
    decomp_df <- decomp_df %>%
      #select(-value) %>% 
      mutate(apply_penalty = grepl(var_to_penalise, Variable),
             sales_after_penalty = ifelse(apply_penalty, sales * (1-penalty_factor), 0),
             sales_less_penalty_var = ifelse(apply_penalty, 0, sales)) %>%
      group_by(date) %>%
      mutate(weights = sales/sum(sales_less_penalty_var),
             value_to_redistr = sum(ifelse(apply_penalty, sales, 0))) %>%
      ungroup() %>%
      mutate(adj_sales = ifelse(apply_penalty, sales_after_penalty, sales + weights * (penalty_factor * value_to_redistr))) %>%
      mutate(sales = adj_sales) %>% 
      select(-apply_penalty, -sales_after_penalty, -sales_less_penalty_var, -weights, -value_to_redistr, -adj_sales)
  }
  
  decomp_df <- decomp_df %>% 
    select(-value) %>% 
    filter(substr(Variable, 1, 2) == 'm_') %>%
    #filter(substr(Variable, 1, nchar(glue::glue('m_{brand_product}'))) == glue::glue('m_{brand_product}')) %>%
    left_join(media_spend, by = c("date" = "date", "Variable" = "variable"))
  
  no_spend <- decomp_df %>% filter(is.na(value)) %>% distinct(Variable) %>% pull()
  
  if(length(no_spend) > 0) {
    stop(glue::glue("tbl_media_spend doesn't contain spend for the following variables: {paste(no_spend, collapse = ', ')}"))
  }
  
  decomp_df_by_year <- decomp_df %>% 
    mutate(date = format(date, "%Y")) %>% 
    group_by(date, Variable, `brand/category_a`, `product/category_b`, `channel/category_c`) %>% 
    summarise(contr = sum(contr),
              sales = sum(sales),
              spend = sum(value)) %>% 
    ungroup() %>%
    mutate(#channel = gsub("^m_", "", Variable),
      #channel = toupper(gsub(brand_product, '', channel)),
      cpa = spend/sales,
      cpq = spend/contr)
  
  # calculate total CPA
  tot_brand_product_spend <- tbl_media_spend %>% 
    filter(variable == glue::glue('m_{brand_product}tot_all_all_all_sp')) %>% 
    filter(date >= min(decomp_df$date) & date <= max(decomp_df$date)) %>%
    mutate(date = format(date, "%Y")) %>% 
    group_by(date, variable) %>% 
    summarise(spend = sum(value))
  
  tot_cpa <- decomp_df_by_year %>% 
    filter(substr(Variable, 1, nchar(glue::glue('m_{brand_product}'))) == glue::glue('m_{brand_product}')) %>%  
    group_by(date) %>% 
    summarise(contr = sum(contr, na.rm = TRUE),
              sales = sum(sales, na.rm = TRUE)) %>% 
    left_join(tot_brand_product_spend, by = 'date') %>% 
    mutate(cpa = spend/sales,
           cpq = spend/contr) %>% 
    rename(Variable = variable) %>% 
    left_join(tbl_map, by = c('Variable' = 'actual.vars'))
  
  decomp_df_by_year <- decomp_df_by_year %>%
    bind_rows(tot_cpa) %>%
    mutate(#date = ifelse(is.na(date), "total", date),
      cpa = ifelse(is.nan(cpa), 0, cpa), # remove any NaNs from div by zero
      cpq = ifelse(is.nan(cpq), 0, cpq))
  
  return(decomp_df_by_year)
  
}

calc_cpa_26wks <- function(decomp_output, tbl_media_spend, tbl_q2s, tbl_map, dep_var_name, 
                           penalty_factor = NULL, var_to_penalise = NULL){
  
  roll_by = "rolling26wks"
  no_of_weeks <- readr::parse_number(roll_by)
  # dep_var_name <- "q_dl_h_w_tm1"
  
  decomp_df <- decomp_with_cico@DecompedData %>% 
    mutate(date = wc_mon(date)) %>% 
    group_by(date, modelled_vars) %>% 
    summarise(contributions = sum(contributions)) %>% 
    ungroup() %>% 
    spread(modelled_vars, contributions) %>% 
    dplyr::arrange(desc(date))
  
  no_of_periods <- round(nrow(decomp_df)/no_of_weeks)+1
  rollingXwk_periods <- rep(1:no_of_periods, times=1, each=no_of_weeks)[1:nrow(decomp_df)]
  
  decomp_df <- decomp_df %>% 
    mutate(rollingXwk_periods = rollingXwk_periods) %>%
    group_by(rollingXwk_periods) %>% 
    mutate(period_name = paste0("26w to ", max(date))) %>% 
    ungroup() %>% 
    gather(modelled_vars, contributions, -date, -period_name, -rollingXwk_periods)
  
  # get rid of '_sp'
  media_spend <- tbl_media_spend %>% 
    mutate(variable = gsub("_[a-z]+$", "", variable)) #%>% distinct(variable) %>% pull()
  
  tbl_map <- tbl_map %>% 
    select(actual.vars, `brand/category_a`, `product/category_b`, `channel/category_c`)
  
  # get brand product channel info from dep var name (for q2s calcs)
  dep_var_name <- gsub('^(q|s)_|_(q|s)$', '', dep_var_name)
  
  tbl_q2s <- tbl_q2s %>% 
    filter(brand_product_ch == dep_var_name) %>% 
    filter(!is.na(value))
  
  brand_product <- gsub('^(pcw_)*([a-z0-9]+_[a-z]+).*', '\\2', unique(tbl_q2s$brand_product_ch))
  
  #decomp_df <- decomp_output@DecompedData %>%
  decomp_df <- decomp_df %>% 
    mutate(Variable = clean_modelled_vars(modelled_vars)) %>%
    # filter(!Variable %in% cico_vars) %>%
    # bind_rows(cico) %>% #distinct(Variable) %>% pull()
    # filter(date %in% cico_dates) %>% # cico will drop first datestamp
    left_join(tbl_map, by = c('Variable' = 'actual.vars')) %>%
    mutate(Variable = gsub("_[a-z]+$", "", Variable)) %>% # drop spend/tvr/vol part of name
    group_by(date, Variable, rollingXwk_periods, period_name, `brand/category_a`, `product/category_b`, `channel/category_c`) %>%
    summarise(contr = sum(contributions)) %>%
    ungroup() %>%
    left_join(tbl_q2s, by = "date") %>%
    mutate(sales = contr * value) # multiple by q2s factor
  
  if (!is.null(penalty_factor)) {
    # apply penalty factor
    decomp_df <- decomp_df %>%
      #select(-value) %>% 
      mutate(apply_penalty = grepl(var_to_penalise, Variable),
             sales_after_penalty = ifelse(apply_penalty, sales * (1-penalty_factor), 0),
             sales_less_penalty_var = ifelse(apply_penalty, 0, sales)) %>%
      group_by(date) %>%
      mutate(weights = sales/sum(sales_less_penalty_var),
             value_to_redistr = sum(ifelse(apply_penalty, sales, 0))) %>%
      ungroup() %>%
      mutate(adj_sales = ifelse(apply_penalty, sales_after_penalty, sales + weights * (penalty_factor * value_to_redistr))) %>%
      mutate(sales = adj_sales) %>% 
      select(-apply_penalty, -sales_after_penalty, -sales_less_penalty_var, -weights, -value_to_redistr, -adj_sales)
  }
  
  decomp_df <- decomp_df %>% 
    select(-value) %>% 
    filter(substr(Variable, 1, 2) == 'm_') %>%
    #filter(substr(Variable, 1, nchar(glue::glue('m_{brand_product}'))) == glue::glue('m_{brand_product}')) %>%
    left_join(media_spend, by = c("date" = "date", "Variable" = "variable")) %>% 
    filter(rollingXwk_periods < 3)
  
  no_spend <- decomp_df %>% filter(is.na(value)) %>% distinct(Variable) %>% pull()
  
  if(length(no_spend) > 0) {
    stop(glue::glue("tbl_media_spend doesn't contain spend for the following variables: {paste(no_spend, collapse = ', ')}"))
  }
  
  decomp_df_by_year <- decomp_df %>% 
    mutate(date = period_name) %>% 
    group_by(date, Variable, `brand/category_a`, `product/category_b`, `channel/category_c`) %>% 
    summarise(contr = sum(contr),
              sales = sum(sales),
              spend = sum(value)) %>% 
    ungroup() %>%
    mutate(#channel = gsub("^m_", "", Variable),
      #channel = toupper(gsub(brand_product, '', channel)),
      cpa = spend/sales,
      cpq = spend/contr)
  
  # calculate total CPA
  
  period_name_lu <- decomp_df %>% 
    distinct(date, period_name)
  
  tot_brand_product_spend <- tbl_media_spend %>% 
    filter(variable == glue::glue('m_{brand_product}tot_all_all_all_sp')) %>% 
    filter(date >= min(decomp_df$date) & date <= max(decomp_df$date)) %>%
    left_join(period_name_lu, by = "date") %>% 
    mutate(date = period_name) %>% 
    group_by(date, variable) %>% 
    summarise(spend = sum(value))
  
  tot_cpa <- decomp_df_by_year %>% 
    filter(substr(Variable, 1, nchar(glue::glue('m_{brand_product}'))) == glue::glue('m_{brand_product}')) %>%  
    group_by(date) %>% 
    summarise(contr = sum(contr, na.rm = TRUE),
              sales = sum(sales, na.rm = TRUE)) %>% 
    left_join(tot_brand_product_spend, by = 'date') %>% 
    mutate(cpa = spend/sales,
           cpq = spend/contr) %>% 
    rename(Variable = variable) %>% 
    left_join(tbl_map, by = c('Variable' = 'actual.vars'))
  
  decomp_df_by_year <- decomp_df_by_year %>%
    bind_rows(tot_cpa) %>%
    mutate(#date = ifelse(is.na(date), "total", date),
      cpa = ifelse(is.nan(cpa), 0, cpa), # remove any NaNs from div by zero
      cpq = ifelse(is.nan(cpq), 0, cpq))
  
  return(decomp_df_by_year)
  
}

plot_cpa <- function(tbl_cpa, type = "s", groupby = "channel") {
  
  if (groupby == 'channel') {
    
    tbl_cpa_grouped <- tbl_cpa %>%
      mutate(channel = `channel/category_c`) 
      
  } else if (groupby == 'brand') {
    
    tbl_cpa_grouped <- tbl_cpa %>% 
      mutate(channel = paste(`brand/category_a` ,`product/category_b`, `channel/category_c`)) 
      
  } else {
    stop('Allowed groupby values are either "channel" or "brand"')
  }
  
  tbl_cpa <- tbl_cpa_grouped %>% 
    group_by(date, channel) %>% 
    summarise(contr = sum(contr),
              sales = sum(sales),
              spend = sum(spend),
              cpa = spend/sales,
              cpq = spend/contr,
              cpa = ifelse(is.nan(cpa), 0, cpa), # remove any NaNs from div by zero
              cpq = ifelse(is.nan(cpq), 0, cpq)) %>% 
    ungroup()
  
  tot_name <- distinct(tbl_cpa, channel) %>% pull()
  
  tot_name <- tot_name[substr(tot_name, nchar(tot_name)-2, nchar(tot_name)) == 'Tot']
  
  if (type == 'q') {
    
    tbl_cpa$channel <- factor(tbl_cpa$channel,
                              levels = unique(tbl_cpa$channel)[order(tbl_cpa$cpq, decreasing = FALSE)])
    
    if (length(tot_name) == 1) {
      tbl_cpa$channel <- relevel(tbl_cpa$channel, tot_name)
    }
    
    p <- tbl_cpa %>% 
      plot_ly(x = ~channel, y = ~cpq, color = ~date, colors = mediacom_palette, type = "bar") %>% 
      layout(yaxis = list(title = 'CPQ', tickprefix="\U00A3"),
             xaxis = list(title = ''))
    
  } else if (type == 's') {
    
    tbl_cpa$channel <- factor(tbl_cpa$channel,
                              levels = unique(tbl_cpa$channel)[order(tbl_cpa$cpa, decreasing = FALSE)])
    
    if (length(tot_name) == 1) {
      tbl_cpa$channel <- relevel(tbl_cpa$channel, tot_name)
    }
    
    p <- tbl_cpa %>% 
      plot_ly(x = ~channel, y = ~cpa, color = ~date, colors = mediacom_palette, type = "bar") %>% 
      layout(yaxis = list(title = 'CPA', tickprefix="\U00A3"),
             xaxis = list(title = ''))
    
  }
  
  return(p)
  
}

show_cpa <- function(tbl_cpa) {
  
  tbl_cpa %>% 
    group_by(date, `brand/category_a` ,`product/category_b`, `channel/category_c`) %>% 
    summarise(contr = sum(contr),
              sales = sum(sales),
              spend = sum(spend),
              cpa = spend/sales,
              cpq = spend/contr,
              cpa = ifelse(is.nan(cpa), 0, cpa), # remove any NaNs from div by zero
              cpq = ifelse(is.nan(cpq), 0, cpq)) %>% 
    ungroup() %>% 
    DT::datatable(class = "compact - hover",
                  colnames = c('date', 'brand', 'product', 'channel', 'contr', 'sales', 'spend', 'cpa', 'cpq'),
                  options = list(dom = 't', scrollY = FALSE, pageLength = 5000),
                  rownames = FALSE) %>% 
    DT::formatRound(c('contr', 'sales'), 0) %>%
    DT::formatCurrency('spend', '\U00A3', digits = 0) %>% 
    DT::formatCurrency(c('cpa', 'cpq'), '\U00A3', digits = 2) 
  
}




create_media_spends_table <- function(assembled_df){
  
  assembled_df <- assembled_df %>% 
    gather('variable', 'value', -date) %>% 
    filter(grepl('^m_.*_sp$', variable)) %>% 
    separate(variable, into = c('abbr_1',
                                'abbr_2',
                                'abbr_3',
                                'abbr_4',
                                'abbr_5'),
             remove = FALSE
    ) %>% 
    mutate(year = lubridate::year(date)) %>% 
    mutate(across(-c('date'), ~ ifelse(. == 'zz', NA, .)))
  
}



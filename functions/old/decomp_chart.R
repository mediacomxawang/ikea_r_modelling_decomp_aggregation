# libraries 
library(tidyverse)
library(lubridate)
library(plotly)
library(janitor)

# stacked bar charts displaying yearly contributions split by group
plot_decomp_stack <- function(decomp_out){
  
  decomp_df <- decomp_output@DecompedData
  
  decomp_df <- decomp_df %>% 
    dplyr::mutate(date = year(lubridate::date)) %>% 
    dplyr::mutate(date = as.character(date)) %>% 
    dplyr::group_by(date, group) %>% 
    dplyr::summarise(contributions = sum(contributions)) %>% 
    dplyr::ungroup() 
  
  year_total <- decomp_df %>% 
    dplyr::group_by(date) %>% 
    dplyr::summarise(total = sum(contributions)) %>% 
    dplyr::ungroup()
  
  decomp_df_percent <- decomp_df %>% 
    dplyr::left_join(year_total, by = "date") %>% 
    dplyr::mutate(contributions = round((contributions/total)*100, 2))
  
  fig <- plotly::plot_ly(decomp_df_percent, x = ~date, y = ~contributions, 
                 type = 'bar',
                 color = ~group,
                 colors = c("grey", "red", "lightblue"),
                 text = ~paste(contributions, "%", sep = ""),
                 textfont = list(color = '#000000', size = 24)
  ) %>%
    plotly::layout(title = 'Contribution', 
           yaxis = list(title = 'contribution', ticksuffix="%"),
           barmode = 'stack'
    )
  
  return(fig)
  
  
}

# bar chart of contribution of media channels
plot_decomp_media <- function(decomp_output, id = "spend"){
  
  decomp_df <-  decomp_output@DecompedData
  
  decomp_df <- decomp_df %>% 
    dplyr::group_by(modelled_vars) %>% 
    dplyr::summarise(contributions = sum(contributions)) %>% 
    dplyr::filter(grepl(id, modelled_vars))
  
  
  decomp_df_percent <- decomp_df %>% 
    dplyr::mutate(contributions = round((contributions/sum(decomp_df$contributions))*100, 1)) %>% 
    dplyr::arrange(-contributions) 
  
  decomp_df_percent <- decomp_df_percent %>% 
    dplyr::mutate(modelled_vars = factor(modelled_vars, levels = decomp_df_percent[["modelled_vars"]]))
  
  
  fig <- plotly::plot_ly(decomp_df_percent, x = ~modelled_vars, y = ~contributions, 
                 type = 'bar',
                 text = ~paste(contributions, "%", sep = ""),
                 textposition = "outside",
                 textfont = list(color = '#000000', size = 18)
  ) %>%
    plotly::layout(title = 'Contribution', 
           xaxis = list(title = ""),
           yaxis = list(title = 'contribution', ticksuffix="%")
    )
  
  return(fig)
}


# exports the decomp output into format we are familiar with
decomp_csv <- function(decomp_output){
  
  decomp_df <-  decomp_output@DecompedData
  
  output_df <- decomp_df %>% 
    dplyr::select(date, modelled_vars, contributions) %>%
    dplyr::mutate(modelled_vars = factor(modelled_vars, levels = unique(modelled_vars))) %>% 
    tidyr::spread(modelled_vars, contributions)
  
  readr::write_csv(output_df, "decomp_output.csv")
  
}

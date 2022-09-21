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




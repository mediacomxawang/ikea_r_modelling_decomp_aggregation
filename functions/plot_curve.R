plot_curve <- function(media_var, dim_ret) {
  
  plot_ly(x = media_var, y = atan(media_var/dim_ret), 
          type = "scatter", mode = "markers", marker = list(color = mediacom_palette[1])) %>% 
    layout(title = glue::glue("Diminishing Returns: {dim_ret}"),
           yaxis = list(title = "Response Index"),
           xaxis = list(title = "Budget"))
  
}

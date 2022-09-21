plot_waterfall <- function(decomp_output, mapping_tbl, 
                           grouping_column = "group", roll_by = "rolling52wks", 
                           title = 'Contribution of Key Drivers', yaxis_title = "Quotes"){
  
  decomp_df <- decomp_group_by_custom_dates(decomp_output, roll_by = roll_by) %>% 
    decomp_group_by_grouping_column(mapping_tbl, grouping_column)
  
  data <- get_data_for_waterfall(decomp_df, roll_by)
  
  start_point <- data$y[1]
  
  fig <- plot_ly(
    data, 
    type = "waterfall",
    #base = 0,
    measure = ~measure,
    x = ~x, 
    textposition = "outside", 
    y= ~y, 
    text =~text,
    connector = list(line = list(color= "rgb(250, 250, 250)")),
    decreasing = list(marker = list(color = "#e1005d")),
    increasing = list(marker = list(color = "rgb(59,179,146)")),
    totals = list(marker = list(color = "#4a4a49"))
  ) 
  
  fig <- fig %>%
    layout(title = title,
           xaxis = list(title = ""),
           yaxis = list(title = yaxis_title, showgrid = FALSE),
           autosize = TRUE,
           showlegend = FALSE,
           
           shapes = list(
             list(type = "rect",
                  fillcolor = "#4a4a49", line = list(color = "#4a4a49"), opacity = 1,
                  x0 = -0.4, x1 = 0.4, xref = "x",
                  y0 = 0.0, y1 = start_point, yref = "y")
    ))
  
  return(fig)
  
}

plot_line <- function(date_var, y, y2 = FALSE, title = NULL, gridlines = FALSE, graphlib = "dygraph") {
  
  # stop if date_var is not Date
  if(sum(class(date_var) %in% c("POSIXct", "POSIXt", "Date")) < 1) {
    stop('date_var is not a Date. Please supply date_var in Date format')
  }
  
  # check if y is a tibble, make tibble if not
  if (!is_tibble(y)) {
    y <- tibble(y)
  }
  
  # clean variable names
  # keep everything after $
  colnames(y) <- sub('.*\\$', '', names(y))
  
  # create title if not specified
  if (is.null(title)) {
    title <- paste(names(y), collapse = " vs ")
  }
  
  if (graphlib == 'plotly') {
    
    # all variables plotted on the left axis 
    d <- y %>% 
      mutate(date = date_var) %>% 
      pivot_longer(1:ncol(y), names_to = 'variable', values_to = 'value') %>% 
      plotly::plot_ly(x = ~date, y = ~value, color = ~variable, 
                      colors = mediacom_palette, type = 'scatter', mode = 'lines',
                      line = list(width = 1)) %>%
      layout(legend = list(orientation = "h",   # show entries horizontally
                           xanchor = "center",  # use center of legend as anchor
                           x = 0.5),
             title = title,
             yaxis = list(title = '', showgrid = gridlines),
             xaxis = list(title = '', showgrid = gridlines))

    if (y2 == TRUE) {
      
      # first y variable is plotted on the right axis 
      ay <- list(
        overlaying = "y",
        side = "right",
        title = names(y)[1],
        automargin = TRUE)
      
      d <- y %>% 
        mutate(date = date_var) %>% 
        pivot_longer(2:ncol(y), names_to = 'variable', values_to = 'value') %>% 
        plotly::plot_ly(x = ~date, y = ~value, color = ~variable,  
                        colors = mediacom_palette[-1], type = 'scatter', mode = 'lines',
                        line = list(width = 1)) %>% 
        add_trace(x = ~date, y = y[1] %>% pull(), 
                  name = names(y)[1], 
                  yaxis = "y2", 
                  line = list(color = mediacom_palette[1], width = 1),
                  mode = "lines", type = "scatter") %>% 
        layout(legend = list(orientation = "h",   # show entries horizontally
                             xanchor = "center",  # use center of legend as anchor
                             x = 0.5),
               title = title,
               yaxis = list(title = '', showgrid = gridlines),
               xaxis = list(title = '', showgrid = gridlines),
               yaxis2 = ay)
        
    }
    
  } else {
    
    # create time series object
    df_ts <- xts::xts(y, order.by = date_var)
    
    FUNC_JSFormatNumber <- "function(x) {return x.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,')}"
    valueFormatter <- "function formatValue(v) {
                     var suffixes = ['', 'K', 'M', 'G', 'T'];
                     if (v < 1000) return v;
                     var magnitude = Math.ceil(String(Math.floor(v)).length / 3-1);
                     if (magnitude > suffixes.length - 1)
                     magnitude = suffixes.length - 1;
                     return String(Math.round(v / Math.pow(10, magnitude * 3), 2)) +suffixes[magnitude]}"
    
    # all variables plotted on the left axis 
    d <- dygraphs::dygraph(df_ts, main = title) %>%
      dygraphs::dyOptions(drawGrid = gridlines,
                          colors = mediacom_palette) %>% 
      dygraphs::dyRangeSelector() %>% 
      dyLegend(width = 600) %>% 
      dygraphs::dyAxis('y', axisLabelFormatter=JS(FUNC_JSFormatNumber))
    
    if (y2 == TRUE) {
      
      # first y variable is plotted on the right axis 
      d <- d %>% 
        dygraphs::dySeries(names(y)[1], axis = 'y2') %>% 
        dygraphs::dyAxis('y2', label = names(y)[1], axisLabelFormatter=JS(FUNC_JSFormatNumber)) %>% 
        dygraphs::dyAxis('y', label = paste0(names(y)[2:length(names(y))], collapse = ", "))
      
    }
    
  }
  
  return(d)
  
}

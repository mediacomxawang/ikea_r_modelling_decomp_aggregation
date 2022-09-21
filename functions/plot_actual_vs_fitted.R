plot_act_vs_fitted <- function(model, date_var, gridlines = FALSE, convert_to_real = TRUE, graphlib = 'dygraphs') {
  
  # create table containing model observations
  model_obs <- broom::augment(model)
  
  # check if .rownames present as a first column
  # if yes, some obs were dropped by lm fn because they were NAs
  if (names(model_obs)[1] == ".rownames") {
    
    # create new date var
    date_var <- date_var[model_obs[1] %>% pull() %>% as.numeric()]
    
    # drop column .rownames
    model_obs <- model_obs %>%
      dplyr::select(-.rownames)
    
  }
  
  model_obs <- model_obs %>% 
    dplyr::select(1, .fitted, .std.resid) # select dep, fitted, resids
  
  if (convert_to_real) {
    
    # exp if dep var is logged
    if (substr(names(model_obs)[1], 1, 4) == "log(") {
      model_obs <- model_obs %>% 
        dplyr::mutate_at(c(1,2), ~exp(.)) %>% 
        dplyr::rename_at(1, ~gsub("log\\(|\\)", "", .))
    }
    
  }
  
  if (graphlib == 'plotly') {
    
    p_avf <- plot_ly(model_obs, x = date_var, y = pull(model_obs, 1), type = 'scatter', mode = 'lines',
                     name = names(model_obs)[1], line = list(color = mediacom_palette[2], width = 1)) %>% 
      add_trace(y = ~.fitted, name = 'fitted', line = list(color = mediacom_palette[1], width = 1)) %>% 
      layout(yaxis = list(title = '', showgrid = gridlines)) %>% 
      layout(legend = list(orientation = "h",   # show entries horizontally
                           xanchor = "center",  # use center of legend as anchor
                           x = 0.5),
             title = 'Actual vs Fitted')
    
    p_resids <- plot_ly(model_obs, x = date_var, y = ~.std.resid, type = 'bar', 
                        marker = list(color = mediacom_palette[6])) %>% 
      layout(title = 'Standard Residuals', showlegend = FALSE,
             yaxis = list(title = '', showgrid = FALSE))
    
  } else {
    
    # convert model obs to time series object
    model_obs_ts <- xts::xts(model_obs, order.by = date_var) 
    
    FUNC_JSFormatNumber <- "function(x) {return x.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,')}"
    valueFormatter <- "function formatValue(v) {
                     var suffixes = ['', 'K', 'M', 'G', 'T'];
                     if (v < 1000) return v;
                     var magnitude = Math.ceil(String(Math.floor(v)).length / 3-1);
                     if (magnitude > suffixes.length - 1)
                     magnitude = suffixes.length - 1;
                     return String(Math.round(v / Math.pow(10, magnitude * 3), 2)) +suffixes[magnitude]}"
    
    # plot actual vs fitted
    p_avf <- dygraphs::dygraph(model_obs_ts[,-3], group = "avf", 
                               main = "Actual vs Fitted"#, ylab = names(model_obs)[1]
    ) %>% 
      dygraphs::dyOptions(drawGrid = gridlines, 
                          colors = mediacom_palette[1:2]) %>% 
      dyLegend(width = 500) %>% 
      dygraphs::dyRangeSelector()
    
    if (convert_to_real) {
      p_avf <- p_avf %>% 
        dygraphs::dyAxis('y', axisLabelFormatter=JS(FUNC_JSFormatNumber))
    }
    
    # plot resids
    p_resids <- dygraphs::dygraph(model_obs_ts$.std.resid, group = "avf", main = "Residuals"#, ylab = "Standard Residuals"
    ) %>%
      dygraphs::dyBarChart() %>% 
      dygraphs::dyOptions(drawGrid = gridlines,
                          colors = mediacom_palette[6]) %>% 
      dygraphs::dyRangeSelector()
    
  }
  
  return(browsable(tagList(p_avf, p_resids)))
  
}
calc_vif <- function(model) {
  #calculate the VIF for each predictor variable in the model
  vif_df <- car::vif(model) %>% as.data.frame()
  
  tibble(variable = rownames(vif_df), value = vif_df$.) %>% 
    plotly::plot_ly(x = ~value, y = ~variable, type = "bar") %>% 
    layout(margin = list(l = 500), yaxis = list(title = "", automargin=T))
  
}

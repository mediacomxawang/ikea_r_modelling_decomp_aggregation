decomp_waterfall <- function(decomp_output, start){
  
  decomp_df <-  decomp_output@DecompedData
  
  decomp_df <- decomp_df %>% 
    group_by(modelled_vars) %>% 
    summarise(contributions = round(sum(contributions))) %>% 
    ungroup() %>%
    mutate(contributions = sort(contributions))
  
  end <- start + sum(decomp_df$contributions) 
  
  x <- c("start", decomp_df$modelled_vars, "end") 
  measure <- c("relative", rep("relative", length(decomp_df$modelled_vars)), "total") 
  y <- c(start, decomp_df$contributions, end)
  text <- round(decomp_df$contributions/sum(decomp_df$contributions), 2)*100
  text <- c(as.character(start), paste(as.character(text), "%", sep = ""), as.character(end))
  data <- data.frame(x=factor(x,levels=x),measure,text,y) 
  
  
  fig <- plot_ly(
    data, 
    name = "20", 
    type = "waterfall",
    #base = 0,
    measure = ~measure,
    x = ~x, 
    textposition = "outside", 
    y= ~y, 
    text =~text,
    connector = list(line = list(color= "rgb(63, 63, 63)")),
    decreasing = list(marker = list(color = "red")),
    increasing = (marker = list(color = "green")),
    totals = list(marker = list(color = "black"))
  ) 
  
  fig <- fig %>%
    layout(title = "Contribution",
           xaxis = list(title = ""),
           yaxis = list(title = ""),
           autosize = TRUE,
           showlegend = TRUE,
           
           shapes = list(
             list(type = "rect",
                  fillcolor = "black", line = list(color = "black"), opacity = 1,
                  x0 = -0.4, x1 = 0.4, xref = "x",
                  y0 = 0.0, y1 = start, yref = "y"))
    )
  
  return(fig)
  
}


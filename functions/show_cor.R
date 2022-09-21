show_cor <- function(model){
  
  res <- cor(model$model)
  res <- round(res, 2)
  rownames(res) <- substr(rownames(res), 1, 22)
  colnames(res) <- substr(colnames(res), 1, 22)
  corrplot::corrplot(res, type = "upper")
  
}
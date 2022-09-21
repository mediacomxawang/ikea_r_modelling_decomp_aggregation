clean_modelled_vars <- function(vars, keep_adstock = FALSE) {
  
  # TODO simplistic way to clean and get the first variable, needs more work 
  # get rid of lags, leads, logs, atan, I everything followed by a comma, +, - or /
  
  clean_vars <- gsub("I\\(|\\)|\\(|atan\\(|rollapply\\(|lag\\(|lead\\(|log\\(|\\,(.*)|\\/(.*)|\\d+\\s\\*\\s|\\d+\\.",
                     "", vars)
  
  if (keep_adstock == FALSE) {
    
    for (i in 1:length(clean_vars)){
      if(!grepl("^d_",clean_vars[i])){
        # clean again to get rid of adstocks for vars which were atan
        clean_vars[i] <- gsub("\\d{2}$", "", clean_vars[i])
      }
      
    }

  }
  
  # get rid of closing paranthesis
  clean_vars <- gsub("\\)", "", clean_vars)
  
  clean_vars <- trimws(clean_vars)
  
  # if (keep_adstock == FALSE) {
  #   # get rid of remaining adstock
  #   clean_vars <- gsub("\\d{2}$", "", clean_vars)
  # }
  
  return(clean_vars)
  
}

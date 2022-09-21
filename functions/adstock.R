

adstock <- function (data, date_var = "date", media_identifiers = c({{media_identifiers}}), 
                     adstock_rate = seq(0.05, 0.95, by = 0.05), 
                     groups = NULL) {
  if(is.null(groups)) {
    select_vars <- c(date_var, grep(paste(media_identifiers, collapse = "|"), 
                                    colnames(data), value = T))
  }
  else{
    select_vars <- c(date_var, grep(paste(c(media_identifiers, groups), collapse = "|"), 
                                    colnames(data), value = T))
  }
  if(length(select_vars) == 0) {
    stop("No variables have been found with the chosen identifiers")
  }
  if(is.null(groups)) {
    adstocked_vars <- data %>% dplyr::select_at(select_vars) %>%
      dplyr::distinct() %>% dplyr::group_split() %>% purrr::map_df(., function(dat) {
        purrr::map_dfc(colnames(dat), 
                       function(var_name) {
                         if(is.numeric(dat[[var_name]])){
                           purrr::map_dfc(adstock_rate, 
                                          function(rate) {
                                            print(glue::glue("Running variable {var_name} rate {rate}"))
                                            data.frame(as.numeric(stats::filter(dat[, var_name], filter = rate, method = "recursive"))) %>%
                                              dplyr::rename(`:=`(!!paste0(var_name, rate * 100), 1))
                                          })
                         }else{
                           return(setNames(dat[,var_name],nm=var_name))
                         }
                       })
      })
  }
  else{
    adstocked_vars <- data %>% dplyr::select_at(select_vars) %>%
      dplyr::distinct() %>% dplyr::group_by_at(groups) %>%  dplyr::group_split() %>% purrr::map_df(., function(dat) {
        purrr::map_dfc(colnames(dat), 
                       function(var_name) {
                         if(is.numeric(dat[[var_name]])){
                           purrr::map_dfc(adstock_rate, 
                                          function(rate) {
                                            print(glue::glue("Running variable {var_name} rate {rate}"))
                                            data.frame(as.numeric(stats::filter(dat[, var_name], filter = rate, method = "recursive"))) %>%
                                              rename(`:=`(!!paste0(var_name, rate * 100), 1))
                                          })
                         }else{
                           return(setNames(dat[,var_name],nm=var_name))
                         }
                       })
      }) %>%
      dplyr::ungroup()
  }
  return(adstocked_vars)
}

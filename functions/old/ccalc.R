#' Ccalc 
#' 
#' Calculates the coefficient, standard error, t-stat and p-value for selected 
#' variables as if they were to be entered into the given model 
#'
#' @param data A data frame which a. was passed to the modelling function and 
#' b. contains variables to be tested
#' @param vars_to_test A character vector of variable names to test in the model
#' @param model A model object as returned by `lm()`
#' @param lag_var A vector of lags to test in the model, e.g. `c(1,2)` to test 
#' variable lagged once and twice. Default NULL (turned off).
#' @return A data frame with input variables and their corresponding statistics
#' as if they were entered in the model one by one.
#' @examples
#' lmfit <- lm(mpg ~ wt, mtcars)
#' ccalc(mtcars, c("cyl", "disp"), lmfit)
#' ccalc(mtcars, c("cyl", "disp"), lmfit, lag_var = c(1,2))


ccalc <- function(data, vars_to_test, model, lag_var = NULL) {
  
  # find specified variables in data
  vars_found <- names(data)[names(data) %in% vars_to_test]
  
  # stop if vars_to_test not found in data
  if(sum(vars_to_test %in% names(data)) == 0) {
    stop(glue::glue('No variables found in data'))
  }
  
  # warn if vars_to_test not found in data
  if(sum(vars_to_test %in% names(data)) < length(vars_to_test)) {
    missing_vars <- vars_to_test[!vars_to_test %in% names(data)]
    warning(glue::glue('No variable called "{missing_vars}" has been found in data'))
  }
  
  # create data frame with all variables to test in the model
  test_df <- data %>% 
    dplyr::select_(.dots = dplyr::all_of(vars_found))
  
  # create lagged vars if lag_var is not NULL
  if (!is.null(lag_var)) {
    
    # lag all variables
    test_df_lag <- purrr::map(lag_var, 
                              function(x) {
                                test_df %>% 
                                  purrr::map(~dplyr::lag(., x)) %>% # takes each column and lags it, returns a list of columns
                                  dplyr::bind_cols() %>% # binds a list of columns to a data frame
                                  setNames(glue::glue("{names(test_df)}_lag{x}")) # renames columns to reflect lag
                              }) %>% 
      dplyr::bind_cols()
    
    # attach lagged variables to main dataframe
    test_df <- dplyr::bind_cols(test_df, test_df_lag)
    data <- dplyr::bind_cols(data, test_df_lag)
    
  }
  
  # get all model coefs
  model_coefs <- broom::tidy(model) %>% dplyr::pull(term)
  
  # get rid of intercept
  model_coefs <- model_coefs[-1]
  
  # get dep var
  model_eq <- model$call$formula
  dep_var <- model_eq[2] %>% as.character()
  
  # test all vars found in data
  # remove variables containing only NA values
  test_df <- test_df[, colSums(is.na(test_df)) != nrow(test_df)]
  browser()
  
  new_coefs <- NULL
  
  for (i in names(test_df)){
    new_model_eq <- glue::glue("{dep_var} ~ {paste(model_coefs, collapse = '+')} + {i}")
    test_model <- lm(as.formula(new_model_eq), data = data)
    
    test_model_stat <- broom::tidy(test_model)
    
    t_stat_on_last_model <- test_model_stat[nrow(test_model_stat)-1,c(2,4)] %>% 
      rename("t.stat.on.last.variable" = statistic,
             "coeff.on.last.variable" = estimate)
    
    DW <- as.data.frame(car::durbinWatsonTest(test_model)$dw) %>% 
      rename("Durbin.Watson" = 'car::durbinWatsonTest(test_model)$dw')    
    
    t_stat_on_test_variable <- tail(test_model_stat,n=1) %>% 
      cbind(.,t_stat_on_last_model) %>% 
      cbind(.,DW)
    
    new_coefs <- rbind(new_coefs,t_stat_on_test_variable)
  }
  
  # order dataframe alphabetically according to variable names
  new_coefs <- new_coefs[order(new_coefs$term),] 
  
  # removes variables already in the main model
  new_coefs <- new_coefs %>% 
    dplyr::filter(!(term %in% model_coefs))
  
  return(new_coefs)
  
} 

#' Display ccalc output 
#' 
#' Displays formatted `ccalc` output in Viewer pane 
#'
#' @param ccalc_data A data frame as returned by `ccalc()`
#' @return A formatted and colour-coded ccalc output in HTML format.
#' @examples
#' lmfit <- lm(mpg ~ wt, mtcars)
#' ccalc(mtcars, c("cyl", "disp"), lmfit, lag_var = c(1,2)) %>% show_ccalc()

show_ccalc <- function(ccalc_data) {
  
  DT::datatable(ccalc_data, class = "compact - hover",
                colnames = c('term', 'estimate', 'std.error', 't-stat', 'p.value','coeff.on.last.variable','t.stat.on.last.variable','Durbin.Watson'),
                options = list(dom = 't', scrollY = FALSE, pageLength = 5000,
                               columnDefs = list(list(
                                 targets = 0,
                                 render = JS(
                                   "function(data, type, row, meta) {",
                                   "return type === 'display' && data.length > 60 ?",
                                   "'<span title=\"' + data + '\">' + data.substr(0, 60) + '...</span>' : data;",
                                   "}")
                               ))), rownames = FALSE, callback = JS('table.page(1).draw(false);')) %>% 
    DT::formatRound(c('estimate','coeff.on.last.variable'), 4) %>% 
    DT::formatRound(c('std.error', 'statistic', 'p.value','t.stat.on.last.variable','Durbin.Watson'), 3) %>% 
    DT::formatStyle('estimate',
                    color = DT::styleInterval(0, c("#FF0000B3", "#00A600B3"))) %>%
    DT::formatStyle('statistic',
                    fontWeight = DT::styleInterval(c(-1.96, 0, 1.96),
                                                   c("bold", "normal", "normal", "bold")))
  
}

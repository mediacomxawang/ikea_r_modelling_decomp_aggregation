#' Display a summary of model statistics
#'
#' @param model A model created using `lm()`
#' @return Returns formatted model statistics in Viewer pane
#' @examples
#' lmfit <- lm(mpg ~ wt, mtcars)
#' show_stats(lmfit)

show_stats <- function(model, date_var = NULL) {
 
  # create table containing model observations
  model_obs <- broom::augment(model)
  
  # get model sample dates
  if (!is.null(date_var)) {
    
    # check if .rownames present as a first column
    # if yes, some obs were dropped by lm fn because they were NAs
    if (names(model_obs)[1] == ".rownames") {
      
      # create new date var
      date_var <- date_var[model_obs[1] %>% dplyr::pull() %>% as.numeric()]
      
    }
    
    min_date <- min(date_var)
    max_date <- max(date_var)
    
  }
  
  if (names(model_obs)[1] == ".rownames") {
    model_obs <- model_obs %>% 
      dplyr::select(-.rownames)
  }
  
  # get dep var name
  dep_var <- names(model_obs)[1]
  
  # get VIF values
  vif_df <- car::vif(model) %>% as.data.frame()
  
  # tidy VIF values
  vif_tidy <- tibble(term = rownames(vif_df), VIF = vif_df$.)
  
  # add VIF values to model stats
  # this will also ensure there is a WARNING displayed
  # if perfect multicollinearity exists in the model
  mod_coeffs <- broom::tidy(model) %>% 
    dplyr::left_join(vif_tidy, by = "term")
  
  mod_coef_tbl <- datatable(mod_coeffs, class = "compact - hover",
                            colnames = c('term', 'estimate', 'std.error', 't-stat', 'p.value', 'VIF'),
                            options = list(dom = 't', scrollY = FALSE, pageLength = 500,
                                           columnDefs = list(list(
                                             targets = 0,
                                             render = JS(
                                               "function(data, type, row, meta) {",
                                               "return type === 'display' && data.length > 60 ?",
                                               "'<span title=\"' + data + '\">' + data.substr(0, 60) + '...</span>' : data;",
                                               "}")
                                           ))), rownames = FALSE, callback = JS('table.page(1).draw(false);')) %>% 
    formatRound('estimate', 4) %>% 
    formatRound(c('std.error', 'statistic', 'p.value', 'VIF'), 2) %>% 
    formatStyle('estimate', 
                color = styleInterval(c(0),
                                      c("#FF0000B3", "#00A600B3"))) %>%
    formatStyle('estimate', 'statistic',
                fontWeight = styleInterval(c(-1.96, 0, 1.96),
                                           c("bold", "normal", "normal", "bold"))) %>%
    formatStyle('statistic', 
                fontWeight = styleInterval(c(-1.96, 0, 1.96),
                                           c("bold", "normal", "normal", "bold")))
  
  # get model statistics
  
  # Durbin-Watson test
  DW <- car::durbinWatsonTest(model)$dw

  # White test statistic
  white_test <- skedastic::white_lm(model, interactions = FALSE, statonly = FALSE) %>% 
    dplyr::pull(p.value)
  
  # Jarque Bera test
  jarque_bera <- augment(model) %>%
    pull(".std.resid") %>% # drop everything except model variables
    tidyr::replace_na(0) %>% # replace NA otherwise test won't run
    tseries::jarque.bera.test()
  
  model_stats <- broom::glance(model) %>% 
    bind_cols(tibble(`Durbin-Watson` = DW,
                     `White test` = white_test,
                     `Jarque-Bera` = jarque_bera$p.value)) %>% 
    rename(F.statistic = statistic)
  
  mod_stat1_tbl <- datatable(model_stats[1:8], class = "compact", 
                             options = list(dom = 't'), rownames = FALSE) %>%
    formatRound(names(model_stats[c(1:5,7:8)]), 2)
  
  mod_stat2_tbl <- datatable(model_stats[9:15], class = "compact",
                             options = list(dom = 't'), rownames = FALSE) %>%
    formatRound(names(model_stats[c(9:10, 13:15)]), 2)
  
  if (!is.null(date_var)) {
    sample_dates <- tagList(list(
      tags$br(),
      glue::glue("Sample: {min_date} to {max_date}"))) 
  } else {
    sample_dates <- tagList(list())
  }
  
  browsable(
    tagList(list(
      glue::glue("Dependent variable: {dep_var}"),
      sample_dates,
      tags$br(),
      tags$br(),
      tags$div(
        mod_coef_tbl,
        style = 'width:95%; height:auto; margin-bottom: 30px;'
      ),
      tags$div(
        mod_stat1_tbl,
        style = 'width:70%; height:80px'),
      tags$div(
        mod_stat2_tbl,
        style = 'width:70%; height:80px')
    )
    )
  )
  
}


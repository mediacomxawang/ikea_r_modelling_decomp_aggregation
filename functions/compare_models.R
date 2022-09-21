
compare_models <- function(model1, model2, model1_name = NULL, model2_name = NULL) {
  
  # assign model 1 name if not specified
  if (is.null(model1_name)) {
    model1_name <- "Model 1"
  }
  
  # assign model 2 name if not specified
  if (is.null(model2_name)) {
    model2_name <- "Model 2"
  }
  
  # get model coeffs
  m1 <- tidy(model1) %>% select(-std.error, -p.value)
  m2 <- tidy(model2) %>% select(-std.error, -p.value)
  
  # full join of model coeffs
  m_joined <- full_join(m1, m2, by = "term")  
  
  m_full <- bind_rows(
    m_joined %>% filter(!is.na(estimate.x)&!is.na(estimate.y)), # gets matching coeffs
    m_joined %>% filter(is.na(estimate.x)|is.na(estimate.y)) # gets coeffs which differ
  ) %>% 
    mutate(pct_coeff_change = estimate.y/estimate.x-1) %>% # calculate pct change in coeffs
    mutate(across(starts_with("estimate"), round, 4)) %>% 
    mutate(across(starts_with("statistic"), round, 2)) %>% 
    mutate(across(starts_with("p"), round, 2)) %>% 
    select(term, estimate.x, statistic.x, pct_coeff_change, estimate.y, statistic.y)
  
  
  # create custom table container (https://rstudio.github.io/DT/)
  sketch = htmltools::withTags(table(
    class = 'compact - hover',
    thead(
      tr(
        th(rowspan = 2, ''),
        th(colspan = 2, model1_name),
        th(rowspan = 2, '% coeff change'),
        th(colspan = 2, model2_name)
      ),
      tr(
        lapply(rep(c('estimate', 'statistic'), 2), th)
      )
    )
  ))
  
  # comparison table of model coeffs and t-stats
  mod_coef_tbl <- datatable(m_full, class = "compact - hover", container = sketch,
                            options = list(dom = 't', scrollY = FALSE, scrollX = FALSE, pageLength = 500, autoWidth = TRUE,
                                           columnDefs = list(list(
                                             targets = 0,
                                             render = JS(
                                               "function(data, type, row, meta) {",
                                               "return type === 'display' && data.length > 60 ?",
                                               "'<span title=\"' + data + '\">' + data.substr(0, 60) + '...</span>' : data;",
                                               "}")
                                           ))), rownames = FALSE, callback = JS('table.page(1).draw(false);')) %>% 
    formatStyle('estimate.x', 
                color = styleInterval(c(0),
                                      c("#FF0000B3", "#00A600B3"))) %>%
    formatStyle('estimate.y', 
                color = styleInterval(c(0),
                                      c("#FF0000B3", "#00A600B3"))) %>%
    formatStyle('estimate.x', 'statistic.x',
                fontWeight = styleInterval(c(-1.96, 0, 1.96),
                                           c("bold", "normal", "normal", "bold"))) %>%
    formatStyle('estimate.y', 'statistic.y',
                fontWeight = styleInterval(c(-1.96, 0, 1.96),
                                           c("bold", "normal", "normal", "bold"))) %>%
    formatStyle('statistic.x',  
                fontWeight = styleInterval(c(-1.96, 0, 1.96), 
                                           c("bold", "normal", "normal", "bold"))) %>% 
    formatStyle('statistic.y',  
                fontWeight = styleInterval(c(-1.96, 0, 1.96), 
                                           c("bold", "normal", "normal", "bold"))) %>% 
    formatStyle('pct_coeff_change', 
                color = styleInterval(c(0),
                                      c("#FF0000B3", "#00A600B3"))) %>% 
    formatPercentage('pct_coeff_change', 1)
  
  # placeholder comparison of Rsq
  stats1 <- broom::glance(model1) %>% select(r.squared, adj.r.squared) %>% 
    bind_cols(tibble(DW = car::durbinWatsonTest(model1)$dw,
                     model = model1_name))
  
  stats2 <- broom::glance(model2) %>% select(r.squared, adj.r.squared) %>% 
    bind_cols(tibble(DW = car::durbinWatsonTest(model2)$dw,
                     model = model2_name))
  
  stats_joined <- bind_rows(stats1, stats2) %>% 
    gather(statistic, value, -model) %>% 
    mutate(value = round(value, 2)) %>% 
    spread(model, value) %>% 
    datatable(class = "compact - hover", rownames = FALSE, options = list(dom = 't'))
  
  # bind both tables together in Viewer
  browsable(
    tagList(list(
      tags$div(
        mod_coef_tbl,
        style = 'width:100%; height:auto; margin-bottom: 30px;'
      ),
      tags$div(
        stats_joined,
        style = 'width:30%; height:80px')
      )
    )
  )
  
}

# compare_models <- function(models) {
#   
#   comp_stats <- purrr::map(models, broom::glance) %>% 
#     dplyr::bind_rows() %>% 
#     mutate(model_no = 1:length(models))
#   
#   comp_coefs <- purrr::map2(models, 1:length(models), 
#                             ~ broom::tidy(.x) %>% 
#                               dplyr::mutate(model_no = glue::glue("model_{.y}"))) %>% 
#     dplyr::bind_rows() %>% 
#     dplyr::select(-p.value, -std.error) %>% 
#     tidyr::gather(metric, value, -term, -model_no) %>% 
#     tidyr::spread(model_no, value)
#   
#   return(list(stats = comp_stats,
#               coefs = comp_coefs))
#   
# }


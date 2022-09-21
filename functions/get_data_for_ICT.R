get_data_for_ICT <- function(model, model_data, decomp_output, brand, product, tbl_map, tbl_media_spend, save_to) {
  
  # save_to <- "updates/202206/outputs/ICT inputs"
  # 
  # model <- eq_dl_h_w_tm1
  # 
  # model_data <- tbl_model
  # 
  # brand <- "DL"
  # 
  # product = "Home_Web"
  # 
  # tbl_map <- tbl_map
  
  if (!dir.exists(glue::glue("{save_to}/{brand}_{product}"))) {
    dir.create(glue::glue("{save_to}/{brand}_{product}"))
  } 
  
  date_var <- model_data$date
  
  spend_data <- tbl_media_spend %>%  
    select(date, variable, value) %>% 
    rename(spend = value) %>% 
    mutate(variable = gsub('_sp$', '', variable))
  
  # ICT_KPI ---------------------------------------------------------------------
  
  # create table containing model observations
  model_obs <- broom::augment(model)
  
  # check if .rownames present as a first column
  if (names(model_obs)[1] == ".rownames") {
    
    date_var <- date_var[model_obs[1] %>% pull() %>% as.numeric()]
    
    model_obs <- model_obs %>% 
      dplyr::select(-.rownames)
    
  }
  
  # get dep var name
  dep_var <- names(model_obs)[1]
  
  dep_var <- clean_modelled_vars(dep_var)
  
  no_of_obs <- nrow(model_obs)
  no_of_vars <- nrow(decomp_output@SetupData)
  
  # model variables mapped
  decomp_long <- decomp_output@DecompedData %>%
    mutate(actual.vars = clean_modelled_vars(modelled_vars)) %>% 
    left_join(select(tbl_map, actual.vars, Group_BaseBreakDown_MediaBreakDown_Price, `product/category_b`,
                     `channel/category_c`, `platform/category_d`,
                     `proposition/category_e`, `creative/category_f`), by = "actual.vars") %>% 
    mutate(decomp_group = stringr::str_to_title(group),
           media_name = ifelse(decomp_group == 'Media',
                               glue::glue('{tolower(`product/category_b`)}_{tolower(`channel/category_c`)}_{tolower(`platform/category_d`)}_{tolower(`proposition/category_e`)}_{tolower(`creative/category_f`)}'),
                               NA),
           map_name = ifelse(decomp_group == 'Media', 
                             paste0('Media_', gsub(' ', '_', media_name)), 
                             stringr::str_to_title(Group_BaseBreakDown_MediaBreakDown_Price))) %>% # can have a lookup to another grouping
    select(date, modelled_vars, actual.vars, decomp_group, contributions, map_name)
  
  # correct incorrect mappings Group_BaseBreakDown_MediaBreakDown_Price - Group
  decomp_long <- decomp_long %>% 
    mutate(decomp_group = ifelse(map_name == "Base", "Base", decomp_group))
  
  if (decomp_long %>% group_by(modelled_vars) %>% summarise(obs = n()) %>% pull(obs) %>% mean() != no_of_obs) {
    stop('Some observations are missing from decomped data')
  }
  
  if(sum(is.na(decomp_long$contributions)) > 0) {
    stop('Decomped data contains missing values')
  }
  
  decomp_mapped <- decomp_long %>% 
    distinct(modelled_vars, actual.vars, decomp_group, map_name)
  
  if (nrow(decomp_mapped) != no_of_vars) {
    stop('Some variables are missing from mapping table')
  }
  
  # csv ICT_KPIMapping ----------------------------------------------------------
  ict_kpi_map <- tibble(modelled_var = dep_var) %>% 
    left_join(read_csv("maps/ICT_KPIMapping.csv")) %>% 
    dplyr::relocate(modelled_var, .after = last_col()) %>% 
    mutate(#model_type = ifelse(substr(dep_var,1,3) == "log", "log", "linear"),
      model_type = ifelse(substr(model$terms[[2]][[1]],1,3) == "log", "log", "linear")) # temp solution due to error in map table)
  
  if (is.na(ict_kpi_map$kpi_id)) {
    stop('KPI is missing from the KPI mapping table. Please update maps/ICT_KPIMapping.csv')
  }
  
  write_csv(ict_kpi_map, glue::glue("{save_to}/{brand}_{product}/ICT_KPIMapping.csv"))
  
  # csv ICT_KPI -----------------------------------------------------------------
  tibble(date = format(decomp_output@KPIData$date, format = "%Y-%m-%d"),
         value = exp(decomp_output@KPIData$y)) %>% # TODO assumes var in log form 
    mutate(country = "UK",
           region = "National",
           sub_region = "National",
           variable_group = "Base", # always Base
           variable_name = dep_var) %>% 
    select(date, country, region, sub_region, variable_group, variable_name, value) %>% 
    write_csv(glue::glue("{save_to}/{brand}_{product}/ICT_KPI.csv"))
  
  # csv ICT_ModelDecomposition --------------------------------------------------
  decomp_long %>%  
    mutate(refresh_date = Sys.Date(),
           kpi_id = dep_var,
           country = "UK",
           region = "National",
           sub_region = "National",
           value = contributions) %>% 
    select(refresh_date, kpi_id, date, country, region, sub_region, decomp_group, map_name, value) %>% 
    group_by(refresh_date, kpi_id, date, country, region, sub_region, decomp_group, map_name) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    # distinct(map_name) %>% pull()
    write_csv(glue::glue("{save_to}/{brand}_{product}/ICT_ModelDecomposition.csv"))
  
  # csv ICT_ModelVariables -------------------------------------------------------
  decomp_output@ModelCoeffs %>% 
    mutate(variable_name = variable,
           variable_clean_name = clean_modelled_vars(variable), #replace
           kpi_name = dep_var,
           coef_mu = coeffs,
           coef_sd = NA,
           adstock_mu = gsub("\\,(.*)|\\/(.*)|\\+(.*)|\\-(.*)|\\*(.*)|atan\\(|lag\\(|lead\\(|log\\(|I\\(|\\)", "", variable),
           adstock_mu = str_match(adstock_mu, "\\d{2}$")[,1],
           adstock_sd = NA,
           atan_mu = ifelse(substr(variable, 1, 5) == "atan(", gsub("(.*)\\/|\\)","", variable), NA),
           atan_sd = NA,
           transform = ifelse(grepl("log", variable), "log", NA),
           transform = ifelse(grepl("lead", variable), "lead", transform),
           key_factor_flag = 0,
           key_factor_pos_sign = 0,
           key_factor_icon = NA
    ) %>% select(-variable, -coeffs) %>% 
    mutate(variable_name = ifelse(substr(variable_clean_name, 1, 2) == 'm_', variable_clean_name, variable_name)) %>% 
    left_join(decomp_mapped, by = c('variable_clean_name' = 'actual.vars')) %>% 
    distinct(variable_name, coef_mu, .keep_all = TRUE) %>% 
    mutate(variable_name = ifelse(decomp_group == 'Media', map_name, variable_name),
           variable_clean_name = ifelse(decomp_group == 'Media', 
                                        gsub('Media_', '', variable_name), variable_clean_name)) %>% 
    select(-modelled_vars, -decomp_group, -map_name) %>% 
    write_csv(glue::glue("{save_to}/{brand}_{product}/ICT_ModelVariables.csv"))
  
  
  # csv ICT_ModelCoefficients ----------------------------------------------------
  model_coeff <- tibble(refresh_date = Sys.Date(),
                        kpi_id = dep_var,
                        country = "UK",
                        region = "National",
                        sub_region = "National",
                        #variable_name = clean_modelled_vars(decomp_output@ModelCoeffs$variable),
                        variable_name = decomp_output@ModelCoeffs$variable,
                        variable_type = ifelse(variable_name == "intercept", "Other", "Coefficient"),
                        stan_variable = NA,
                        value = decomp_output@ModelCoeffs$coeffs,
                        se_mean = NA,
                        sd = NA,
                        `2.50%` = NA,
                        `25%` = NA,
                        `50%` = NA,
                        `75%` = NA,
                        `90%` = NA,
                        `97.50%` = NA,
                        n_eff = NA,
                        Rhat = NA,
                        variable = decomp_output@ModelCoeffs$variable) %>% 
    # left_join(decomp_mapped, by = c('variable_name' = 'actual.vars')) %>% 
    left_join(decomp_mapped, by = c('variable_name' = 'modelled_vars')) %>% 
    mutate(variable_name = ifelse(decomp_group == 'Media', map_name, variable_name)) %>% 
    select(-actual.vars, -decomp_group, -map_name) 
  
  model_coeff_adstock <- model_coeff %>% 
    filter(substr(variable_name, 1, 6) == 'Media_') %>% 
    mutate(variable_type = "Adstock") %>% 
    mutate(value = gsub("\\,(.*)|\\/(.*)|\\+(.*)|\\-(.*)|\\*(.*)|atan\\(|lag\\(|lead\\(|log\\(|I\\(|\\)", "", variable)) %>% 
    mutate(value = str_match(value, "\\d{2}$")[,1]) %>% 
    mutate(value = ifelse(is.na(value), 0, as.numeric(value)))
  
  model_coeff_atan <- model_coeff %>% 
    filter(substr(variable_name, 1, 6) == 'Media_') %>% 
    mutate(variable_type = "Atan") %>% 
    mutate(value = ifelse(substr(variable, 1, 5) == "atan(", as.numeric(gsub("(.*)\\/|\\)","", variable)), 1))
  
  model_coeff_final <- bind_rows(model_coeff, model_coeff_adstock, model_coeff_atan) %>% 
    select(-variable)
  
  write_csv(model_coeff_final, glue::glue("{save_to}/{brand}_{product}/ICT_ModelCoefficients.csv"))
  
  # csv ICT_MappingTable ------------------------------------------------------
  tibble(variable_name = decomp_output@ModelCoeffs$variable) %>% 
    left_join(select(decomp_output@SetupData, modelled_vars, ref_points), by = c("variable_name" = "modelled_vars")) %>% 
    left_join(decomp_mapped, by = c("variable_name" = "modelled_vars")) %>% 
    mutate(frequency = "Weekly",
           reference_point = ref_points,
           agg_method = "Sum", # lookup
           variable_name = ifelse(decomp_group == "Media", map_name, variable_name),
           decomp_group = ifelse(variable_name == "d_300320_covid", "Covid_dummies", decomp_group)) %>% 
    select(variable_name, frequency, reference_point, agg_method, decomp_group, map_name) %>%
    write_csv(glue::glue("{save_to}/{brand}_{product}/ICT_MappingTable.csv"))
  
  
  # csv ICT_MediaData -----------------------------------------------------------
  
  media_data <- model_data %>% 
    select(date, starts_with("m_")) %>% 
    gather(variable_name, value, -date) %>% 
    filter(variable_name %in% clean_modelled_vars(decomp_output@ModelCoeffs$variable)) %>% 
    left_join(tbl_map, by = c("variable_name" = "actual.vars")) %>% 
    mutate(variable_group = stringr::str_to_title(group),
           media_group = tolower(Group_Base_MediaBreakDown_Price),
           country = "UK",
           region = "National",
           sub_region = "National",
           var_to_match_spend = gsub('_[a-z]+$', '', variable_name),
           media_name = glue::glue('{tolower(`product/category_b`)}_{tolower(`channel/category_c`)}_{tolower(`platform/category_d`)}_{tolower(`proposition/category_e`)}_{tolower(`creative/category_f`)}'),
           variable_name = gsub(' ', '_', media_name)) %>% 
    left_join(spend_data, by = c("date" = "date", "var_to_match_spend" = "variable")) %>% 
    select(date, country, region, sub_region, variable_group, media_group, variable_name, value, spend) %>% 
    mutate(date = as.character(format(date, format = "%Y-%m-%d")))
  
  unmatched_spend <- media_data %>% 
    filter(is.na(spend)) %>% 
    distinct(variable_name) %>% pull()
  
  if (length(unmatched_spend) > 0) {
    stop(glue::glue("Spend could not be matched to the following variables: {paste0(unmatched_spend, collapse = ', ')}"))
  }
  
  write_csv(media_data, glue::glue("{save_to}/{brand}_{product}/ICT_MediaData.csv"))
  
  # csv ICT_MediaMapping
  media_data %>% 
    distinct(variable_name, media_group) %>% 
    mutate(media_channel_id = 1:n(),
           variable_name = paste0('Media_', variable_name),
           media_clean_name = gsub('Media_', '', variable_name),
           media_clean_name = gsub('_', ' ', media_clean_name),
           campaign_name = "Total Media",
           media_splits = "Media Channel") %>%
    select(media_channel_id, variable_name, media_clean_name, campaign_name, media_splits) %>% 
    write_csv(glue::glue("{save_to}/{brand}_{product}/ICT_MediaMapping.csv"))
  
  # csv ICT_CustomVars
  decomp_output@ModelDataWide %>% 
    mutate(date = as.character(format(date_var, format = "%Y-%m-%d"))) %>%
    select(-2) %>% # drop intercept
    gather(variable_name, value, -date) %>%
    mutate(country = 'UK',
           region = 'National',
           sub_region = 'National',
           actual.vars = clean_modelled_vars(variable_name)) %>%
    left_join(select(tbl_map, actual.vars, group), by = 'actual.vars') %>%
    rename(variable_group = group) %>% 
    mutate(variable_group = stringr::str_to_title(variable_group)) %>% 
    select(-actual.vars) %>% 
    filter(variable_group != "Media") %>% 
    write_csv(glue::glue("{save_to}/{brand}_{product}/ICT_CustomVars.csv"))
  
  # csv ICT_Snapshots -----------------------------------------------------------
  
  #create data frame with 0 rows and 3 columns
  snapshots <- data.frame(matrix(ncol = 4, nrow = 0))
  
  #provide column names
  colnames(snapshots) <- c('snapshot_id', 'snapshot_name', 'start_date', 'end_date')
  
  # tibble(snapshot_id = "",
  #        snapshot_name = "",
  #        start_date = "",
  #        end_date = "") %>% 
  
  write_csv(snapshots, glue::glue("{save_to}/{brand}_{product}/ICT_Snapshots.csv"))
  
  # # zip files
  # # Read the 2 CSV file names from working directory
  # Zip_Files <- list.files(path = glue::glue("{save_to}/{brand}_{product}"), pattern = ".csv$", full.names = TRUE)
  # 
  # # Zip the files and place the zipped file in working directory
  # zip::zip(zipfile = glue::glue("{save_to}/{brand}_{product}"), files = Zip_Files)
  
}
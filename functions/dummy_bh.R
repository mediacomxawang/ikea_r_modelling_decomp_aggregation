

dummy_bh <- function(df){
  
  df_start_date <- as.Date(df$date[1])
  df_end_date <- as.Date(df$date[nrow(df)]) + 6
  
  # pull the dummies fron the bigQuery database
  dummy_df <- dummy_bh_pull(df_start_date, df_end_date)
  
  # format the data
  dummy_df <- dummy_df %>% 
    rename(date = Date) %>% 
    select(-upload_time) %>%  
    mutate(variable = paste("dummy_", tolower(variable), sep = ""))
  
  date_col <- df %>% 
    select(date) %>% 
    mutate(range = row_number())
  
  tmp_df <-  data.frame(date = seq.Date(df_start_date, df_end_date, by = "days"))
  
  tmp_df <- tmp_df %>% 
    left_join(date_col, by = "date") %>% 
    mutate(range = zoo::na.locf(range)) 
  
  dummy_df <- dummy_df %>% 
    left_join(tmp_df) %>% 
    select(-date) %>% 
    spread(variable, value)
  
  output_df <- date_col %>% 
    left_join(dummy_df, by = "range") %>% 
    select(-range) %>% 
    mutate(across(where(is.numeric), ~if_else(is.na(.x), 0, .x)))
  
  return(output_df)
  
}


dummy_bh_pull <- function(start_date, end_date){
  
  gcp_project <- "apt-vine-260114"
  gcp_schema <- "seasonality"
  gcp_table <- "seasonality_data"
  
  # Set GCP project ID
  # mediacomGCP::gcp_project_set(gcp_project)
  
  
  
  # Connect to bigQuery database
  
  con <- DBI::dbConnect(
    bigrquery::bigquery(),
    project = gcp_project,
    dataset = gcp_schema,
    billing = gcp_project
  )
  
  
  
  # read in data
  tbl_bh_dummies <- DBI::dbGetQuery(con, 
                                    glue::glue("select * from {gcp_project}.{gcp_schema}.{gcp_table} WHERE date between '{start_date}' and '{end_date}'"))
  
  
  # # Close the database connections
  DBI::dbDisconnect(con)
  
  bh_df <- tbl_bh_dummies %>% 
    filter(grepl("^day|^bh", variable, ignore.case = TRUE))
  
  return(bh_df)
  
}



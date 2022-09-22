library(dplyr)
library(readr)
library(tidyr)
library(broom)
library(tidyverse)
library(purrr)
library(dygraphs)
library(formattable)

options("scipen" = 100,"digits"=4) # force NOT to write any numbers in scientific format

setwd('C:/Users/Anna.Wang/Documents/R Modelling/IKEA Pilot US3 Example')
source('functions/source_all_functions.R')

# Main mapping table 
path_to_map <- c('updates/Texas/models/_ss/tbl_mapping_dl_TX.csv')

# Assembled modeling file
path_to_mmm <- c('data/assembled.csv')

# set main data set time range
min_date <- '2018-01-07'
max_date <- '2021-08-22'


# load mapping table, add a row that indicates the intercept - this can be added in the mapping table directly
tbl_map <- read_csv(path_to_map) %>% 
  add_row(actual.vars = "intercept",
          Group_Base_Media_Price = "Base",
          Group_Base_MediaBreakDown_Price = "Base",
          Group_BaseBreakDown_MediaBreakDown_Price = "Base",
          Group_Modeller = "Base",
          Group_StackedBar = "Base",
          group = "Base",
          ref_points = "none",
          agg_method = "sum",
          variable_display = "Intercept",
          isforecastable = "yes")

if (length(unique(tbl_map$actual.vars))!=nrow(tbl_map)){
  stop(glue::glue('Check tbl_map! There is duplicate values in actual.vars.'))
}

# load main data set
tbl_data_raw <- read_csv(path_to_mmm) %>% 
  mutate(date = as.Date(date,"%d/%m/%Y")) %>% 
  left_join(read_csv("data/Extra dummies.csv",col_types = cols(date = col_date(format = "%d/%m/%Y"))), by ='date') %>%
  left_join(read_csv("data/addition4.csv",col_types = cols(date = col_date(format = "%d/%m/%Y"))), by ='date') %>%
  left_join(read_csv("data/addition2.csv",col_types = cols(date = col_date(format = "%d/%m/%Y"))), by ='date') %>%
  filter(date >= min_date & date <= max_date)

# create media spend table
tbl_media_spend <- create_media_spends_table(tbl_data_raw) %>% 
  select(date, variable, value)



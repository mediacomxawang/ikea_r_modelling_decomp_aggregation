library(tidyverse)
library(broom)
library(DT)
library(dygraphs)
library(plotly)
library(car)
library(zoo)
library(htmltools)
library(utils) # for export function
# library(mediacomModel)

mediacom_palette <- c("#e1005d", "#3487bb", "#ee7707", "#363061", "#3bb392",
                      "#D9D9D9", "#4a4a49", "#fff381", "#a2c617", "#97305f")

wc_mon <- function(datevar) {
  
  dow <- as.POSIXlt(datevar)$wday -1
  dow[dow== -1] <- 6
  wcm <- datevar - dow
  wcm
  
}

source("functions/adstock.R")
# source("functions/varmap.R")
source("functions/plots.R")
source("functions/plot_line.R")
source("functions/plot_actual_vs_fitted.R")
source("functions/plot_cpa.R")
source("functions/plot_curve.R")
source("functions/decomp_group_by_custom_dates.R")
source("functions/decomp_group_by_grouping_column.R")
source("functions/plot_waterfall.R")
source("functions/plot_decomp.R")
source("functions/compare_models.R")
source("functions/clean_modelled_vars.R")
source("functions/decomp.R")
source("functions/decomp_linear.R")
source("functions/vif.R")
source("functions/decomp_export.R")
# source("functions/whats_new.R")
source("functions/show_cor.R")
source("functions/dummy.R")
source("functions/calculate_media_timeperiod.R")
source("functions/get_data_for_cico.R")
source("functions/add_missing_vars_to_map_tbl.R")
source("functions/get_data_for_ICT.R")
source("functions/get_data_for_rmd.R")
source("functions/get_decomp_with_cico.R")
source("functions/get_data_for_waterfall.R")
source("functions/export_waterfalls.R")
source("functions/rename_media_halos.R")
source("functions/create_media_spends_table.R")
source("functions/get_sales_decomp.R")
source("functions/calculate_incrementality.R")
source('updates/us3_tx_pilot/models/setup.R')

# specify model's date range period 
min_date_model <- '2018-03-18' # need to be adjusted with the max lag you use in the model 
max_date_model <- '2021-08-22'

# ------------------------------------------------------------------------------------------------
# Variable creation -----------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

# create dummies (optional - more examples in readme)
dummy_seasonal <- dummy_weekly_data(tbl_data_raw) # year, month, week, fiscal year, tertial dummies

# attach dummies to main data
tbl_data_raw <- tbl_data_raw %>% 
  left_join(dummy_seasonal, by = 'date')

tbl_data_raw$serv_inc_del40k <- ifelse(tbl_data_raw$serv_inc_del>40000, 1, 0)
tbl_data_raw$sa_down_tot <- tbl_data_raw$sa_down_tot+1
# create adstocks - create adstock for media, family media
grep('(_sp|_im|_cl|_grp|_egrp|_vo)$|^fammed_.*(_sp|_vol|_cl|_sent)|^fam_.', names(tbl_data_raw), value = TRUE)
tbl_adstocks <- adstock(tbl_data_raw, date_var = 'date', media_identifiers = '(_sp|_im|_cl|_grp|_egrp|_vo)$|^fammed_.*(_sp|_vol|_cl|_sent)|^fam_.')

# define data set used for specific model, 
# i.e. a limited version of the main data set based on KPI availability data
tbl_model <- tbl_data_raw %>% 
  filter(date >= min_date_model & date <= max_date_model) %>% 
  left_join(tbl_adstocks, by = 'date')


# check all variables are in mapping table,
# populate if not
# d_, dummy_, bh_, payday_, "covid" and ww_ variable are auto mapped
tbl_map <- add_missing_vars_to_map_tbl(tbl_data_raw, tbl_map)

# # rename media halos
# tbl_map <- rename_media_halos(tbl_map, brand = brand, product = product)

# ------------------------------------------------------------------------------------------------
# Visual Analysis --------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

# plot variables (date, tibble(var1, var2, varn))
# plot_line(tbl_model$date, tibble(tbl_model$kpi_ss_tx_na, tbl_model$m_snap_tot_im), y2 = TRUE,
#           title = 'Store Sales Against Media', graphlib = 'plotly')
# plot_line(tbl_model$date, tibble(tbl_model$fam_dm_vol70))
# plot_line(tbl_model$date, tibble(tbl_model$m_dl_h_brtv_tot_all_all_sp, tbl_model$m_dl_h_drtv_tot_all_all_sp))
# # turn off secondary y axis, turn on gridlines
# plot_line(tbl_model$date, tibble(tbl_model$m_dl_h_brtv_tot_all_all_sp, tbl_model$m_dl_h_drtv_tot_all_all_sp),
#           y2 = FALSE, gridlines = TRUE)

# plot curves
# plot_curve(tbl_model$m_dl_h_brtv_tot_all_all_sp, 30000)

# ------------------------------------------------------------------------------------------------
# Models -----------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

# texas online sales
tx_os <- lm(
  log(kpi_ws_tx) ~ # your dependent variable
    
    d_20200315+
    I(d_20200517+d_20200524)+
    I(d_20200426+d_20200503)+
    covid_gm_resid_TX+
    log(sa_down_tot)+
    rollapply(bm_tru_pc,3,mean,fill=NA,align='right')+
    gt_stor+
    d_jan+
    d_mar+
    d_apr+
    d_jun+
    d_sep+
    d_nov+
    bh_xms+
    lag(bh_xms,1)+
    nat_dis_wint_st_tx+
    I(bh_eastmon + bh_valday + bh_stpatsday)+
    I(bh_civic + bh_memday + bh_indday + bh_mthrday + bh_famday + bh_hlwn)+
    I(lag(pro_overstock_d,1)+pro_appl_d+pro_b2c_d+lag(pro_livingrm_d,1))+
    atan(m_ooh_tot_sp/33000)+
    atan(m_cat_tot_vo/140000)+
    atan(lag(m_cstcon_tot_im20,1)/500000)+
    I(atan(m_nat_tot_sp/4000)+atan(m_spons_tot_sp60/4000))+
    atan(lag(m_adtvv_tot_im30,1)/900000)+
    atan(m_ctv_tot_im50/400000)+
    atan(m_ctv_dig_tot_im/5500000)+
    atan(m_digaud_tot_im/2000000)+
    I(atan(lag(m_stddsp_tot_im80,2)/70000000)+atan(lag(m_vid_tot_im20,1)/12000000))+
    I(atan(m_snap_tot_im30/180000)+atan(lag(m_tik_tot_im,1)/40000))+
    I(atan(m_pint_tot_im/2100000)+atan(lag(m_fbk_tot_im,1)/20000000))+
    I(atan(lag(m_ppc_gen_im80,2)/8000000)+atan(lag(m_ppc_fbased_im70,1)/7000000)+atan(m_ppc_bra_im/70000))+
    atan(lag(m_tvv_tot_egrp70,2)/10)+
    I(atan(lag(m_spttvv_tot_egrp70,2)/10)+atan(lag(m_tvspon_tot_egrp,1)/5))+
    atan(lag(m_spotrad_tot_egrp,1)/200)+
    atan(fam_eml_tot_opn/80000)+
    atan(fam_dm_vol70/100000)+
    atan(lag(m_eml_tot_im,2)/1900000)+
    I(atan(lag(m_rchmed_tot_im70,2)/80000000)+atan(lag(bw_earn_im30,1)/100000000))+
    I(atan(pr_tot_im_tx/700000000)+atan(pr_infl_eng/101000))+
    I(serv_inc_del40k+coll_fedex_pup_parcel)+
    c_gt_bbb+
    I(lead(d_oct,1)*d_year_2020)

  ,
  data = tbl_model)


# show model stats
show_stats(tx_os, date_var = tbl_model$date)

# select one of actual vs fitted
plot_act_vs_fitted(tx_os, tbl_model$date, convert_to_real = FALSE) # log form
plot_act_vs_fitted(tx_os, tbl_model$date) # real
plot_act_vs_fitted(tx_os, tbl_model$date, graphlib = 'plotly') # real

# cor matrix
show_cor(tx_os)

# ------------------------------------------------------------------------------------------------
# Decomp -----------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
model <- tx_os
model_data <- tbl_model
date_var <- tbl_model$date
mapping_tbl <- tbl_map

decomp_input <- get_data_for_decomp(tx_os, tbl_model, tbl_model$date, tbl_map)

CoeffsData <- decomp_input$CoeffsData
ModelData.wide <- decomp_input$ModelData.wide 
VarSetup <- decomp_input$VarSetup
kpiData <- decomp_input$kpiData

decomp_output <- LogLinearDecomper(decomp_input$CoeffsData, 
                                   decomp_input$ModelData.wide, 
                                   decomp_input$VarSetup,
                                   decomp_input$kpiData)

decomp_output@DecompGroupProps
decomp_output@DecompGraph

# export to excel
decomp_export_output(decomp_output, tbl_map, "updates/Texas/outputs/_ws/regular_decomp.xlsx")


# ------------------------------------------------------------------------------------------------
# Decomp with Carrys -----------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

cico_input <- get_data_for_cico(decomp_output)

tbl_cico <- calculate_media_timeperiod(cico_input$media_decomp,
                                       cico_input$adstock_rates,
                                       date_mapping = cico_input$date_mapping,
                                       freq = 'week')

# ------------------------------------------------------------------------------------------------
# Decomp adjusted with CiCo values ---------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

decomp_with_cico <- get_decomp_with_cico(decomp_output, decomp_input, tbl_cico, tbl_map)

# export to excel
decomp_export_output(decomp_with_cico, tbl_map, "updates/Texas/outputs/_ws/cico_decomp.xlsx")

# ------------------------------------------------------------------------------------------------
# Export Results ---------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

# # # saves results to 'updates/{update_folder_name}/outputs/model rds/{dep_var_name}_info.rds'
get_data_for_rmd(update_folder_name = 'Texas',
                 dep_var_name = 'tx_os',
                 model = tx_os,
                 model_data = tbl_model,
                 decomp_output = decomp_output,
                 decomp_output_with_cico = decomp_with_cico,
                 tbl_media_spend = tbl_media_spend,
                 tbl_cico = tbl_cico,
                 tbl_map = tbl_map)

# ------------------------------------------------------------------------------------------------
# Waterfalls ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

plot_waterfall(decomp_with_cico, tbl_map, grouping_column = "Group_BaseBreakDown_MediaBreakDown_Price", 
               roll_by = "rolling52wks", title = "", yaxis_title = "Quotes")

# media grouped together
tbl_map_v2 <- tbl_map %>% 
  mutate(media_grouped = ifelse(Group_Base_Media_Price == 'Media', Group_Base_Media_Price, 
                                Group_BaseBreakDown_MediaBreakDown_Price))

plot_waterfall(decomp_with_cico, tbl_map_v2, grouping_column = "media_grouped", 
               roll_by = "rolling52wks")

plot_waterfall(decomp_with_cico, tbl_map, grouping_column = "Group_BaseBreakDown_MediaBreakDown_Price", 
               roll_by = "rolling26wks")

# export all waterfalls to Excel
export_waterfalls(decomp_with_cico, tbl_map, "updates/202206/outputs/waterfalls/q_dl_h_w_tm1.xlsx")

# sand diagram
plot_decomp(decomp_output, tbl_cico = NULL, mapping_tbl = tbl_map, grouping_column = "Group_BaseBreakDown_MediaBreakDown_Price")

# different date groupings
plot_decomp(decomp_output, tbl_cico, roll_by = "week", tbl_map, grouping_column = "Group_Base_Media_Price")
plot_decomp(decomp_output, tbl_cico, roll_by = "month", tbl_map, "Group_Base_MediaBreakDown_Price")

# stacked bars
plot_decomp(decomp_output, tbl_cico, roll_by = "year", tbl_map, "Group_StackedBar")
plot_decomp(decomp_output, tbl_cico, roll_by = "FY", tbl_map, "Group_StackedBar", FY_start = 4)
plot_decomp(decomp_output, tbl_cico, roll_by = "rolling52wks", tbl_map, "Group_StackedBar")
plot_decomp(decomp_output, tbl_cico, roll_by = "rolling26wks", tbl_map, "Group_StackedBar")
plot_decomp(decomp_output, tbl_cico, roll_by = "HY", tbl_map, "Group_StackedBar")

# 100% stacked bars
plot_decomp(decomp_output, tbl_cico, roll_by = "year", tbl_map, "group", pct = TRUE)


# ------------------------------------------------------------------------------------------------
# Ccalc Example ----------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

vars_to_test <- grep("m_dl_h_rad.*_sp", names(tbl_model), value = T)

tbl_ccalc <- ccalc(tbl_model, vars_to_test, eq_dl_h_w_tm1)

show_ccalc(tbl_ccalc)

# test lagged variables
ccalc(tbl_model, vars_to_test, eq_dl_h_w_tm1, lag_var = c(1:2)) %>% show_ccalc()

# ------------------------------------------------------------------------------------------------
# Varmap Example ---------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

# Adstock/Divisor
tbl_varmap <- varmap(tbl_model, var_to_test = "m_dl_h_brtv_tot_all_all_tvr", model = eq_dl_h_w_tm1, 
                     tbl_calc_adstock_from = tbl_data_raw)

show_varmap(tbl_varmap)

# moving averages/lags
tbl_varmap <- varmap(tbl_model, var_to_test = "m_dl_h_brtv_tot_all_all_tvr", model = eq_dl_h_w_tm1, type = "ma")

show_varmap(tbl_varmap)



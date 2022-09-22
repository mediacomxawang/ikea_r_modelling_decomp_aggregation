source('updates/Texas/models/_ss/setup.R')

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

# create media adstocks - create adstock for media, family media
grep('^m.*(_sp|_im|_cl|_grp|_egrp|_vo)|^fammed_.*(_sp|_vol|_cl|_sent)|^fam_.', names(tbl_data_raw), value = TRUE)
tbl_adstocks <- adstock(tbl_data_raw, date_var = 'date', media_identifiers = '^m.*(_sp|_im|_cl|_grp|_egrp|_vo)|^fammed_.*(_sp|_vol|_cl|_sent)|^fam_.')

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
# 
# plot variables (date, tibble(var1, var2, varn))
plot_line(tbl_model$date, tibble(tbl_model$kpi_ss_tx_na, tbl_model$m_snap_tot_im), y2 = TRUE,
          title = 'Store Sales Against Media', graphlib = 'plotly')
# plot_line(tbl_model$date, tibble(3*atan(lag(tbl_model$m_adtvv_tot_im70,1)/900000),tbl_replacement_vars_lagged_lead_atan_weight$`3 * atan(lag(m_adtvv_tot_im70, 1)/900000)`),y2=TRUE)
# plot_line(tbl_model$date, tibble(tbl_model$m_dl_h_brtv_tot_all_all_sp, tbl_model$m_dl_h_drtv_tot_all_all_sp))
# # turn off secondary y axis, turn on gridlines
# plot_line(tbl_model$date, tibble(tbl_model$m_dl_h_brtv_tot_all_all_sp, tbl_model$m_dl_h_drtv_tot_all_all_sp),
#           y2 = FALSE, gridlines = TRUE)

# plot curves
# plot_curve(tbl_model$m_dl_h_brtv_tot_all_all_sp, 30000)

# ------------------------------------------------------------------------------------------------
# Models -----------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

# texas store sales
tx_ss <- lm(
  log(kpi_ss_tx_na) ~ # your dependent variable
    
    log(kpi_wv_tx)+
    bm_tru_pc_3mav+
    lag(osa_reg_tx,1)+
    cov_sto_open_tx_80dec+
    gt_livroom+
    d_stropn_570+
    ww_prcp_tot+
    ww_snw_lta+
    nat_dis_wint_st_tx+
    sh_tot+
    d_jan+
    d_mar+
    d_apr+
    lag(bh_xms,1)+
    I(lag(bh_thnksgiv,1)+lead(bh_thnksgiv,1))+
    I(bh_gdfri+bh_laborday+bh_stpatsday)+
    I(bh_eastmon+bh_cybrmon+bh_blkfri)+
    I(pro_spcactvty_d+pro_fameve_d)+
    atan(bw_earn_reach/18000000)+
    atan(lag(m_cat_tot_vo15,1)/30000)+
    atan(m_spttvv_tot_egrp20/150)+
    atan(m_spotrad_tot_egrp60/300)+
    atan(m_mag_tot_sp/250000)+
    I(atan(fam_dm_vol/1240000)+atan(lag(m_eml_tot_im,1)/900000)+atan(fam_eml_tot_opn30/710000))+
    I(atan(lag(m_adtvv_tot_im70,1)/900000)+atan(m_ctv_tot_im/4500000))+
    atan(m_ctv_dig_tot_im50/440000)+
    atan(m_cstcon_tot_im/9000000)+
    atan(m_digaud_tot_im/10000000)+
    atan(m_snap_tot_im/1500000)+
    I(atan(lag(m_rchmed_tot_im,1)/86000000)+atan(lag(m_pint_tot_im60,1)/1200000))+
    atan(lag(m_fbk_tot_im60,1)/1600000)+
    atan(m_tik_tot_im60/170000)+
    atan(m_vid_tot_im10/6000000)+
    atan(m_stddsp_tot_im15/28000000)+
    atan(m_ooh_tot_im/10000000)+
    I(atan(lag(pr_infl_im,1)/200000)+atan(lag(pr_tot_im,2)/9000000000))+
    I(atan(lag(m_tvv_pre_egrp50,1)/150)+atan(lag(m_tvv_post_egrp60,2)/10))+
    I(atan(m_ppc_fbased_im/200000)+atan(m_ppc_bra_im/500000))+
    atan(m_ppc_gen_im/200000)+
    c_gt_way_ratio+
    c_bm_bbb_awatocons_pc+
    I(d_20181202+d_20181209)+
    d_20190203+
    d_20200531
  ,
  data = tbl_model)

# show model stats
show_stats(tx_ss, date_var = tbl_model$date)

# select one of actual vs fitted
plot_act_vs_fitted(tx_ss, tbl_model$date, convert_to_real = FALSE) # log form
plot_act_vs_fitted(tx_ss, tbl_model$date) # real
plot_act_vs_fitted(tx_ss, tbl_model$date, graphlib = 'plotly') # real

# cor matrix
show_cor(tx_ss)

# ------------------------------------------------------------------------------------------------
# Decomp -----------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

model <- tx_ss
model_data <- tbl_model
date_var <- tbl_model$date
mapping_tbl <- tbl_map

decomp_input <- get_data_for_decomp(tx_ss, tbl_model, tbl_model$date, tbl_map)

CoeffsData <- decomp_input$CoeffsData
ModelData.wide <- decomp_input$ModelData.wide 
VarSetup <- decomp_input$VarSetup
kpiData <- decomp_input$kpiData

decomp_output <- LogLinearDecomper(decomp_input$CoeffsData, 
                                   decomp_input$ModelData.wide, 
                                   decomp_input$VarSetup,
                                   decomp_input$kpiData)
decomp_output@DecompGroupProps
decomp_output@DecompGraphm

# export to excel
# need to change _ss to _ws if we are decomping a web model 
decomp_export_output(decomp_output, tbl_map, "updates/Texas/outputs/_ss/regular_decomp.xlsx")


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
decomp_export_output(decomp_with_cico, tbl_map, "updates/Texas/outputs/_ss/cico_decomp.xlsx")

# ------------------------------------------------------------------------------------------------
# Export Results ---------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

# # saves results to 'updates/{update_folder_name}/outputs/model rds/{dep_var_name}_info.rds'
get_data_for_rmd(update_folder_name = 'Texas',
                 dep_var_name = 'tx_ss',
                 model = tx_ss,
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

# vars_to_test <- grep("m_ctv_dig.*_im", names(tbl_model), value = T)
vars_to_test <- grep("m_ctv_dig_post_im", names(tbl_model), value = T)

tbl_ccalc <- ccalc(tbl_model, vars_to_test, tx_ss)

show_ccalc(tbl_ccalc)

# test lagged variables
ccalc(tbl_model, vars_to_test, tx_ss, lag_var = c(1:2)) %>% show_ccalc()

# ------------------------------------------------------------------------------------------------
# Varmap Example ---------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

# Adstock/Divisor
tbl_varmap <- varmap(tbl_model, var_to_test = "m_ctv_tot_im", model = tx_ss, 
                     tbl_calc_adstock_from = tbl_data_raw)

show_varmap(tbl_varmap)

# moving averages/lags
tbl_varmap <- varmap(tbl_model, var_to_test = "m_dl_h_brtv_tot_all_all_tvr", model = eq_dl_h_w_tm1, type = "ma")

show_varmap(tbl_varmap)

# ------------------------------------------------------------------------------------------------
# Input for MOT ----------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# needs to be the output of CiCo 
mot_input_wkly_decomp <- decomp_output@DecompedData

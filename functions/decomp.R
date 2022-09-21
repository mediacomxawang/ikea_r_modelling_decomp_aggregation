get_data_for_decomp <- function(model, model_data, date_var, mapping_tbl) {
  
  # get coefficients as they appear in the model - grouped form
  CoeffsData <- tidy(model)[1:2] %>% 
    setNames(c("variable", "coeffs")) %>% 
    mutate(variable = ifelse(variable == "(Intercept)", "intercept", variable))
  
  # get regressor data 
  model_obs <- broom::augment(model)
  
  # initiate empty df for later to fill the data for split variables 
  tbl_replacement_vars <- tibble()
  
  # find variables which are grouped together - NEED TO ENSURE GROUPED VARIABLES ARE in the form I(variable a + variable b + ...)
  combined_vars <- CoeffsData %>% 
    filter(grepl("^I\\(", variable)) %>% 
    filter(grepl("\\+", variable))
  
  if (nrow(combined_vars) > 0) { #corresponding to row 161 bracket
    
    # combined_vars_split aims to split out the grouped variables and get the grouped coefficient to their corresponding split variable

    combined_vars_split <- map(seq(nrow(combined_vars)), function(i) {
      tibble(variable = combined_vars$variable[i],
             splits = stringr::str_split(combined_vars$variable[i], "\\+")[[1]],
             coeffs = combined_vars$coeffs[i])
    }) %>% bind_rows() %>%
      mutate(splits = trimws(splits),
             splits = gsub("I\\(|\\)|\\(|rollapply\\(|atan\\(|lag\\(|lead\\(|log\\(|\\,(.*)|\\/(.*)|\\d+\\s\\*\\s|\\d+\\.", "", splits),
             split_is_dummy = ifelse(substr(splits, 1, 2) == "d_" | substr(splits, 1, 3) == "bh_" |substr(splits,1,4) == "pro_", TRUE,FALSE),
             has_atan = grepl('atan\\(', variable),
             has_lagged = grepl('lag',variable),
             has_lead = grepl('lead',variable)) %>% 
      mutate(has_weight = grepl('\\d\\s\\*\\s',variable))

    # this is to find atan((var1+var2)/divisor) - do we need this for ikea? 
    atan_vars <- combined_vars_split %>% 
      filter(has_atan == TRUE) %>% 
      group_by(variable) %>% 
      mutate(splits = paste0(clean_modelled_vars(splits), collapse = "_")) %>% 
      distinct(variable, .keep_all = TRUE) %>% 
      select(variable, splits)
    
    # here is to find the divisor, lag, or lead in the split variables if there are any 
    if (nrow(combined_vars_split) > 0) {
      
      # extract divisor & lag & lead
      if (filter(combined_vars_split, has_atan | has_lagged | has_lead | has_weight) %>% nrow() > 0) {
        
        # aa will contain lag and lead and divisor for all the split variables for the ones that have lag, lead or divisor
        aa <- combined_vars_split %>% 
          filter(has_atan | has_lagged | has_lead | has_weight)
        
        split_with_atan_lag_lead_weight <- map(seq(nrow(aa)), function(i) {
          tibble(split_with_atan_lag_lead_weight = stringr::str_split(unique(aa$variable)[i], "\\+")[[1]])
        }) %>% bind_rows() %>% drop_na() %>% 
          mutate(split_is_lagged = as.character(grepl('lag',split_with_atan_lag_lead_weight)),
                 split_is_lead = as.character(grepl('lead',split_with_atan_lag_lead_weight)),
                 split_is_atan = as.character(grepl('atan',split_with_atan_lag_lead_weight)),
                 split_has_weight = as.character(grepl('\\d\\s\\*\\s',split_with_atan_lag_lead_weight)))%>% 
          mutate(split_with_atan_lag_lead_weight = trimws(split_with_atan_lag_lead_weight),
                 split_with_atan_lag_lead_weight = gsub('I\\(', '', split_with_atan_lag_lead_weight),
                 split_with_atan_lag_lead_weight = gsub('\\)\\)', ')', split_with_atan_lag_lead_weight)) %>% 
          mutate(lag = as.numeric(ifelse(split_is_lagged==TRUE,substr(sub(".*, ","",split_with_atan_lag_lead_weight),1,1),0)), # first remove all characters before the comma and extract the first digit
                 lead = as.numeric(ifelse(split_is_lead==TRUE,substr(sub(".*, ","",split_with_atan_lag_lead_weight),1,1),0))) %>% 
          mutate(divisor = gsub("(.*)\\/|\\)","", split_with_atan_lag_lead_weight),
                 divisor = ifelse(split_is_atan==TRUE,gsub(",.*","", divisor),NA),
                 divisor = as.numeric(replace_na(divisor,0))) %>% 
          mutate(weight = as.numeric(ifelse(split_has_weight==TRUE,sapply(strsplit(split_with_atan_lag_lead_weight,split = '\\s\\*\\s'),'[',1),1)))

        aa <- cbind(aa, split_with_atan_lag_lead_weight) 
        
      } else {
        aa <- combined_vars_split %>%
          filter(split_is_atan) %>%
          mutate(split_with_atan_lag_lead_weight = NA,
                 divisor = NA)
      }
      
      combined_vars_split <- combined_vars_split %>% 
        filter(!(has_atan | has_lagged | has_lead | has_weight)) %>% 
        bind_rows(aa) %>% 
        mutate(split_with_atan_lag_lead_weight = ifelse(is.na(split_with_atan_lag_lead_weight), splits, split_with_atan_lag_lead_weight)) %>% 
        mutate_if(is.numeric , replace_na, replace = 0) %>% 
        mutate_if(is.character, ~replace_na(.,FALSE)) %>% 
        mutate(weight=ifelse(has_weight==FALSE,1,weight))
      
      CoeffsData <- CoeffsData %>%
        left_join(combined_vars_split) %>%
        mutate(variable = ifelse(is.na(splits), variable, split_with_atan_lag_lead_weight)) %>%
        select(variable,coeffs)
      
      # this should contain all of the grouped variables
      vars_to_drop_from_model_obs <- combined_vars_split %>% 
        distinct(variable) %>% pull()
      
      # drop all of the grouped variables
      model_obs <- model_obs %>% select(-all_of(vars_to_drop_from_model_obs))
      
      vars_to_replace_in_model_data <- combined_vars_split %>% 
        distinct(split_with_atan_lag_lead_weight) %>% 
        pull()

      # initiate empty df for later to fill the data for split variables that has lead or lag before it gets re-apply atan
      tbl_replacement_vars_lagged_lead_atan_weight <- model_data %>% 
        select(date)
      
      # this is a helper table that contains the original variable and the lag, lead and divisor used on the variable
      vars_to_lag_lead_atan_weight_with <- combined_vars_split %>% 
        select(splits,split_with_atan_lag_lead_weight,split_is_lagged,lag,split_is_lead,lead,split_is_atan,divisor,split_has_weight,weight)
      
      for (i in 1:nrow(vars_to_lag_lead_atan_weight_with)){
        
        if(vars_to_lag_lead_atan_weight_with$split_is_lagged[i]==TRUE){
          
          n_lag <- as.numeric(vars_to_lag_lead_atan_weight_with$lag[i])
          variable_data <- model_data[,as.character(vars_to_lag_lead_atan_weight_with$splits[i])] 
          lagged_variable <- lag(variable_data,n_lag)
          
            if (vars_to_lag_lead_atan_weight_with$split_is_atan[i]==TRUE){
              lagged_atan_variable <- atan(lagged_variable/vars_to_lag_lead_atan_weight_with$divisor[i])
            } else {
              lagged_atan_variable <- lagged_variable
            }
          
          lagged_atan_weight_variable <- lagged_atan_variable*vars_to_lag_lead_atan_weight_with$weight[i]
          
          colnames(lagged_atan_weight_variable) <- vars_to_lag_lead_atan_weight_with$split_with_atan_lag_lead_weight[i]
          
          tbl_replacement_vars_lagged_lead_atan_weight <- cbind(tbl_replacement_vars_lagged_lead_atan_weight,lagged_atan_weight_variable) 
          
        } else if (vars_to_lag_lead_atan_weight_with$split_is_lead[i]==TRUE){
          # we don't normally atan a lead variable so did not include the atan transformation here
          n_lead <- as.numeric(vars_to_lag_lead_atan_weight_with$lead[i])
          variable_data <- model_data[,as.character(vars_to_lag_lead_atan_weight_with$splits[i])] 
          lead_variable <- lead(variable_data,n_lead)
          colnames(lead_variable) <- vars_to_lag_lead_atan_weight_with$split_with_atan_lag_lead_weight[i]
          
          tbl_replacement_vars_lagged_lead_atan_weight <- cbind(tbl_replacement_vars_lagged_lead_atan_weight,lead_variable)
          
        } else {
          # this applies to variables that do not have lag or lead, just atan 
          no_lagged_lead_variable <- model_data[,as.character(vars_to_lag_lead_atan_weight_with$splits[i])]
          
            if (vars_to_lag_lead_atan_weight_with$split_is_atan[i]==TRUE){
              no_lagged_lead_atan_variable <- atan(no_lagged_lead_variable/vars_to_lag_lead_atan_weight_with$divisor[i])
            } else {
              no_lagged_lead_atan_variable <- no_lagged_lead_variable
            }
          
          no_lagged_lead_atan_weight_variable <- no_lagged_lead_atan_variable*vars_to_lag_lead_atan_weight_with$weight[i]
          
          colnames(no_lagged_lead_atan_weight_variable) <- vars_to_lag_lead_atan_weight_with$split_with_atan_lag_lead_weight[i]
          
          tbl_replacement_vars_lagged_lead_atan_weight <- cbind(tbl_replacement_vars_lagged_lead_atan_weight,no_lagged_lead_atan_weight_variable) 
          
        }
        
      }

      # select raw variables and apply atan transformation if needed
      tbl_replacement_vars <- tbl_replacement_vars_lagged_lead_atan_weight %>% 
        select(-date)
      
    } 
    
  } # corresponding to row 16 bracket
  
  # check if .rownames present as a first column
  # if yes, some obs were dropped by lm fn because they were NAs
  if (names(model_obs)[1] == ".rownames") {
    
    # create new date var
    date_var <- date_var[model_obs[1] %>% pull() %>% as.numeric()]
    
    tbl_replacement_vars <- tbl_replacement_vars[model_obs[1] %>% pull() %>% as.numeric(),]
    
    # drop column .rownames
    model_obs <- model_obs %>%
      dplyr::select(-.rownames) %>% 
      bind_cols(tbl_replacement_vars)
    
  } else {
    if (ncol(tbl_replacement_vars) > 0) {
      model_obs <- model_obs %>%
        bind_cols(tbl_replacement_vars)
    } else {
      model_obs <- model_obs
    }
    
  }
  
  augmented_df <- model_obs[-1] %>%
    select(-starts_with(".")) %>% 
    select(all_of(CoeffsData$variable[-1]))
  
  ModelData.wide <- bind_cols(tibble(date = date_var) %>% mutate(intercept = 1), augmented_df)
  
  kpiData <- bind_cols(tibble(date = date_var), model_obs[1]) %>% setNames(c("date", "y"))
  
  mapping_table <- mapping_tbl %>% 
    select(actual.vars, group, ref_points)
  
  # check for any variable transformations in order to match them to the correct decomp group
  if (nrow(combined_vars) > 0) {
    model_vars <- CoeffsData %>% 
      left_join(atan_vars, by = "variable") %>% 
      mutate(variable = ifelse(!is.na(splits), splits, variable))
  } else {
    model_vars <- CoeffsData
  }
  

  clean_vars <- clean_modelled_vars(model_vars$variable)
  
  VarSetup <- tibble(index = 1:nrow(CoeffsData),
                     actual.vars = clean_vars) %>%
    mutate(modelled_vars = CoeffsData$variable) %>% 
    mutate(alpha = ifelse(actual.vars == "intercept", 1, 0)) %>% 
    left_join(mapping_table, by = "actual.vars") %>% 
    mutate(group = ifelse(actual.vars == "intercept", "Base", group),
           ref_points = ifelse(actual.vars == "intercept", "none", ref_points),
           ref_points = tolower(ref_points),
           decomp_group = ifelse(grepl("^m_|^fam|^bw_|stropn|^pro_|^pr_|^kpi_wv|^sa_|^serv_|^coll_|^div_|^orgsoc_|^orgseo_",actual.vars), modelled_vars,"Base")) 

  # test different non-base groups
  # decomp_group = ifelse(grepl("^m_|^fam_|^kpi_wv|^gt_|^osa_",actual.vars), modelled_vars,"Base")
  # decomp_group = ifelse(actual.vars == "intercept", "Base", modelled_vars)
  
  unmapped_vars <- VarSetup %>% filter(is.na(group)) %>% pull(actual.vars)
  
  if (sum(is.na(VarSetup$group)) > 0) {
    stop(glue::glue('Unmapped variables found. 
                    Please check and populate mapping table with missing 
                    information. Could not locate following variables: 
                    {paste0(unmapped_vars, collapse = "\n")}'))
  }
  
  return(list(CoeffsData = CoeffsData,
              ModelData.wide = ModelData.wide,
              VarSetup = VarSetup,
              kpiData = kpiData))
  
}

# Business Science Log-Linear Decomp Function/Calculator

#' @title LogLinearDecomper
#' 
#' @description This MediaCom tool allows you analytical decompose linearly across all variables in a log-linear model. 
#'              It has been written in as S4 object form so take note to use the at-reference when calling objects from within the output object.
#' @usage LogLinearDecomper(CoeffsData, ModelData.wide, VarSetup, kpiData)
#' 
#' @param CoeffsData       2x2 dataframe of the 'variable' (variable names) and 'coeffs' (regression model coefficients) values in each column
#' @param ModelData.wide   nxk dataframe of 'date', 'intercept' and other variables used in your model to be decomped. 
#'                         Note that it should start with the date, intercept and then the other variables follow. It should be a dataframe in wide form.
#' @param VarSetup         kx7 dataframe of setup parameters that the decomp tool would use. 
#'                         7 columns composed of 'index', 'actual.vars' (real variable name), 'modelled_vars' (transformed varaible name), 
#'                         'group' (group name to categorised decomped var into), decomp_group ('BASE' or 'modelled.var name'), 
#'                         'alpha' (intercept or not?) and 'ref_points' ('none', 'min', or 'max')
#' @param kpiData          2x2 dataframe of 'date' and kpi variable 'y' in its log form ie. as it entered the model.



LogLinearDecomper <- function(CoeffsData = NULL,
                              ModelData.wide = NULL,
                              VarSetup = NULL,
                              kpiData = NULL){
  #A. import libraries
  {
    library(dplyr)
    library(tidyr)
    library(ggplot2)
  }
  
  #B. internal functions to be deployed:
  {
    ##1. decomper calculator to decompose the decomp from the multiplicative world
    decomper <- function(log.decomp.flat = NULL, 
                         predicted.vec = NULL,
                         base.group = TRUE){
      
      #column-wise decomposition
      decomp.obj <- log.decomp.flat %>% 
        select(-1) %>% # drop date
        mutate(across(!matches("^Base$"), function(x){
          exp(x + Base) - exp(Base)
        })) %>% 
        mutate(Base = exp(Base)) # decomp base by itself
      
      #implement redistribution by group
      if(base.group==TRUE){
        
        #redistribute in base group
        redist <- rowSums(decomp.obj)
        
        to_redist <- predicted.vec - redist
        
        # contr_no_base <- decomp.obj %>% select(-BASE) %>% abs() %>% rowSums()
        
        decomp_no_base <- decomp.obj %>% select(-Base) %>% abs()
        
        redist.share <- (decomp_no_base / rowSums(decomp_no_base))
        
        # if any rowSum of decomp_no_base is 0, it produces NaN values (Infinity)
        # replace them with zero?
        redist.share[is.na(redist.share)] <- 0
        
        decomp.obj <- decomp.obj %>% 
          mutate(across(!matches("^Base$"), function(x){
            x + (
              (  ifelse(is.nan(abs(x)/rowSums(decomp_no_base)), 0, abs(x)/rowSums(decomp_no_base))  ) * to_redist
            ) # var = redist share * to_redist
          }))
        
      }else{
        #redistribute outside base group
        decomp.obj <- decomp.obj[-1]
        redist <- rowSums(decomp.obj)
        to.redist <- (c(as.matrix(predicted.vec)) - redist)
        redist.share <- (abs(decomp.obj) / rowSums(abs(decomp.obj)))
        for(k in 1:ncol(decomp.obj)) {
          decomp.obj[k] <- (decomp.obj[k]
                            + (redist.share[k] * to.redist))
        }
      }
      
      #return output as tibble object
      list(decomped.contribs = decomp.obj%>%as_tibble(),
           direct.contribs = redist,
           synergy.contribs = to_redist,
           synergy.shares = redist.share%>%as_tibble(),
           target.total.contrib = predicted.vec)
    }
    
    ##2. linear coefficient from decomp
    decomp.lin.coeff <- function(model.data.wide = NULL,
                                 decomp.data.wide = NULL){
      #create the matrix to collect ouput - note we set intercept to zero, as it is constant
      coeffs.linear <- cbind.data.frame(modelled_vars = names(decomp.data.wide)[1],
                                        linear.coeffs = 0)
      for(k in 2:ncol(decomp.data.wide)){
        coeffs.linear <- rbind.data.frame(coeffs.linear,
                                          cbind.data.frame(modelled_vars = names(decomp.data.wide)[k],
                                                           linear.coeffs = c(cov(model.data.wide[k],decomp.data.wide[k])
                                                                             / var(model.data.wide[k]))
                                          ))
      }
      #return output as tibble object
      coeffs.linear%>%
        mutate(modelled_vars = as.character(modelled_vars))%>%
        as_tibble()
    }
  }
  options(dplyr.summarise.inform = FALSE)
  
  #C. Loglinear Decomp process commences:
  {
    
    ##1. Create log-decomp and convert to flat file in the form that fits into decomper calc
    #take off reference points - reference point is taken on the transformed data
    xmat <- ModelData.wide[-1] %>% as.data.frame()
    
    # TODO: check names match
    # TODO: xmat[k] is a data frame, can't take mean from it
    for(k in 1:nrow(VarSetup)){
      if(tolower(VarSetup$`ref_points`[k]) == "min"){
        xmat[k] <- xmat[k] - min( xmat[k])
      }
      if(tolower(VarSetup$`ref_points`[k]) == "max"){
        xmat[k] <- xmat[k] - max( xmat[k])
      } 
      if(tolower(VarSetup$`ref_points`[k]) == "mean"){
        xmat[k] <- xmat[,k] - mean(xmat[,k])
      } 
      if(grepl("\\d+", VarSetup$`ref_points`[k])) {
        xmat[k] <- xmat[,k] - as.numeric(VarSetup$`ref_points`[k])  # searching for numeric value in the reference point            
      }else{
        xmat[k] <- xmat[k]
      }
    }
    
    #create log-decomp object
    beta_mat <- CoeffsData[-1]
    ypred.00 <- exp(as.matrix(ModelData.wide[-1]) %*% as.matrix(beta_mat))
    LogDecomp <- as.data.frame(matrix(nrow=nrow(xmat),ncol=ncol(xmat)))
    
    for (j in 1:ncol(xmat)) {
      LogDecomp[j] <- xmat[j] * as.matrix(beta_mat)[j]
    }
    redist <- log(ypred.00) - rowSums(LogDecomp)
    LogDecomp[1] <- LogDecomp[1] + redist # add redistributed reference points to intercept
    colnames(LogDecomp) <- colnames(xmat)
    ypred <- exp(rowSums(LogDecomp))
    y.var <- as.matrix(kpiData[2])
    
    ##2. extract ypreds (mean versus median estimated) and r-squares
    rsq <- 1 - (sum((exp(y.var)-ypred)^2) / sum((exp(y.var) - mean(exp(y.var)))^2))
    
    ##3. visualise the Actual versus Fitted
    plot.data <- as.data.frame(matrix(nrow=nrow(xmat),ncol=3))
    colnames(plot.data) <- c('date','KPI.predicted','KPI.actual')
    plot.data <- plot.data %>%
      mutate(date = as.Date(as.matrix(kpiData[1])),
             KPI.predicted = ypred,
             KPI.actual = exp(y.var)) %>% 
      pivot_longer(!date, names_to = 'variable', values_to = 'KPI')
    
    label.plot <- paste('Actual versus Predicted (AvP plot)',
                        paste('rsq Update: ', round(rsq*100, 2), '%', sep = ''), 
                        sep="\n")
    
    plot.object <- ggplot(plot.data, aes(x=date, y=KPI, colour=variable)) + 
      geom_line() + ggtitle(label.plot) +
      theme(title=element_text(size=7), text=element_text(size=8)) +
      ylab('KPI Measure/Variable') +
      xlab('Date')
    
    ##4. Log-Linear Decomposition: using partial using the Partial-Purge Method
    #i. step one:: group-wise decomp
    # create the flatfile of the log-decomp for the tool
    log.decomp.flat <- cbind.data.frame(date = as.Date(as.matrix(kpiData[1])), 
                                        LogDecomp) %>%
      pivot_longer(!date, names_to = "modelled_vars", values_to = "contributions") %>% 
      left_join(VarSetup, by = "modelled_vars") %>% 
      select(date, modelled_vars, group, decomp_group, alpha, contributions) %>%
      arrange(modelled_vars,date)
    
    # create group-wise decomp file
    group.log.decomp.wide <- log.decomp.flat %>%
      group_by(decomp_group, date)%>%
      summarise(contribs.group = sum(contributions))%>%
      spread(decomp_group, contribs.group)%>%
      arrange(date)
    
    # use when you want to understand how decomper function works
    # log.decomp.flat <- group.log.decomp.wide
    # predicted.vec <- ypred
    # base.group = TRUE
    
    # use the decomper function to get decomp for non-Base vars
    step01.decomp <- decomper(log.decomp.flat = group.log.decomp.wide, 
                              predicted.vec = ypred,
                              base.group = TRUE)
    
    #ii. step two:: within-Base decomp
    # slice data out
    base.data <- log.decomp.flat%>%filter(decomp_group=='Base' & alpha==0)%>%
      select(date,modelled_vars,contributions)%>%
      spread(modelled_vars,contributions)%>%
      arrange(date)
    
    base.data <- cbind.data.frame(base.data[1],
                                  LogDecomp %>% select(intercept) %>% rename(Base = intercept),
                                  base.data[2:ncol(base.data)])%>%
      as_tibble()
    
    # use when you want to understand how decomper function works
    # log.decomp.flat <- base.data
    # predicted.vec <- step01.decomp$decomped.contribs %>% pull(Base)
    # base.group = TRUE
    # 
    # run decomper function second time to get decomp for base group vars
    step02.decomp <- decomper(log.decomp.flat = base.data, 
                              predicted.vec = step01.decomp$decomped.contribs %>% pull(Base),
                              base.group = TRUE)
    
    #iii. bind decomped variables together into a dataframe
    # ColumnBind the decomps from each decomp round
    final.decomp <- cbind.data.frame(date = group.log.decomp.wide%>%select(date),
                                     step02.decomp$decomped.contribs,
                                     step01.decomp$decomped.contribs%>%select(-Base)) %>%
      as_tibble() %>% 
      rename(intercept = Base)
    
    #iv. flatten the decomp and re-introduce the groupings variables
    final.decomp <- final.decomp%>%
      pivot_longer(!date, names_to = 'modelled_vars', values_to = 'contributions') %>% 
      left_join(VarSetup, by = "modelled_vars") %>%
      select(date, modelled_vars, group, decomp_group, contributions)
    
    #v. plot current decomp
    p.purge.decomp.gr <- (ggplot(data=(final.decomp),aes(x=date,y=contributions))+
                            ggtitle('Decomp Plot: Partial Purge Method')+
                            geom_bar(aes(fill = group),stat='identity')+
                            theme(legend.position = "bottom"))
    
    #vi. tabulate proportion of contribution for each group
    #visualise the proportions of each contribution by group
    propotion.contribs.groups <- final.decomp%>%group_by(group)%>%
      summarise(tot.contrib = sum(contributions))%>%
      mutate(prop.contrib = formattable::percent(tot.contrib / sum(final.decomp$contributions)))
  }
  
  #D. Obtain Linear Coefficients:
  {
    # convert the final decomp flatfile to wide
    
    final.decomp.wide <- final.decomp%>%
      select(date,modelled_vars,contributions)%>%
      spread(modelled_vars,contributions)%>%
      arrange(date)%>%
      select(names(xmat))
    
    # use function to create linear coefficients and save in object
    linear.coeffs.decomp <- decomp.lin.coeff(model.data.wide = xmat,
                                             decomp.data.wide = final.decomp.wide)
  }
  
  #E. Save and declare as S4-Class Objects: Return Output afterwards
  {
    #' inputs = {CoeffsData, ModelData.wide, VarSetup, kpiData} 
    #' outputs = {final.decomp, p.purge.decomp.gr, linear.coeffs.decomp}
    
    #i. set class for final output/object (setting it up as an S4-class)
    setClass("LogLinearDecompCalculator",
             representation(ModelCoeffs='data.frame',
                            ModelDataWide='data.frame',
                            SetupData='data.frame',
                            KPIData='data.frame',
                            DecompedData='data.frame',
                            DecompedDataList='list',
                            DecompedLinearCoeffs='data.frame',
                            DecompGraph='list',
                            DecompGroupProps='data.frame',
                            tool_info="character"))
    
    #ii. parse calculations into the S4-class accodingly
    output.object <- new("LogLinearDecompCalculator",
                         ModelCoeffs=CoeffsData%>%as_tibble(),
                         ModelDataWide=ModelData.wide%>%as_tibble(),
                         SetupData=VarSetup%>%as_tibble(),
                         KPIData=kpiData%>%as_tibble(),
                         DecompedData=final.decomp,
                         DecompedDataList=list(decomp.nonBASE=step01.decomp, 
                                               decomp.BASE=step02.decomp),
                         DecompedLinearCoeffs=linear.coeffs.decomp,
                         DecompGraph=list(p.purge.decomp.gr),
                         DecompGroupProps=propotion.contribs.groups,
                         tool_info="MediaCom Business Science: Standardised Decomposition Method for a Log-Linear Model - version02b")
    
    #iii. return final output
    return(output.object)
  }
                              }

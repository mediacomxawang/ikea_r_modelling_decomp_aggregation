# A Simple Linear Decomp Function/Calculator

#' @title LinearDecomper
#' 
#' @description This MediaCom tool allows you analytical decompose linearly 
#' across all variables in a linear model. It has been written in as S4 object 
#' form so take note to use the at-reference when calling objects from within 
#' the output object.
#' @usage LinearDecomper(CoeffsData, ModelData.wide, VarSetup, kpiData)
#' 
#' @param CoeffsData       2x2 dataframe of the 'variable' (variable names) and 'coeffs' (regression model coefficients) values in each column
#' @param ModelData.wide   nxk dataframe of 'date', 'intercept' and other variables used in your model to be decomped. 
#'                         Note that it should start with the date, intercept and then the other variables follow. It should be a dataframe in wide form.
#' @param VarSetup         kx7 dataframe of setup parameters that the decomp tool would use. 
#'                         7 columns composed of 'index', 'actual.vars' (real variable name), 'modelled_vars' (transformed varaible name), 
#'                         'group' (group name to categorised decomped var into), decomp_group ('BASE' or 'modelled.var name'), 
#'                         'alpha' (intercept or not?) and 'ref_points' ('none', 'min', or 'max')
#' @param kpiData          2x2 dataframe of 'date' and kpi variable 'y' as it entered the model.



LinearDecomper <- function(CoeffsData = NULL,
                              ModelData.wide = NULL,
                              VarSetup = NULL,
                              kpiData = NULL){
  #A. imprort libraries
  {
    library(dplyr)
    library(tidyr)
    library(ggplot2)
  }
  
  options(dplyr.summarise.inform = FALSE)
  
  #C. Loglinear Decomp process commences:
  {
    
    ##1. Create log-decomp and convert to flat file in the form that fits into decomper calc
    #take off reference points
    xmat <- ModelData.wide[-1] %>% as.data.frame()
    
    # TODO: check names match
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
        xmat[k] <- xmat[,k] - as.numeric(VarSetup$`ref_points`[k])              
      }else{
        xmat[k] <- xmat[k]
      }
    }
    
    #create log-decomp object
    beta_mat <- CoeffsData[-1]
    ypred.00 <- as.matrix(as.matrix(ModelData.wide[-1]) %*% as.matrix(beta_mat))
    LogDecomp <- as.data.frame(matrix(nrow=nrow(xmat),ncol=ncol(xmat)))
    for (j in 1:ncol(xmat)) {
      LogDecomp[j] <- xmat[j] * as.matrix(beta_mat)[j]
    }
    redist <- ypred.00 - rowSums(LogDecomp)
    LogDecomp[1] <- LogDecomp[1] + redist # add redist to intercept
    colnames(LogDecomp) <- colnames(xmat)
    ypred <- rowSums(LogDecomp)
    y.var <- as.matrix(kpiData[2])
    
    #iv. flatten the decomp and re-introduce the groupings variables
    final.decomp <- LogDecomp %>%
      mutate(date = ModelData.wide$date) %>% 
      pivot_longer(!date, names_to = 'modelled_vars', values_to = 'contributions') %>% 
      left_join(VarSetup, by = "modelled_vars") %>%
      select(date, modelled_vars, group, decomp_group, contributions)
    
    # #v. plot current decomp
    # p.purge.decomp.gr <- (ggplot(data=(final.decomp),aes(x=date,y=contributions))+
    #                         ggtitle('Decomp Plot: Partial Purge Method')+
    #                         geom_bar(aes(fill = group),stat='identity')+
    #                         theme(legend.position = "bottom"))
    
    #vi. tabulate proportion of contribution for each group
    #visualise the proportions of each contribution by group
    propotion.contribs.groups <- final.decomp%>%group_by(group)%>%
      summarise(tot.contrib = sum(contributions))%>%
      mutate(prop.contrib = tot.contrib / sum(final.decomp$contributions) * 100)
  }
  
  #E. Save and declare as S4-Class Objects: Return Output afterwards
  {
    #' inputs = {CoeffsData, ModelData.wide, VarSetup, kpiData} 
    #' outputs = {final.decomp, p.purge.decomp.gr}
    
    #i. set class for final output/object (setting it up as an S4-class)
    setClass("LinearDecompCalculator",
             representation(ModelCoeffs='data.frame',
                            ModelDataWide='data.frame',
                            SetupData='data.frame',
                            KPIData='data.frame',
                            DecompedData='data.frame',
                            DecompGraph='list',
                            DecompGroupProps='data.frame',
                            tool_info="character"))
    
    #ii. parse calculations into the S4-class accodingly
    output.object <- new("LinearDecompCalculator",
                         ModelCoeffs=CoeffsData%>%as_tibble(),
                         ModelDataWide=ModelData.wide%>%as_tibble(),
                         SetupData=VarSetup%>%as_tibble(),
                         KPIData=kpiData%>%as_tibble(),
                         DecompedData=final.decomp,
                         DecompGraph=list(),
                         DecompGroupProps=propotion.contribs.groups,
                         tool_info="MediaCom Business Science: Standardised Decomposition Method for a Linear Model - version01")
    
    #iii. return final output
    return(output.object)
  }
}

#' Rename media halos
#'
#' @param data <data.frame> the DLG table map
#' @param brand <string> name of brand that *is* being modeled
#' @param product <string> name of product that *is* being modeled
#'
#' @return tbl_map with non-matching brands/producs as media halos
#' @export
#'
#' @examples
#' rename_media_halos(tbl_map, brand = 'directline', product = 'motor')
#' 
rename_media_halos <- function(data, brand = NULL, product = NULL)
{
  
  # Check that the user has specified a brand and product
  if(is.null(brand) | is.null(product)){
    stop(glue::glue('Please specify both a brand and product - current values are:\nBrand: {brand}\nProduct: {product}'))
  }
  
  # Make brand and product lowercase and remove any potential spaces
  brand = gsub(' ', '', tolower(brand))
  product = gsub(' ', '', tolower(product))
  
  # List of brands/products we model for - this will need to be updated if we ever expand the brands/products
  brand_list <- c('directline', 'churchill', 'dl4b', 'greenflag')
  product_list <- c('home', 'motor', 'pet', 'rescue', 'landlord', 'sme', 'van', 'tradesperson')
  
  # Edge cases - check the user has specified a valid brand and/or product name
  if(!any(brand %in% brand_list)){
    stop(glue::glue('{brand} is not an accepted brand name.\nPlease specify one of {toString(brand_list)}.'))
  }
  
  if(!any(product %in% product_list)){
    stop(glue::glue('{product} is not an accepted product name.\nPlease specify one of {toString(product_list)}.'))
  }
  
  # Rename media contributions to 'Y Media Halo' where Y are products which do not match the modeled KPI
  data %>% 
    dplyr::mutate(
      # Change Group_BaseBreakDown_MediaBreakDown_Price
      # Look for cases where brand/category_a and/or `product/category_b do not match the modeled brand and product KPI - case when range and same brand, then have 'Range {Media Channel}'
      Group_BaseBreakDown_MediaBreakDown_Price = case_when(
        Group_BaseBreakDown_MediaBreakDown_Price != 'Masterbrand' & Group_Base_Media_Price == 'Media' & (tolower(`brand/category_a`) != brand | (tolower(`product/category_b`) != product & tolower(`product/category_b`) != 'range')) ~ paste(`product/category_b`, 'Media Halo', sep = ' '),
        Group_BaseBreakDown_MediaBreakDown_Price != 'Masterbrand' & Group_Base_Media_Price == 'Media' & tolower(`brand/category_a`) == brand & tolower(`product/category_b`) == 'range' ~ paste('Range', `channel/category_c`, sep = ' '),
        TRUE ~ Group_BaseBreakDown_MediaBreakDown_Price
      ),
      
      # Account for incorrect brand/product mapping
      Group_BaseBreakDown_MediaBreakDown_Price = case_when(
        Group_BaseBreakDown_MediaBreakDown_Price != 'Masterbrand' & grepl('^all all - test & learn', Group_BaseBreakDown_MediaBreakDown_Price, ignore.case = TRUE) ~ paste('Total', `channel/category_c`, 'Media Halo', sep = ' '),
        TRUE ~ Group_BaseBreakDown_MediaBreakDown_Price
      ),
      
      # Change Group_StackedBar
      Group_StackedBar = case_when(
        Group_BaseBreakDown_MediaBreakDown_Price != 'Masterbrand' & Group_Base_Media_Price == 'Media' & tolower(`brand/category_a`) == brand & tolower(`product/category_b`) == product ~ paste0(stringr::str_to_title(product), ' Media'),
        Group_BaseBreakDown_MediaBreakDown_Price != 'Masterbrand' & Group_Base_Media_Price == 'Media' & tolower(`brand/category_a`) == brand & tolower(`product/category_b`) == 'range' ~ 'Range Media',
        Group_BaseBreakDown_MediaBreakDown_Price != 'Masterbrand' & Group_Base_Media_Price == 'Media' & (tolower(`brand/category_a`) != brand | (tolower(`product/category_b`) != product & tolower(`product/category_b`) != 'range')) ~ 'Media Halo',
        TRUE ~ Group_StackedBar
      )
    )
  
}


rename_pcw_variables <- function(data, pcw_site = NULL, pcw_spend_group = 'PCW Market Dynamic')
{
  # Valid PCW sites which we are modelling - this will need to be updated if we ever add others
  pcw_sites <- c('conf', 'msup', 'cptm', 'gcom')
  # Account for uppercase in input
  pcw_site <- tolower(pcw_site)
  
  # Check that the user has specified a brand and product
  if(!is.null(pcw_site) & !pcw_site %in% pcw_sites){
    stop(glue::glue('Invalid PCW site {pcw_site} specified.\nPlease specify one of {toString(pcw_sites)}'))
  }
  
  data %>% 
    dplyr::mutate(
      # Change Group_BaseBreakDown_MediaBreakDown_Price
      # Look for cases where the pcw site does not match the modelled var, and  rename accordingly
      Group_BaseBreakDown_MediaBreakDown_Price = case_when(
        grepl('^pcw_.*sp$', actual.vars) & !grepl(pcw_site, actual.vars) ~ 'PCW Competitors',
        grepl('^pcw_.*sp$', actual.vars) & grepl(pcw_site, actual.vars) ~ pcw_spend_group,
        TRUE ~ Group_BaseBreakDown_MediaBreakDown_Price
      ),
      
      # Change Group_StackedBar
      Group_StackedBar = case_when(
        grepl('^pcw_.*sp$', actual.vars) & !grepl(pcw_site, actual.vars) ~ 'PCW Competitors',
        grepl('^pcw_.*sp$', actual.vars) & grepl(pcw_site, actual.vars) ~ pcw_spend_group,
        TRUE ~ Group_StackedBar
      )
    )
  
}


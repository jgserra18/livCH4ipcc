#' Calculate volatile solids excreted from housing systems
#'
#' @description Total volatile solids excreted from housing systems
#'
#' @param VS_manure Volatile solids excreted from manure (kg/(head.yr))
#' @param VS_straw Volatile solids excreted from straw (kg/(head.yr))
#'
#' @return Numeric value representing volatile solids (kg/(head.yr))
#' @export
#'
#' @examples
#' housing_volatile_solids(VS_manure = 1000, VS_straw = 200)
housing_volatile_solids = function(VS_manure,
                                   VS_straw) {
  
  return(VS_manure + VS_straw)
}

#' Calculate volatile solids excreted from manure
#'
#' @description Calculates the volatile solids excreted from manure based on manure amount and composition
#'
#' @param m_manure_excreted Amount of manure excreted (kg/(head.yr))
#' @param dm_manure Dry matter fraction of manure (0-1)
#' @param f_dm_VS Share of volatile solids of dry matter (default: 0.8 or 80%)
#' @param f_ash Ash content in manure (0-1, default: 0.2 or 20%)
#'
#' @return Numeric value representing volatile solids from manure (kg/(head.yr))
#' @export
#'
#' @examples
#' manure_volatile_solids(m_manure_excreted = 20000, dm_manure = 0.15)
manure_volatile_solids = function(m_manure_excreted,
                                  dm_manure,
                                  f_dm_VS = 0.8,
                                  f_ash = 0.2) {
  
  # Calculate volatile solids from manure
  # Note: We're using (1-f_ash) instead of f_grass from the original function
  # as this aligns better with the documentation and typical VS calculations
  return(m_manure_excreted * dm_manure * f_dm_VS * (1-f_ash))
}


#' Calculate volatile solids from straw bedding
#'
#' @description Calculates the volatile solids from straw bedding based on straw amount and composition
#'
#' @param m_straw Amount of straw (kg/(head.yr))
#' @param dm_straw Dry matter fraction of straw (0-1)
#' @param fraction_ash Ash content in straw (0-1, default: 0.045 or 4.5%)
#' @param fraction_actual_grass Fraction of actual days on grass (0-1)
#'
#' @return Numeric value representing volatile solids from straw (kg/(head.yr))
#' @export
#'
#' @examples
#' straw_volatile_solids(m_straw = 500, dm_straw = 0.85)
straw_volatile_solids = function(m_straw,
                                 dm_straw,
                                 fraction_ash = 0.045,
                                 fraction_actual_grass) {
  
  return( m_straw * dm_straw * (1-fraction_ash) * (1-fraction_actual_grass))
}

#' grass_volatile_solids
#'
#' @param m_manure_excreted amount of manure excreted (kg/(head.yr))
#' @param dm_manure dry matter fraction of manure (0-1)
#' @param fraction_dm_VS share of volatile solids of dry matter (80%)
#' @param fraction_grass share of feeding days on grass (%)
#' @description
#' volatile solids excreted from straw
#' 
#' @returns
#' @unit (kg/(head.yr))
#' @export
grass_volatile_solids = function(m_manure_excreted,
                                 dm_manure,
                                 fraction_dm_VS = 0.8,
                                 fraction_grass) {
  
  return( m_manure_excreted/365 * dm_manure * fraction_dm_VS * fraction_grass )
}


#' manure_management_CH4
#'
#' @param VS_housing total volatile solids excreted from housing systems  (kg/(head.yr))
#' @param ch4_conv_factor_livestock CH4 conversion factor for a given livestock and manure type
#' @param max_manure_CH4 maximum CH4 producing capacity for manure produced by a given livestock type (m3 CH4/kg VS)
#' @param VS_grass  total volatile solids excreted from grass (kg/(head.yr))
#' @description
#' CH4 emissions for manure for a given livestock and manure types
#' 
#' @returns int 
#' @export
#' @unit kg CH4/(head.yr)
manure_management_CH4 = function(VS_housing,
                                 ch4_conv_factor_livestock,
                                 max_manure_CH4,
                                 VS_grass) {
  
  Housing_CH4 = VS_housing * ch4_conv_factor_livestock / 100 * 0.67 * max_manure_CH4
  Grass_CH4 = VS_grass * ch4_conv_factor_livestock / 100 * 0.67 * max_manure_CH4
  
  return(
    list(
      Housing = Housing_CH4,
      Grass = Grass_CH4,
      Total = Housing_CH4 + Grass_CH4
    )
  )
}

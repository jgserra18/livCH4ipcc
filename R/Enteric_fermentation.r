#' entFerment_EF_CH4
#'
#' @param gr_energy_in
#' @param ch4_conv_rate
#' @param conv_factor
#'
#' @returns
#' @export
#'
#' @examples
entFerment_EF_CH4 = function(gr_energy_in,
                             ch4_conv_rate,
                             conv_factor = 55.65) {

  return(gr_energy_in * ch4_conv_rate/100 * 365 / conv_factor )
}


#' entFerment_tier1_CH4
#'
#' @param animal_no livestock number (head /yr)
#' @param EF_CH4 Enteric Fermentation CH4 emission factor (kg CH4/(head.yr))
#' @description
#' tier 1 approach to derive CH4 emissions for poultry and fur animals
#'
#' @returns
#' @export
#' @unit kg CH4/yr
#' @examples
entFerment_tier1_CH4 = function(animal_no,
                                EF_CH4) {

  return(EF_CH4 * animal_no)
}



#' .entFerment_tier2_dairyCattle_EF_winter
#'
#' @param feed feed (kg DM/head)
#' @param GE_winter gross energy per kg DM (Mj/(head.yr))
#' @param ch4_conv_rate_beet CH4 conversion rate of beet (% GE)
#' @param f_grass fraction of the year fed on grass (0-1)
#' @param f_beet fraction of the year fed on beets (0-1)
#' @param conv_factor energy content of CH4 (Mj/CH4)
#' @description
#' calculates the emission factor for winter feed for dairy cattle using IPCC 2019 tier 2
#' @unit kg CH4/(head.yr)
#' @returns int
.entFerment_tier2_dairyCattle_EF_winter = function(feed,
                                                   GE_winter,
                                                   ch4_conv_rate_beet,
                                                   f_grass,
                                                   f_beet,
                                                   conv_factor = 55.65) {


  return(
    feed *
      (( GE_winter/conv_factor ) * ch4_conv_rate_beet/100 * (1 - f_grass - f_beet) +
      ( GE_winter/conv_factor ) * ch4_conv_rate_beet/100 * f_beet)
  )
}


#' .entFerment_tier2_other_EF_winter
#'
#' @param feed_unit feeding units (FU)
#' @param GE_winter gross energy per kg DM (Mj/(head.yr))
#' @param ch4_conv_rate  methane conversion rate (% GE)
#' @param f_grass fraction of the year fed on grass (0-1)
#' @param conv_factor energy content of CH4 (Mj/CH4)
#' @description
#' calculates the emission factor for winter feed for animals other than dairy cattle using IPCC 2019 tier 2
#'
#' @unit kg CH4/(head.yr)
#' @returns
.entFerment_tier2_other_EF_winter = function(feed_unit,
                                             GE_winter,
                                             ch4_conv_rate,
                                             f_grass,
                                             conv_factor = 55.6) {

  return(  feed_unit * (GE_winter / conv_factor) * ch4_conv_rate/100 * (1-f_grass) )
}



#' .entFerment_tier2_dairyCattle_EF_summer
#'
#' @param feed feed (kg DM/head)
#' @param GE_summer gross energy per kg DM (Mj/(head.yr))
#' @param ch4_conv_rate_grass   CH4 conversion rate of grass (% GE)
#' @param f_grass fraction of the year fed on grass (0-1)
#' @param conv_factor energy content of CH4 (Mj/CH4)
#' @description
#' calculates the emission factor for summer feed for dairy cattle using IPCC 2019 tier 2
#'
#' @unit kg CH4/(head.yr)
#' @returns int
.entFerment_tier2_dairyCattle_EF_summer = function(feed,
                                                   GE_summer,
                                                   ch4_conv_rate_grass,
                                                   f_grass,
                                                   conv_factor = 55.65) {

  return(
    feed *
      ( GE_summer/conv_factor ) * ch4_conv_rate_grass/100 * f_grass
  )
}

#' .entFerment_tier2_other_EF_summer
#'
#' @param feed_unit feeding units (FU)
#' @param GE_summer gross energy per kg DM (Mj/(head.yr))
#' @param ch4_conv_rate_grass  methane conversion rate of grass (% GE)
#' @param f_grass fraction of the year fed on grass (0-1)
#' @param conv_factor energy content of CH4 (Mj/CH4)
#' @description
#' calculates the emission factor for summer feed for animals other than dairy cattle using IPCC 2019 tier 2
#'
#' @unit kg CH4/(head.yr)
#' @returns
.entFerment_tier2_other_EF_summer = function(feed_unit,
                                             GE_summer,
                                             ch4_conv_rate_grass,
                                             f_grass,
                                             conv_factor = 55.65) {

  return(  feed_unit * (GE_summer / conv_factor) * ch4_conv_rate_grass/100 * f_grass )
}

entFerment_tier2_EF = function(EF_summer,
                               EF_winter) {

  return(EF_summer + EF_winter)
}


entFerment_tier2_CH4 = function(animal_no,
                                EF_annual) {

  return(animal_no * EF_annual)
}
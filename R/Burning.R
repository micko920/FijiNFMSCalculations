#' Emissions From Fire
#'
#' This function references Equations (20-27) to estimate
#' the forest degradation emissions due to fire for the reporting
#' year. The resulting value is expressed in tCO2e/yr.
#'
#' @references [TBC - ERPD citation - Section 8.3.3.2]
#'
#'
#' @param Age The age of the compartment that burnt in the year
#' @param MAIBsw The mean annual increment of above and below ground biomass in
#'   softwood plantations
#' @param RootToShootDryLandSmall Root-to-shoot ratio for tropical moist
#'   deciduous forest < 125 tB ha-1
#' @param Area Area burnt in softwood plantations in the year
#'
#' @return Emissions from Forest Degradation (Fire) - tCO2e
#' @export
CalcEstEmFire <- function(Age,
                       MAIBsw, # Mean Annual Increment Biomass softwood
                       RootToShootDryLandSmall,
                       Area,
                       local_CombustFactor = CombustFactor,
                       local_GWP_CO2 = GWPCO2, local_EF_CO2 = EFCO2,
                       local_GWP_CH4 = GWPCH4, local_EF_CH4 = EFCH4,
                       local_GWP_N2O = GWPN2O, local_EF_N2O = EFN2O
                       ) {
  # Estimate AGB
  AGB <- Age * (MAIBsw * (1 - RootToShootDryLandSmall))
  # Estimate BGB
  BGB <- Age * (MAIBsw * RootToShootDryLandSmall)
  # CO2 ABG emissions -> CO2e
  EmCO2_AG <- Area * AGB * local_CombustFactor * local_GWP_CO2 * local_EF_CO2  * 0.001
  # CO2 BGB emissions -> CO2e
  EmCO2_BG <- ConvBiomassToCO2e(Area * BGB)
  # CH4 ABG emissions -> CO2e
  EmCH4 <- Area * AGB * local_CombustFactor * local_GWP_CH4 * local_EF_CH4 * 0.001
  # N_2O ABG -> CO2e
  EmN2O <- Area * AGB * local_CombustFactor * local_GWP_N2O  * local_EF_N2O * 0.001
  # sum emissions for each gas and put into dataframe
  df <-data.frame(sum(EmCO2_AG), sum(EmCO2_BG), sum(EmCH4), sum(EmN2O))
  return(sum(df))
}

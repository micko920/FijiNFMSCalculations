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
CalcEmFire <- function(Age,
                       MAIBsw, # Mean Annual Increment Biomass softwood
                       RootToShootDryLandSmall,
                       Area) {
  # Estimate AGB
  AGB <- Age * (MAIBsw / (1 + RootToShootDryLandSmall))
  # Estimate BGB
  BGB <- Age * (MAIBsw * RootToShootDryLandSmall)
  # CO2 ABG emissions
  EmCO2AGB <- Area * AGB * CombustFactor * GWPCO2 * EFCO2  * 0.001
  # CO2 BGB emissions
  EmCO2BGB <- Area * BGB * CombustFactor * GWPCO2 * EFCO2 * 0.001
  # CH4 ABG emissions
  EmCH4 <- Area * AGB * CombustFactor * GWPCH4 * EFCH4 * 0.001
  # N_2O (above-ground biomass)
  EmN2O <- Area * AGB * CombustFactor * GWPN2O  * EFN2O * 0.001
  # sum emissions for each gas and put into dataframe
  df <-data.frame(sum(EmCO2AGB), sum(EmCO2BGB), sum(EmCH4), sum(EmN2O))
  return(sum(df))
}

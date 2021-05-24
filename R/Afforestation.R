#' Removals from Afforestation - Total of upland and lowland
#' 
#' This function references Equation (34) to calculate the removals from
#' afforestation/reforestation for the year The resulting value is expressed in
#' tCO2e.Upland and lowland data was provided for the FRL but total
#' afforestation area, not aggregated for upland and lowland,  will be provided
#' for future reporting.
#'
#' @references [TBC - ERPD citation - Section 8.3.4.1]
#' 
#' @param AreaTotal Total Area of Afforestation over the period
#' @param MAIV Mean annual volume increment for afforestation/reforestation
#'    m^3/hectare/year
#' @param BCEF Biomass Conversion and expansion factor for increments in humid
#' tropical natural forests
#' @param RootToShootRatio Root-to-shoot ratio for tropical forests
#' @return Emission Removals from Afforestation - tCO2e
#' @export
CalcRemARTotal <- function(AreaTotal,
                           MAIV,
                           BCEF,
                           RootToShootRatio) {
  # Biomass gains from afforested area over the yr
  MAIC <- MAIV * BCEF * (1 + RootToShootRatio)
  Biomass <- AreaTotal * MAIC * (-1)
  # Removals from afforestation/reforestation for the year
  CO2e <- ConvBiomassToCO2e(Biomass)
  return(CO2e)
}

#' Removals from Afforestation - upland and lowland
#' 
#' This function references Equation (34) to calculate the removals from
#' afforestation/reforestation for the year The resulting value is expressed in
#' tCO2e. Upland and lowland data was provided for the FRL but total
#' afforestation area, not aggregated for upland and lowland,  will be provided
#' for future reporting. The net removals can be obtained with the 
#' CalcRemARTotal() function.
#'
#' @references [TBC - ERPD citation - Section 8.3.4.1]
#' 
#' @param AreaUpland Area of afforestation/reforestation in Natural Forest,
#'   Upland stratum in year
#' @param AreaLowland Area of afforestation/reforestation in Natural Forest,
#'   Lowland stratum in year
#' @param MAIV Mean annual volume increment for afforestation/reforestation
#'   m^3/hectare/year
#' @param BCEF Biomass Conversion and expansion factor for increments in humid
#' tropical natural forests
#' @param RootToShootRatio Root-to-shoot ratio for tropical forests
#' @seealso [CalcRemARTotal()]
#' @return Emission Removals from Afforestation - tCO2e
#' @export
CalcRemAR <- function(AreaUpland,
                      AreaLowland,
                      MAIV,
                      BCEF,
                      RootToShootRatio) {
  return(CalcRemARTotal(AreaUpland + AreaLowland, MAIV, BCEF, RootToShootRatio))
}

#' Biomass to Carbon Conversion
#'
#' This function uses the reference parameters in Table 2 to convert tonnes of
#' dry biomass to Carbon. All values are presented in tonnes.
#'
#' @references [TBC - ERPD citation - Section 8: Table 2]
#'
#'
#' @param Biomass Tonnes of biomass dry matter (tB)
#' @return  Converting Biomass to Carbon
#' @export
ConvBiomassToCarbon <- function(Biomass) {
  # Biomass to Carbon
  Carbon <- Biomass * get("BiomassToCarbonConv")
  return(Carbon)
}

#' Carbon to CO2e Conversion
#'
#' This function uses the reference parameters in Table 2 to convert Carbon to
#' Carbon Dioxide equivalent values. All values are presented in tonnes.
#'
#' @references [TBC - ERPD citation - Section 8: Table 2]
#'
#' @param Carbon Tonnes of Carbon (tC)
#' @return  Converting Carbon to CO2e
#' @export
ConvCarbonToCO2e <- function(Carbon) {
  # Carbon to CO2e
  CO2e <- Carbon * get("CarbonToCO2eConv")
  return(CO2e)
}

#' Convert Biomass to CO2e
#'
#' This function uses the defined functions ConvBiomassToCarbon and
#' ConvCarbonToCO2e to convert tonnes of dry biomass directly to Carbon Dioxide
#' equivalent values. All values are presented in tonnes.
#'
#' @references [TBC - ERPD citation - Section 8]
#'
#' @param Biomass Tonnes of biomass dry matter (tB)
#' @seealso [ConvBiomassToCarbon()]
#' @seealso [ConvCarbonToCO2e()]
#' @return  Converting Biomass to CO2e
#' @export
ConvBiomassToCO2e <- function(Biomass) {
  # Biomass to Carbon
  Carbon <- ConvBiomassToCarbon(Biomass)
  # Carbon to CO2e
  CO2e <- ConvCarbonToCO2e(Carbon)
  # Emissions (tCO2e)
  return(CO2e)
}

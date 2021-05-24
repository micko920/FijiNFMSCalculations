#' Emissions From Deforestation Lowland and Upland
#' 
#' This function references Equations (11) and (12) in order to calculate the
#' emissions from deforestation in upland or lowland areas. The resulting
#' emission is expressed in tCO2e.
#' 
#' @references [TBC - ERPD citation - Section 8.3.2]
#' 
#'
#' @param Area Area deforested in upland or lowland
#' @param EF Emissions Factor for deforestation in upland or lowland forest
#'   tCO2e/ha
#' @return  Emission for deforestation in upland or lowland area
#' @export
CalcEmDF <- function(Area, # use area for upland deforestation or lowland deforestation
                     EF # use EF for either upland or lowland
) {
  # Area deforested upland or lowland (AD) x Emission factor upland or lowland
  CO2e <- Area * EF
  return(CO2e)
}

#' Emissions From Deforestation
#' 
#' This function references Equation (13) in order to calculate the
#' emissions from deforestation in upland and lowland areas. The resulting
#' emission is expressed in tCO2e.
#' 
#' @references [TBC - ERPD citation - Section 8.3.2]
#' 
#'
#' @param EmEstDFUp  Emission Estimate from Lowland Deforestation
#' @param EmEstDFLow Emission Estimate from Upland Deforestation
#' @seealso [CalcEmDF()]
#' @return  Net Emission from deforestation 
#' @export
CalcEmTotalDF <- function (EmEstDFUp, EmEstDFLow) {
  return(EmEstDFUp + EmEstDFLow)
}


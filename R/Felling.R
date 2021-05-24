#' Felling in Natural Forest
#'  
#' @description This function uses Equation (14) to calculate the gross emissions from logging
#' activities. Emissions are presented in tco2e.
#'
#' @references [TBC - ERPD citation - Section 8.3.3.1]
#'
#' @param Volume Volume of area logged (ha)
#' @param EF Total Emissions Factor (TEF)
#' 
#' @return Emissions from Logging of Natural Forests - tCO2e  
#' @export
CalcEmFell <- function(Volume, # volume of area logged
                       EF # Total Emissions Factor
) {
  # Convert volume to carbon and then calculate emissions
  Carbon <- Volume * EF
  CO2e <- ConvCarbonToCO2e(Carbon)
  # total emissions from Logging
  return(CO2e)
}

#' Removals from regrowth on Felled Areas in Natural Forests
#'  
#' @description This function uses Equation (16) to calculate the gross removals from regrowth
#' on felled areas in natural forest. Emissions are presented in tCO2e.
#'
#' @references [TBC - ERPD citation - Section 8.3.3.1]
#'
#' @param Area Area of natural forest logged
#' @param MAIC Mean Annual Increment Carbon
#' @return Removals from regrowth on Felled Areas in Natural Forests - tCO2e 
#' @export
CalcRemFell <- function(Area, # area of natural forest logged
                        MAIC # Mean Annual Increment Carbon
) {
  # Convert area to carbon and then emissions
  Carbon <- Area * MAIC
  CO2e <- ConvCarbonToCO2e(Carbon) * (-1)
  # total removals from Logging
  return(CO2e)
}

#' Net emissions from Logging of Natural Forests (tCO2e)
#'  
#' @description This function used functions CalcEMFell and CalcREMFell to calculate net
#' emissions from logging. Emissions are presented in tCO2e.
#'
#' @references [TBC - ERPD citation - Section 8.3.3.1]
#'
#' @param EmEstFell Emissions from logging
#' @param RemEstFell Removals from regrowth on felled areas 
#' @seealso [CalcEmFell()]
#' @seealso [CalcRemFell()]
#' @return Emissions from Logging of Natural Forests - tCO2e
#' @export
CalcEmTotalFell <- function (EmEstFell, RemEstFell) {
  return(EmEstFell + RemEstFell)
}


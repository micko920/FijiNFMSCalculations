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
CalcEstEmFell <- function(Volume, # volume of area logged
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
CalcEstRemFell <- function(Area, # area of natural forest logged
                        MAIC, # Mean Annual Increment Carbon
                        Age
) {
  # Convert area to carbon and then emissions
  Carbon <- Age * (sapply(Area,as.numeric)) * MAIC
  CO2e <- ConvCarbonToCO2e(sum(Carbon)) * (-1)
  # total removals from Logging
  return(CO2e)
}

#' Net emissions from Logging of Natural Forests (tCO2e)
#'
#' @description This function used functions CalcEstEmFell and CalcEstRemFell to calculate net
#' emissions from logging. Emissions are presented in tCO2e.
#'
#' @references [TBC - ERPD citation - Section 8.3.3.1]
#'
#' @param EstEmFell Emissions from logging
#' @param EstRemFell Removals from regrowth on felled areas
#' @seealso [CalcEstEmFell()]
#' @seealso [CalcEstRemFell()]
#' @return Emissions from Logging of Natural Forests - tCO2e
#' @export
CalcNetEmRemsFell <- function (EstEmFell, EstRemFell) {
  return(EstEmFell + EstRemFell)
}


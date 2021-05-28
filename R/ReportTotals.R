# Final table of results
#
#
#' Gross Emissions 
#'  
#' @description This function uses equation (1) to calculate the gross emissions 
#' from all sources. Emissions are presented in tco2e.
#'
#' @references [TBC - ERPD citation - Section 8.3.1]
#'
#' @param EstEmRemsDefor Gross emissions from deforestation
#' @param EstEmFell Gross emissions from logging
#' @param EmFireTotal Gross emissions from burning
#' @param EmEstFPTotal Gross emissions from forest plantations
#' @seealso [CalcEmDF()]
#' @seealso [CalcEmFell()]
#' @seealso [CalcEmEstTotalFP()]
#' 
#' @return Gross Emissions - tCO2e  
#' @export
CalcGrossEmTotal <- function (EstEmRemsDefor, EstEmFell, EmFireTotal, EmEstFPTotal) {
  return(EstEmRemsDefor + EstEmFell + EmFireTotal + EmEstFPTotal)
}


#' Gross Removals 
#'  
#' @description This function uses equation (2) to calculate the gross removals 
#' from all sources. Emissions are presented in tco2e.
#'
#' @references [TBC - ERPD citation - Section 8.3.1]
#'
#' @param EstRemFell Gross removals from degradation
#' @param RemEstAR Gross removals from afforestation
#' @param RemEstFPTotal Gross removals from forest plantations
#' @seealso [CalcRemFell()]
#' @seealso [CalcRemARTotal()]
#' @seealso [CalcRemTotalFP()]
#' @return Gross Removals - tCO2e  
#' @export
CalcGrossRemTotal <- function (EstRemFell, RemEstAR, RemEstFPTotal) {
  return (EstRemFell + RemEstAR + RemEstFPTotal)
}


# 
#' Forest Degradation Net Emissions
#'  
#' @description This function uses equation (3) to calculate the net emissions 
#' from Forest Degradation. Emissions are presented in tco2e.
#'
#' @references [TBC - ERPD citation - Section 8.3.1]
#'
#' @param EstEmFell Gross emissions from logging
#' @param EstRemFell Gross removals from regrowth on logged areas
#' @param EmFireTotal Gross emissions from burning
#' @seealso [CalcEmFell()]
#' @seealso [CalcRemFell()]
#' @seealso [CalcEmFire()]
#' @return Forest Degradation Net Emissions - tCO2e  
#' @export

CalcFDEst <- function (EstEmFell, EstRemFell, EmFireTotal) {
  return (EstEmFell + EstRemFell + EmFireTotal)
}


#' Gross Removals from Enhancements 
#'  
#' @description This function uses equation (2) to calculate the Gross Removals 
#' from Enhancements. Emissions are presented in tco2e.
#'
#' @references [TBC - ERPD citation - Section 8.3.1]
#'
#' @param FPTotal Gross removals from Forest Plantations
#' @param RemEstAR Gross removals from Afforestation
#' @seealso [CalcRemTotalFP()]
#' @seealso [CalcRemARTotal()]
#' @return Gross Removals from Enhancements - tCO2e  
#' @export
CalcECEst <- function (FPTotal, RemEstAR) {
  return (FPTotal + RemEstAR)
}


#' Net Emissions 
#' 
#' @description This function uses equation (3) to calculate net emissions.
#' Emissions are presented in tco2e.
#'
#' @references [TBC - ERPD citation - Section 8.3.1]
#'
#' @param GrossEmTotal Gross emissions
#' @param GrossRemTotal Gross removals 
#' @seealso [CalcGrossEmTotal()]
#' @seealso [CalcGrossRemTotal]
#' @return Net Emissions  - tCO2e  
#' @export
CalcNetEmTotal <- function (GrossEmTotal, GrossRemTotal) {
  return (GrossEmTotal + GrossRemTotal)
}


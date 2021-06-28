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
#' @param GrossEmDefor Gross emissions from deforestation
#' @param EstEmFell Gross emissions from logging
#' @param EstEmFire Gross emissions from burning
#' @param GrossEmFPln Gross emissions from forest plantations
#' @seealso [CalcEstEmDefor()]
#' @seealso [CalcEstEmFell()]
#' @seealso [CalcGrossEmFPln()]
#' 
#' @return Gross Emissions - tCO2e  
#' @export
CalcGrossEm <- function (GrossEmDefor, EstEmFell, EstEmFire, GrossEmFPln) {
  return(GrossEmDefor + EstEmFell + EstEmFire + GrossEmFPln)
}


#' Gross Removals 
#'  
#' @description This function uses equation (2) to calculate the gross removals 
#' from all sources. Emissions are presented in tco2e.
#'
#' @references [TBC - ERPD citation - Section 8.3.1]
#'
#' @param EstRemFell Gross removals from degradation
#' @param EstRemARefor Gross removals from afforestation
#' @param GrossRemFPln Gross removals from forest plantations
#' @seealso [CalcEstRemFell()]
#' @seealso [CalcGrossRemARefor()]
#' @seealso [CalcGrossRemFPln()]
#' @return Gross Removals - tCO2e  
#' @export
CalcGrossRem <- function (EstRemFell, EstRemARefor, GrossRemFPln) {
  return (EstRemFell + EstRemARefor + GrossRemFPln)
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
#' @param EstEmFire Gross emissions from burning
#' @seealso [CalcEstEmFell()]
#' @seealso [CalcEstRemFell()]
#' @seealso [CalcEstEmFire()]
#' @return Forest Degradation Net Emissions - tCO2e  
#' @export

CalcEstEmRemsFDeg <- function (EstEmFell, EstRemFell, EstEmFire) {
  return (EstEmFell + EstRemFell + EstEmFire)
}


#' Gross Removals from Enhancements 
#'  
#' @description This function uses equation (2) to calculate the Gross Removals 
#' from Enhancements. Emissions are presented in tco2e.
#'
#' @references [TBC - ERPD citation - Section 8.3.1]
#'
#' @param NetEmRemsFPln Gross removals from Forest Plantations
#' @param EstRemARefor Gross removals from Afforestation
#' @seealso [CalcGrossRemFPln()]
#' @seealso [CalcGrossRemARefor()]
#' @return Gross Removals from Enhancements - tCO2e  
#' @export
CalcEstEmRemsEnh <- function (NetEmRemsFPln, EstRemARefor) {
  return (NetEmRemsFPln + EstRemARefor)
}


#' Net Emissions 
#' 
#' @description This function uses equation (3) to calculate net emissions.
#' Emissions are presented in tco2e.
#'
#' @references [TBC - ERPD citation - Section 8.3.1]
#'
#' @param GrossEm Gross emissions
#' @param GrossRem Gross removals 
#' @seealso [CalcGrossEm()]
#' @seealso [CalcGrossRem()]
#' @return Net Emissions  - tCO2e  
#' @export
CalcNetEmRems <- function (GrossEm, GrossRem) {
  return (GrossEm + GrossRem)
}


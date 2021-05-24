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
#' @param EmEstDFTotal Gross emissions from deforestation
#' @param EmEstFell Gross emissions from logging
#' @param EmFireTotal Gross emissions from burning
#' @param EmEstFPTotal Gross emissions from forest plantations
#' @seealso [CalcEmDF()]
#' @seealso [CalcEmFell()]
#' @seealso [CalcEmEstTotalFP()]
#' 
#' @return Gross Emissions - tCO2e  
#' @export
CalcGrossEmTotal <- function (EmEstDFTotal, EmEstFell, EmFireTotal, EmEstFPTotal) {
  return(EmEstDFTotal + EmEstFell + EmFireTotal + EmEstFPTotal)
}


#' Gross Emissions without degradation
#'  
#' @description This function uses equation (1) to calculate the gross emissions 
#' from all sources apart from degradation. Emissions are presented in tco2e.
#'
#' @references [TBC - ERPD citation - Section 8.3.1]
#'
#' @param EmEstDFTotal Gross emissions from deforestation
#' @param EmEstFPTotal Gross emissions from forest plantations
#' @seealso [CalcEmDF()]
#' @seealso [CalcEmEstTotalFP()]
#' @return Gross Emissions without degradation - tCO2e  
#' @export
CalcGrossEmNoFDTotal <- function (EmEstDFTotal, EmEstFPTotal) {
  return (EmEstDFTotal + EmEstFPTotal)
}

#' Gross Removals 
#'  
#' @description This function uses equation (2) to calculate the gross removals 
#' from all sources. Emissions are presented in tco2e.
#'
#' @references [TBC - ERPD citation - Section 8.3.1]
#'
#' @param RemEstFell Gross removals from degradation
#' @param RemEstAR Gross removals from afforestation
#' @param RemEstFPTotal Gross removals from forest plantations
#' @seealso [CalcRemFell()]
#' @seealso [CalcRemARTotal()]
#' @seealso [CalcRemTotalFP()]
#' @return Gross Removals - tCO2e  
#' @export
CalcGrossRemTotal <- function (RemEstFell, RemEstAR, RemEstFPTotal) {
  return (RemEstFell + RemEstAR + RemEstFPTotal)
}

#' Gross Removals without degradation
#'  
#' @description This function uses equation (2) to calculate the gross removals 
#' from all sources apart from degradation. Emissions are presented in tco2e.
#'
#' @references [TBC - ERPD citation - Section 8.3.1]
#'
#' @param RemEstAR Gross removals from afforestation
#' @seealso [CalcRemARTotal()]
#' @return Gross Removals without degradation - tCO2e  
#' @export

CalcGrossRemNoFDTotal <- function (RemEstAR) {
  return (RemEstAR)
}

# 
#' Forest Degradation Net Emissions
#'  
#' @description This function uses equation (3) to calculate the net emissions 
#' from Forest Degradation. Emissions are presented in tco2e.
#'
#' @references [TBC - ERPD citation - Section 8.3.1]
#'
#' @param EmEstFell Gross emissions from logging
#' @param RemEstFell Gross removals from regrowth on logged areas
#' @param EmFireTotal Gross emissions from burning
#' @seealso [CalcEmFell()]
#' @seealso [CalcRemFell()]
#' @seealso [CalcEmFire()]
#' @return Forest Degradation Net Emissions - tCO2e  
#' @export

CalcFDEst <- function (EmEstFell, RemEstFell, EmFireTotal) {
  return (EmEstFell + RemEstFell + EmFireTotal)
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


#' Emissions from Degradation - Total of upland and lowland
#'
#' This function was added in Nov 2022 to incorporate work done by Eric Bullock
#'
#' Assessment of emissions from degradation using the
#' reference data for activity data (area of degradation) and statistical
#' inference using GEDI data and global biomass models for the emission
#' factors.
#'
#' Date: 12/10/2022
#' User: Eric Bullock
#' Contact: eric.bullock@usda.gov
#'
#' Description: Preliminary assessment of the area of degradation, emission
#' factors, and CO2 emissions during Fiji's reference period.
#'
#' Methodology
#' Forest type map: Data from Fiji's preliminary NFI and Landsat were used to
#' create a landcover and forest type map for 2006. The NFI data was used to
#' train a Random Forest classifier using Landsat metrics as predictors.
#' Classification was performed on Google Earth Engine.
#'
#' Activity data: Areas and standard errors were calculated using a the same
#' reference sample derived used for reporting emissions from deforestation. A
#' new class 'degradation' was defined as reference sample units that converted
#' from closed to open forest. Inference of the area and standard errors was
#' performed using an unbiased ratio estimator that accounts for differences
#' between the classes in a stratification and the reference label (Stehman,
#' 2014).
#'
#' Emission factors: Aboveground biomass density was calculated for open and
#' closed forests using the forest type map for defining the populations. Lidar
#' data from GEDI was used with global biomass models and hybrid statistical
#' inference to calculate mean aboveground biomass and uncertainty (Patterson
#' et al., 2019). Biomass density was converted to carbon and CO2e, and the
#' difference between classes defined the emission factor.
#'
#' @references [TBC]
#'
#' @param Area Area of degradation over the period
#' @param EF Biomass conversion and expansion factor for forest degradation
#' @param RootToShootRatio Root-to-shoot ratio for tropical forests
#' @return Emissions from Native Forest Degradation - tCO2e
#' @export
CalcEstEmNFDeg <- function(Area,
                           EF,
                           RootToShootRatio) {
  # Forest Degradation Biomass is the area times the AGB Emission factor and the below ground biomass
  Biomass <- Area * EF * (1 + RootToShootRatio)
  # Emissions from forrest degradation for the year
  CO2e <- ConvBiomassToCO2e(Biomass)
  return(CO2e)
}

#' Net emissions from Degradation of Natural Forests (tCO2e)
#'
#' @description This function used functions CalcEstEmNFDeg to calculate net
#' emissions from degradation Emissions are presented in tCO2e.
#'
#' @references [TBC - ERPD citation]
#'
#' @param EstEmNFDeg Emissions from degradation
#' @seealso [CalcEstEmNFDeg()]
#' @return Emissions from degradation of Natural Forests - tCO2e
#' @export
CalcNetEmRemsNFDeg <- function (EstEmNFDeg) {
  return(EstEmNFDeg)
}

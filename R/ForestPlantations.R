#' Forest Plantations - Emissions from Hardwood Plantations
#'
#' @description This function refers to Equations (51-54) to calculate the gross
#'   annual emissions from hardwood plantations. Note Equations 53 and 54 are
#'   covered by the ConvBiomassToCo2e function. Emissions are presented in
#'   tCO2e.
#'
#' @references [TBC - ERPD citation - Section 8.3.4.2]
#'
#' @param Volume Volume of hardwood plantation harvested (m^3)
#' @param BioConvExp Biomass conversion and expansion factor for hardwood
#'   logging
#' @param RootToShoot Root-to-shoot ratio for tropical rainforests
#' @return Emissions from Hardwood Plantations
#' @export
CalcEstEmFPlnHwd <- function(Volume,
                             BioConvExp,
                             RootToShoot) {
  # estimate AGB and BGB  losses
  AGBExtracted <- Volume * BioConvExp
  BGBExtracted <- Volume * BioConvExp * RootToShoot
  # estimate total biomass
  TotalBiomass <- AGBExtracted + BGBExtracted
  # Convert total Biomass to Carbon and CO2e
  CO2e <- ConvBiomassToCO2e(TotalBiomass)
  return(CO2e)
}

#' Forest Plantations - Emissions from Softwood Plantations
#'
#' @description This function refers to Equations (38-40) to calculate the gross
#'   annual emissions from softwood plantations. Note Equations 39 and 40 are
#'   covered by the ConvBiomassToCo2e function. Emissions are presented in
#'   tCO2e.
#'
#' @references [TBC - ERPD citation - Section 8.3.4.2]
#'
#' @param Volume Volume of softwood plantation harvested (ha)
#' @param Recovery Recovery rate in softwood plantations
#' @param WoodDensity wood density of pine wood harvested in softwood
#' plantations g/cm^3
#' @param RootToShoot Root-to-shoot ratio for tropical moist
#'   deciduous forest > 125 tB ha-1
#' @return Emissions from Softwood Plantations
#' @export
CalcEstEmFPlnSwd <- function(Volume,
                             Recovery,
                             WoodDensity,
                             RootToShoot) {
  # estimate AGB and BGB  losses
  AGBExtracted <- Volume * (1/Recovery) * WoodDensity
  BGBExtracted <- Volume * (1/Recovery) * WoodDensity * RootToShoot
  # estimate total biomass
  TotalBiomass <- AGBExtracted + BGBExtracted
  # Convert total Biomass to Carbon and CO2e
  CO2e <- ConvBiomassToCO2e(TotalBiomass)
  return(CO2e)
}

#' Forest Plantations - Removals from Hardwood Plantations
#'
#' @description This function references Equations (56,57) & (60-63) and to calculate
#'   the removals from Hardwood Plantations. Note Equations (62) and (63) are
#'   covered by the ConvBiomassToCo2e function. Removals are presented in tCO2e.
#'
#' @references [TBC - ERPD citation - Section 8.3.4.2]
#'
#' @param AreaJustGrowsHW  Initial area of forest at start of year
#' @param AreaPlanted      Area planted during the year
#' @param AreaHarvested    Area from area stocked which is harvested during the
#' year
#' @param MAIV           Mean annual volume increment in hardwood plantations
#'   m^3/hectare/year
#' @param BioConvExpInc Biomass conversion and expansion factor for increment
#' taken from IPCC, 2006, Vol 4
#' @param RootToShootRatio Root-to-shoot ratio for tropical forests

#' @return Removals from Hardwood Plantations
#' @export
CalcEstRemFPlnHwd <- function(AreaJustGrowsHW, # Initial area of forest at start of year
                              AreaPlanted, # Area planted during the year
                              AreaHarvested, # Area from area stocked which is harvested during the year
                              MAIV,
                              BioConvExpInc,
                              RootToShootRatio,
                              Age) {
  MAIBhw <- MAIV * BioConvExpInc * (1 + RootToShootRatio)
  # mean annual removals from forest that just grows (existing stock, neither planted nor harvested)
  # MGG - patch no leggacy
  # Rem1 <- AreaJustGrowsHW * MAIChw
  # Biomass from area planted over year
  Rem2 <- Age * (sapply(AreaPlanted, as.numeric)) * MAIBhw
  # Carbon from area planted over year (make into months?)
  # MGG - patch no leggacy
  # Rem3 <- (AreaHarvested) * MAIChw
  # total carbon from forest plantations
  # MGG - patch no leggacy
  #RemTotal <- Rem1 + Rem2 + Rem3
  RemTotal <- Rem2
  CO2e <- ConvBiomassToCO2e(sum(RemTotal)) * (-1)
  return(CO2e)
}

#' Forest Plantations - Removals from Softwood Plantations
#'
#' @description This function references Equations (41), (42), (46), (47-49) to
#'   calculate the removals from Softwood Plantations. Note Equations (48) and
#'   (49) are covered by the ConvBiomassToCo2e function. Removals are presented
#'   in tCO2e.
#'
#' @references [TBC - ERPD citation - Section 8.3.4.2]
#'
#' @param MAIBsw Mean annual biomass increment in Softwood Plantations
#' @param AreaJustGrowsSW, Area stocked - area harvested in that year
#' @param AreaPlanted, Area planted during the year
#' @param AreaHarvested Area from area stocked which is harvested during the
#' year
#' @return Removals from Softwood Plantations
#' @export
CalcEstRemFPlnSwd <- function(MAIBsw, # Mean annual biomass increment in Softwood Plantations
                              AreaJustGrowsSW, # Area stocked - area harvested in that year
                              AreaPlanted, # Area planted during the year
                              AreaHarvested, # Area from area stocked which is harvested during the year
                              Age
) {
  # Calc mean annual increment C for volume m^3
  MAICsw <- ConvBiomassToCarbon(MAIBsw)
  # mean annual removals from forest that just grows (existing stock, neither planted nor harvested)
  # MGG - patch no leggacy
  # Rem1 <- AreaJustGrowsSW * MAICsw
  # Carbon from area planted over year
  Rem2 <- Age * (sapply(AreaPlanted, as.numeric)) * MAICsw
  # Carbon from area planted over year (make into months?)
  # MGG - patch no leggacy
  # Rem3 <- (AreaHarvested) * MAICsw
  # total carbon from forest plantations
  # MGG - patch no leggacy
  #RemTotal <- Rem1 + Rem2 + Rem3
  RemTotal <- Rem2
  CO2e <- ConvCarbonToCO2e(sum(RemTotal)) * (-1)
  return(CO2e)
}

#' Gross emissions Forest Plantations (Hard- and Softwood)
#'
#' @description This function refers to theCalcEstEmFPlnHwd and
#'   CalcEstEmFPlnSwd functions to calculate the gross emissions from forest
#'   plantations. Gross Removals are presented in tCO2.
#'
#' @references [TBC - ERPD citation - Section 8.3.4.2]
#'
#' @param EstEmFPlnHwd Emissions from Hardwood Plantations
#' @param EstEmFPlnSwd Emissions from Softwood Plantations
#' @seealso [CalcEstEmFPlnHwd()]
#' @seealso [CalcEstEmFPlnSwd()]
#' @return Gross Emissions from Hard and Softwood Plantations
#' @export
CalcGrossEmFPln <- function (EstEmFPlnHwd, EstEmFPlnSwd) {
  return(EstEmFPlnHwd + EstEmFPlnSwd)
}

#' Gross removals from Forest Plantations (Hard- and Softwood)
#'
#' @description This function refers to the CalcEstRemFPlnHwd and
#'   CalcEstRemFPlnSwd functions to calculate the gross removals from forest
#'   plantations. Gross Removals are presented in tCO2e.
#'
#' @references [TBC - ERPD citation - Section 8.3.4.2]
#'
#' @param EstRemFPlnHwd Removals from Softwood Plantations
#' @param EstRemFPlnSwd Removals from Softwood Plantations
#' @seealso [CalcEstRemFPlnHwd()]
#' @seealso [CalcEstRemFPlnSwd()]
#' @return Gross Removals from Hard and Softwood Plantations
#' @export
CalcGrossRemFPln <- function (EstRemFPlnHwd, EstRemFPlnSwd) {
  return(EstRemFPlnHwd + EstRemFPlnSwd)
}

#' Net Emissions from Forest Plantations (Hard and Softwood)
#'
#' @description This function references functions CalcGrossEmFPln and
#'   CalcGrossRemFPln to calculate net emissions from Hard and Softwood
#'   Plantations. Values are presented in tCO2e.
#'
#' @references [TBC - ERPD citation - Section 8.3.4.2]
#'
#' @param GrossEmFPln Gross emissions from Soft and Hardwood Plantations
#' @param GrossRemFPln Gross removals from Soft and Hardwood Plantations
#' @seealso [CalcGrossEmFPln()]
#' @seealso [CalcGrossRemFPln()]
#' @return Net emissions from Hard and Softwood Plantations
#' @export
CalcNetEmRemsFPln <- function (GrossEmFPln, GrossRemFPln) {
  return(GrossEmFPln + GrossRemFPln)
}


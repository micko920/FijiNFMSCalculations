# Default Parameters

# Root-to-shoot ratio (IPCC, 2006; Vol. 4, Chap. 4, Tab. 4.4, R for tropical moist
# deciduous with AGB >125 t ha^-1 and AGB <125 t ha^-1)):

#' @export
RootToShootTropRain <- 0.37 # Root to Shoot Tropical Rainforest  (IPCC) (was param.Rlwk)

#' @export
errRootToShootTropRain <- 0.25 # Error root-to-shoot ratio tropical rainforest (was param.errRlwk)


#' @export
RootToShootDryLandSmall <- 0.2 # Root to shoot ratio dry lowland < 125 t ha^-1);  (was 0.2)

#' @export
errLowerRtSDryLandSmall <- 0.09 # Lower bound for triangle distribution

#' @export
errUpperRtSDryLandSmall <- 0.25 # Upper bound for triangle distribution


#' @export
RootToShootDryLandBig <- 0.24 # Root to shoot ratio dry lowland > 125 t ha^-1); (was rdl)

#' @export
errLowerRtSDryLandBig <- 0.22 # Lower bound for triangle distribution

#' @export
errUpperRtSDryLandBig <- 0.33 # Upper bound for triangle distribution
## shoot ratio for tropical mountain systems ?? Not used anywhere

# Biomass conversion and expansion factor for Volumn inc. humid trop. Nat. Forests (IPPC 2006, chp4, Table 4.5)

#' @export
BiomassConvExpansionAR <- 1.1     # Biomass conversion and expansion factor for volume increments in
## humid tropical natural forests (IPCC)

#' @export
errBiomassConvExpansionAR <- 0.25 # Relative error in Biomass Conversion and Expansion Factor

#' @export
BiomassConvExpansionHW <- 1.05 # Biomass conversion and expansion factor (Hardwood) (was param.bcefrhw)

#' @export
errBiomassConvExpansionHW <- 0.25 # Relative error in BiomassConvExpansion (was param.errbcefrhw)

#' @export
BiomassConvExpansionIncHW <- 1.1 # Biomass convesion and exp. factor (increment Hardwood) (was param.bcefihw)

#' @export
errBiomassConvExpansionIncHW <- 0.25 # Relative error in BiomassConvExpansionIncHW  (was param.errbcefihw)


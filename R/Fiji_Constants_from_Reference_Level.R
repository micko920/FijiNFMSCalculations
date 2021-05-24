# 1. Deforestation ####

# Total Emissions Factor (TEF)
# To 'translate' volumes extracted to carbon loss, the 'Total Emissions Factor' from
# Haas (2015) was used. The TEF has three components:

#' @export
EFFelling <- 0.69 # Emission Factor Felling, C loss caused by the log itself
# (including logging residuals) (was param.efell)

#' @export
EFDamage <- 0.15 # C loss due to damage to the remaining stand (was param.edam)

#' @export
EFInfrastructure <- 0.21 # C loss caused by logging infrastructure establishment  (was param.einfr)

#' @export
TEF <- EFFelling + EFDamage + EFInfrastructure # Total Emissions Factor
# TEF                 <- 1.05    # Total Emissions Factor

#' @export
errTEF <- 0.25 # Relative error of TEF  (was param.errtef)


#' @export
EFDFUp <- 199.679 # Emissions Factor Deforestation Upland, extracted from FRL data

#' @export
EFDFUp_UCI   <- 200.0    # TODO: get real number

#' @export
EFDFUp_LCI   <- 199.0    # TODO: get real number

#' @export
EFDFLow <- 259.400 # Emissions Factor  Deforestation Lowland, extracted from FRL data

#' @export
EFDFLow_UCI   <- 260.0   # TODO: get real number

#' @export
EFDFLow_LCI   <- 259.0   # TODO: get real number



# 2. Forest Degradation ####
# Felling


#' @export
MAICFell <- 0.99 # Mean annual increment (MAI) of total C (above- and below-ground carbon)
# (Mussong, unpublished)for conventional logging in Natural Forest)
# (was param.maiclnf)


#' @export
ErrMAICFell <- 0.5 # Relative error in MAICFell (was param.errmaiclnf)

#' @export
ErrAreaFell <- 0.25 # Relative error in area, felling in Natural Forest (was param.errlnf)

# Burning ####


#' @export
MAIBsw <- 10 # Mean annual biomass increment in Softwood Plantations (was param.maibp)

#' @export
errMAIBsw <- 0.25 # Relative error in 'maibp'(was param.errmaibp)


# 3. Enhancement of Carbon stocks ####

# Afforestation **************

# Note: there is a discrepancy between FRL data and calculated data for MAICAGBar:
# MAICBar : This was removed from the calc to go back to basic calc to handle uncertainty
# and sensitivity analysis
#MAICAGBar <- 1.918 # Mean annual total carbon increment AGB only (AR) (was param.maicar in FRL)
# Resolve the issue with the MAICAGBar between FRL and ERPD!!!!!
# From fiji_frl_main_document_GENERAL.pdf and ERDP.pdf, MAICAGBar is calculated as:
# MAICAGBar <- MAIVar * BiomassConvExpansionAR  * BiomassToCarbonConv
# MAIVar                <-  3.71      # Mean Annual Volume Increment (AR) m^3 ha^-1 yr^-1 (not used in FRL)
# BCEFar                <-  1.1       # Biomass conversion factor tB (m^3)^-1 (not used in FRL)
# Thus MAICAGBar       <- 1.91807 when calculated as above.
# This has been rounded to a common 4 sig figs to remove the discrepancy.
# subsequently there is a discrepancy in MAICar between FRL and calculated.
# FRL value used:
#MAICAGBar <- 1.91846 # Mean annual total carbon increment AGB only (AR) (was param.maicar in FRL)
#errMAICAGBar <- 0.5 # Relative error in MAICar(was param.errmaicar)


#' @export
MAIVar <- 3.71  # Mean annual volumn increament Affor/Refor (provided by Fiji Hardwood Corp)

#' @export
errMAIVar <- 0.5 # Relative error for MAI volumn inc Affor/Refor (MAIRVar)

# Forest Plantations ####

#' @export
WoodDensitySW <- 0.47 # Wood density Softwood,see Cown (1981). Pine plantations are mostly
# located below 300 m a.s.l. (was param.wdsw)

#' @export
SDWoodDensitySW <- 0.0509 # Standard deviation of WoodDensitySW (was param.sdwdsw)

#' @export
RecoveryRateSW <- 0.76 # 0.76 is the ratio between utilizable volume and total tree volume. The value
# was taken from Waterloo (1994), Fig. 11.7, page 221. (was param.volTovol)

#' @export
errRecoveryRateSW <- 0.05 # Relative error in RecoveryRateSW


#' @export
AGBLossSW <- (1 / RecoveryRateSW) * WoodDensitySW # (was VolToAGBPine in FRL)
# AGBLossSW                    <- 0.618421 # Volume to AGB in Softwood



#' @export
MAIVhw <- 5.85 # Weighted mean annual volume increment (Hardwood) (was param.maivhww )

#' @export
errMAIVhw <- 0.25 # Relative error in MAIVhw and 'maivhww' (was param.errmaivhww )


# Error on HW growing area has been removed.
#errAreaJustGrowsHW <- 0.5 # Relative error in Hardwood Area that just grows

# Mean annual AGB increment in Hardwood plantation [tB ha^-1 yr^-1] (not a parameter in FRL)

#' @export
MAIAGBhw <- MAIVhw * BiomassConvExpansionIncHW
# MAIAGBhw                     <- 6.435  # Mean annual AGB increment in Hardwood plantation [tB ha^-1 yr^-1]



#' @export
MAIChw <- (MAIAGBhw * (1 + RootToShootTropRain) * BiomassToCarbonConv)
# MAIChw                       <- 4.14350 # Mean annual increment C for volume m^3 [t ha^-1 yr^-1]

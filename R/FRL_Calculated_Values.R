# Calculated Values from FRL



# Fiji's Forest reference Level. Sum of values above, no specific variable name? FRL frltab[13,2]

#' @export
FRL <- 1636800 # (tCO2e/yr) Sum of Net Emissions from Fiji Baseline (FRL) to 6 significant figures

#' @export
FRL_UCI <- 2444030

#' @export
FRL_LCI <- 953460


#' @export
FRLDeforestation <- 2696831 # from Table12.5 of ERPD

#' @export
FRLDeforestation_UCI <- 3373850

#' @export
FRLDeforestation_LCI <- 2143830

### TODO: Mistake in the value this needs to be recalculated and checked in FRL, Outside LCI
# from Table12.5 of ERPD

#' @export
FRLForestDegradation <- 310442 # Net emissions forest degradation aanefd frltab[11,2]

#' @export
FRLForestDegradation_UCI <- 358537

#' @export
FRLForestDegradation_LCI <- 274025

### Note: UCI and LCI incorrectly swapped in ERPD
# from Table12.5 of ERPD

#' @export
FRLRemovalsBySinks <- -1370469 # Net emissions enhancement of forest carbon stocks (EC). Includes Aforestation and Hardword and Softwood Plantations aaneec frltab[12,2]

#' @export
FRLRemovalsBySinks_UCI <- -975054

#' @export
FRLRemovalsBySinks_LCI <- -1661630




#' @export
FRLAdjustments <- 0 # Always zero

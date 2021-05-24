# Global Constants



#' @export
CarbonToCO2eConv <- 44 / 12 # Carbon to CO_2e conversion (IPCC default * -1) (was param.etacc)

# Tropical Carbon fraction of AGB (IPCC, 2006; Vol. 4, Chap. 4, Table 4.3)

#' @export
BiomassToCarbonConv <- 0.47 # Biomass to carbon conversion (IPCC default)
BiomassToCarbonConv_LCI <- 0.44
BiomassToCarbonConv_UCI <- 0.49

#' @export
CombustFactor <- 0.46 # Combustion Factor in softwood Proportion of pre-fire fuel biomass consumed (IPCC, 2006)

#' @export
errCombustFactor <- 0.5 # Combustion Factor in softwood Proportion of pre-fire fuel biomass consumed (IPCC, 2006)

#' @export
EFCO2 <- 1580 # mean for emissions factor CO2 (IPCC, 2006; Vol. 4, Chap. 2, Table 2.5)

#' @export
EFCO2sd <- 90 # standard deviation for emissions factor CO2 (IPCC, 2006; Vol. 4, Chap. 2, Table 2.5)

#' @export
GWPCO2 <- 1 # global warming potential for CO2

#' @export
EFN2O <- 0.2 # mean for emissions factor N2O

#' @export
GWPN2O <- 265 # global warming potential for N2O

#' @export
EFCH4 <- 6.8 # mean for emissions factor CH4

#' @export
GWPCH4 <- 28 # global warming potential for CH4

#' @export
errGHG <- 0.5 # relative error for greehouse gas factors



# Uncertainty Analysis

#' @export
CI <- 0.9 # Confidence Interval

#' @export
QLCI <- 0.05 # Lower quantile (confidence bounds, 0.05 = 90% CI)

#' @export
QUCI <- 0.95 # Upper quantile (confidence bounds, 0.95 = 90% CI)
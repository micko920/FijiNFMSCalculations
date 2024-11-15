% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ForestPlantations.R
\name{CalcEstEmFPlnSwd}
\alias{CalcEstEmFPlnSwd}
\title{Forest Plantations - Emissions from Softwood Plantations}
\usage{
CalcEstEmFPlnSwd(Volume, Recovery, WoodDensity, RootToShoot)
}
\arguments{
\item{Volume}{Volume of softwood plantation harvested (ha)}

\item{Recovery}{Recovery rate in softwood plantations}

\item{WoodDensity}{wood density of pine wood harvested in softwood
plantations g/cm^3}

\item{RootToShoot}{Root-to-shoot ratio for tropical moist
deciduous forest > 125 tB ha-1}
}
\value{
Emissions from Softwood Plantations
}
\description{
This function refers to Equations (38-40) to calculate the gross
annual emissions from softwood plantations. Note Equations 39 and 40 are
covered by the ConvBiomassToCo2e function. Emissions are presented in
tCO2e.
}
\references{
\link{TBC - ERPD citation - Section 8.3.4.2}
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReportTotals.R
\name{CalcGrossRem}
\alias{CalcGrossRem}
\title{Gross Removals}
\usage{
CalcGrossRem(EstRemFell, EstRemARefor, GrossRemFPln)
}
\arguments{
\item{EstRemFell}{Gross removals from degradation}

\item{EstRemARefor}{Gross removals from afforestation}

\item{GrossRemFPln}{Gross removals from forest plantations}
}
\value{
Gross Removals - tCO2e
}
\description{
This function uses equation (2) to calculate the gross removals
from all sources. Emissions are presented in tco2e.
}
\references{
\link{TBC - ERPD citation - Section 8.3.1}
}
\seealso{
\code{\link[=CalcEstRemFell]{CalcEstRemFell()}}

\code{\link[=CalcGrossRemARefor]{CalcGrossRemARefor()}}

\code{\link[=CalcGrossRemFPln]{CalcGrossRemFPln()}}
}

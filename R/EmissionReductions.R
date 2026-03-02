# Final Emission Reductions Calc

# Emissions


#' @export
CalcMpGrossEmDefor <- function(Year1GrossEmDefor, Year2GrossEmDefor) {
  return(Year1GrossEmDefor + Year2GrossEmDefor)
}

#' @export
CalcMpEstRemARefor <- function(Year1EstRemARefor, Year2EstRemARefor) {
  return(Year1EstRemARefor + Year2EstRemARefor)
}

#' @export
CalcMpNetEmRemsFPln <- function(Year1NetEmRemsFPln, Year2NetEmRemsFPln) {
  return(Year1NetEmRemsFPln + Year2NetEmRemsFPln)
}

#' @export
CalcMpEstEmRemsDegradation <- function(Year1EstEmRemsDegradation, Year2EstEmRemsDegradation) {
  return(Year1EstEmRemsDegradation + Year2EstEmRemsDegradation)
}


#' @export
CalcMpEstEmRemsFDeg <- function(Year1EstEmRemsFDeg, Year2EstEmRemsFDeg) {
  return(Year1EstEmRemsFDeg + Year2EstEmRemsFDeg)
}

#' @export
CalcMpEstEmRemsFDegNonProxy <- function(Year1EstEmRemsFDegNonProxy, Year2EstEmRemsFDegNonProxy) {
  return(Year1EstEmRemsFDegNonProxy + Year2EstEmRemsFDegNonProxy)
}

#' @export
CalcMpEstEmRemsEnh <- function(Year1EstEmRemsEnh, Year2EstEmRemsEnh) {
  return(Year1EstEmRemsEnh + Year2EstEmRemsEnh)
}

#' @export
CalcMpEstEmRemsDeforEnh <- function(Year1GrossEmDefor, Year2GrossEmDefor, Year1EstEmRemsEnh, Year2EstEmRemsEnh, Year1EstEmRemsFDegNonProxy, Year2EstEmRemsFDegNonProxy) {
  return(
    CalcMpGrossEmDefor(Year1GrossEmDefor,Year2GrossEmDefor) +
      CalcMpEstEmRemsEnh(Year1EstEmRemsEnh,Year2EstEmRemsEnh) +
      CalcMpEstEmRemsFDegNonProxy(Year1EstEmRemsFDegNonProxy,Year2EstEmRemsFDegNonProxy)
      
  )
}



#' @export
CalcMpNetEmRems <- function(Year1NetEmRems, Year2NetEmRems) {
  return(Year1NetEmRems + Year2NetEmRems)
}


#' @export
CalcMpEstFRL <- function(ErpaYearlyFRL) {
  return(ErpaYearlyFRL)
}


#' @export
CalcMpEstERs <- function(MpEstFRL, MpNetEmRems) {
  return(MpEstFRL - MpNetEmRems)
}

#' @export
CalcMpEstFRLFDeg <- function(ErpaYearlyFRLFDeg) {
  return(CalcMpEstFRL(ErpaYearlyFRLFDeg))
}

#' @export
CalcMpEstERsFDeg <- function(MpEstFRLFDeg, MpEstEmRemsFDeg) {
  return(MpEstFRLFDeg - MpEstEmRemsFDeg)
}

#' @export
CalcMpEstERsFDegNonProxy <- function(MpEstFRLFDeg, MpEstEmRemsFDeg) {
  return(MpEstFRLFDeg - MpEstEmRemsFDeg)
}


#' @export
CalcMpEstFRLDefEnh <- function(ErpaYearlyFRLDefor, ErpaYearlyFRLEnh, ErpaYearlyFRLFDegNonProxy) {
  return(CalcMpEstFRL(ErpaYearlyFRLDefor) + CalcMpEstFRL(ErpaYearlyFRLEnh) + CalcMpEstFRL(ErpaYearlyFRLFDegNonProxy))
}

#' @export
CalcMpEstERsDefEnh <- function(FRLDefor, FRLEnh, FRLFDegNonProxy, EmRemsDefor, EmRemsEnh, EmRemsFDegNonProxy) {
  return(CalcMpEstFRLDefEnh(FRLDefor, FRLEnh, FRLFDegNonProxy) - (EmRemsDefor + EmRemsEnh + EmRemsFDegNonProxy))
}

#' @export
CalcMpEstFRLDefor <- function(ErpaYearlyFRLDefor) {
  return(CalcMpEstFRL(ErpaYearlyFRLDefor))
}

#' @export
CalcMpEstERsDefor <- function(FRLDefor, EmRemsDefor) {
  return(CalcMpEstFRLDefor(FRLDefor) - (EmRemsDefor))
}

#' @export
CalcMpEstFRLARefor <- function(ErpaYearlyFRLARefor) {
  return(CalcMpEstFRL(ErpaYearlyFRLARefor))
}

#' @export
CalcMpEstERsARefor <- function(FRLARefor, EmRemsARefor) {
  return(CalcMpEstFRLARefor(FRLARefor) - (EmRemsARefor))
}


#' @export
CalcMpEstFRLDegradation <- function(ErpaYearlyFRLDegradation) {
  return(CalcMpEstFRL(ErpaYearlyFRLDegradation))
}

#' @export
CalcMpEstERsDegradation <- function(FRLDegradation, EmRemsDegradation) {
  return(CalcMpEstFRLDegradation(FRLDegradation) - (EmRemsDegradation))
}

#' @export
CalcMpEstFRLFPln <- function(ErpaYearlyFRLFPln) {
  return(CalcMpEstFRL(ErpaYearlyFRLFPln))
}

#' @export
CalcMpEstERsFPln <- function(FRLFPln, EmRemsFPln) {
  return(CalcMpEstFRLFPln(FRLFPln) - (EmRemsFPln))
}

#' @export
CalcMpEstFRLEnh <- function(ErpaYearlyFRLEnh) {
  return(CalcMpEstFRL(ErpaYearlyFRLEnh))
}

#' @export
CalcMpEstERsEnh <- function(FRLEnh, EmRemsEnh) {
  return(CalcMpEstFRLEnh(FRLEnh) - (EmRemsEnh))
}

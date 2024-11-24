# Final Emission Reductions Calc

# Emissions


#' @export
CalcMpGrossEmDefor <- function(Year1GrossEmDefor, Year2GrossEmDefor) {
  return(Year1GrossEmDefor + Year2GrossEmDefor)
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
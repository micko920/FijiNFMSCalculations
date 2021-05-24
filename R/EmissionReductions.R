# Final Emission Reductions Calc

# Emissions


#' @export
CalcMpEstEmRemsDefor <- function(Year1EstEmRemsDefor, Year2EstEmRemsDefor) {
  return(Year1EstEmRemsDefor + Year2EstEmRemsDefor)
}


#' @export
CalcMpEstEmRemsFDeg <- function(Year1EstEmRemsFDeg, Year2EstEmRemsFDeg) {
  return(Year1EstEmRemsFDeg + Year2EstEmRemsFDeg)
}


#' @export
CalcMpEstEmRemsEnh <- function(Year1EstEmRemsEnh, Year2EstEmRemsEnh) {
  return(Year1EstEmRemsEnh + Year2EstEmRemsEnh)
}


#' @export
CalcMpNetEmRems <- function(Year1NetEmRems, Year2NetEmRems) {
  return(Year1NetEmRems + Year2NetEmRems)
}


#' @export
CalcMpEstFRL <- function(ErpaYearlyFRL) {
  return(ErpaYearlyFRL * 2)
}


#' @export
CalcMpEstERs <- function(MpEstFRL, MpNetEmRems) {
  return(MpEstFRL - MpNetEmRems)
}

#' @export
CalcMpEstFRLFDeg <- function(ErpaYearlyFRLFDeg) {
  return(ErpaYearlyFRLFDeg * 2)
}

#' @export
CalcMpEstERsFDeg <- function(MpEstFRLFDeg, MpEstEmRemsFDeg) {
  return(MpEstFRLFDeg - MpEstEmRemsFDeg)
}



#' @export
CalcMpEstERsDefEnh <- function(FRLDefor, FRLEnh, EmRemsDefor, EmRemsEnh) {
  return((CalcMpEstFRL(FRLDefor) + CalcMpEstFRL(FRLEnh)) - (EmRemsDefor + EmRemsEnh))
}
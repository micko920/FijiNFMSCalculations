
#' @export
calcFRLAdjustedAreas <- function() {

  debug_er <<- debug_frl
  # Call the ER calculation which includes AA Boot The 10 is related to the
  # original Accuracy Assessment period which was 2006 to 2016 not inclusive.
  # This is not the same as the FRL period which is inclusive. (ie 11 years)
  return(CalcAdjustedAreas(lcc_mapped_areas, aa_sample, 10))
}

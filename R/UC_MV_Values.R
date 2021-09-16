



#' @export
createUC_MV_Values <- function(MV) {
  result <- list()

  #### Values with Uncertainty #####


  result$DeforAreaLow <- ValueWithUncertainty(
    Value = MV$DeforAreaLow,
    LowerCI = quantile(MV$McDeforAreaLow,probs=QLCI),
    UpperCI = quantile(MV$McDeforAreaLow,probs=QUCI),
    model = create_vwuSampled(MV$McDeforAreaLow), fixed = FALSE
  )
  names(result$DeforAreaLow) <- c("DeforAreaLow")

  result$DeforAreaUp <- ValueWithUncertainty(
    Value = MV$DeforAreaUp,
    LowerCI = quantile(MV$McDeforAreaUp,probs=QLCI),
    UpperCI = quantile(MV$McDeforAreaUp,probs=QUCI),
    model = create_vwuSampled(MV$McDeforAreaUp), fixed = FALSE
  )
  names(result$DeforAreaUp) <- c("DeforAreaUp")

  result$AReforArea <- ValueWithUncertainty(
    Value = MV$AReforArea,
    LowerCI = quantile(MV$McAReforArea,probs=QLCI),
    UpperCI = quantile(MV$McAReforArea,probs=QUCI),
    model = create_vwuSampled(MV$McAReforArea), fixed = FALSE
  )
  names(result$AReforArea) <- c("AReforArea")

  result$FDegFellArea <- ValueWithUncertainty(
    Value = MV$FDegFellArea,
    LowerCI = MV$FDegFellArea - MV$FDegFellArea * ErrAreaFell,
    UpperCI = MV$FDegFellArea + MV$FDegFellArea * ErrAreaFell,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$FDegFellArea) <- c("FDegFellArea")

  return(result)
}

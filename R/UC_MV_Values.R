



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

  ## MGG - patch to ARefor allow growth tables.
  # This only allows 1 value for all years
  # TBD Fix this to allow a different value for each MP year
  result$AReforArea <- ValueWithUncertainty(
   Value = MV$AReforArea$area_ha[1],
   LowerCI = quantile(MV$McAReforArea,probs=QLCI),
   UpperCI = quantile(MV$McAReforArea,probs=QUCI),
   model = create_vwuSampled(MV$McAReforArea), fixed = FALSE
  )
  names(result$AReforArea) <- c("AReforArea")

  ## MGG - patch for FDeg growth tables
  result$FDegFellArea <- ValueWithUncertainty(
    Value = MV$FDegFellArea$area_ha,
    LowerCI = MV$FDegFellArea$area_ha - MV$FDegFellArea$area_ha * ErrAreaFell,
    UpperCI = MV$FDegFellArea$area_ha + MV$FDegFellArea$area_ha * ErrAreaFell,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$FDegFellArea) <- c("FDegFellArea")

  result$FPlnAreaPlantHwd <- ValueWithUncertainty(
    Value = MV$FPlnAreaPlantHwd$area_ha,
    LowerCI = MV$FPlnAreaPlantHwd$area_ha - MV$FPlnAreaPlantHwd$area_ha * ErrAreaARefor,
    UpperCI = MV$FPlnAreaPlantHwd$area_ha + MV$FPlnAreaPlantHwd$area_ha * ErrAreaARefor,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$FPlnAreaPlantHwd) <- c("FPlnAreaPlantHwd")

  result$FPlnAreaPlantSwd <- ValueWithUncertainty(
    Value = MV$FPlnAreaPlantSwd$area_ha,
    LowerCI = MV$FPlnAreaPlantSwd$area_ha - MV$FPlnAreaPlantSwd$area_ha * ErrAreaARefor,
    UpperCI = MV$FPlnAreaPlantSwd$area_ha + MV$FPlnAreaPlantSwd$area_ha * ErrAreaARefor,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$FPlnAreaPlantSwd) <- c("FPlnAreaPlantSwd")

  result$NFDegArea <- ValueWithUncertainty(
    Value = MV$NFDegArea,
    LowerCI = MV$NFDegArea_LCI,
    UpperCI = MV$NFDegArea_UCI,
    model = vwuTriangle, fixed = FALSE
  )
  names(result$NFDegArea) <- c("NFDegArea")

  return(result)
}

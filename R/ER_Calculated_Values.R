# ER_Calculated_Value
# Values calculated from monitored data input



#' @export
CalcEmRemsValues <- function(MonitoredValues) {
  result <- list()
  ################# 1. Deforestation (DF) #############

  # 1.3 Net emissions from deforestation: = AD (area) x EF (emission factor)

  # Emission estimates for deforestation (tCO2e):

  result$EstEmDeforUp <- CalcEstEmDefor(
    MonitoredValues$DeforAreaUp,
    EFDeforUp
  )

  result$EstEmDeforLow <- CalcEstEmDefor(
    MonitoredValues$DeforAreaLow,
    EFDeforLow
  )

  # Total emissions from Deforestation (tCO2e)

  result$GrossEmDefor <- CalcGrossEmDefor(
    result$EstEmDeforUp,
    result$EstEmDeforLow
  )

  #################### 2. Forest Degradation (FD) ########

  ## 2.1 Felling in Natural Forest (Emissions and Removals)

  ## Yearly EMISSIONS from felling in natural forest (tCO2e)

  # Estimate of CO2e emissions from felling

  result$EstEmFell <- CalcEstEmFell(
    MonitoredValues$FDegFellVol,
    TEF
  )

  ## Yearly REMOVALS from felling in natural forest (tCO2e)
  #  no need to (* delta t) as delta t = 1 for 1 year

  # Estimate of CO2e removals from felling

  result$EstRemFell <- CalcEstRemFell(
    MonitoredValues$FDegFellArea,
    MAICFell
  )


  result$NetEmRemsFell <- CalcNetEmRemsFell(
    result$EstEmFell,
    result$EstRemFell
  )

  ##***********************************************************
  ## 2.2 Biomass Burning

  # Uses area and age data from 2018 compartments:

  result$EstEmFire <- CalcEstEmFire(
    MonitoredValues$FDegBurnData$age_yrs,
    MAIBsw,
    RootToShootDryLandSmall,
    MonitoredValues$FDegBurnData$area_ha
  )

  #*********************************************************
  # 2.3 Fuelwood - Excluded from ER calculations
  #**********************************************************

  #*******************************************************************
  ############## 3. Enhancement ##########
  # 3.1 Afforestation

  # Yearly Removals from Afforestation  (tCO2e)

  result$EstRemARefor <- CalcGrossRemARefor(
    MonitoredValues$AReforArea,
    MAIVar, BiomassConvExpansionARefor, RootToShootTropRain
  )
  # 3.2 Forest Plantations

  # Emissions Hardwood plantations ####

  result$EstEmFPlnHwd <- CalcEstEmFPlnHwd(
    MonitoredValues$FPlnVolHarvHwd,
    BiomassConvExpansionHW,
    RootToShootTropRain
  )

  # Emissions Softwood plantations ####

  result$EstEmFPlnSwd <- CalcEstEmFPlnSwd(
    MonitoredValues$FPlnVolHarvSwd,
    RecoveryRateSW, WoodDensitySW,
    RootToShootDryLandBig
  )

  # Removals Hardwood plantations ####

  result$EstRemFPlnHwd <- CalcEstRemFPlnHwd(
    MonitoredValues$FPlnAreaJustGrowsHwd,
    MonitoredValues$FPlnAreaPlantHwd,
    MonitoredValues$FPlnAreaHarvHwd ,
    MAIVhw, BiomassConvExpansionIncHW, RootToShootTropRain
  )

  # Estimate of softwood removals for yr (tCO2e) ####

  result$EstRemFPlnSwd <- CalcEstRemFPlnSwd(
    MAIBsw,
    MonitoredValues$FPlnAreaJustGrowsSwd,
    MonitoredValues$FPlnAreaPlantSwd,
    MonitoredValues$FPlnAreaHarvSwd
  )

  # **************************************************************
  # Gross emissions Forest Plantations (Hard- and Softwood)

  result$GrossEmFPln <- CalcGrossEmFPln(
    result$EstEmFPlnHwd,
    result$EstEmFPlnSwd
  )

  # Gross removals Forest Plantations (Hard- and Softwood)

  result$GrossRemFPln <- CalcGrossRemFPln(
    result$EstRemFPlnHwd,
    result$EstRemFPlnSwd
  )

  # Net Emissions from Forest Plantations (Hard- and Softwood)

  result$NetEmRemsFPln <- CalcNetEmRemsFPln(
    result$GrossEmFPln,
    result$GrossRemFPln
  )

  #*************************************************************
  # 4. Final table of results

  # Gross Emissions Total

  result$GrossEm <- CalcGrossEm(
    result$GrossEmDefor,
    result$EstEmFell,
    result$EstEmFire,
    result$GrossEmFPln
  )

  # Gross Removals Total

  result$GrossRem <- CalcGrossRem(
    result$EstRemFell,
    result$EstRemARefor,
    result$GrossRemFPln
  )

  # Forest Degradation Total

  result$EstEmRemsFDeg <- CalcEstEmRemsFDeg(
    result$EstEmFell,
    result$EstRemFell,
    result$EstEmFire
  )

  # Enhancement Total

  result$EstEmRemsEnh <- CalcEstEmRemsEnh(
    result$NetEmRemsFPln,
    result$EstRemARefor
  )

  # Net Emissions Total

  result$NetEmRems <- CalcNetEmRems(
    result$GrossEm,
    result$GrossRem
  )

  return(result)
}

#' @export
CalcERValues <- function(EmRems, ErpaYearlyFRL, ErpaYearlyFRLFDeg) {
  ER <- list()
  ER$MpGrossEmDefor <- CalcMpGrossEmDefor(
    EmRems$year1$GrossEmDefor,
    EmRems$year2$GrossEmDefor
  )
  ER$MpEstEmRemsFDeg <- CalcMpEstEmRemsFDeg(
    EmRems$year1$EstEmRemsFDeg,
    EmRems$year2$EstEmRemsFDeg
  )
  ER$MpEstEmRemsEnh <- CalcMpEstEmRemsEnh(
    EmRems$year1$EstEmRemsEnh,
    EmRems$year2$EstEmRemsEnh
  )
  ER$MpNetEmRems <- CalcMpNetEmRems(
    EmRems$year1$NetEmRems,
    EmRems$year2$NetEmRems
  )
  ER$MpEstFRL <- CalcMpEstFRL(ErpaYearlyFRL)
  ER$MpEstERs <- CalcMpEstERs(ER$MpEstFRL, ER$MpNetEmRems)

  ER$MpEstFRLFDeg <- CalcMpEstFRL(ErpaYearlyFRLFDeg)
  ER$MpEstERsFDeg <- CalcMpEstERsFDeg(ER$MpEstFRLFDeg, ER$MpEstEmRemsFDeg)
  return(ER)
}

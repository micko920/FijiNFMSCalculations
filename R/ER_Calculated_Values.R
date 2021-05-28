# ER_Calculated_Value
# Values calculated from monitored data input



#' @export
CalcEmRemsValues <- function(MonitoredValues) {
  result <- list()
  ################# 1. Deforestation (DF) #############

  # 1.3 Net emissions from deforestation: = AD (area) x EF (emission factor)

  # Emission estimates for deforestation (tCO2e):

  result$EmEstDeforUp <- CalcEmDF(
    MonitoredValues$DeforAreaUp,
    EFDeforUp
  )

  result$EmEstDeforLow <- CalcEmDF(
    MonitoredValues$DeforAreaLow,
    EFDeforLow
  )

  # Total emissions from Deforestation (tCO2e)

  result$EstEmRemsDefor <- CalcEmTotalDF(
    result$EmEstDeforUp,
    result$EmEstDeforLow
  )

  #################### 2. Forest Degradation (FD) ########

  ## 2.1 Felling in Natural Forest (Emissions and Removals)

  ## Yearly EMISSIONS from felling in natural forest (tCO2e)

  # Estimate of CO2e emissions from felling

  result$EstEmFell <- CalcEmFell(
    MonitoredValues$FDegFellVol,
    TEF
  )

  ## Yearly REMOVALS from felling in natural forest (tCO2e)
  #  no need to (* delta t) as delta t = 1 for 1 year

  # Estimate of CO2e removals from felling

  result$EstRemFell <- CalcRemFell(
    MonitoredValues$FDegFellArea,
    MAICFell
  )


  result$EstFellTotal <- CalcEmTotalFell(
    result$EstEmFell,
    result$EstRemFell
  )

  ##***********************************************************
  ## 2.2 Biomass Burning

  # Uses area and age data from 2018 compartments:

  result$EmFire <- CalcEmFire(
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

  result$RemEstAR <- CalcRemARTotal(
    MonitoredValues$ARArea,
    MAIVar, BiomassConvExpansionAR, RootToShootTropRain
  )
  # 3.2 Forest Plantations

  # Emissions Hardwood plantations ####

  result$EmEstFPHW <- CalcEmForPlantHW(
    MonitoredValues$FPlnVolHarvHwd,
    BiomassConvExpansionHW,
    RootToShootTropRain
  )

  # Emissions Softwood plantations ####

  result$EmEstFPSW <- CalcEmForPlantSW(
    MonitoredValues$FPlnVolHarvSwd,
    RecoveryRateSW, WoodDensitySW,
    RootToShootDryLandBig
  )

  # Removals Hardwood plantations ####

  result$RemEstFPHW <- CalcRemForPlantHW(
    MonitoredValues$FPlnAreaJustGrowsHwd,
    MonitoredValues$FPlnAreaPlantHwd,
    MonitoredValues$FPlnAreaHarvHwd ,
    MAIVhw, BiomassConvExpansionIncHW, RootToShootTropRain
  )

  # Estimate of softwood removals for yr (tCO2e) ####

  result$RemEstFPSW <- CalcRemForPlantSW(
    MAIBsw,
    MonitoredValues$FPlnAreaJustGrowsSwd,
    MonitoredValues$ FPlnAreaPlantSwd,
    MonitoredValues$FPlnAreaHarvSwd
  )

  # **************************************************************
  # Gross emissions Forest Plantations (Hard- and Softwood)

  result$EmEstFPTotal <- CalcEmEstTotalFP(
    result$EmEstFPHW,
    result$EmEstFPSW
  )

  # Gross removals Forest Plantations (Hard- and Softwood)

  result$RemEstFPTotal <- CalcRemTotalFP(
    result$RemEstFPHW,
    result$RemEstFPSW
  )

  # Net Emissions from Forest Plantations (Hard- and Softwood)

  result$FPTotal <- CalcTotalFP(
    result$EmEstFPTotal,
    result$RemEstFPTotal
  )

  #*************************************************************
  # 4. Final table of results

  # Gross Emissions Total

  result$GrossEmTotal <- CalcGrossEmTotal(
    result$EstEmRemsDefor,
    result$EstEmFell,
    result$EmFire,
    result$EmEstFPTotal
  )

  # Gross Removals Total

  result$GrossRemTotal <- CalcGrossRemTotal(
    result$EstRemFell,
    result$RemEstAR,
    result$RemEstFPTotal
  )

  # Forest Degradation Total

  result$EstEmRemsFDeg <- CalcFDEst(
    result$EstEmFell,
    result$EstRemFell,
    result$EmFire
  )

  # Enhancement Total

  result$EstEmRemsEnh <- CalcECEst(
    result$FPTotal,
    result$RemEstAR
  )

  # Net Emissions Total

  result$NetEmRems <- CalcNetEmTotal(
    result$GrossEmTotal,
    result$GrossRemTotal
  )

  return(result)
}

#' @export
CalcERValues <- function(EmRems, ErpaYearlyFRL, ErpaYearlyFRLFDeg) {
  ER <- list()
  ER$MpEstEmRemsDefor <- CalcMpEstEmRemsDefor(
    EmRems$year1$EstEmRemsDefor,
    EmRems$year2$EstEmRemsDefor
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

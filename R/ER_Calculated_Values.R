# ER_Calculated_Value
# Values calculated from monitored data input



#' @export
CalcEmRemsValues <- function(MonitoredValues) {
  result <- list()
  ################# 1. Deforestation (DF) #############

  # 1.3 Net emissions from deforestation: = AD (area) x EF (emission factor)

  # Emission estimates for deforestation (tCO2e):

  result$EmEstDFUp <- CalcEmDF(
    MonitoredValues$DFAreaUp,
    EFDFUp
  )

  result$EmEstDFLow <- CalcEmDF(
    MonitoredValues$DFAreaLow,
    EFDFLow
  )

  # Total emissions from Deforestation (tCO2e)

  result$EmEstDFTotal <- CalcEmTotalDF(
    result$EmEstDFUp,
    result$EmEstDFLow
  )

  #################### 2. Forest Degradation (FD) ########

  ## 2.1 Felling in Natural Forest (Emissions and Removals)

  ## Yearly EMISSIONS from felling in natural forest (tCO2e)

  # Estimate of CO2e emissions from felling

  result$EmEstFell <- CalcEmFell(
    MonitoredValues$FDFellVol,
    TEF
  )

  ## Yearly REMOVALS from felling in natural forest (tCO2e)
  #  no need to (* delta t) as delta t = 1 for 1 year

  # Estimate of CO2e removals from felling

  result$RemEstFell <- CalcRemFell(
    MonitoredValues$FDFellArea,
    MAICFell
  )


  result$EstFellTotal <- CalcEmTotalFell(
    result$EmEstFell,
    result$RemEstFell
  )

  ##***********************************************************
  ## 2.2 Biomass Burning

  # Uses area and age data from 2018 compartments:

  result$EmFire <- CalcEmFire(
    MonitoredValues$FDBurnData$age_yrs,
    MAIBsw,
    RootToShootDryLandSmall,
    MonitoredValues$FDBurnData$area_ha
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
    MonitoredValues$FPVolHarvHW,
    BiomassConvExpansionHW,
    RootToShootTropRain
  )

  # Emissions Softwood plantations ####

  result$EmEstFPSW <- CalcEmForPlantSW(
    MonitoredValues$FPVolHarvSW,
    RecoveryRateSW, WoodDensitySW,
    RootToShootDryLandBig
  )

  # Removals Hardwood plantations ####

  result$RemEstFPHW <- CalcRemForPlantHW(
    MonitoredValues$FPAreaJustGrowsHW,
    MonitoredValues$FPAreaPlantHW,
    MonitoredValues$FPAreaHarvestHW,
    MAIVhw, BiomassConvExpansionIncHW, RootToShootTropRain
  )

  # Estimate of softwood removals for yr (tCO2e) ####

  result$RemEstFPSW <- CalcRemForPlantSW(
    MAIBsw,
    MonitoredValues$FPAreaJustGrowsSW,
    MonitoredValues$FPAreaPlantSW,
    MonitoredValues$FPAreaHarvestSW
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
    result$EmEstDFTotal,
    result$EmEstFell,
    result$EmFire,
    result$EmEstFPTotal
  )

  # Gross Emissions without degradation

  result$GrossEmNoFDTotal <- CalcGrossEmNoFDTotal(
    result$EmEstDFTotal,
    result$EmEstFPTotal
  )

  # Gross Removals Total

  result$GrossRemTotal <- CalcGrossRemTotal(
    result$RemEstFell,
    result$RemEstAR,
    result$RemEstFPTotal
  )

  # Gross Removals without degradation

  result$GrossRemNoFDTotal <- CalcGrossRemNoFDTotal(
    result$RemEstAR
  )

  # Forest Degradation Total

  result$FDEst <- CalcFDEst(
    result$EmEstFell,
    result$RemEstFell,
    result$EmFire
  )

  # Enhancement Total

  result$ECEst <- CalcECEst(
    result$FPTotal,
    result$RemEstAR
  )

  # Net Emissions Total

  result$NetEmTotal <- CalcNetEmTotal(
    result$GrossEmTotal,
    result$GrossRemTotal
  )

  return(result)
}

#' @export
CalcERValues <- function(EmRems, ErpaYearlyFRL, ErpaYearlyFRLFDeg) {
  ER <- list()
  ER$MpEstEmRemsDefor <- CalcMpEstEmRemsDefor(
    EmRems$year1$EmEstDFTotal,
    EmRems$year2$EmEstDFTotal
  )
  ER$MpEstEmRemsFDeg <- CalcMpEstEmRemsFDeg(
    EmRems$year1$FDEst,
    EmRems$year2$FDEst
  )
  ER$MpEstEmRemsEnh <- CalcMpEstEmRemsEnh(
    EmRems$year1$ECEst,
    EmRems$year2$ECEst
  )
  ER$MpNetEmRems <- CalcMpNetEmRems(
    EmRems$year1$NetEmTotal,
    EmRems$year2$NetEmTotal
  )
  ER$MpEstFRL <- CalcMpEstFRL(ErpaYearlyFRL)
  ER$MpEstERs <- CalcMpEstERs(ER$MpEstFRL, ER$MpNetEmRems)

  ER$MpEstFRLFDeg <- CalcMpEstFRL(ErpaYearlyFRLFDeg)
  ER$MpEstERsFDeg <- CalcMpEstERsFDeg(ER$MpEstFRLFDeg, ER$MpEstEmRemsFDeg)
  return(ER)
}

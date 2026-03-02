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
    MonitoredValues$FDegFellArea$area_ha,
    MAICFell,
    MonitoredValues$FDegFellArea$age_yrs
  )


  result$NetEmRemsFell <- CalcNetEmRemsFell(
    result$EstEmFell,
    result$EstRemFell
  )

  ## Yearly EMISSIONS from degradataion in natural forest (tCO2e)

  result$EstEmNFDeg <- CalcEstEmNFDeg(
    MonitoredValues$NFDegArea,
    EFNFDeg,
    RootToShootTropRain
  )

  result$NetEmRemsNFDeg <- CalcNetEmRemsNFDeg(
    result$EstEmNFDeg
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
    MonitoredValues$AReforArea$area_ha,
    MAIVar, BiomassConvExpansionARefor, RootToShootTropRain,
    MonitoredValues$AReforArea$age_yrs
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
    MonitoredValues$FPlnAreaPlantHwd$area_ha,
    MonitoredValues$FPlnAreaHarvHwd,
    MAIVhw, BiomassConvExpansionIncHW, RootToShootTropRain,
    MonitoredValues$FPlnAreaPlantHwd$age_yrs
  )

  # Estimate of softwood removals for yr (tCO2e) ####

  result$EstRemFPlnSwd <- CalcEstRemFPlnSwd(
    MAIBsw,
    MonitoredValues$FPlnAreaJustGrowsSwd,
    MonitoredValues$FPlnAreaPlantSwd$area_ha,
    MonitoredValues$FPlnAreaHarvSwd,
    MonitoredValues$FPlnAreaPlantSwd$age_yrs
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
    result$GrossEmFPln,
    result$EstEmNFDeg
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
    result$EstRemFell
  )

  result$EstEmRemsFDegNonProxy <- CalcEstEmRemsFDegNonProxy(
    result$EstEmFire,
    result$EstEmNFDeg
  )
  
  result$EstEmRemsDegradation <- CalcEstEmRemsDegradation(
    result$EstEmFell,
    result$EstRemFell,
    result$EstEmFire,
    result$EstEmNFDeg
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
CalcERValues <- function(EmRems, period, mp_frl, frl) {
  ER <- list()
  ER$MpGrossEmDefor <- CalcMpGrossEmDefor(
    EmRems$year1$GrossEmDefor,
    EmRems$year2$GrossEmDefor
  )
  
  ER$MpEstRemARefor <- CalcMpEstRemARefor(
    EmRems$year1$EstRemARefor,
    EmRems$year2$EstRemARefor
  )
  
  ER$MpNetEmRemsFPln <- CalcMpNetEmRemsFPln(
    EmRems$year1$NetEmRemsFPln,
    EmRems$year2$NetEmRemsFPln
  )
  
  ER$MpEstEmRemsDegradation <- CalcMpEstEmRemsDegradation(
    EmRems$year1$EstEmRemsDegradation,
    EmRems$year2$EstEmRemsDegradation
  )
  
  ER$MpEstEmRemsFDeg <- CalcMpEstEmRemsFDeg(
    EmRems$year1$EstEmRemsFDeg,
    EmRems$year2$EstEmRemsFDeg
  )
  ER$MpEstEmRemsFDegNonProxy <- CalcMpEstEmRemsFDegNonProxy(
    EmRems$year1$EstEmRemsFDegNonProxy,
    EmRems$year2$EstEmRemsFDegNonProxy
  )
  ER$MpEstEmRemsEnh <- CalcMpEstEmRemsEnh(
    EmRems$year1$EstEmRemsEnh,
    EmRems$year2$EstEmRemsEnh
  )
  ER$MpEstEmRemsDefEnh <- CalcMpEstEmRemsDeforEnh(
    EmRems$year1$GrossEmDefor,
    EmRems$year2$GrossEmDefor,
    EmRems$year1$EstEmRemsEnh,
    EmRems$year2$EstEmRemsEnh,
    EmRems$year1$EstEmRemsFDegNonProxy,
    EmRems$year2$EstEmRemsFDegNonProxy
  )
  ER$MpNetEmRems <- CalcMpNetEmRems(
    EmRems$year1$NetEmRems,
    EmRems$year2$NetEmRems
  )

  ### Yearly FRL Hack TODO - FIX
  
  ErpaYearlyFRL = mp_frl$MP_FRL["NetFRL", period]
  ErpaYearlyFRLUCI = mp_frl$UCI["NetFRL", period]
  ErpaYearlyFRLLCI = mp_frl$LCI["NetFRL", period]
  ErpaYearlyFRLDefor = mp_frl$MP_FRL["Defor", period]
  ErpaYearlyFRLDeforUCI = mp_frl$UCI["Defor", period]
  ErpaYearlyFRLDeforLCI = mp_frl$LCI["Defor", period]
  ErpaYearlyFRLDegradation = mp_frl$MP_FRL["Degradation", period]
  ErpaYearlyFRLDegradationUCI =  mp_frl$UCI["Degradation", period]
  ErpaYearlyFRLDegradationLCI = mp_frl$LCI["Degradation", period]
  ErpaYearlyFRLARefor = mp_frl$MP_FRL["ARefor", period]
  ErpaYearlyFRLAReforUCI =  mp_frl$UCI["ARefor", period]
  ErpaYearlyFRLAReforLCI = mp_frl$LCI["ARefor", period]
  ErpaYearlyFRLFPln = mp_frl$MP_FRL["FPln", period]
  ErpaYearlyFRLFPlnUCI =  mp_frl$UCI["FPln", period]
  ErpaYearlyFRLFPlnLCI = mp_frl$LCI["FPln", period]
  ErpaYearlyFRLFDeg = mp_frl$MP_FRL["FDeg", period]
  ErpaYearlyFRLFDegUCI =  mp_frl$UCI["FDeg", period]
  ErpaYearlyFRLFDegLCI = mp_frl$LCI["FDeg", period]
  ErpaYearlyFRLEnh = mp_frl$MP_FRL["Sinks", period]
  ErpaYearlyFRLEnhUCI = mp_frl$UCI["Sinks", period]
  ErpaYearlyFRLEnhLCI = mp_frl$LCI["Sinks", period]
  ErpaYearlyFRLFDegNonProxy = mp_frl$MP_FRL["FDegNonProxy", period]
  ErpaYearlyFRLFDegNonProxyUCI = mp_frl$UCI["FDegNonProxy", period]
  ErpaYearlyFRLFDegNonProxyLCI = mp_frl$LCI["FDegNonProxy", period]

  ER$year1$EstFRL <- frl["NetFRL",2]
  ER$year2$EstFRL <- frl["NetFRL",3]
  ER$year1$EstERs <- CalcMpEstERs(ER$year1$EstFRL, EmRems$year1$NetEmRems)
  ER$year2$EstERs <- CalcMpEstERs(ER$year2$EstFRL, EmRems$year2$NetEmRems)
  ER$MpEstFRL <- CalcMpEstFRL(ErpaYearlyFRL)
  ER$MpEstERs <- CalcMpEstERs(ER$MpEstFRL, ER$MpNetEmRems)
  
  ER$year1$EstFRLDefor <- frl["Defor",2]
  ER$year2$EstFRLDefor <- frl["Defor",3]
  ER$year1$EstERsDefor <- CalcMpEstERsDefor(ER$year1$EstFRLDefor, EmRems$year1$GrossEmDefor)
  ER$year2$EstERsDefor <- CalcMpEstERsDefor(ER$year2$EstFRLDefor, EmRems$year2$GrossEmDefor)
  ER$MpEstFRLDefor <- CalcMpEstFRL(ErpaYearlyFRLDefor)
  ER$MpEstERsDefor <- CalcMpEstERsDefor(ER$MpEstFRLDefor, ER$MpGrossEmDefor) 
  
  ER$year1$EstFRLDegradation <- frl["Degradation",2]
  ER$year2$EstFRLDegradation <- frl["Degradation",3]
  ER$year1$EstERsDegradation <- CalcMpEstERsDegradation(ER$year1$EstFRLDegradation, EmRems$year1$EstEmRemsDegradation)
  ER$year2$EstERsDegradation <- CalcMpEstERsDegradation(ER$year2$EstFRLDegradation, EmRems$year2$EstEmRemsDegradation)
  ER$MpEstFRLDegradation <- CalcMpEstFRL(ErpaYearlyFRLDegradation)
  ER$MpEstERsDegradation <- CalcMpEstERsDegradation(ER$MpEstFRLDegradation, ER$MpEstEmRemsDegradation) 
  
  ER$year1$EstFRLARefor <- frl["ARefor",2]
  ER$year2$EstFRLARefor <- frl["ARefor",3]
  ER$year1$EstERsARefor <- CalcMpEstERsARefor(ER$year1$EstFRLARefor, EmRems$year1$EstRemARefor)
  ER$year2$EstERsARefor <- CalcMpEstERsARefor(ER$year2$EstFRLARefor, EmRems$year2$EstRemARefor)
  ER$MpEstFRLARefor <- CalcMpEstFRL(ErpaYearlyFRLARefor)
  ER$MpEstERsARefor <- CalcMpEstERsARefor(ER$MpEstFRLARefor, ER$MpEstRemARefor) 
  
  ER$year1$EstFRLFPln <- frl["FPln",2]
  ER$year2$EstFRLFPln <- frl["FPln",3]
  ER$year1$EstERsFPln <- CalcMpEstERsFPln(ER$year1$EstFRLFPln, EmRems$year1$NetEmRemsFPln)
  ER$year2$EstERsFPln <- CalcMpEstERsFPln(ER$year2$EstFRLFPln, EmRems$year2$NetEmRemsFPln)
  ER$MpEstFRLFPln <- CalcMpEstFRL(ErpaYearlyFRLFPln)
  ER$MpEstERsFPln <- CalcMpEstERsFPln(ER$MpEstFRLFPln, ER$MpNetEmRemsFPln) 
  
  ER$year1$EstFRLFDeg <- frl["FDeg",2]
  ER$year2$EstFRLFDeg <- frl["FDeg",3]
  ER$year1$EstERsFDeg <- CalcMpEstERsFDeg(ER$year1$EstFRLFDeg, EmRems$year1$EstEmRemsFDeg)
  ER$year2$EstERsFDeg <- CalcMpEstERsFDeg(ER$year2$EstFRLFDeg, EmRems$year2$EstEmRemsFDeg)
  ER$MpEstFRLFDeg <- CalcMpEstFRL(ErpaYearlyFRLFDeg)
  ER$MpEstERsFDeg <- CalcMpEstERsFDeg(ER$MpEstFRLFDeg, ER$MpEstEmRemsFDeg)

  ER$year1$EstFRLFDegNonProxy <- frl["FDegNonProxy",2]
  ER$year2$EstFRLFDegNonProxy <- frl["FDegNonProxy",3]
  ER$year1$EstERsFDegNonProxy <- CalcMpEstERsFDeg(ER$year1$EstFRLFDegNonProxy, EmRems$year1$EstEmRemsFDegNonProxy)
  ER$year2$EstERsFDegNonProxy <- CalcMpEstERsFDeg(ER$year2$EstFRLFDegNonProxy, EmRems$year2$EstEmRemsFDegNonProxy)
  
  ER$MpEstFRLFDegNonProxy <- CalcMpEstFRL(ErpaYearlyFRLFDegNonProxy)
  ER$MpEstERsFDegNonProxy <- CalcMpEstERsFDeg(ER$MpEstFRLFDegNonProxy, ER$MpEstEmRemsFDegNonProxy)
  
  ER$year1$EstFRLEnh <- frl["Sinks",2]
  ER$year2$EstFRLEnh <- frl["Sinks",3]
  ER$year1$EstERsEnh <- CalcMpEstERsEnh(ER$year1$EstFRLEnh, EmRems$year1$EstEmRemsEnh)
  ER$year2$EstERsEnh <- CalcMpEstERsEnh(ER$year2$EstFRLEnh, EmRems$year2$EstEmRemsEnh)
  ER$MpEstFRLEnh <- CalcMpEstFRL(ErpaYearlyFRLEnh)
  ER$MpEstERsEnh <- CalcMpEstERsEnh(ER$MpEstFRLEnh, ER$MpEstEmRemsEnh)
  
  
  ER$year1$EstERsDefEnh <- CalcMpEstERsDefEnh(ER$year1$EstFRLDefor, ER$year1$EstFRLEnh, ER$year1$EstFRLFDegNonProxy, 
                                                EmRems$year1$GrossEmDefor, EmRems$year1$EstEmRemsEnh, EmRems$year1$EstEmRemsFDegNonProxy)
  ER$year2$EstERsDefEnh <- CalcMpEstERsDefEnh(ER$year2$EstFRLDefor, ER$year2$EstFRLEnh, ER$year2$EstFRLFDegNonProxy, 
                                                EmRems$year2$GrossEmDefor, EmRems$year2$EstEmRemsEnh, EmRems$year2$EstEmRemsFDegNonProxy)
  ER$MpEstFRLDefEnh <- CalcMpEstFRLDefEnh(ErpaYearlyFRLDefor, ErpaYearlyFRLEnh, ErpaYearlyFRLFDegNonProxy)
  ER$MpEstERsDefEnh <- CalcMpEstERsDefEnh(ErpaYearlyFRLDefor, ErpaYearlyFRLEnh, ErpaYearlyFRLFDegNonProxy, 
                                          ER$MpGrossEmDefor, ER$MpEstEmRemsEnh, ER$MpEstEmRemsFDegNonProxy)
  return(ER)
}

# Test CalcEmRemsValues


test_that("Test it can be called", {
  # Dummy Monitored Values

  dummyMonitoredValues <- list()

  # data taken from output of FRL Accuracy Assessment
  dummyMonitoredValues$DFAreaLow <- 8332.15 # Area of deforestation in natural forest lowland (ha) # Uncertainty to be considered
  dummyMonitoredValues$DFAreaLow_UCI <- 8437
  dummyMonitoredValues$DFAreaLow_LCI <- 5531

  # data taken from output of FRL Accuracy Assessment
  dummyMonitoredValues$DFAreaUp <- 2681.64 # Area of deforestation in natural forest upland (ha) # Uncertainty to be considered
  dummyMonitoredValues$DFAreaUp_UCI <- 2889
  dummyMonitoredValues$DFAreaUp_LCI <- 1627

  dummyMonitoredValues$ARArea <- 6180 # Area of Afforestation lowland and upland (ha) (Not split into lowland and upland)
  # ARAreaLow      #ARArea = Sum of ARAreaLow and ARAreaUp
  # ARAreaUp       #ARArea = Sum of ARAreaLow and ARAreaUp
  # data taken from output of FRL Accuracy Assessment
  dummyMonitoredValues$ARArea_UCI <- 8124
  dummyMonitoredValues$ARArea_LCI <- 4415

  # Biomass Burned - Area and Average age of forest burned (used FRL data here)
  dummyMonitoredValues$FDBurnData <- read.table("../../data/FRLBurnData.txt", header = T)[235:294, c("year", "area_ha", "age_yrs")]


  # Without Uncertainty
  dummyMonitoredValues$FPVolHarvHW <- 62199.6 # volume of hardwood harvested (m3)
  dummyMonitoredValues$FPAreaStockHW <- 56950.5 # Existing stock at start of yr (hardwood)
  dummyMonitoredValues$FPAreaPlantHW <- 3050.30 # Area planted during the yr (hardwood)
  dummyMonitoredValues$FPAreaHarvestHW <- 3316.60 # Area harvested during the yr (hardwood)
  dummyMonitoredValues$FPAreaJustGrowsHW <- dummyMonitoredValues$FPAreaStockHW - dummyMonitoredValues$FPAreaHarvestHW
  dummyMonitoredValues$FPVolHarvSW <- 334463 # volume of softwood harvested (m3)
  dummyMonitoredValues$FPAreaStockSW <- 49106.0 # Existing stock at start of yr (softwood)
  dummyMonitoredValues$FPAreaPlantSW <- 370.820 # Area planted during the yr (softwood)
  dummyMonitoredValues$FPAreaHarvestSW <- 1282.40 # Area harvested during the yr (softwood)
  dummyMonitoredValues$FPAreaJustGrowsSW <- dummyMonitoredValues$FPAreaStockSW - dummyMonitoredValues$FPAreaHarvestSW
  dummyMonitoredValues$FDFellVol <- 50731.5 # Volume of wood extracted from natural forest (metric tonnes)
  dummyMonitoredValues$FDFellArea <- 11669.9 # Area of natural forest felled (ha)

  expect_silent(result <- CalcEmRemsValues(dummyMonitoredValues))
  # Results Table has expected Deforestation Estimate
  expect_equal(round(result$EmEstDFTotal), 2696827)

  ####################
  # Forest Degradation

  # This value does not match the FRL. The FRL uses an average of 2015-2018, this value is 2018 only.
  # Results Table has expected Forest Degradation Estimate
  expect_equal(round(result$FDEst), 234514)

  #############
  # Enhancement

  # Results Table has expected Enhancement Estimate
  expect_equal(round(result$ECEst), -1227582)

  #######
  # Total

  # Results Table has expected Total Estimate
  expect_equal(round(result$NetEmTotal), 1703759)
})

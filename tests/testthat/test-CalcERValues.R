# Test CalcEmRemsValues
library(testthat)



test_that("Test it can be called", {
  # Dummy Monitored Values

  dummyMonitoredValues <- list()

  # data taken from output of FRL Accuracy Assessment
  dummyMonitoredValues$DeforAreaLow <- 8332.15 # Area of deforestation in natural forest lowland (ha) # Uncertainty to be considered
  dummyMonitoredValues$DeforAreaLow_UCI <- 8437
  dummyMonitoredValues$DeforAreaLow_LCI <- 5531

  # data taken from output of FRL Accuracy Assessment
  dummyMonitoredValues$DeforAreaUp <- 2681.64 # Area of deforestation in natural forest upland (ha) # Uncertainty to be considered
  dummyMonitoredValues$DeforAreaUp_UCI <- 2889
  dummyMonitoredValues$DeforAreaUp_LCI <- 1627

  dummyMonitoredValues$AReforArea <- 6180 # Area of Afforestation lowland and upland (ha) (Not split into lowland and upland)
  # AReforAreaLow      #AReforArea = Sum of AReforAreaLow and AReforAreaUp
  # AReforAreaUp       #AReforArea = Sum of AReforAreaLow and AReforAreaUp
  # data taken from output of FRL Accuracy Assessment
  dummyMonitoredValues$AReforArea_UCI <- 8124
  dummyMonitoredValues$AReforArea_LCI <- 4415

  # Biomass Burned - Area and Average age of forest burned (used FRL data here)
  dummyMonitoredValues$FDegBurnData <- read.table("../../data/FRLBurnData.txt", header = T)[235:294, c("year", "area_ha", "age_yrs")]


  # Without Uncertainty
  dummyMonitoredValues$FPlnVolHarvHwd <- 62199.6 # volume of hardwood harvested (m3)
  dummyMonitoredValues$FPlnAreaStockHwd <- 56950.5 # Existing stock at start of yr (hardwood)
  dummyMonitoredValues$FPlnAreaPlantHwd <- 3050.30 # Area planted during the yr (hardwood)
  dummyMonitoredValues$FPlnAreaHarvHwd <- 3316.60 # Area harvested during the yr (hardwood)
  dummyMonitoredValues$FPlnAreaJustGrowsHwd <- dummyMonitoredValues$FPlnAreaStockHwd - dummyMonitoredValues$FPlnAreaHarvHwd
  dummyMonitoredValues$FPlnVolHarvSwd <- 334463 # volume of softwood harvested (m3)
  dummyMonitoredValues$FPlnAreaStockSwd <- 49106.0 # Existing stock at start of yr (softwood)
  dummyMonitoredValues$FPlnAreaPlantSwd <- 370.820 # Area planted during the yr (softwood)
  dummyMonitoredValues$FPlnAreaHarvSwd <- 1282.40 # Area harvested during the yr (softwood)
  dummyMonitoredValues$FPlnAreaJustGrowsSwd <- dummyMonitoredValues$FPlnAreaStockSwd - dummyMonitoredValues$FPlnAreaHarvSwd
  dummyMonitoredValues$FDegFellVol <- 50731.5 # Volume of wood extracted from natural forest (metric tonnes)
  dummyMonitoredValues$FDegFellArea <- 11669.9 # Area of natural forest felled (ha)

  expect_silent(result <- CalcEmRemsValues(dummyMonitoredValues))
  # Results Table has expected Deforestation Estimate
  expect_equal(round(result$GrossEmDefor), 2696827)

  ####################
  # Forest Degradation

  # This value does not match the FRL. The FRL uses an average of 2015-2018, this value is 2018 only.
  # Results Table has expected Forest Degradation Estimate
  expect_equal(round(result$EstEmRemsFDeg), 234514)

  #############
  # Enhancement

  # Results Table has expected Enhancement Estimate
  expect_equal(round(result$EstEmRemsEnh), -1227582)

  #######
  # Total

  # Results Table has expected Total Estimate
  expect_equal(round(result$NetEmRems), 1703759)
})

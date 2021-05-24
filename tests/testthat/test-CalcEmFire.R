# Test CalcEmFire: Emissions from Biomass Burning


# input FDBurnData2018 is a data frame with 2 columns, one age, one area.
# bioburn_ghgs is a table of greenhouse gas data with 4 columns and 3 rows.
# output file "EmFireResults2018"  below, is total emissions for each type of gas for 2018

FDBurnData <- read.table("../../data/FRLBurnData.txt", header = T)
FDBurnData2018 <- FDBurnData[235:294, c("year", "area_ha", "age_yrs")]
bioburn_ghgs <- read.table("../../data/bioburn_ghgs.txt", header = T)


# add this and full file from sw_barea[235:294,6:9]***********

# make one of these wrong and see if the cell is given************
EmFireResults2018 <- data.frame(
  X.EmCO2AGB. = 58505.16630,
  X.EmCO2BGB. = 15315.02328,
  X.EmCH4. = 7050.242825,
  X.EmN2O. = 1962.515072
)


# test_that("2018 Data example", {
#   expect_equal(CalcEmFire(
#     FDBurnData2018$age_yrs, MAIBsw, RootToShootDryLandSmall, FDBurnData2018$area_ha,
#     bioburn_ghgs
#   ), EmFireResults2018)
# })

# # Get an error:
# test_that("2018 Data example divide by zero", {
#   expect_equal(CalcEmFire(
#     FDBurnData2018$age_yrs, MAIBsw, -1, FDBurnData2018$area_ha,
#     bioburn_ghgs
#   ), EmFireResults2018)
# })

# test_that("2018 Data example", {
# Error
# expect_equal(CalcEmFire(FDBurnData2018$age_yrs,  MAIBsw, RootToShootDryLandSmall, FDBurnData2018$area_ha,
#                        bioburn_ghgs, BiomassToCarbonConv, CarbonToCO2eRatio), sw_barea[235:294,6:9])
# })



# From the FRL the average Emissions from Fire for 4 years was calculated to be 157,487.87.
# For this function below total emissions will be average * 4 (i.e. 2018 - 2015) = 629,951.4765 when using
# all burn data from the 4 years

# swfiret # emissions from each gas type for each year
# FRLTotal <- sum(swfiret$total) # sum of all gas emissions

# test_that("FRL Data example", {
# Error
#  expect_equal(sum(CalcEmFire(FDBurnData$age_yrs,  MAIBsw, RootToShootDryLandSmall, FDBurnData$area_ha,
#                          bioburn_ghgs, BiomassToCarbonConv, CarbonToCO2eRatio)),FRLTotal )
# })

# test_that("FRL Data example", {
#   expect_equal(CalcEmFire(
#     FDBurnData$age_yrs, MAIBsw, RootToShootDryLandSmall, FDBurnData$area_ha,
#     bioburn_ghgs
#   ), 629951.4765)
# })


# test_that("FRL Data example - divide by zero", {
# error
#  expect_equal(sum(CalcEmFire(FDBurnData$age_yrs,  MAIBsw, -1, FDBurnData$area_ha,
#                              bioburn_ghgs, BiomassToCarbonConv, CarbonToCO2eRatio)),FRLTotal )
# })

# test_that("FRL Data example", {
# error
#  expect_equal(sum(CalcEmFire(FDBurnData$age_yrs,  MAIBsw, RootToShootDryLandSmall, FDBurnData$area_ha,
#                              bioburn_ghgs, BiomassToCarbonConv, CarbonToCO2eRatio)),sw_barea[,6:9] )
# })
# sw_barea[,6:9]

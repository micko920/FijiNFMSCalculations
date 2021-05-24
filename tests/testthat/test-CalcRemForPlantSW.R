# Test CalcRemForPlantSW: Removals from Forest Plantations - Softwood


# FPAreaStockSW = 49106 # Existing stock at start of yr (ha) GET FROM PREVIOUS yr?
# FPAreaPlantSW =   370.82 # Area planted during the yr (ha)
# FPAreaHarvestSW = 1282.402 # Area harvested during the yr (ha)

# w <- MAIBsw * 0.47, (Area 1)
# x <- AreaStocked - AreaHarvested * w,  (Area 2)
# y <- AreaPlanted * w, (area 3)
# z <- AreaHarvested * w,
# Total <- x + y + z, convert to CO2e * (-1) (total area converted to CO2e)

# CalcRemForPlantSW <- function(MAIBsw, # Mean annual biomass increment in Softwood Plantations
#                               AreaJustGrowsSW, # Area stocked - area harvested in that year
#                               AreaPlanted, # Area planted during the year
#                               AreaHarvested # Area from area stocked which is harvested during the year
#                         )

test_that("Baseline Estimate example", {
  expect_equal(CalcRemForPlantSW(MAIBsw, 49106, 370.82, 1282.402), -874750.60)
})

test_that("Baseline Data example", {
  expect_equal(CalcRemForPlantSW(MAIBsw, 1, 2, 1), -68.93333330)
})

test_that("all areas sum to zero", {
  expect_equal(CalcRemForPlantSW(MAIBsw, 1, -1, 1), -17.23333330)
})


test_that("one area zero", {
  expect_equal(CalcRemForPlantSW(MAIBsw, 0, 1, 1), -34.46666670)
})


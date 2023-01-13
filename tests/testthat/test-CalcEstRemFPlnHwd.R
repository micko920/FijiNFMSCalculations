# Test CalcEstRemFPlnHwd: Removals from Forest Plantations - Hardwood


# FPlnAreaStockHwd = 56950.45 # Existing stock at start of yr (ha)
# FPlnAreaPlantHwd =  3050.3 # Area planted during the yr (ha)
# FPlnAreaHarvHwd = 3316.6 # Area harvested during the yr (ha)


# x <- AreaStocked - AreaHarvested * MAIChw, (Area 1)
# y <- AreaPlanted * MAIChw, (area 2)
# z <- AreaHarvested * MAIChw (Area 3)
# Total <- x + y + z, convert to CO2e * (-1) Total Area converted to biomass then CO2e

test_that("Baseline Data example", {
  expect_equal(signif(CalcEstRemFPlnHwd(56950.45, 3050.3, 3316.6, MAIVhw, BiomassConvExpansionIncHW, RootToShootTropRain, 1.0),5), -46343)
})

test_that("Baseline Data example", {
  expect_equal(signif(CalcEstRemFPlnHwd(2, 1, 1, MAIVhw, BiomassConvExpansionIncHW, RootToShootTropRain, 1.0),5), -15.193)
})

test_that("One area zero", {
  expect_equal(signif(CalcEstRemFPlnHwd(0, 1, 1, MAIVhw, BiomassConvExpansionIncHW, RootToShootTropRain, 1.0),5), -15.193)
})

test_that("Areas sum to zero", {
  expect_equal(signif(CalcEstRemFPlnHwd(0, 0, 1, MAIVhw, BiomassConvExpansionIncHW, RootToShootTropRain, 1.0),5), -0.0)
})

test_that("negative numbers", {
  expect_equal(signif(CalcEstRemFPlnHwd(-2, 1, 1, MAIVhw, BiomassConvExpansionIncHW, RootToShootTropRain, 1.0),5), -15.1930)
})

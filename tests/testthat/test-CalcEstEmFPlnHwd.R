# Test CalcEstEmFPlnHwd: Emissions from Forest Plantations - Hardwood


# FPlnVolHarvHwd <- 62199.55 # volume of hardwood harvested (m3). Here used mean(hw$vol_m3)


# x <-Volume * BiomassConvExpansionHW, y <- Volume * BiomassConvExpansionHW * RootToShootTropRain RootToShootDryLandBig,
# z <- x + y, then convcarb(z) then convCO2

test_that("Baseline Data example", {
  expect_equal(CalcEstEmFPlnHwd(62199.55, BiomassConvExpansionHW, RootToShootTropRain), 154193.617)
})

test_that("Baseline Data example", {
  expect_equal(CalcEstEmFPlnHwd(1, BiomassConvExpansionHW, RootToShootTropRain), 2.479015)
})

test_that("zeros", {
  expect_equal(CalcEstEmFPlnHwd(0, BiomassConvExpansionHW, RootToShootTropRain), 0)
})

test_that("negative numbers", {
  expect_equal(CalcEstEmFPlnHwd(-0.1, BiomassConvExpansionHW, RootToShootTropRain), -0.2479015)
})

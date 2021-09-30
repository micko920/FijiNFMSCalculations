# Test CalcEstEmFPlnSwd: Emissions from Forest Plantations - Softwood

#  FPlnVolHarvSwd = 334463.2 # volume of softwood harvested (m3) - used average from FRL

# x <-Volume * AGBLossSW, y <- Volume * AGBLossSW * RootToShootDryLandBig,
# z <- x + y, then convcarb(z) then convCO2

test_that("Baseline FRL example", {
  expect_equal(CalcEstEmFPlnSwd(334463.2, RecoveryRateSW, WoodDensitySW, RootToShootDryLandBig), 442001.33)
})

test_that("Baseline Data example", {
  expect_equal(CalcEstEmFPlnSwd(1, RecoveryRateSW, WoodDensitySW, RootToShootDryLandBig), 1.32152456)
})

test_that("zeros", {
  expect_equal(CalcEstEmFPlnSwd(0, RecoveryRateSW, WoodDensitySW, RootToShootDryLandBig), 0)
})

test_that("negative numbers", {
  expect_equal(CalcEstEmFPlnSwd(-0.1, RecoveryRateSW, WoodDensitySW, RootToShootDryLandBig), -0.132152456)
})
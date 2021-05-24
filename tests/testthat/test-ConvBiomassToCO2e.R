
# Convert Biomass to Carbon and then to CO2e = parameter * 0.47 * (44/12) i.e. parameter * 1.723333333333

test_that("Baseline actual Data", {
  expect_equal(ConvBiomassToCO2e(256480.5), 442001.4, tolerance = 1e-4)
})

test_that("Baseline test Data", {
  expect_equal(ConvBiomassToCO2e(1), 1.72333333333)
})

test_that("Baseline test Data fraction", {
  expect_equal(ConvBiomassToCO2e(2 / 7), 0.492380952)
})

test_that("Set parameter to zero", {
  expect_equal(ConvBiomassToCO2e(0), 0)
})


test_that("Negative number", {
  expect_equal(ConvBiomassToCO2e(-1), -1.72333333333)
})

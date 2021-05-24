# Convert Carbon to CO2e results in (parameter * (44/12))


test_that("Baseline Data example - paramter set to 1", {
  expect_equal(ConvCarbonToCO2e(1), 44 / 12)
})

test_that("Baseline Data example - paramter set to 0", {
  expect_equal(ConvCarbonToCO2e(0), 0)
})

test_that("Baseline Data example - paramter set to 1", {
  expect_equal(ConvCarbonToCO2e(1), 3.6666667)
})

test_that("negative entry", {
  expect_equal(ConvCarbonToCO2e(-12), -44)
})

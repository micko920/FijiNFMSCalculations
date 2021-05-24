# Test ConvBiomassToCarbon

# Conv Carb results in (parameter * 0.47)


test_that("Baseline Data example - parameter to 1", {
  expect_equal(ConvBiomassToCarbon(1), 0.47)
})

test_that("Parameter to Zero", {
  expect_equal(ConvBiomassToCarbon(0), 0)
})


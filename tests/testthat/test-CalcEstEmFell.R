# Test CalcEstEmFell, calculates the emissions from felling


# EmFell <- CalcEstEmFell(FDegFellVol , TEF)
# TEF                 = 1.05    # Total Emissions Factor
# FDegFellVol         = 50731.45 # Volume of wood extracted from natural forest (m^3)

# Function: x <- a * TEF, y <- x * 44/12)


test_that("Baseline FRL example", {
  expect_equal(CalcEstEmFell(50731.45, 1.05), 195316.0825)
})


test_that("Test integers", {
  expect_equal(CalcEstEmFell(4, 3), 44)
})


test_that("Multiply by zero", {
  expect_equal(CalcEstEmFell(4, 0), 0)
})


test_that("Multiply by zero", {
  expect_equal(CalcEstEmFell(0, 3), 0)
})

test_that("Multiply by negative", {
  expect_equal(CalcEstEmFell(4, -3), -44)
})

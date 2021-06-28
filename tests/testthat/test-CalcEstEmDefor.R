# Test CalcEstEmDefor Calcalute Emissions from Deforestation


# DeforAreaUp   = 2681.642 # Area of deforestation in natural forest upland (ha)
# DeforAreaLow   = 8332.147 # Area of deforestation in natural forest lowland (ha)
# EFDeforUp          = 199.6785  # Emissions Factor Deforestation Upland
# EFDeforLow         = 259.4006  # Emissions Factor  Deforestation Lowland

# Function multiplies activity data (AD) by Emissions Factor (EF)
# x= a * 199.6785, y= b * 259.4006, z+ a * b

test_that("Baseline FRL Data example", {
  expect_equal(CalcEstEmDefor(2681.642, 199.6785), 535466.2521)
})

test_that("rational number", {
  expect_equal(CalcEstEmDefor(2, (1 / 3)), 0.66666667)
})

test_that("Mult by AD = zero", {
  expect_equal(CalcEstEmDefor(0, 3), 0)
})

test_that("Mult by EF = zero", {
  expect_equal(CalcEstEmDefor(3, 0), 0)
})

test_that("Mult by both zero", {
  expect_equal(CalcEstEmDefor(0, 0), 0)
})

test_that("Mult by negative number", {
  expect_equal(CalcEstEmDefor(-1, 1), -1)
})


test_that("Confirm not equal", {
  expect_false(isTRUE(all.equal(CalcEstEmDefor(1, 2), 20)))
})

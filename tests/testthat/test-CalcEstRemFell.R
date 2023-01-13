# Test CalcEstRemFell calculate removals from Felling

# MAICFell             = 0.99   # Mean annual increment (MAI) of total C (above- and below-ground carbon)
# FDegFellArea         =11669.87
# x <- a * MAICFell, y =  convCO2 (x) * (-1) (i.e. * -44/12)

test_that("Baseline FRL example", {
  expect_equal(CalcEstRemFell(11669.87, 0.99, 1.0), -42361.6281)
})

test_that("Baseline Data example", {
  expect_equal(CalcEstRemFell(1, MAICFell, 1.0), -3.63)
})

test_that("Baseline Data example", {
  expect_equal(CalcEstRemFell(1, 1, 1.0), (-44 / 12))
})

test_that("Baseline Data example", {
  expect_equal(CalcEstRemFell((1 / 3), 1, 1.0), (-44 / 36))
})


test_that("Zeros", {
  expect_equal(CalcEstRemFell(0, MAICFell, 1.0), 0)
})

test_that("negative numbers", {
  expect_equal(CalcEstRemFell(-1, MAICFell, 1.0), 3.63)
})

# Test CalcEstRemARefor Removals from Afforestation


# Yearly Removals from Afforestation  (tCO2e)
# MAICar                 =  2.628283  # Mean annual total carbon increment including AGB and BGB
# AReforAreaUp = 1224.132  # Area of afforestation in natural forest upland (ha)
# AReforAreaLow = 4955.452 # Area of afforestation in natural forest lowland (ha)
# x <- a + b, y <-  x * MAICar * (-1) * 44/12


test_that("Baseline Estimate example", {
  expect_equal(CalcEstRemARefor(1224.132, 4955.452, MAIVar, BiomassConvExpansionARefor, RootToShootTropRain), (-59540.940))
})

test_that("Baseline Data example", {
  expect_equal(CalcEstRemARefor(1, 0, MAIVar, BiomassConvExpansionARefor, RootToShootTropRain), (-9.635105))
})

test_that("Areas both zero", {
  expect_equal(CalcEstRemARefor(0, 0, MAIVar, BiomassConvExpansionARefor, RootToShootTropRain), 0)
})

test_that("Total area = 1", {
  expect_equal(CalcEstRemARefor(0.1, 0.9, MAIVar, BiomassConvExpansionARefor, RootToShootTropRain), (-9.635105))
})

test_that("Negative numbers", {
  expect_equal(CalcEstRemARefor(-0.1, -0.9, MAIVar, BiomassConvExpansionARefor, RootToShootTropRain), (9.635105))
})

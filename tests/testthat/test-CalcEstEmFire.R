library(testthat)

## This test needs to be reviewed/redone ##

# Test CalcEmFire: Emissions from Biomass Burning


# input FDegBurnData2018 is a data frame with 2 columns, one age, one area.
# bioburn_ghgs is a table of greenhouse gas data with 4 columns and 3 rows.
# output file "EmFireResults2018"  below, is total emissions for each type of gas for 2018

FDegBurnData <- read.table("../../data/FRLBurnData.txt", header = T)
FDegBurnData2018 <- FDegBurnData[235:294, c("year", "area_ha", "age_yrs")]
bioburn_ghgs <- read.table("../../data/bioburn_ghgs.txt", header = T)


compare_summary_equal <- function(samples, min, qtr1, med, u, qtr3, max, sigfig, ...) {
  sample_summary <- stats::quantile(samples)
  sample_summary <- signif(c(sample_summary[1L:3L], mean(samples), sample_summary[4L:5L]), sigfig)
  names(sample_summary) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
  
  expect_summary <- c(min, qtr1, med, u, qtr3, max)
  names(expect_summary) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
  
  return(expect_equal(sample_summary, expect_summary, ...))
}


get_test_data <- function(yrs,ha) {
  test_data <- list()
  #    COMF i 	Gg,i 
  #CO2	0.46	1580
  #N2O	0.46	6.8
  #CH4	0.46	0.2
  test_data$COMF_CO2 <- 0.46
  test_data$COMF_N2O <- 0.46
  test_data$COMF_CH4 <- 0.46
  
  test_data$EF_CO2 <- 1580
  test_data$EF_N2O <- 0.2
  test_data$EF_CH4 <- 6.8
  
  #GWP - AR5
  #CO2	1
  #N2O	28
  #CH4	265
  test_data$GWP_CO2 <- 1
  test_data$GWP_N2O <- 265
  test_data$GWP_CH4 <- 28
  
  #Annual increment in Pine Plantation (tB ha-1 yr-1) 			          10	From Waterloo [1994]
  #Annual increment in Pine Plantation (tB ha-1 yr-1) Aboveground			8	
  #Annual increment in Pine Plantation (tB ha-1 yr-1) Belowground			2	
  
  #Root:to:Shoott ratio			0.2	
  test_data$RootToShootRatio <- 0.2
  
  #MAIB_AGB = 10 * 0.8 = 8
  #MAIB_BGB = 10 * 0.2 = 2
  test_data$MAIB <- 10
  test_data$MAIB_AGB <- test_data$MAIB * (1 - test_data$RootToShootRatio)
  test_data$MAIB_BGB <- test_data$MAIB * test_data$RootToShootRatio
  
  expect_equal(MAIBsw,test_data$MAIB)
  
  test_data$AreaBurnt <- ha
  test_data$Age       <- yrs
  
  test_data$AGB_Stock <- test_data$Age * test_data$MAIB_AGB
  
  test_data$BGB_Stock <- test_data$Age * test_data$MAIB_BGB
  
  #EM_CO2_ABG = ((AreaBurent * AGB_Stock * COMF_CO2 * EF_CO2) / 1000) * GWP_CO2
  #EM_CO2_ABG = ((4.9 * 32 * 0.46 * 1580) / 1000) * 1 = 114
  test_data$EM_CO2_ABG <- test_data$AreaBurnt * test_data$AGB_Stock * 
    test_data$COMF_CO2 * test_data$EF_CO2 * 0.001 * test_data$GWP_CO2
  
  #EM_CO2_BGB = AreaBurent * BGB * 0.47 * (44/12)
  #EM_CO2_BGB = 4.9 * 8 * 0.47 * (44/12) = 68
  test_data$EM_CO2_BGB <- test_data$AreaBurnt * test_data$BGB_Stock * 0.47 * (44/12)
  
  #EM_N2O_ABG = ((AreaBurent * AGB_Stock * COMF_N2O * EF_N2O) / 1000) * GWP_N2O
  #EM_N2O_ABG = ((4.9 * 32 * 0.46 * 1580) / 1000) * 1 = 114
  test_data$EM_N2O_ABG <- test_data$AreaBurnt * test_data$AGB_Stock * 
    test_data$COMF_N2O * test_data$EF_N2O * 0.001 * test_data$GWP_N2O
  
  #EM_CH4_ABG = ((AreaBurent * AGB_Stock * COMF_CH4 * EF_CH4) / 1000) * GWP_CH4
  #EM_CH4_ABG = ((4.9 * 32 * 0.46 * 1580) / 1000) * 1 = 114
  test_data$EM_CH4_ABG <- test_data$AreaBurnt * test_data$AGB_Stock * 
    test_data$COMF_CH4 * test_data$EF_CH4 * 0.001 * test_data$GWP_CH4
  
  #Em = CO2_AGB + N2O_AGB + CH4_AGB + CO2_BGB
  test_data$EM <- test_data$EM_CO2_ABG + test_data$EM_N2O_ABG + test_data$EM_CH4_ABG + test_data$EM_CO2_BGB
  
  ## FRL params
  test_data$params <- list()
  test_data$params$etacf    <- 0.47
  test_data$params$etacc    <- 44/12 
  test_data$params$rdlk1    <- 0.20
  test_data$params$lcirdlk1 <- 0.09
  test_data$params$ucirdlk1 <- 0.25
  test_data$params$maibp    <- 10
  test_data$params$errmaibp <- 0.25
  test_data$params$sdCO2EF  <- 90
  test_data$params$errghg   <- 0.00001
  test_data$params$runs     <- 10000
  test_data$params$qlci     <- 0.05
  test_data$params$quci     <- 0.95
  
  return(test_data)
  
}





test_that("Single Data example", {
  #PlantingYear_YEAR	AREABURNT	YEARBURN	AGE	AbovegroundBiomass Stock	BelowgroundBiomass Stock	CO2	N20	CH4	CO2
  #2012	              4.9	      2015	    4	  32	                      8	                        114	4	  14	68
  test_data <- get_test_data(c(4),c(4.9))
  expect_equal(bioburn_ghgs$combustion_factor[1],test_data$COMF_CO2)
  expect_equal(bioburn_ghgs$combustion_factor[3],test_data$COMF_N2O)
  expect_equal(bioburn_ghgs$combustion_factor[2],test_data$COMF_CH4)
  expect_equal(CombustFactor,test_data$COMF_CO2)
  expect_equal(CombustFactor,test_data$COMF_N2O)
  expect_equal(CombustFactor,test_data$COMF_CH4)
  
  expect_equal(bioburn_ghgs$emission_factor[1],test_data$EF_CO2)
  expect_equal(bioburn_ghgs$emission_factor[3],test_data$EF_N2O)
  expect_equal(bioburn_ghgs$emission_factor[2],test_data$EF_CH4)
  expect_equal(EFCO2,test_data$EF_CO2)
  expect_equal(EFN2O,test_data$EF_N2O)
  expect_equal(EFCH4,test_data$EF_CH4)
  
  expect_equal(bioburn_ghgs$global_warming_potential[1],test_data$GWP_CO2)
  expect_equal(bioburn_ghgs$global_warming_potential[3],test_data$GWP_N2O)
  expect_equal(bioburn_ghgs$global_warming_potential[2],test_data$GWP_CH4)
  expect_equal(GWPCO2,test_data$GWP_CO2)
  expect_equal(GWPN2O,test_data$GWP_N2O)
  expect_equal(GWPCH4,test_data$GWP_CH4)
  
  
  expect_equal(RootToShootDryLandSmall,test_data$RootToShootRatio)
  expect_equal(MAIBsw,test_data$MAIB)
  expect_equal(8,test_data$MAIB_AGB)
  expect_equal(2,test_data$MAIB_BGB)
  expect_equal(32,test_data$AGB_Stock)
  expect_equal(8,test_data$BGB_Stock)
  expect_equal(signif(test_data$EM_CO2_ABG,7),113.9622)
  expect_equal(signif(test_data$EM_CO2_BGB,5),67.555)
  expect_equal(signif(test_data$EM_N2O_ABG,5),3.8228)
  expect_equal(signif(test_data$EM_CH4_ABG,5),13.7330)
  expect_equal(signif(test_data$EM,6),199.073)
  
  
 
})

test_that("Multi Data example", {
  #PlantingYear_YEAR	AREABURNT	YEARBURN	AGE	AbovegroundBiomass Stock	BelowgroundBiomass Stock	CO2	N20	CH4	CO2
  #2012	              4.9	  2015	4	  32	8	  114	  4	  14	  68
  #2014	              8.1	  2015	2	  16	4	  94	  3	  11	  56
  #2015	              25.93	2015	1	  8	  2	  151	  5	  18	  89
  #2015	              9.7	  2015	1	  8	  2	  56	  2	  7	    33
  #1994	              7.8	  2015	22	176	44	998	  33	120	  591
  #2002	              15	  2015	14	112	28	1221	41	147	  724
  #2001	              6.2	  2015	15	120	30	541	  18	65	  321
  #1991	              47.8	2015	25	200	50	6948	233	837	  4119
  #2015	              2.4	  2015	1	  8	  2	  14	  0	  2	    8
  #2014	              3.9	  2015	2	  16	4	  45	  2	  5	    27
  #2012	              5.5	  2015	4	  32	8	  128	  4	  15	  76
  #2012	              18	  2015	4	  32	8	  419	  14	50	  248
  #2012	              41.2	2015	4	  32	8	  958	  32	115	  568
  #Total              196.43 ##   #   792 198 11687 392 1408  6927
  #Grand Total 20415
  
  test_data <- get_test_data(c(4,2,1,1,22,14,15,25,1,2,4,4,4),
                             c(4.9,8.1,25.93,9.7,7.8,15,6.2,47.8,2.4,3.9,5.5,18,41.2))
  
  expect_equal(c(32,16,8,8,176,112,120,200,8,16,32,32,32),
               test_data$AGB_Stock)
  expect_equal(c(8,4,2,2,44,28,30,50,2,4,8,8,8),
               test_data$BGB_Stock)
  
  expect_equal(
    floor(test_data$EM_CO2_ABG),
    c(113,94,150,56,997,1221,540,6948,13,45,127,418,958)
  )
  expect_equal(floor(sum(test_data$EM_CO2_ABG)),11687)
  
  expect_equal(
    floor(test_data$EM_N2O_ABG),
    c(3,3,5,1,33,40,18,233,0,1,4,14,32)
  )
  expect_equal(floor(sum(test_data$EM_N2O_ABG)),392)
  
  expect_equal(
    floor(test_data$EM_CH4_ABG),
    c(13,11,18,6,120,147,65,837,1,5,15,50,115)
  )
  expect_equal(floor(sum(test_data$EM_CH4_ABG)),1408)
  
  expect_equal(
    floor(test_data$EM_CO2_BGB),
    c(67,55,89,33,591,723,320,4118,8,26,75,248,568)
  )
  expect_equal(floor(sum(test_data$EM_CO2_BGB)), 6927)
  
  expect_equal(floor(sum(test_data$EM)),20415)
})

test_that("Test function - Single Data example", {
  #PlantingYear_YEAR	AREABURNT	YEARBURN	AGE	AbovegroundBiomass Stock	BelowgroundBiomass Stock	CO2	N20	CH4	CO2
  #2012	              4.9	      2015	    4	  32	                      8	                        114	4	  14	68
  test_data <- get_test_data(c(4),c(4.9))

  expect_equal(
    CalcEstEmFire(
      c(4), 
      MAIBsw, RootToShootDryLandSmall, 
      c(4.9)
    ), 
    sum(test_data$EM))
  
  expect_equal(signif(
    CalcEstEmFire(
      c(4), 
      MAIBsw, RootToShootDryLandSmall, 
      c(4.9)
    ), 6), 
    199.0730)
  
  expect_equal(signif(CalcEstEmFire(
    FDegBurnData2018$age_yrs, MAIBsw, RootToShootDryLandSmall, FDegBurnData2018$area_ha
  ), 6), 98110.7)
})


test_that("Test Function - Multi Data example", {
  #PlantingYear_YEAR	AREABURNT	YEARBURN	AGE	AbovegroundBiomass Stock	BelowgroundBiomass Stock	CO2	N20	CH4	CO2
  #2012	              4.9	  2015	4	  32	8	  114	  4	  14	  68
  #2014	              8.1	  2015	2	  16	4	  94	  3	  11	  56
  #2015	              25.93	2015	1	  8	  2	  151	  5	  18	  89
  #2015	              9.7	  2015	1	  8	  2	  56	  2	  7	    33
  #1994	              7.8	  2015	22	176	44	998	  33	120	  591
  #2002	              15	  2015	14	112	28	1221	41	147	  724
  #2001	              6.2	  2015	15	120	30	541	  18	65	  321
  #1991	              47.8	2015	25	200	50	6948	233	837	  4119
  #2015	              2.4	  2015	1	  8	  2	  14	  0	  2	    8
  #2014	              3.9	  2015	2	  16	4	  45	  2	  5	    27
  #2012	              5.5	  2015	4	  32	8	  128	  4	  15	  76
  #2012	              18	  2015	4	  32	8	  419	  14	50	  248
  #2012	              41.2	2015	4	  32	8	  958	  32	115	  568
  #Total              196.43 ##   #   792 198 11687 392 1408  6927
  #Grand Total 20415
  
  test_data <- get_test_data(c(4,2,1,1,22,14,15,25,1,2,4,4,4),
                             c(4.9,8.1,25.93,9.7,7.8,15,6.2,47.8,2.4,3.9,5.5,18,41.2))
  expect_equal(
    CalcEstEmFire(
      c(4,2,1,1,22,14,15,25,1,2,4,4,4), 
      MAIBsw, RootToShootDryLandSmall, 
      c(4.9,8.1,25.93,9.7,7.8,15,6.2,47.8,2.4,3.9,5.5,18,41.2)
    ), 
    sum(test_data$EM))
  
  expect_equal(signif(
    CalcEstEmFire(
      c(4,2,1,1,22,14,15,25,1,2,4,4,4), 
      MAIBsw, RootToShootDryLandSmall, 
      c(4.9,8.1,25.93,9.7,7.8,15,6.2,47.8,2.4,3.9,5.5,18,41.2)
    ), 6), 
    20415.4)

  
  expect_equal(floor(sum(test_data$EM)),20415)
})


test_that("FRL - test basic call calcFRLBurning", {
  sw_barea <- FDegBurnData
  debug_frl <- 0
  FRLParams <- get_test_data(c(1),c(1))$params
  #FRLParams$runs <- 100
  set.seed(08121976) # Seed set to remove random nature of MC Analysis for LCI & UCI
  
  fire <- calcFRLBurningRun(debug_frl, FDegBurnData,FRLParams,bioburn_ghgs)
  #expect_equal(floor(fire$rs_fd_bb$aa_em_tco2e_yr),157487)
  expect_equal(floor(fire$rs_fd_bb$aa_em_tco2e_yr),186535)
  #expect_equal(floor(fire$rs_fd_bb$lci_aa_em_tco2e_yr),128967)
  expect_equal(floor(fire$rs_fd_bb$lci_aa_em_tco2e_yr),150304)
  #expect_equal(floor(fire$rs_fd_bb$uci_aa_em_tco2e_yr),185801)
  expect_equal(floor(fire$rs_fd_bb$uci_aa_em_tco2e_yr),218021)
  #expect_equal(floor(fire$fd_bb_aae),157487)
  expect_equal(floor(fire$fd_bb_aae),186535)
  #compare_summary_equal(fire$v_fd_bb_aae,107818,144465,156477,156794,168709,219855,6)
  compare_summary_equal(fire$v_fd_bb_aae,123041,168522,182767,183242,197406,256134,6)
})

#' @export
calcFRLHardwoodPlantations <- function() {

  # Volumes extracted from Hardwood Plantations
  # These data were provided by Fiji Hardwood Corporation Limited (FHCL)
  hw <- hwsw_volharv[, 1:2] # Hardwood data
  names(hw) <- c("year", "vol_m3") # Rename columns
  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLHardwoodPlantations.R", ":10"))
    print(hw) # Print 'hw'
  }


  # Compute AGB for extracted volumes for the years 2006 to 2016
  hw$agb_extracted_t <- hw$vol_m3 * FRLParams$bcefrhw
  # Compute BGB for extracted volumes for the years 2006 to 2016
  hw$bgb_extracted_t <- hw$vol_m3 * FRLParams$bcefrhw * FRLParams$Rlwk
  # Compute TB for extracted volumes for the years 2006 to 2016
  hw$biomass_extracted_t <- hw$agb_extracted_t + hw$bgb_extracted_t

  # Convert to total carbon
  hw$carbon_extracted_t <- hw$biomass_extracted_t * FRLParams$etacf
  # Average annual carbon extraction
  mcem <- mean(hw$carbon_extracted_t)

  # Uncertainty assessment (Monte Carlo simulations)
  # Vector to collect simulation runs
  resmcem <- vector()

  # Run simulation
  for (i in 1:FRLParams$runs) { # i <- 1
    # Draw a random biomass conversion and expansion factor from a Triangular dist.
    bcefrmi <- rtriangle(1,
      theta = FRLParams$bcefrhw,
      lower = FRLParams$bcefrhw - FRLParams$bcefrhw * FRLParams$errbcefrhw,
      upper = FRLParams$bcefrhw + FRLParams$bcefrhw * FRLParams$errbcefrhw
    )
    # Draw a random root-to-shoot ratio (tropical rainforest)
    rwli <- rtriangle(1, FRLParams$Rlwk, FRLParams$Rlwk - FRLParams$Rlwk * FRLParams$errRlwk, FRLParams$Rlwk + FRLParams$Rlwk * FRLParams$errRlwk)
    # Average annual C loss
    resmcem[i] <- mean(((hw$vol_m3 * bcefrmi) + (hw$vol_m3 * bcefrmi * rwli)) *
      FRLParams$etacf)
  }

  # Average annual emissions from Hardwood Plantations
  (ec_hw_aae <- mcem * FRLParams$etacc) # Estimate
  lci_ec_hw_aae <- quantile(resmcem * FRLParams$etacc, probs = FRLParams$qlci) # Lower CI limit
  uci_ec_hw_aae <- quantile(resmcem * FRLParams$etacc, probs = FRLParams$quci) # Upper CI limit
  v_ec_hw_aae <- resmcem * FRLParams$etacc # MC estimates


  # Mean annual increment (tree biomass)
  maibm <- FRLParams$maivhww * FRLParams$bcefihw + FRLParams$maivhww * FRLParams$bcefihw * FRLParams$Rlwk

  # Mean annual increment C [t ha^-1 yr^-1]
  maicm <- maibm * FRLParams$etacf

  # Area planted between 2006 and 2010 (data from FHCL)
  area_planted_0110_ha <- 3050.3 # in hectares

  # Area stocked in 2011
  A_ha_2011 <- 56652

  # Areas harvested and planted in Hardwood Plantations
  hw$ahmt <- hw_ahp[, 2] # Areas harvested
  hw$apmt <- hw_ahp[, 3] # Areas planted

  # Area stocked December 31, 2005
  A_ha_2005 <- A_ha_2011 + sum(hw$ahmt[1:5]) - sum(hw$apmt[1:5])

  # Area that was neither harvested nor planted between 2006 and 2016. It just grows...
  (atm <- A_ha_2005 - sum(hw$ahmt))

  # Mean annual C removals on areas that just grow during the Reference Period
  ctm <- atm * maicm

  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLHardwoodPlantations.R", ":79"))
    print(ctm)
  }

  # Accumulation of C on planted areas over the Reference Period
  hw$cpmt <- hw$apmt * FRLParams$deltaT * maicm
  # Average annual C accumulation on planted areas over the Reference Period
  mcpm <- mean(hw$cpmt)

  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLHardwoodPlantations.R", ":89"))
    print(hw$cpmt)
    print(mcpm)
  }

  # C accumulation on areas that were harvested in year t
  mchm <- mean(hw$ahmt * FRLParams$rdeltaT * maicm)

  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLHardwoodPlantations.R", ":107"))
    print(hw$ahmt)
    print(mchm)
  }

  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLHardwoodPlantations.R", ":113"))
    harvested_cstock <- growthTotals(
      FRLParams$Ty, hw$ahmt,
      growthMatrix(FRLParams$Ty, hw$ahmt, FRLParams$deltaT) * maicm
    )
    print(harvested_cstock)
  }

  # Total average annual C removals
  mcrm <- mcpm + ctm + mchm

  # Uncertainty assessment (removals) ....................................................
  resmcrm <- vector() # Vector that collects the results

  # Run simulation .......................................................................
  for (i in 1:FRLParams$runs) { # i <- 1
    hwi <- hw # Create a copy of 'hw'
    # Random realization of MAI volume
    maivmi <- rtriangle(1,
      theta = FRLParams$maivhww,
      lower = FRLParams$maivhww - FRLParams$maivhww * FRLParams$errmaivhw,
      upper = FRLParams$maivhww + FRLParams$maivhww * FRLParams$errmaivhw
    )
    # Random realization of BCEF_IM
    bcefimi <- rtriangle(1,
      theta = FRLParams$bcefihw,
      lower = FRLParams$bcefihw - FRLParams$bcefihw * FRLParams$errbcefihw,
      upper = FRLParams$bcefihw + FRLParams$bcefihw * FRLParams$errbcefihw
    )
    # Random root-to-shoot ratio for Wet Lowland
    rwli <- rtriangle(1,
      theta = FRLParams$Rlwk,
      lower = FRLParams$Rlwk - FRLParams$Rlwk * FRLParams$errRlwk,
      upper = FRLParams$Rlwk + FRLParams$Rlwk * FRLParams$errRlwk
    )
    # Compute MAI for C
    maicmi <- (maivmi * bcefimi + maivmi * bcefimi * rwli) * FRLParams$etacf
    # Average annual C accumulation on areas that just grow
    atmi <- rtriangle(
      n = 1,
      theta = atm,
      lower = atm - atm * FRLParams$errHwPlantations,
      upper = atm + atm * FRLParams$errHwPlantations
    )
    ctmi <- atmi * maicmi
    # Carbon accumulation in plantations that were harvested in year t
    cthmi <- mean(hw$ahmt * FRLParams$rdeltaT * maicmi)
    # C accumulation on planted areas over the Reference Period ........................
    ## Random draw for the area planted in year t (2006-2010)
    apmt2001_2010 <- runif(n = 10, min = 0, max = area_planted_0110_ha)
    ## Ensure that the sum of random draws does not exceed the total area planted
    ## between 2001 and 2010
    apmt2001_2010 <- apmt2001_2010 * area_planted_0110_ha / sum(apmt2001_2010)
    # Combine sampled areas planted (2006-2010) and reported areas planted (2011-2016)
    hwi$apmt <- c(apmt2001_2010[6:10], hw$apmt[6:11])
    # Compute C gain on areas planted
    hwi$cpmt <- hwi$apmt * FRLParams$deltaT * maicmi
    # Total average annual C accumulation
    resmcrm[i] <- mean(hwi$cpmt) + # C gain areas planted
      ctmi + # C gain on areas that "just grow"
      cthmi # C gains from harvested compartments
  }

  # Average annual removals from Hardwood Plantations
  ec_hw_aar <- mcrm * FRLParams$etacc # Estimate
  lci_ec_hw_aar <- quantile(resmcrm * FRLParams$etacc, probs = FRLParams$qlci) # Lower CI limit
  uci_ec_hw_aar <- quantile(resmcrm * FRLParams$etacc, probs = FRLParams$quci) # Upper CI limit
  v_ec_hw_aar <- resmcrm * FRLParams$etacc # MC estimates

  # Net emissions from Hardwood Plantations
  ec_hw_aane <- ec_hw_aae - ec_hw_aar # Point estimate
  v_ec_hw_aane <- v_ec_hw_aae - v_ec_hw_aar # MC estimates

  # Net emissions Hardwood Plantations
  lciv_ec_hw_aane <- quantile(v_ec_hw_aae -   # Emissions Hardwood
                              v_ec_hw_aar,    # Removals Hardwood
                              probs = FRLParams$qlci)
  uciv_ec_hw_aane <- quantile(v_ec_hw_aae -
                              v_ec_hw_aar,
                              probs = FRLParams$quci)

  # Growth tables projection rather than average yearly growth
  yearly_growth <- c(0.5,rep_len(1,FRLParams$Tl-1))
  area <- hw$apmt

  hwd_planted_cstock <- growthTotals(
        FRLParams$Ty, area,
        growthMatrix(FRLParams$Ty,
                     area,
                     yearly_growth,
                     projection=7, offset = 14) * maicm,
        projection=7, offset = 14)

  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLHardwoodPlantations", ":204"))
    print(hwd_planted_cstock)
  }

  # Uncertainty analysis
  vhwcstock <- matrix(nrow=0,ncol=length(FRLParams$Ty)+4)
  gm <- growthMatrix(FRLParams$Ty,rep_len(1,length(FRLParams$Ty)),c(0.5,rep_len(1,length(FRLParams$Ty)-1)), projection=7, offset = 14)

  mArea  <- mean(area)
  # MC simulation
  for (i in 1:FRLParams$runs) { # i <- 1
    vAreas <- rtriangle(
      n = 1,
      theta = mArea,
      lower = mArea - mArea * FRLParams$errHwPlantations,
      upper = mArea + mArea * FRLParams$errHwPlantations
    )
    # Random realization of MAI volume
    maivmi <- rtriangle(1,
      theta = FRLParams$maivhww,
      lower = FRLParams$maivhww - FRLParams$maivhww * FRLParams$errmaivhw,
      upper = FRLParams$maivhww + FRLParams$maivhww * FRLParams$errmaivhw
    )
    # Random realization of BCEF_IM
    bcefimi <- rtriangle(1,
      theta = FRLParams$bcefihw,
      lower = FRLParams$bcefihw - FRLParams$bcefihw * FRLParams$errbcefihw,
      upper = FRLParams$bcefihw + FRLParams$bcefihw * FRLParams$errbcefihw
    )
    # Random root-to-shoot ratio for Wet Lowland
    rwli <- rtriangle(1,
      theta = FRLParams$Rlwk,
      lower = FRLParams$Rlwk - FRLParams$Rlwk * FRLParams$errRlwk,
      upper = FRLParams$Rlwk + FRLParams$Rlwk * FRLParams$errRlwk
    )
    # Compute MAI for C
    maicmi <- (maivmi * bcefimi + maivmi * bcefimi * rwli) * FRLParams$etacf
    r <- colSums(gm * vAreas * maicmi)
    if (i==1) vhwcstock <- r
    else vhwcstock <- rbind(vhwcstock, r)
  }
  colnames(vhwcstock) <- colnames(gm)

  # Yearly removals
  ec_hw_cstock <- hwd_planted_cstock[c(-1,-2)] * FRLParams$etacc# Estimate
  mucstock <- apply(vhwcstock * FRLParams$etacc,2, mean)
  lcihwcstock <- apply(vhwcstock * FRLParams$etacc,2, quantile, probs = FRLParams$qlci) # Lower confidence limit
  ucihwcstock <- apply(vhwcstock * FRLParams$etacc,2, quantile, probs = FRLParams$quci) # Upper confidence limit
  v_ec_hw_cstock <- vhwcstock * FRLParams$etacc * -1# MC estimates
  # Result table CStock
  rs_ec_hw_cstock <- data.frame(
    rbind(
      removals_tco2e_yr = ec_hw_cstock * -1,
      lci_removals_tco2e_yr = lcihwcstock * -1,
      uci_removals_tco2e_yr = ucihwcstock * -1,
      mu_removals_tco2e_yr = mucstock * -1),
    check.names = F
  )


  result <- list()
  result$hw_h_area <- hw$ahmt
  result$hw_p_area <- hw$apmt
  result$ec_hw_aae <- ec_hw_aae
  result$ec_hw_aar <- ec_hw_aar
  result$ec_hw_aane <- ec_hw_aane
  result$lci_ec_hw_aar <- lci_ec_hw_aar
  result$uci_ec_hw_aar <- uci_ec_hw_aar
  result$lci_ec_hw_aae <- lci_ec_hw_aae
  result$uci_ec_hw_aae <- uci_ec_hw_aae
  result$v_ec_hw_aar <- v_ec_hw_aar
  result$v_ec_hw_aae <- v_ec_hw_aae
  result$v_ec_hw_aane <- v_ec_hw_aane
  result$lciv_ec_hw_aane <- lciv_ec_hw_aane
  result$uciv_ec_hw_aane <- uciv_ec_hw_aane
  result$lciv_ec_hw_aane <- lciv_ec_hw_aane
  result$uciv_ec_hw_aane <- uciv_ec_hw_aane
  result$rs_ec_hw_cstock <- rs_ec_hw_cstock
  result$v_ec_hw_cstock <- v_ec_hw_cstock
  return(result)
}

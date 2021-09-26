
#' @export
calcFRLSoftwoodPlantations <- function() {
  # Volumes extracted from Softwood Plantations
  sw <- hwsw_volharv[, c(1, 3)] # Softwood data
  names(sw) <- c("year", "vol_m3") # Rename columns
  if (debug_frl) print(sw) # Print 'sw'

  volToAgbPine <- 1 / FRLParams$volTovol * FRLParams$wdsw

  # Compute AGB for extracted volumes for the years 2006 to 2016
  sw$agb_extracted_t <- sw$vol_m3 * volToAgbPine
  # Compute BGB for extracted volumes for the years 2006 to 2016
  sw$bgb_extracted_t <- sw$vol_m3 * volToAgbPine * FRLParams$rdlk2
  # Compute TB for extracted volumes for the years 2006 to 2016
  sw$biomass_extracted_t <- sw$agb_extracted_t + sw$bgb_extracted_t
  # Convert to total carbon
  sw$carbon_extracted_t <- sw$biomass_extracted_t * FRLParams$etacf
  # Convert to carbon dioxide equivalents
  sw$co2_emissions <- sw$carbon_extracted_t * FRLParams$etacc

  # Average annual carbon extraction
  mcep <- mean(sw$carbon_extracted_t)

  # Uncertainty assessment (emissions from Softwood Plantations)
  # List to collect simulation runs
  rescept <- list() # Needed later (MC simulation for removals)

  # Run MC simulation ....................................................................
  for (i in 1:FRLParams$runs) { # i <- 1
    # Draw a random volTovol (recovery proportion Softwood)
    volToAgbPinei <- 1 / rnorm(
      1, FRLParams$volTovol,
      FRLParams$volTovol *
        FRLParams$errvolTovol
    ) *
      rnorm(1, FRLParams$wdsw, FRLParams$sdwdsw)

    # Random root-to-shoot ratio (tropical moist deciduous forest; IPCC default)
    rdli <- rtriangle(n = 1, theta = FRLParams$rdlk2, lower = FRLParams$lcirdlk2, upper = FRLParams$ucirdlk2)

    # Total carbon (AGC + BGC) loss for the years 2006 to 2016
    rescept[[i]] <- ((sw$vol * volToAgbPinei) +
      (sw$vol * volToAgbPinei * rdli)) * FRLParams$etacf
  }

  # Average per year
  respcem <- sapply(rescept, mean)

  # Average annual emissions from Softwood Plantations
  ec_sw_aae <- mcep * FRLParams$etacc # Estimate
  lci_ec_sw_aae <- quantile(respcem * FRLParams$etacc, probs = FRLParams$qlci) # Lower CI limit
  uci_ec_sw_aae <- quantile(respcem * FRLParams$etacc, probs = FRLParams$quci) # Upper CI limit
  v_ec_sw_aae <- respcem * FRLParams$etacc # MC estimates

  # 3 Sep 2021 - Data provided by Carly Green from Fiji Pine
  netStockedArea <- data.frame(
    year = c(2006:2016),
    area = c(
      33071,
      33872,
      33509,
      32336,
      32322,
      31334,
      30897,
      30601,
      31117,
      29527,
      23960
    ))

  netStockedArea$c_t  <- FRLParams$maicp * netStockedArea$area
  print(mean(netStockedArea$c_t))


  A2006 <- 49503
  sw$area_harvested_ha <- sw$carbon_extracted_t / (FRLParams$maicp * FRLParams$cuttingc)
  sw$area_planted_ha <- sw_hvol_parea[,3]
  A2005 <- A2006 + sw$area_harvested_ha[1] - sw$area_planted_ha[1]

  # Area that was neither planted nor harvested during the Reference Period; they just
  # growth...
  atp <- A2005 - sum(sw$area_harvested_ha)
  # Mean annual C removals on areas that just grow during the Reference Period
  ctp <- atp * FRLParams$maicp
  # Accumulation of C on planted areas over the Reference Period
  sw$cgain_t <- sw$area_planted_ha * FRLParams$deltaT * FRLParams$maicp
  # C accumulation on areas that were harvested in year t
  mchp <- mean(sw$area_harvested_ha * FRLParams$rdeltaT * FRLParams$maicp)
  # Average annual C accumulation on planted areas over the Reference Period
  mcpp <- mean(sw$cgain_t)

  # Total average annual C removals
  mcrp <- mcpp + ctp + mchp

  # Uncertainty analysis (removals in Softwood Plantations)
  resmcrp <- vector() # Vector that collects the results

  resmcrpNew <- vector() # Vector that collects the results
  # Run simulation .......................................................................
  for (i in 1:FRLParams$runs) { # i <- 1
    swi <- sw # Create a copy of 'sw'
    netStockedAreai <- netStockedArea
    # Random realization of MAI AGB (25% error)
    maicpi <- rtriangle(1,
      theta = FRLParams$maicp,
      lower = FRLParams$maicp - FRLParams$maicp * FRLParams$errmaicp,
      upper = FRLParams$maicp + FRLParams$maicp * FRLParams$errmaicp
    )
    # Convert MAI (volume) to MAI (carbon)
    # Random cutting cycle length
    cli <- rtriangle(
      n = 1, theta = FRLParams$cuttingc,
      lower = FRLParams$cuttingc - FRLParams$errcuttingc,
      upper = FRLParams$cuttingc + FRLParams$errcuttingc
    )
    # Compute areas harvested
    swi$ahpt <- rescept[[i]] / (maicpi * cli)
    # Average annual C accumulation on areas that just grow
    ctpi <- atp * maicpi
    # Carbon accumulation in plantation compartments that were harvested in year t
    cthpi <- mean(swi$area_harvested_ha * FRLParams$rdeltaT * maicpi)
    # C accumulation on planted areas over the Reference Period
    swi$cppt <- swi$area_planted_ha * FRLParams$deltaT * maicpi
    # Total average annual C accumulation
    resmcrp[i] <- mean(swi$cppt) + ctpi + cthpi

    # Error in area is not included
    netStockedAreai$c_t  <- maicpi * netStockedArea$area

    resmcrpNew[i] <- mean(netStockedAreai$c_t)
  }

  # Average annual removals from Softwood Plantations ....................................
  ec_sw_aar <- mcrp * FRLParams$etacc # Estimate
  lci_ec_sw_aar <- quantile(resmcrp * FRLParams$etacc, probs = FRLParams$qlci) # Lower CI limit
  uci_ec_sw_aar <- quantile(resmcrp * FRLParams$etacc, probs = FRLParams$quci) # Upper CI limit
  v_ec_sw_aar <- resmcrp * FRLParams$etacc # MC estimate

  ec_sw_aarNew <- mean(netStockedArea$c_t) * FRLParams$etacc # Estimate
  lci_ec_sw_aarNew <- quantile(resmcrpNew * FRLParams$etacc, probs = FRLParams$qlci) # Lower CI limit
  uci_ec_sw_aarNew <- quantile(resmcrpNew * FRLParams$etacc, probs = FRLParams$quci) # Upper CI limit
  v_ec_sw_aarNew <- resmcrpNew * FRLParams$etacc # MC estimate

  if (debug_frl) {
    print("original model of softwood growth")
    print(c(
      ec_sw_aar,
      lci_ec_sw_aar,
      uci_ec_sw_aar,
      length(v_ec_sw_aar)
    ))


    print("new data provided by fiji for softwood plantations")
    print(c(
      ec_sw_aarNew,
      lci_ec_sw_aarNew,
      uci_ec_sw_aarNew,
      length(v_ec_sw_aarNew)
    ))
  }

  # Net emissions Softwood Plantations
  lciv_ec_sw_aane <- quantile(v_ec_sw_aae -   # Emissions Softwood
                              v_ec_sw_aarNew,    # Removals Softwood
                              probs = FRLParams$qlci)
  uciv_ec_sw_aane <- quantile(v_ec_sw_aae -
                              v_ec_sw_aarNew,
                              probs = FRLParams$quci)

  result <- list()
  result$ec_sw_aae <- ec_sw_aae
  result$ec_sw_aar <- ec_sw_aarNew
  result$lci_ec_sw_aae <- lci_ec_sw_aae
  result$uci_ec_sw_aae <- uci_ec_sw_aae
  result$lci_ec_sw_aar <- lci_ec_sw_aarNew
  result$uci_ec_sw_aar <- uci_ec_sw_aarNew
  result$v_ec_sw_aae <- v_ec_sw_aae
  result$v_ec_sw_aar <- v_ec_sw_aarNew
  result$lciv_ec_sw_aane <- lciv_ec_sw_aane
  result$uciv_ec_sw_aane <- uciv_ec_sw_aane
  return(result)
}

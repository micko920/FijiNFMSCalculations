

#' @export
calcFRLContributions <- function(frl_table_data, titles) {
  # Contributions of the different sources and sinks .....................................
  # Contributions to gross emissions
  contributione <- round(frl_table_data$aa_emissions_tco2e_yr /
    sum(frl_table_data$aa_emissions_tco2e_yr) * 100, 2)
  names(contributione) <- titles
  contributione <- data.frame(contributione)

  # Contributions to gross removals
  contributionr <- round(frl_table_data$aa_removals_tco2e_yr /
    sum(frl_table_data$aa_removals_tco2e_yr) * 100, 2)
  names(contributionr) <- titles
  contributionr <- data.frame(contributionr)

  # Contributions to net emissions
  contributionn <- round(abs(frl_table_data$aa_net_emissions_tco2e_yr) /
    sum(abs(frl_table_data$aa_net_emissions_tco2e_yr)) * 100, 2)
  names(contributionn) <- titles
  contributionn <- data.frame(contributionn)

  # Summary of contributions
  contributions <- data.frame(
    sourceSink = titles,
    gross_emissions = contributione[, 1],
    gross_removals = contributionr[, 1],
    net_emissions = contributionn[, 1]
  )

  # Contributions (including emissions from fuelwood consumption)
  contributionsfuel <- contributions
  # Contributions of all sources and sinks in percent (including fuelwood)
  if (debug_frl) print(contributionsfuel)
}

#' @export
calcFRLNet <- function(
      aa_emissions_tco2e_yr,
      v_aa_emissions_tco2e_yr,
      aa_removals_tco2e_yr,
      v_aa_removals_tco2e_yr
  ) {
  # Lower confidence limit
  lci_aa_emissions_tco2e_yr <- quantile(v_aa_emissions_tco2e_yr, probs = FRLParams$qlci)
  # Upper confidence limit
  uci_aa_emissions_tco2e_yr <- quantile(v_aa_emissions_tco2e_yr, probs = FRLParams$quci)

  # Lower confidence limit
  lci_aa_removals_tco2e_yr <- quantile(v_aa_removals_tco2e_yr, probs = FRLParams$qlci)
  # Upper confidence limit
  uci_aa_removals_tco2e_yr <- quantile(v_aa_removals_tco2e_yr, probs = FRLParams$quci)

  # The FRL ==============================================================================
  # Estimate
  aa_net_emissions_tco2e_yr <- aa_emissions_tco2e_yr + aa_removals_tco2e_yr
  # MC estimates
  v_aa_net_emissions_tco2e_yr <- v_aa_emissions_tco2e_yr + v_aa_removals_tco2e_yr
  # Lower confidence limit
  lci_aa_net_emissions_tco2e_yr <- quantile(v_aa_emissions_tco2e_yr +
    v_aa_removals_tco2e_yr,
  probs = FRLParams$qlci
  )
  # Upper confidence limit
  uci_aa_net_emissions_tco2e_yr <- quantile(v_aa_emissions_tco2e_yr +
    v_aa_removals_tco2e_yr,
  probs = FRLParams$quci
  )

  result <- list()
  result$lci_aa_emissions_tco2e_yr <-lci_aa_emissions_tco2e_yr
  result$uci_aa_emissions_tco2e_yr <-uci_aa_emissions_tco2e_yr
  result$lci_aa_removals_tco2e_yr <-lci_aa_removals_tco2e_yr
  result$uci_aa_removals_tco2e_yr <-uci_aa_removals_tco2e_yr
  result$aa_net_emissions_tco2e_yr <-aa_net_emissions_tco2e_yr
  result$v_aa_net_emissions_tco2e_yr <-v_aa_net_emissions_tco2e_yr
  result$lci_aa_net_emissions_tco2e_yr <-lci_aa_net_emissions_tco2e_yr
  result$uci_aa_net_emissions_tco2e_yr <-uci_aa_net_emissions_tco2e_yr
  return(result)
}

#' @export
calcFRLTable_IncludingFuel <- function(frl_table) {
  # Gross emissions and removals (all sources and sinks) =================================
  # Total average annual gross emissions .................................................
  aa_emissions_tco2e_yr <- c(
      FRLDeforestation$rs_df[1, 1], # DF gross emission
      FRLFelling$rs_fd_lg[1, 2], # FD logging gross emissions
      FRLBurning$rs_fd_bb[1, 1], # FD fire gross emissions
      FRLFuelwood$rs_fd_fu[1, 1], # FD fuelwood gross emissions
      0, # EC AR gross emissions
      FRLPlantations$rs_ec_pl[3, 2]
    )

  # MC estimates
  v_aa_emissions_tco2e_yr <- (FRLDeforestation$v_df_L_aae + FRLDeforestation$v_df_U_aae) + # MC Emissions deforestation
    FRLFelling$v_fd_lg_aae + # MC Emissions FD logging
    FRLBurning$v_fd_bb_aae + # MC Emissions FD fire
    FRLFuelwood$v_fd_fu_aae + # MC Emissions FD fuelwood
    FRLHardwoodPlantations$v_ec_hw_aae + # MC Emissions EC Hardwood
    FRLSoftwoodPlantations$v_ec_sw_aae # MC Emissions EC Softwood

  # Total average annual gross removals ..................................................
  aa_removals_tco2e_yr <- c(
      0, # DF gross removals
      FRLFelling$rs_fd_lg[2, 2], # FD logging gross removals
      0, # FD fire gross removals
      0, # FD fuelwood gross removals
      FRLHardwoodPlantations$rs_ec_ar[1, 1], # EC AR gross removals
      FRLPlantations$rs_ec_pl[6, 2]
    )

  # MC estimates
  v_aa_removals_tco2e_yr <- (FRLFelling$v_fd_lg_aar + # MC gross removals FD logging
    FRLHardwoodPlantations$v_ec_ar_aar + # MC gross removals AR
    FRLHardwoodPlantations$v_ec_hw_aar + # MC gross removals Hardwood Plantations
    FRLSoftwoodPlantations$v_ec_sw_aar) * -1 # MC gross removals Softwood Plantations

  frl_net <- calcFRLNet(
      aa_emissions_tco2e_yr,
      v_aa_emissions_tco2e_yr,
      aa_removals_tco2e_yr,
      v_aa_removals_tco2e_yr
  )

  result <- list()
  result$aa_emissions_tco2e_yr <-aa_emissions_tco2e_yr
  result$aa_removals_tco2e_yr <-aa_removals_tco2e_yr
  result$aa_net_emissions_tco2e_yr <-frl_net$aa_net_emissions_tco2e_yr
  result$lci_aa_emissions_tco2e_yr <-frl_net$lci_aa_emissions_tco2e_yr
  result$uci_aa_emissions_tco2e_yr <-frl_net$uci_aa_emissions_tco2e_yr
  result$uci_aa_removals_tco2e_yr <-frl_net$uci_aa_removals_tco2e_yr
  result$lci_aa_removals_tco2e_yr <-frl_net$lci_aa_removals_tco2e_yr
  result$lci_aa_net_emissions_tco2e_yr <-frl_net$lci_aa_net_emissions_tco2e_yr
  result$uci_aa_net_emissions_tco2e_yr <-frl_net$uci_aa_net_emissions_tco2e_yr
  return(result)
}

#' @export
calcFRLTable_ExFuel <- function(frl_table) {
  # Gross emissions and removals (all sources and sinks; excluding fuelwood) =============
  # Total average annual gross emissions .................................................
  aa_emissions_tco2e_yr <- c(
      FRLDeforestation$rs_df[1, 1], # DF gross emission
      FRLFelling$rs_fd_lg[1, 2], # FD logging gross emissions
      FRLBurning$rs_fd_bb[1, 1], # FD fire gross emissions
      0, # EC AR gross emissions
      FRLPlantations$rs_ec_pl[3, 2]
    )

  # MC estimates
  v_aa_emissions_tco2e_yr <- (FRLDeforestation$v_df_L_aae + FRLDeforestation$v_df_U_aae) + # MC Emissions deforestation
    FRLFelling$v_fd_lg_aae + # MC Emissions FD logging
    FRLBurning$v_fd_bb_aae + # MC Emissions FD fire
    FRLHardwoodPlantations$v_ec_hw_aae + # MC Emissions EC Hardwood
    FRLSoftwoodPlantations$v_ec_sw_aae # MC Emissions EC Softwood

  # Total average annual gross removals
  aa_removals_tco2e_yr <- c(
      0, # DF gross removals
      FRLFelling$rs_fd_lg[2, 2], # FD logging gross removals
      0, # FD fire gross removals
      FRLHardwoodPlantations$rs_ec_ar[1, 1], # EC AR gross removals
      FRLPlantations$rs_ec_pl[6, 2]
    )
  # MC estimates
  v_aa_removals_tco2e_yr <- (FRLFelling$v_fd_lg_aar + # MC gross removals FD logging
    FRLHardwoodPlantations$v_ec_ar_aar + # MC gross removals AR
    FRLHardwoodPlantations$v_ec_hw_aar + # MC gross removals Hardwood Plantations
    FRLSoftwoodPlantations$v_ec_sw_aar) * -1 # MC gross removals Softwood Plantations

  frl_net <- calcFRLNet(
      aa_emissions_tco2e_yr,
      v_aa_emissions_tco2e_yr,
      aa_removals_tco2e_yr,
      v_aa_removals_tco2e_yr
  )

  result <- list()
  result$aa_emissions_tco2e_yr <-aa_emissions_tco2e_yr
  result$aa_removals_tco2e_yr <-aa_removals_tco2e_yr
  result$aa_net_emissions_tco2e_yr <-frl_net$aa_net_emissions_tco2e_yr
  result$lci_aa_emissions_tco2e_yr <-frl_net$lci_aa_emissions_tco2e_yr
  result$uci_aa_emissions_tco2e_yr <-frl_net$uci_aa_emissions_tco2e_yr
  result$uci_aa_removals_tco2e_yr <-frl_net$uci_aa_removals_tco2e_yr
  result$lci_aa_removals_tco2e_yr <-frl_net$lci_aa_removals_tco2e_yr
  result$lci_aa_net_emissions_tco2e_yr <-frl_net$lci_aa_net_emissions_tco2e_yr
  result$uci_aa_net_emissions_tco2e_yr <-frl_net$uci_aa_net_emissions_tco2e_yr
  return(result)
}


#' @export
calcFRLTable <- function() {
  # Table of sources and sinks considered in the FRL .....................................
  sourcesSinks <- data.frame(
    # DF    = deforestation
    # FDL   = forest degradation (logging)
    # FDF   = forest degradation (fire)
    # FUEL  = fuelwood
    # ECAR  = enhancement of forest carbon stocks (afforestation/reforestation)
    # ECHS  = enhancement of forest carbon stocks (Hard- and Softwood Plantations)
    source_sink = c("DF", "FDL", "FDF", "FUEL", "ECAR", "ECHS"),
    description = c(
      "Deforestation",
      "Forest degradation (logging)",
      "Forest degradation (fire)",
      "Fuelwood consumption",
      "Enhancement of forest carbon stocks (afforestation/reforestation)",
      "Enhancement of forest carbon stocks (Hard- and Softwood Plantations)"
    )
  )

  # Fiji's Forest Reference Level (FRL) ##################################################
  # FRL table (including all sources and sinks) ..........................................
  frl_table <- data.frame(
    source_sink = c(
      # Sources and sinks:
      # DF = deforestation
      # FD = forest degradation
      # ECAR = enhancement of carbon stocks (afforestation/reforestation)
      # ECHS = enhancement of carbon stocks (Hard- and Softwood Plantations)
      "DF",
      "FDL",
      "FDF",
      "FUEL",
      "ECAR",
      "ECHS"
    ),

    # Average annual gross emissions .......................................
    aa_emissions_tco2e_yr = c(
      FRLDeforestation$rs_df[1, 1], # DF gross emission
      FRLFelling$rs_fd_lg[1, 2], # FD logging gross emissions
      FRLBurning$rs_fd_bb[1, 1], # FD fire gross emissions
      FRLFuelwood$rs_fd_fu[1, 1], # FD fuelwood gross emissions
      0, # EC AR gross emissions
      FRLPlantations$rs_ec_pl[3, 2]
    ), # EC Plantations gross emissions

    # Lower confidence limit of average annual gross emissions
    lci_aa_emissions_tco2e_yr = c(
      FRLDeforestation$rs_df[1, 2],
      FRLFelling$rs_fd_lg[1, 3],
      FRLBurning$rs_fd_bb[1, 2],
      FRLFuelwood$rs_fd_fu[1, 2],
      0,
      FRLPlantations$rs_ec_pl[3, 3]
    ),

    # Upper confidence limit of average annual gross emissions
    uci_aa_emissions_tco2e_yr = c(
      FRLDeforestation$rs_df[1, 3],
      FRLFelling$rs_fd_lg[1, 4],
      FRLBurning$rs_fd_bb[1, 3],
      FRLFuelwood$rs_fd_fu[1, 3],
      0,
      FRLPlantations$rs_ec_pl[3, 4]
    ),

    # Average annual gross removals ........................................
    aa_removals_tco2e_yr = c(
      0, # DF gross removals
      FRLFelling$rs_fd_lg[2, 2], # FD logging gross removals
      0, # FD fire gross removals
      0, # FD fuelwood gross removals
      FRLHardwoodPlantations$rs_ec_ar[1, 1], # EC AR gross removals
      FRLPlantations$rs_ec_pl[6, 2]
    ), # EC Plantations gross removals

    # Lower confidence limit of average annual gross removals
    lci_aa_removals_tco2e_yr = c(
      0,
      FRLFelling$rs_fd_lg[2, 3],
      0,
      0,
      FRLHardwoodPlantations$rs_ec_ar[1, 2],
      FRLPlantations$rs_ec_pl[6, 3]
    ),

    # Upper confidence limit of average annual gross removals
    uci_aa_removals_tco2e_yr = c(
      0,
      FRLFelling$rs_fd_lg[2, 4],
      0,
      0,
      FRLHardwoodPlantations$rs_ec_ar[1, 3],
      FRLPlantations$rs_ec_pl[6, 4]
    ),

    # Average annual net emissions .........................................
    aa_net_emissions_tco2e_yr = c(
      FRLDeforestation$rs_df[1, 1], # DF net emissions
      FRLFelling$rs_fd_lg[3, 2], # FD logging net emissions
      FRLBurning$rs_fd_bb[1, 1], # FD fire net emissions
      FRLFuelwood$rs_fd_fu[1, 1], # FD fuelwood net emissions
      FRLHardwoodPlantations$rs_ec_ar[1, 1], # EC AR net emissions
      FRLPlantations$rs_ec_pl[9, 2]
    ), # EC Plantations net emissions

    # Lower confidence limit of average annual net emissions
    lci_aa_net_emissions_tco2e_yr = c(
      FRLDeforestation$rs_df[1, 2],
      FRLFelling$rs_fd_lg[3, 3],
      FRLBurning$rs_fd_bb[1, 2],
      FRLFuelwood$rs_fd_fu[1, 2],
      FRLHardwoodPlantations$rs_ec_ar[1, 2],
      FRLPlantations$rs_ec_pl[9, 3]
    ),

    # Upper confidence limit of average annual net emissions
    lci_aa_net_emissions_tco2e_yr = c(
      FRLDeforestation$rs_df[1, 3],
      FRLFelling$rs_fd_lg[3, 4],
      FRLBurning$rs_fd_bb[1, 3],
      FRLFuelwood$rs_fd_fu[1, 3],
      FRLHardwoodPlantations$rs_ec_ar[1, 3],
      FRLPlantations$rs_ec_pl[9, 4]
    )
  )

  # Average annual emissions and removals from EC ========================================
  # EC = enhancement of forest carbon stocks
  # Estimate of average annual net emissions
  aaneec <- FRLHardwoodPlantations$rs_ec_ar[1, 1] + # Gross/net removals AR
    FRLPlantations$rs_ec_pl[9, 2] # Net emissions Forest Plantations
  # Lower confidence limit
  lciaaneec <- quantile((FRLHardwoodPlantations$v_ec_ar_aar * -1) + # MC gross/net emissions AR
    (FRLSoftwoodPlantations$v_ec_sw_aae + FRLHardwoodPlantations$v_ec_hw_aae) - # MC gross emissions Plantations
    (FRLSoftwoodPlantations$v_ec_sw_aar + FRLHardwoodPlantations$v_ec_hw_aar), # MC gross removals Plantations
  probs = FRLParams$qlci
  )
  # Upper confidence limit
  uciaaneec <- quantile((FRLHardwoodPlantations$v_ec_ar_aar * -1) + # MC gross/net emissions AR
    (FRLSoftwoodPlantations$v_ec_sw_aae + FRLHardwoodPlantations$v_ec_hw_aae) - # MC gross emissions Plantations
    (FRLSoftwoodPlantations$v_ec_sw_aar + FRLHardwoodPlantations$v_ec_hw_aar), # MC gross removals Plantations
  probs = FRLParams$quci
  )

  # Average annual emissions and removals from forest degradation ========================
  # Gross emissions forest degradation (FD) ..............................................
  aaefd <- FRLFelling$rs_fd_lg[1, 2] + # Gross emissions FD logging
    FRLBurning$rs_fd_bb[1, 1] + # Gross emissions FD biomass burning
    FRLFuelwood$rs_fd_fu[1, 1] # Gross emissions FD fuelwood
  # Lower confidence limit
  lciaaefd <- quantile(FRLFelling$v_fd_lg_aae + # MC gross emissions logging
    FRLBurning$v_fd_bb_aae + # MC gross emissions biomass burning
    FRLFuelwood$v_fd_fu_aae, # MC gross emissions fuelwood
  probs = FRLParams$qlci
  )
  # Upper confidence limit
  uciaaefd <- quantile(FRLFelling$v_fd_lg_aae + # Gross emissions logging
    FRLBurning$v_fd_bb_aae + # Gross emissions biomass burning
    FRLFuelwood$v_fd_fu_aae, # Gross emissions fuelwood
  probs = FRLParams$quci
  )

  # Gross removals forest degradation (FD) ...............................................
  aarfd <- FRLFelling$rs_fd_lg[2, 2] # Gross removals FD logging
  # Lower confidence limit
  lciaarfd <- quantile(FRLFelling$v_fd_lg_aar, # MC gross removals logging
    probs = FRLParams$qlci
  )
  # Upper confidence limit
  uciaarfd <- quantile(FRLFelling$v_fd_lg_aar, # MC gross removals logging
    probs = FRLParams$quci
  )

  # Net emissions forest degradation (FD) ................................................
  # Gross emissions FD fuelwood was removed from this calculation

  aanefd <- FRLFelling$rs_fd_lg[1, 2] + # Gross emissions FD logging
    FRLBurning$rs_fd_bb[1, 1] + # Gross emissions FD fire
    FRLFelling$rs_fd_lg[2, 2] # Gross removals FD logging

  aanefdf <- aanefd
  # Lower confidence limit
  lciaanefd <- quantile(FRLFelling$v_fd_lg_aae + # MC gross emissions FD logging
    FRLBurning$v_fd_bb_aae - # MC gross emissions FD fire
    FRLFelling$v_fd_lg_aar, # MC gross removals FD logging
  probs = FRLParams$qlci
  )
  lciaanefdf <- lciaanefd
  # Upper confidence limit
  uciaanefd <- quantile(FRLFelling$v_fd_lg_aae + # MC gross emissions FD logging
    FRLBurning$v_fd_bb_aae - # MC gross emissions FD fire
    FRLFelling$v_fd_lg_aar, # MC gross removals FD logging
  probs = FRLParams$quci
  )
  uciaanefdf <- uciaanefd



  frl_IncludingFuel <- calcFRLTable_IncludingFuel()
  calcFRLContributions(frl_IncludingFuel, c("DF", "FDL", "FDF", "FUEL", "ECAR", "ECHS"))
  # FRL table including all sources and sinks ............................................
  frl_table_data <- rbind(
    frl_table[, -1],
    c(
      sum(frl_IncludingFuel$aa_emissions_tco2e_yr), frl_IncludingFuel$lci_aa_emissions_tco2e_yr,
      frl_IncludingFuel$uci_aa_emissions_tco2e_yr, sum(frl_IncludingFuel$aa_removals_tco2e_yr),
      frl_IncludingFuel$uci_aa_removals_tco2e_yr, frl_IncludingFuel$lci_aa_removals_tco2e_yr,
      sum(frl_IncludingFuel$aa_net_emissions_tco2e_yr), frl_IncludingFuel$lci_aa_net_emissions_tco2e_yr,
      frl_IncludingFuel$uci_aa_net_emissions_tco2e_yr
    )
  )
  frl_tableIncludingFuel <- data.frame(
    source_sink = c(as.character(frl_table[, 1]), "FRL"),
    frl_table_data
  )

  if (debug_frl) print(frl_table_data)


  frl_ExFuel <- calcFRLTable_ExFuel()
  calcFRLContributions(frl_ExFuel, c("DF", "FDL", "FDF", "ECAR", "ECHS"))
  # FRL table including all sources and sinks ............................................
  frl_table_data <- rbind(
    frl_table[-c(4,7),-1],
    c(
      sum(frl_ExFuel$aa_emissions_tco2e_yr), frl_ExFuel$lci_aa_emissions_tco2e_yr,
      frl_ExFuel$uci_aa_emissions_tco2e_yr, sum(frl_ExFuel$aa_removals_tco2e_yr),
      frl_ExFuel$uci_aa_removals_tco2e_yr, frl_ExFuel$lci_aa_removals_tco2e_yr,
      sum(frl_ExFuel$aa_net_emissions_tco2e_yr), frl_ExFuel$lci_aa_net_emissions_tco2e_yr,
      frl_ExFuel$uci_aa_net_emissions_tco2e_yr
    )
  )
  frl_tableExFuel <- data.frame(
    source_sink = c(as.character(frl_table[-c(4,7),1]), "FRL"),
    frl_table_data
  )

  if (debug_frl) print(frl_table_data)

  # FRL result table =====================================================================
  frl <- frl_tableExFuel

  # The final result table of Fiji's Forest Reference Level 2006-2016 ....................
  frltab <- data.frame(
    source_sink = c(
      # Gross emissions ...............................................
      "aaeDF", # Gross emissions deforestation
      "aaeFD_L", # Gross emissions FD logging
      "aaeFD_BSW", # Gross emissions FD biom. burning Softwood
      "aaeEC_HS", # Gross emissions EC Hard- & Softwood Plantations
      "aae_Combined", # Gross emissions (all sources)

      # Gross removals ................................................
      "aarFD_L", # Gross removals FD logging
      "aarEC_AR", # Gross removals EC AR
      "aarEC_HS", # Gross removals EC hard- & Softwood Plantations
      "aar_Combined", # Gross removals (all sinks)

      # Net emissions .................................................
      "aaneDF", # Net emissions deforestation
      "aaneFD", # Net emissions forest degradation
      "aaneEC", # Net emissions EC

      # The FRL
      "FRL" # Fiji's Forest reference Level
    ),
    estimate_tco2e_yr = c(
      frl[1, 2], # aaeDF
      frl[2, 2], # aaeFD_L
      frl[3, 2], # aaeFD_BSW
      frl[5, 2], # aaeEC_HS
      sum(frl[c(1, 2, 3, 5), 2]), # aae_Combined

      frl[2, 5], # aarFD_L
      frl[4, 5], # aarEC_AR
      frl[5, 5], # aarEC_HS
      sum(frl[c(2, 4, 5), 5]), # aar_Combined

      frl[1, 8], # aaneDF
      sum(frl[2:3, 8]), # aaneFD
      sum(frl[c(4:5), 8]), # aaneEC

      frl[6, 8] # FRL
    ),
    lci_tco2e_yr = c(
      frl[1, 3], # aaeDF
      frl[2, 3], # aaeFD_L
      frl[3, 3], # aaeFD_BSW
      frl[5, 3], # aaeEC_HS
      frl[6, 3], # aae_Combined

      frl[2, 6], # aarFD_L
      frl[4, 6], # aarEC_AR
      frl[5, 6], # aarEC_HS
      frl[6, 6], # aar_Combined

      frl[1, 9], # aaneDF
      lciaanefd, # aaneFD
      lciaaneec, # aaneEC

      frl[6, 9] # FRL
    ),
    uci_tco2e_yr = c(
      frl[1, 4], # aaeDF
      frl[2, 4], # aaeFD_L
      frl[3, 4], # aaeFD_BSW
      frl[5, 4], # aaeEC_HS
      frl[6, 4], # aae_Combined

      frl[2, 7], # aarFD_L
      frl[4, 7], # aarEC_AR
      frl[5, 7], # aarEC_HS
      frl[6, 7], # aar_Combined

      frl[1, 10], # aaneDF
      uciaanefd, # aaneFD
      uciaaneec, # aaneEC

      frl[6, 10] # FRL
    )
  )
  result <- list()
  result$frltab <- frltab

  return(result)
}

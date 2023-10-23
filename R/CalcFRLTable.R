

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
  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLTable.R", ":36"))
    print(contributionsfuel)
  }
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
      FRLNaturalForestDegradation$rs_fd_nf[1,1], # FD natural forest
      0, # EC AR gross emissions
      FRLPlantations$rs_ec_pl[3, 2]
    )

  # MC estimates
  v_aa_emissions_tco2e_yr <- (FRLDeforestation$v_df_L_aae + FRLDeforestation$v_df_U_aae) + # MC Emissions deforestation
    FRLFelling$v_fd_lg_aae + # MC Emissions FD logging
    FRLBurning$v_fd_bb_aae + # MC Emissions FD fire
    FRLFuelwood$v_fd_fu_aae + # MC Emissions FD fuelwood
    FRLNaturalForestDegradation$v_fd_nf_aae + # MC Emissions FD Natural Forest
    FRLHardwoodPlantations$v_ec_hw_aae + # MC Emissions EC Hardwood
    FRLSoftwoodPlantations$v_ec_sw_aae # MC Emissions EC Softwood

  # Total average annual gross removals ..................................................
  aa_removals_tco2e_yr <- c(
      0, # DF gross removals
      FRLFelling$rs_fd_lg[2, 2], # FD logging gross removals
      0, # FD fire gross removals
      0, # FD fuelwood gross removals
      0, # FD natural forest removals
      FRLAfforestation$rs_ec_ar[1, 1], # EC AR gross removals
      FRLPlantations$rs_ec_pl[6, 2]
    )

  # MC estimates
  v_aa_removals_tco2e_yr <- (FRLFelling$v_fd_lg_aar + # MC gross removals FD logging
    FRLAfforestation$v_ec_ar_aar + # MC gross removals AR
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
  result$v_aa_emissions_tco2e_yr <-v_aa_emissions_tco2e_yr
  result$aa_removals_tco2e_yr <-aa_removals_tco2e_yr
  result$v_aa_removals_tco2e_yr <-v_aa_removals_tco2e_yr
  result$aa_net_emissions_tco2e_yr <-frl_net$aa_net_emissions_tco2e_yr
  result$v_aa_net_emissions_tco2e_yr <-frl_net$v_aa_net_emissions_tco2e_yr
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
      FRLNaturalForestDegradation$rs_fd_nf[1,1], # FD natural forest
      0, # EC AR gross emissions
      FRLPlantations$rs_ec_pl[3, 2]
    )

  # MC estimates
  v_aa_emissions_tco2e_yr <- (FRLDeforestation$v_df_L_aae + FRLDeforestation$v_df_U_aae) + # MC Emissions deforestation
    FRLFelling$v_fd_lg_aae + # MC Emissions FD logging
    FRLBurning$v_fd_bb_aae + # MC Emissions FD fire
    FRLNaturalForestDegradation$v_fd_nf_aae + # MC Emissions FD Natural Forest
    FRLHardwoodPlantations$v_ec_hw_aae + # MC Emissions EC Hardwood
    FRLSoftwoodPlantations$v_ec_sw_aae # MC Emissions EC Softwood

  # Total average annual gross removals
  aa_removals_tco2e_yr <- c(
      0, # DF gross removals
      FRLFelling$rs_fd_lg[2, 2], # FD logging gross removals
      0, # FD fire gross removals
      0, # FD natural forest removals
      FRLAfforestation$rs_ec_ar[1, 1], # EC AR gross removals
      FRLPlantations$rs_ec_pl[6, 2]
    )
  # MC estimates
  v_aa_removals_tco2e_yr <- (FRLFelling$v_fd_lg_aar + # MC gross removals FD logging
    FRLAfforestation$v_ec_ar_aar + # MC gross removals AR
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
  result$v_aa_emissions_tco2e_yr <-v_aa_emissions_tco2e_yr
  result$aa_removals_tco2e_yr <-aa_removals_tco2e_yr
  result$v_aa_removals_tco2e_yr <-v_aa_removals_tco2e_yr
  result$aa_net_emissions_tco2e_yr <-frl_net$aa_net_emissions_tco2e_yr
  result$v_aa_net_emissions_tco2e_yr <-frl_net$v_aa_net_emissions_tco2e_yr
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
    # FDNF  = natural forest degradation
    # FUEL  = fuelwood
    # ECAR  = enhancement of forest carbon stocks (afforestation/reforestation)
    # ECHS  = enhancement of forest carbon stocks (Hard- and Softwood Plantations)
    source_sink = c("DF", "FDL", "FDF", "FUEL", "FDNF", "ECAR", "ECHS"),
    description = c(
      "Deforestation",
      "Forest degradation (logging)",
      "Forest degradation (fire)",
      "Fuelwood consumption",
      "Forest degradation (natural forest)",
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
      "FDNF",
      "ECAR",
      "ECHS"
    ),

    # Average annual gross emissions .......................................
    aa_emissions_tco2e_yr = c(
      FRLDeforestation$rs_df[1, 1], # DF gross emission
      FRLFelling$rs_fd_lg[1, 2], # FD logging gross emissions
      FRLBurning$rs_fd_bb[1, 1], # FD fire gross emissions
      FRLFuelwood$rs_fd_fu[1, 1], # FD fuelwood gross emissions
      FRLNaturalForestDegradation$rs_fd_nf[1,1], # FD natural forest
      0, # EC AR gross emissions
      FRLPlantations$rs_ec_pl[3, 2]
    ), # EC Plantations gross emissions

    # Lower confidence limit of average annual gross emissions
    lci_aa_emissions_tco2e_yr = c(
      FRLDeforestation$rs_df[1, 2],
      FRLFelling$rs_fd_lg[1, 3],
      FRLBurning$rs_fd_bb[1, 2],
      FRLFuelwood$rs_fd_fu[1, 2],
      FRLNaturalForestDegradation$rs_fd_nf[1,2],
      0,
      FRLPlantations$rs_ec_pl[3, 3]
    ),

    # Upper confidence limit of average annual gross emissions
    uci_aa_emissions_tco2e_yr = c(
      FRLDeforestation$rs_df[1, 3],
      FRLFelling$rs_fd_lg[1, 4],
      FRLBurning$rs_fd_bb[1, 3],
      FRLFuelwood$rs_fd_fu[1, 3],
      FRLNaturalForestDegradation$rs_fd_nf[1,3],
      0,
      FRLPlantations$rs_ec_pl[3, 4]
    ),

    # Average annual gross removals ........................................
    aa_removals_tco2e_yr = c(
      0, # DF gross removals
      FRLFelling$rs_fd_lg[2, 2], # FD logging gross removals
      0, # FD fire gross removals
      0, # FD fuelwood gross removals
      0, # FD natural forest removals
      FRLAfforestation$rs_ec_ar[1, 1], # EC AR gross removals
      FRLPlantations$rs_ec_pl[6, 2]
    ), # EC Plantations gross removals

    # Lower confidence limit of average annual gross removals
    lci_aa_removals_tco2e_yr = c(
      0,
      FRLFelling$rs_fd_lg[2, 3],
      0,
      0,
      0,
      FRLAfforestation$rs_ec_ar[1, 2],
      FRLPlantations$rs_ec_pl[6, 3]
    ),

    # Upper confidence limit of average annual gross removals
    uci_aa_removals_tco2e_yr = c(
      0,
      FRLFelling$rs_fd_lg[2, 4],
      0,
      0,
      0,
      FRLAfforestation$rs_ec_ar[1, 3],
      FRLPlantations$rs_ec_pl[6, 4]
    ),

    # Average annual net emissions .........................................
    aa_net_emissions_tco2e_yr = c(
      FRLDeforestation$rs_df[1, 1], # DF net emissions
      FRLFelling$rs_fd_lg[3, 2], # FD logging net emissions
      FRLBurning$rs_fd_bb[1, 1], # FD fire net emissions
      FRLFuelwood$rs_fd_fu[1, 1], # FD fuelwood net emissions
      FRLNaturalForestDegradation$rs_fd_nf[1,1], # FD natural forest
      FRLAfforestation$rs_ec_ar[1, 1], # EC AR net emissions
      FRLPlantations$rs_ec_pl[9, 2]
    ), # EC Plantations net emissions

    # Lower confidence limit of average annual net emissions
    lci_aa_net_emissions_tco2e_yr = c(
      FRLDeforestation$rs_df[1, 2],
      FRLFelling$rs_fd_lg[3, 3],
      FRLBurning$rs_fd_bb[1, 2],
      FRLFuelwood$rs_fd_fu[1, 2],
      FRLNaturalForestDegradation$rs_fd_nf[1,2],
      FRLAfforestation$rs_ec_ar[1, 2],
      FRLPlantations$rs_ec_pl[9, 3]
    ),

    # Upper confidence limit of average annual net emissions
    lci_aa_net_emissions_tco2e_yr = c(
      FRLDeforestation$rs_df[1, 3],
      FRLFelling$rs_fd_lg[3, 4],
      FRLBurning$rs_fd_bb[1, 3],
      FRLFuelwood$rs_fd_fu[1, 3],
      FRLNaturalForestDegradation$rs_fd_nf[1,3],
      FRLAfforestation$rs_ec_ar[1, 3],
      FRLPlantations$rs_ec_pl[9, 4]
    )
  )

  # Average annual emissions and removals from EC ========================================
  # EC = enhancement of forest carbon stocks
  # Estimate of average annual net emissions
  aaneec <- FRLAfforestation$rs_ec_ar[1, 1] + # Gross/net removals AR
    FRLPlantations$rs_ec_pl[9, 2] # Net emissions Forest Plantations
  v_aaneec <- (FRLAfforestation$v_ec_ar_aar * -1) + # MC gross/net emissions AR
    (FRLSoftwoodPlantations$v_ec_sw_aae + FRLHardwoodPlantations$v_ec_hw_aae) - # MC gross emissions Plantations
    (FRLSoftwoodPlantations$v_ec_sw_aar + FRLHardwoodPlantations$v_ec_hw_aar)   # MC gross removals Plantations
  # Lower confidence limit
  lciaaneec <- quantile(v_aaneec, probs = FRLParams$qlci)
  # Upper confidence limit
  uciaaneec <- quantile(v_aaneec, probs = FRLParams$quci)

  # Average annual emissions and removals from forest degradation ========================
  # Gross emissions forest degradation (FD) ..............................................
  aaefd <- FRLFelling$rs_fd_lg[1, 2] + # Gross emissions FD logging
    FRLBurning$rs_fd_bb[1, 1] + # Gross emissions FD biomass burning
    FRLNaturalForestDegradation$rs_fd_nf[1,1] + # Gross emissions FD natural forest
    FRLFuelwood$rs_fd_fu[1, 1] # Gross emissions FD fuelwood
  v_aaefd <- FRLFelling$v_fd_lg_aae + # MC gross emissions logging
    FRLBurning$v_fd_bb_aae + # MC gross emissions biomass burning
    FRLNaturalForestDegradation$v_fd_nf_aae + # MC gross emissions FD natural forest
    FRLFuelwood$v_fd_fu_aae  # MC gross emissions fuelwood
  # Lower confidence limit
  lciaaefd <- quantile(v_aaefd, probs = FRLParams$qlci)
  # Upper confidence limit
  uciaaefd <- quantile(v_aaefd, probs = FRLParams$quci)

  # Gross removals forest degradation (FD) ...............................................
  aarfd <- FRLFelling$rs_fd_lg[2, 2] # Gross removals FD logging
  v_aarfd <- FRLFelling$v_fd_lg_aar  # MC gross removals logging
  # Lower confidence limit
  lciaarfd <- quantile(v_aarfd, probs = FRLParams$qlci)
  # Upper confidence limit
  uciaarfd <- quantile(v_aarfd, probs = FRLParams$quci)

  # Net emissions forest degradation (FD) ................................................
  # Gross emissions FD fuelwood was removed from this calculation

  aanefd <- FRLFelling$rs_fd_lg[1, 2] + # Gross emissions FD logging
    FRLBurning$rs_fd_bb[1, 1] + # Gross emissions FD fire
    FRLNaturalForestDegradation$rs_fd_nf[1,1] + # Gross emissions FD natural forest
    FRLFelling$rs_fd_lg[2, 2] # Gross removals FD logging

  aanefdf <- aanefd
  v_aanefd <- FRLFelling$v_fd_lg_aae + # MC gross emissions FD logging
    FRLBurning$v_fd_bb_aae + # MC gross emissions FD fire
    FRLNaturalForestDegradation$v_fd_nf_aae - # MC gross emissions FD natural forest
    FRLFelling$v_fd_lg_aar   # MC gross removals FD logging
  # Lower confidence limit
  lciaanefd <- quantile(v_aanefd, probs = FRLParams$qlci)
  lciaanefdf <- lciaanefd
  # Upper confidence limit
  uciaanefd <- quantile(v_aanefd, probs = FRLParams$quci)
  uciaanefdf <- uciaanefd



  frl_IncludingFuel <- calcFRLTable_IncludingFuel()
  calcFRLContributions(frl_IncludingFuel, c("DF", "FDL", "FDF", "FUEL", "FDNF", "ECAR", "ECHS"))
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

  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLTable.R", ":427"))
    print(frl_table_data)
  }


  frl_ExFuel <- calcFRLTable_ExFuel()
  calcFRLContributions(frl_ExFuel, c("DF", "FDL", "FDF", "FDNF", "ECAR", "ECHS"))
  # FRL table including all sources and sinks ............................................
  frl_table_data <- rbind(
    frl_table[-c(4,8),-1],
    c(
      sum(frl_ExFuel$aa_emissions_tco2e_yr), frl_ExFuel$lci_aa_emissions_tco2e_yr,
      frl_ExFuel$uci_aa_emissions_tco2e_yr, sum(frl_ExFuel$aa_removals_tco2e_yr),
      frl_ExFuel$uci_aa_removals_tco2e_yr, frl_ExFuel$lci_aa_removals_tco2e_yr,
      sum(frl_ExFuel$aa_net_emissions_tco2e_yr), frl_ExFuel$lci_aa_net_emissions_tco2e_yr,
      frl_ExFuel$uci_aa_net_emissions_tco2e_yr
    )
  )
  frl_tableExFuel <- data.frame(
    source_sink = c(as.character(frl_table[-c(4,8),1]), "FRL"),
    frl_table_data
  )

  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLTable.R", ":451"))
    print(frl_table_data)
  }

  # FRL result table =====================================================================
  frl <- frl_tableExFuel

  # The final result table of Fiji's Forest Reference Level 2006-2016 ....................
  frltab <- data.frame(
    source_sink = c(
      # Gross emissions ...............................................
      "aaeDF", # Gross emissions deforestation
      "aaeFD_L", # Gross emissions FD logging
      "aaeFD_BSW", # Gross emissions FD biom. burning Softwood
      "aaeFD_NF", # Gross emissions FD natural forest
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
      frl[4, 2], # aaeFD_NF
      frl[6, 2], # aaeEC_HS
      sum(frl[c(1, 2, 3, 4, 6), 2]), # aae_Combined

      frl[2, 5], # aarFD_L
      frl[5, 5], # aarEC_AR
      frl[6, 5], # aarEC_HS
      sum(frl[c(2, 5, 6), 5]), # aar_Combined

      frl[1, 8], # aaneDF
      sum(frl[2:4, 8]), # aaneFD
      sum(frl[c(5:6), 8]), # aaneEC

      frl[7, 8] # FRL
    ),
    lci_tco2e_yr = c(
      frl[1, 3], # aaeDF
      frl[2, 3], # aaeFD_L
      frl[3, 3], # aaeFD_BSW
      frl[4, 3], # aaeFD_NF
      frl[6, 3], # aaeEC_HS
      frl[7, 3], # aae_Combined

      frl[2, 6], # aarFD_L
      frl[5, 6], # aarEC_AR
      frl[6, 6], # aarEC_HS
      frl[7, 6], # aar_Combined

      frl[1, 9], # aaneDF
      lciaanefd, # aaneFD
      lciaaneec, # aaneEC

      frl[7, 9] # FRL
    ),
    uci_tco2e_yr = c(
      frl[1, 4], # aaeDF
      frl[2, 4], # aaeFD_L
      frl[3, 4], # aaeFD_BSW
      frl[4, 4], # aaeFD_NF
      frl[6, 4], # aaeEC_HS
      frl[7, 4], # aae_Combined

      frl[2, 7], # aarFD_L
      frl[5, 7], # aarEC_AR
      frl[6, 7], # aarEC_HS
      frl[7, 7], # aar_Combined

      frl[1, 10], # aaneDF
      uciaanefd, # aaneFD
      uciaaneec, # aaneEC

      frl[7, 10] # FRL
    )
  )

  result <- list()
  result$frltab <- frltab
  result$v_aaneec <- v_aaneec
  result$v_aaefd <- v_aaefd
  result$v_aarfd <- v_aarfd
  result$v_aanefd <- v_aanefd
  result$frl <- frl_ExFuel

  return(result)

}

#' @export
calcFRLMonitoringPeriodProjection <- function() {

  names <- data.frame(value = c(
      "FRLDeforestation Em", # 1
      "FRLBurning Em",  # 2
      "FRLFelling Em", # 3
      "FRLNaturalForestDegradation Em",  # 4
      "FRLHardwoodPlantations Em",  # 5
      "FRLSoftwoodPlantations Em",  # 6
      "FRLAfforestation Rem",  # 7
      "FRLFelling Rem",  # 8
      "FRLHardwoodPlantations Rem",  # 9
      "FRLSoftwoodPlantations Rem",  # 10
      "FCPF 4.1 Deforestation", # 11
      "FCPF 4.1 Degradation", # 12
      "FCPF 4.1 Sinks",  # 13
      "FCPF 4.1 Net Yearly Referenece Level", # 14
      "FDeg", # 15
      "Defor", # 16
      "FDegNonProxy" #17
      ))

  base_yearly <- data.frame(replicate(5, c(
    FRLDeforestation$rs_df$aa_em_tco2e_yr,
    FRLBurning$rs_fd_bb$aa_em_tco2e_yr,
    FRLFelling$rs_fd_lg$em[1],
    FRLNaturalForestDegradation$rs_fd_nf$aa_em_tco2e_yr,
    FRLHardwoodPlantations$ec_hw_aae,
    FRLSoftwoodPlantations$ec_sw_aae
    )))
  names(base_yearly) <- c(2019:2023)




  overall_frl <- rbind(
    base_yearly,
    FRLAfforestation$rs_ec_ar_cstock[6,],
    FRLFelling$rs_ec_lnf_cstock[6,],
    FRLHardwoodPlantations$rs_ec_hw_cstock[6,],
    FRLSoftwoodPlantations$rs_ec_sw_cstock[6,]
  )


  total_frl <- rbind(
      colSums(overall_frl[c(1),]),  #11
      colSums(overall_frl[c(2,3,4,8),]),  #12
      colSums(overall_frl[c(5,6,7,9,10),]), #13
      colSums(overall_frl[c(1,2,3,4,5,6,7,8,9,10),]), #14
      colSums(overall_frl[c(2,3,8),]), #15
      colSums(overall_frl[c(1),]), #16
      colSums(overall_frl[c(4),]) #17
  )

  overall_frl <- rbind(overall_frl,total_frl)


  overall_frl <- cbind(names, overall_frl)

  row.names(overall_frl) <- NULL


  uc_base_yearly <- list()
  uc_base_yearly$v <- data.frame(replicate(5, rbind(
      list(FRLDeforestation$v_df_U_aae + FRLDeforestation$v_df_L_aae),
      list(FRLBurning$v_fd_bb_aae),
      list(FRLFelling$v_fd_lg_aae),
      list(FRLNaturalForestDegradation$v_fd_nf_aae),
      list(FRLHardwoodPlantations$v_ec_hw_aae),
      list(FRLSoftwoodPlantations$v_ec_sw_aae)
      )))
  uc_base_yearly$uci <- data.frame(replicate(5, c(
      FRLDeforestation$rs_df$uci_aa_em_tco2e_yr,
      FRLBurning$rs_fd_bb$uci_aa_em_tco2e_yr,
      FRLFelling$rs_fd_lg$uci[1],
      FRLNaturalForestDegradation$rs_fd_nf$uci_aa_em_tco2e_yr,
      FRLHardwoodPlantations$uci_ec_hw_aae,
      FRLSoftwoodPlantations$uci_ec_sw_aae
      )))
  uc_base_yearly$lci <- data.frame(replicate(5, c(
      FRLDeforestation$rs_df$lci_aa_em_tco2e_yr,
      FRLBurning$rs_fd_bb$lci_aa_em_tco2e_yr,
      FRLFelling$rs_fd_lg$lci[1],
      FRLNaturalForestDegradation$rs_fd_nf$lci_aa_em_tco2e_yr,
      FRLHardwoodPlantations$lci_ec_hw_aae,
      FRLSoftwoodPlantations$lci_ec_sw_aae
      )))
  names(uc_base_yearly$v) <- c(2019:2023)
  names(uc_base_yearly$uci) <- c(2019:2023)
  names(uc_base_yearly$lci) <- c(2019:2023)



  uc_yearly <- list()
  uc_yearly$v <- rbind(
      uc_base_yearly$v,
      apply(FRLAfforestation$v_ec_ar_cstock,2,list),
      apply(FRLFelling$v_ec_lnf_cstock,2,list),
      apply(FRLHardwoodPlantations$v_ec_hw_cstock,2,list),
      apply(FRLSoftwoodPlantations$v_ec_sw_cstock,2,list)
    )
  uc_yearly$uci <- rbind(
      uc_base_yearly$uci,
      FRLAfforestation$rs_ec_ar_cstock[8,],
      FRLFelling$rs_ec_lnf_cstock[8,],
      FRLHardwoodPlantations$rs_ec_hw_cstock[8,],
      FRLSoftwoodPlantations$rs_ec_sw_cstock[8,]
    )
  uc_yearly$lci <- rbind(
      uc_base_yearly$lci,
      FRLAfforestation$rs_ec_ar_cstock[7,],
      FRLFelling$rs_ec_lnf_cstock[7,],
      FRLHardwoodPlantations$rs_ec_hw_cstock[7,],
      FRLSoftwoodPlantations$rs_ec_sw_cstock[7,]
    )

  row.names(uc_yearly$lci) <- NULL
  row.names(uc_yearly$uci) <- NULL


  total_frl <- list()
  total_frl$v <- data.frame(rbind(
      apply(uc_yearly$v[c(1),],2,function(v) { list(Reduce("+",v)) }), #11
      apply(uc_yearly$v[c(2,3,4,8),],2,function(v) { list(Reduce("+",v)) }), #12
      apply(uc_yearly$v[c(5,6,7,9,10),],2,function(v) { list(Reduce("+",v)) }), #13
      apply(uc_yearly$v[c(1,2,3,4,5,6,7,8,9,10),],2,function(v) { list(Reduce("+",v)) }), #14
      apply(uc_yearly$v[c(2,3,8),],2,function(v) { list(Reduce("+",v)) }), #15
      apply(uc_yearly$v[c(1),],2,function(v) { list(Reduce("+",v)) }), #16
      apply(uc_yearly$v[c(4),],2,function(v) { list(Reduce("+",v)) }) #17
  ))
  names(total_frl$v) <- c(2019:2023)
  total_frl$mu <- apply(total_frl$v,c(1,2),function(v) { mean(unlist(v)) })
  total_frl$lci <- apply(total_frl$v,c(1,2),function(v) { quantile(unlist(v), probs = FRLParams$qlci) })
  total_frl$uci <- apply(total_frl$v,c(1,2),function(v) { quantile(unlist(v), probs = FRLParams$quci) })


  uc_yearly$uci <- rbind(uc_yearly$uci,total_frl$uci)
  uc_yearly$lci <- rbind(uc_yearly$lci,total_frl$lci)


  uc_yearly$uci <- cbind(
      names,
      uc_yearly$uci
    )
  uc_yearly$lci <- cbind(
      names,
      uc_yearly$lci
    )

  uc_mp_frl <- list()
  uc_mp_frl$v <- data.frame(rbind(
      list(apply(total_frl$v[1,c(2,3)],1,function(v) { Reduce("+",list(unlist(v[1]), unlist(v[2]) )) })), # 11
      list(apply(total_frl$v[2,c(2,3)],1,function(v) { Reduce("+",list(unlist(v[1]), unlist(v[2]) )) })), # 12
      list(apply(total_frl$v[3,c(2,3)],1,function(v) { Reduce("+",list(unlist(v[1]), unlist(v[2]) )) })), # 13
      list(apply(total_frl$v[4,c(2,3)],1,function(v) { Reduce("+",list(unlist(v[1]), unlist(v[2]) )) })), # 14
      list(apply(total_frl$v[5,c(2,3)],1,function(v) { Reduce("+",list(unlist(v[1]), unlist(v[2]) )) })), # 15
      list(apply(total_frl$v[6,c(2,3)],1,function(v) { Reduce("+",list(unlist(v[1]), unlist(v[2]) )) })), # 16
      list(apply(total_frl$v[7,c(2,3)],1,function(v) { Reduce("+",list(unlist(v[1]), unlist(v[2]) )) })) # 17
  ))


 mp_frl <- data.frame(
      MP_FRL = c(
        sum(overall_frl[13,c(2,3)]),
        sum(overall_frl[14,c(2,3)]),
        sum(overall_frl[15,c(2,3)]),
        sum(overall_frl[16,c(2,3)]),
        sum(overall_frl[17,c(2,3)])
      ),
      MU = unlist(lapply(uc_mp_frl$v[3:7,],mean)),
      LCI = unlist(lapply(uc_mp_frl$v[3:7,],quantile,probs= FRLParams$qlci)),
      UCI = unlist(lapply(uc_mp_frl$v[3:7,],quantile,probs= FRLParams$quci)),
      halfwidth = NA
  )

  mp_frl$halfwidth<-round(100*
          (
            mp_frl[4] - mp_frl[3]
            )
            /2
            /mp_frl[2],
          1
      )
  row.names(mp_frl) <- c("Enh", "NetFRL", "FDeg", "Defor", "FDegNonProxy")


  ErpaYearlyFRL <- list()
  ErpaYearlyFRL$yearly <- overall_frl
  ErpaYearlyFRL$uc_yearly <- uc_yearly
  ErpaYearlyFRL$mp_frl <- mp_frl


  result <- list()
  result$erpa_yearly <- ErpaYearlyFRL

  return(result)
}

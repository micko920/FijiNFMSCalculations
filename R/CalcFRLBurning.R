
#' @export
calcFRLBurning <- function() {
  # Structure of 'sw_barea'
  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLBurning.R", ":6"))
    print(str(sw_barea))
  }


  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLBurning.R", ":12"))
    # Aggregate compartment data for the years 2015 to 2018 ................................
    ## Total area burnt in year t
    sw_barea_agg <- aggregate(area_ha ~ year, sw_barea, sum)
    # Average age of burnt compartments
    sw_barea_agg$avg_age_yrs <- aggregate(age_yrs ~ year, sw_barea, mean)[, 2]
    # Number of compartments burnt in year t
    sw_barea_agg$count <- aggregate(age_yrs ~ year, sw_barea, length)[, 2]
    # Rearrange columns
    sw_barea_agg <- sw_barea_agg[, c(1, 4, 2, 3)]
    # Show
    print(sw_barea_agg)
  }

  # Above- and below-ground biomass in compartments
  # 0.2 = Rdll Root-to-shoot ratio tropical moist deciduous forest < 125 tB ha-1
  sw_barea$agb <- sw_barea$age_yrs * (FRLParams$maibp / (1 + FRLParams$rdlk1)) # AGB
  sw_barea$bgb <- sw_barea$age_yrs * (FRLParams$maibp * FRLParams$rdlk1) # BGB

  # Table of greenhouse gases
  names(bioburn_ghgs)[1] <- "GHG"

  # Table of greenhouse gases
  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLBurning.R", ":36"))
    print(bioburn_ghgs)
  }

  # Emissions (in tCO2e) for each gas (and each compartment)
  # CO_2 (above-ground biomass)
  sw_barea$co2agb <- sw_barea$area_ha * sw_barea$agb * bioburn_ghgs[1, 2] *
    bioburn_ghgs[1, 3] * bioburn_ghgs[1, 4] * 0.001
  # CO_2 (below-ground biomass)
  sw_barea$co2bgb <- sw_barea$area_ha * sw_barea$bgb * FRLParams$etacf *
    FRLParams$etacc * bioburn_ghgs[1, 2]
  # CH_4 (above-ground biomass)
  sw_barea$ch4 <- sw_barea$area_ha * sw_barea$agb * bioburn_ghgs[2, 2] *
    bioburn_ghgs[2, 3] * bioburn_ghgs[2, 4] * 0.001
  # N_2O (above-ground biomass)
  sw_barea$n2o <- sw_barea$area_ha * sw_barea$agb * bioburn_ghgs[3, 2] *
    bioburn_ghgs[3, 3] * bioburn_ghgs[3, 4] * 0.001

  # Sum of emissions per year
  swfiret <- aggregate(. ~ year, sw_barea[, c(1, 6:9)], sum)

  # Compute totals of gases for each year
  swfiret$total <- rowSums(swfiret[, -1])
  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLBurning.R", ":57"))
    print(swfiret)
  }

  # Average annual emissions [tCO2e yr^-1] from biomass burning in Softwood Plantations .
  fd_bb_aae <- mean(swfiret$total)

  # Uncertainty analysis
  # Create vectors that collect the results of the MC simulation
  v_fd_bb_aae <- vector()

  # Random inputs for the MC simulation
  mcf <- data.frame(
    # Root to shoot ratio (IPCC [2006] default values)
    r2s = rtriangle(FRLParams$runs,
      theta = FRLParams$rdlk1,
      lower = FRLParams$lcirdlk1,
      upper = FRLParams$ucirdlk1
    ),
    # Mean annual total biomass increment in Softwood Plantations
    # (estimate from Waterloo [1994]). 25% error.
    maibsw = rtriangle(
      FRLParams$runs,
      theta = FRLParams$maibp,
      lower = FRLParams$maibp - FRLParams$maibp * FRLParams$errmaibp,
      upper = FRLParams$maibp + FRLParams$maibp * FRLParams$errmaibp
    ),
    # Combustion factor in Softwood Plantations (IPCC, 2006).
    cfsw = rtriangle(
      FRLParams$runs,
      theta = bioburn_ghgs[1,2],
      lower = bioburn_ghgs[1,2] - bioburn_ghgs[1,2] * FRLParams$errghg,
      upper = bioburn_ghgs[1,2] + bioburn_ghgs[1,2] * FRLParams$errghg
    ),
    # Emission factor CO_2 (IPCC default)
    gefco2 = rnorm(FRLParams$runs, bioburn_ghgs[1,3], FRLParams$sdCO2EF),
    # Emission factor CH_4 (IPCC default)
    gefch4 = rtriangle(
      FRLParams$runs,
      theta = bioburn_ghgs[2,3],
      lower = bioburn_ghgs[2,3] - bioburn_ghgs[2,3] * FRLParams$errghg,
      upper = bioburn_ghgs[2,3] + bioburn_ghgs[2,3] * FRLParams$errghg
    ),
    # Emission factor N_2O (IPCC default)
    gefn2o = rtriangle(
      FRLParams$runs,
      theta = bioburn_ghgs[3,3],
      lower = bioburn_ghgs[3,3] - bioburn_ghgs[3,3] * FRLParams$errghg,
      upper = bioburn_ghgs[3,3] + bioburn_ghgs[3,3] * FRLParams$errghg
    ),
    # Global warming potential CO_2 (IPCC default)
    gwpco2 = rep(bioburn_ghgs[1, 4], FRLParams$runs),
    # Global warming potential CH_4 (IPCC default)
    gwpch4 = rtriangle(
      FRLParams$runs,
      theta = bioburn_ghgs[2,4],
      lower = bioburn_ghgs[2,4] - bioburn_ghgs[2,4] * FRLParams$errghg,
      upper = bioburn_ghgs[2,4] + bioburn_ghgs[2,4] * FRLParams$errghg
    ),
    # Global warming potential N_2O (IPCC default)
    gwpn2o = rtriangle(
      FRLParams$runs,
      theta = bioburn_ghgs[3,4],
      lower = bioburn_ghgs[3,4] - bioburn_ghgs[3,4] * FRLParams$errghg,
      upper = bioburn_ghgs[3,4] + bioburn_ghgs[3,4] * FRLParams$errghg
    )
  )

  # MC simulation
  for (i in 1:FRLParams$runs) { # i <- 1
    # Create a copy of 'sw_barea'
    sw_bareai <- sw_barea

    # Compute AGB and BGB for each compartment .........................................
    sw_bareai$agb <- sw_bareai$age_yrs * (mcf$maibsw[i] / (1 + mcf$r2s[i]))
    sw_bareai$bgb <- sw_bareai$age_yrs * (mcf$maibsw[i] * mcf$r2s[i])

    # Compute emissions ................................................................
    # CO_2 (AGB)
    sw_bareai$co2agb <- sw_bareai$area_ha * sw_bareai$agb * mcf[i, "cfsw"] *
      mcf[i, "gefco2"] * mcf[i, "gwpco2"] * 0.001
    # CO_2 (BGB)
    sw_bareai$co2bgb <- sw_bareai$area_ha * sw_bareai$bgb * FRLParams$etacf *
      FRLParams$etacc * mcf[i, "cfsw"]
    # CH_4 (AGB)
    sw_bareai$ch4 <- sw_bareai$area_ha * sw_bareai$agb * mcf[i, "cfsw"] *
      mcf[i, "gefch4"] * mcf[i, "gwpch4"] * 0.001
    # N_2O (AGB)
    sw_bareai$n2o <- sw_bareai$area_ha * sw_bareai$agb * mcf[i, "cfsw"] *
      mcf[i, "gefn2o"] * mcf[i, "gwpn2o"] * 0.001

    # Aggregate results ................................................................
    swfireti <- aggregate(. ~ year, sw_bareai[, c(1, 6:9)], sum)
    swfireti$total <- rowSums(swfireti[, -1])

    # Annual average emissions .........................................................
    v_fd_bb_aae[i] <- mean(swfireti$total) # Including AGB and BGB
  }

  # Get 90%-confidence bounds of emission estimates (including AGB and BGB)
  lcifdfsweaae <- quantile(v_fd_bb_aae, prob = c(FRLParams$qlci)) # Lower bound
  ucifdfsweaae <- quantile(v_fd_bb_aae, prob = c(FRLParams$quci)) # Upper bound

  # Result table (AGB and BGB) ...........................................................
  rs_fd_bb <- data.frame(
    aa_em_tco2e_yr = fd_bb_aae,
    lci_aa_em_tco2e_yr = lcifdfsweaae,
    uci_aa_em_tco2e_yr = ucifdfsweaae
  )
  row.names(rs_fd_bb) <- "1"

  # Show result table
  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLBurning.R", ":170"))
    print(rs_fd_bb)
  }

  result <- list()
  result$rs_fd_bb <- rs_fd_bb
  result$fd_bb_aae <- fd_bb_aae
  result$v_fd_bb_aae <- v_fd_bb_aae

  return(result)
}

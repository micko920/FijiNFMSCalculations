


#' @export
calcFRLBurningAlg <- function(sw_barea,maibp,rdlk1,bioburn_ghgs) {
  result <- list()
  result$sw_barea <- sw_barea
  # Sum of emissions per year
  result$swfiret$total <- sapply(split(result$sw_barea[,c(1:3)],
                                  f = result$sw_barea$year),
    function(x) {
      return(CalcEstEmFire(x["age_yrs"], maibp, rdlk1,x["area_ha"],
                           local_CombustFactor = bioburn_ghgs[1,"combustion_factor"],
                           local_GWP_CO2 = bioburn_ghgs[1,"global_warming_potential"], 
                           local_EF_CO2 = bioburn_ghgs[1,"emission_factor"],
                           local_GWP_CH4 = bioburn_ghgs[2,"global_warming_potential"], 
                           local_EF_CH4 = bioburn_ghgs[2,"emission_factor"],
                           local_GWP_N2O = bioburn_ghgs[3,"global_warming_potential"], 
                           local_EF_N2O = bioburn_ghgs[3,"emission_factor"]))
    }
  )
  
  
  # Average annual emissions [tCO2e yr^-1] from biomass burning in Softwood Plantations .
  result$fd_bb_aae <- mean(result$swfiret$total)

  return(result)
}


  
#' @export
calcFRLBurningRun <- function(debug_frl,sw_barea,FRLParams,bioburn_ghgs) {
  # Structure of 'sw_barea'
  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLBurning.R", ":36"))
    print(str(sw_barea))
  }


  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLBurning.R", ":42"))
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

  # Table of greenhouse gases
  names(bioburn_ghgs)[1] <- "GHG"

  # Table of greenhouse gases
  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLBurning.R", ":61"))
    print(bioburn_ghgs)
  }

  fire <- calcFRLBurningAlg(sw_barea,
                            FRLParams$maibp,
                            FRLParams$rdlk1,
                            bioburn_ghgs
                            )
  
  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLBurning.R", ":72"))
    print(fire$swfiret)
  }

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
    
    bioburn_ghgsi <- bioburn_ghgs
    bioburn_ghgsi[1, 2] <- mcf[i, "cfsw"] 
    bioburn_ghgsi[1, 3] <- mcf[i, "gefco2"]
    bioburn_ghgsi[1, 4] <- mcf[i, "gwpco2"]
    bioburn_ghgsi[2, 2] <- mcf[i, "cfsw"]
    bioburn_ghgsi[2, 3] <- mcf[i, "gefch4"]
    bioburn_ghgsi[2, 4] <- mcf[i, "gwpch4"]
    bioburn_ghgsi[3, 2] <- mcf[i, "cfsw"]
    bioburn_ghgsi[3, 3] <- mcf[i, "gefn2o"]
    bioburn_ghgsi[3, 4] <- mcf[i, "gwpn2o"]
    
    # Compute emissions ................................................................
    firei <- calcFRLBurningAlg(sw_barea,
                              mcf$maibsw[i],
                              mcf$r2s[i],
                              bioburn_ghgsi
                             )
    
    # Annual average emissions .........................................................
    v_fd_bb_aae[i] <- mean(firei$swfiret$total) # Including AGB and BGB
  }

  # Get 90%-confidence bounds of emission estimates (including AGB and BGB)
  lcifdfsweaae <- quantile(v_fd_bb_aae, prob = c(FRLParams$qlci)) # Lower bound
  ucifdfsweaae <- quantile(v_fd_bb_aae, prob = c(FRLParams$quci)) # Upper bound

  # Result table (AGB and BGB) ...........................................................
  rs_fd_bb <- data.frame(
    aa_em_tco2e_yr = fire$fd_bb_aae,
    lci_aa_em_tco2e_yr = lcifdfsweaae,
    uci_aa_em_tco2e_yr = ucifdfsweaae
  )
  row.names(rs_fd_bb) <- "1"

  # Show result table
  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLBurning.R", ":176"))
    print(rs_fd_bb)
  }

  result <- list()
  result$rs_fd_bb <- rs_fd_bb
  result$fd_bb_aae <- fire$fd_bb_aae
  result$v_fd_bb_aae <- v_fd_bb_aae

  return(result)
}


#' @export
calcFRLBurning <- function() {
  return(calcFRLBurningRun(debug_frl,sw_barea,FRLParams,bioburn_ghgs))
}

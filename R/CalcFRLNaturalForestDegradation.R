# dnf_area <- list()
# dnf_area$yearly <- 874.735
# dnf_area$se <- 245.872
# dnf_area$uci <- 874.735 + 404.4594
# dnf_area$lci <- 874.735 - 404.4594

#' @export
calcFRLNaturalForestDegradation <- function() {
  # Emissions from natural forest degradation
  fd_nf_aae <- CalcEstEmNFDeg(dnf_area$yearly, EFNFDeg, RootToShootTropRain )
  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLNaturalForestDegradation.R", ":12"))
    print(fd_nf_aae)
  }

  # Uncertainty analysis
  # Create vector
  v_fd_nf_aae <- vector()

  # Monte Carlo simulation
  for (i in 1:FRLParams$runs) { # i <- 1
    areai <- rnorm(
      n = 1, mean = dnf_area$yearly,
      sd = dnf_area$se
    )

    EFNFDegi <- rnorm(
      n = 1, mean = EFNFDeg,
      sd = EFNFDeg_SD
    )

    R2shooti <- rtriangle(
      n = 1, theta = RootToShootTropRain,
      lower = RootToShootTropRain - (RootToShootTropRain * errRootToShootTropRain),
      upper = RootToShootTropRain + (RootToShootTropRain * errRootToShootTropRain)
    )

    # Compute average annual emissions from natural forest degradation
    v_fd_nf_aae[i] <- CalcEstEmNFDeg(areai, EFNFDegi, R2shooti)
  }

  # Compute 90%-confidence bounds
  lcinfdc <- quantile(v_fd_nf_aae, prob = c(FRLParams$qlci))
  ucinfdc <- quantile(v_fd_nf_aae, prob = c(FRLParams$quci))

  # Result table 'emissions from nfdwood'
  rs_fd_nf <- data.frame(
    aa_em_tco2e_yr = fd_nf_aae,
    lci_aa_em_tco2e_yr = lcinfdc,
    uci_aa_em_tco2e_yr = ucinfdc
  )

  row.names(rs_fd_nf) <- "1"

  # Show result table for fuelwood
  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLNaturalForestDegradation.R", ":57"))
    print(rs_fd_nf)
  }

  result <- list()
  result$rs_fd_nf <- rs_fd_nf
  result$fd_nf_aae <- fd_nf_aae
  result$v_fd_nf_aae <- v_fd_nf_aae

  return(result)
}

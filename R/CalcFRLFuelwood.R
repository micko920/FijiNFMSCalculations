
#' @export
calcFRLFuelwood <- function() {
  # Data on fuelwood consumption
  # Number of households using fuelwood in rural and urban areas
  fuelwhh <- data.frame(
    year = c(2007, 2017),
    rural = c(60850, 35210),
    urban = c(12829, 6718)
  )
  # Average fuelwood consumption per household in rural and urban areas
  fuelwch <- data.frame(
    year = c(2007, 2017),
    rural = c(0.927, 0.927),
    urban = c(0.378, 0.378)
  )

  # Emissions from fuelwood consumption
  fd_fu_aae <- mean(rowSums(fuelwhh[, -1] * fuelwch[, -1]) * FRLParams$etacf * FRLParams$etacc)
  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLFuelwood.R", ":21"))
    print(fd_fu_aae)
  }

  # Uncertainty analysis
  # Create vector
  v_fd_fu_aae <- vector()

  # Monte Carlo simulation
  for (i in 1:FRLParams$runs) { # i <- 1
    # Random sample of the number of households using fuelwood
    fuelwhhi <- data.frame(
      year = c(2007, 2017),
      rural = c(
        rnorm(1, fuelwhh[1, 2], fuelwhh[1, 2] * .1),
        rnorm(1, fuelwhh[2, 2], fuelwhh[2, 2] * .1)
      ),
      urban = c(
        rnorm(1, fuelwhh[1, 3], fuelwhh[1, 3] * .1),
        rnorm(1, fuelwhh[2, 3], fuelwhh[2, 3] * .1)
      )
    )

    # Random sample of the average fuelwood consumption per household
    fuelwchi <- data.frame(
      year = c(2007, 2017),
      rural = c(
        rnorm(1, fuelwch[1, 2], fuelwch[1, 2] * .25),
        rnorm(1, fuelwch[2, 2], fuelwch[2, 2] * .25)
      ),
      urban = c(
        rnorm(1, fuelwch[1, 3], fuelwch[1, 3] * .25),
        rnorm(1, fuelwch[2, 3], fuelwch[2, 3] * .25)
      )
    )

    # Compute average annual emissions from fuelwood consumption
    v_fd_fu_aae[i] <- mean(rowSums(fuelwhhi[, -1] * fuelwchi[, -1]) *
      FRLParams$etacf * FRLParams$etacc)
  }

  # Compute 90%-confidence bounds
  lcifuelc <- quantile(v_fd_fu_aae, prob = c(FRLParams$qlci))
  ucifuelc <- quantile(v_fd_fu_aae, prob = c(FRLParams$quci))

  # Result table 'emissions from fuelwood'
  rs_fd_fu <- data.frame(
    aa_em_tco2e_yr = fd_fu_aae,
    lci_aa_em_tco2e_yr = lcifuelc,
    uci_aa_em_tco2e_yr = ucifuelc
  )

  row.names(rs_fd_fu) <- "1"

  # Show result table for fuelwood
  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLFuelwood.R", ":77"))
    print(rs_fd_fu)
  }

  result <- list()
  result$rs_fd_fu <- rs_fd_fu
  result$fd_fu_aae <- fd_fu_aae
  result$v_fd_fu_aae <- v_fd_fu_aae

  return(result)
}


#' @export
calcFRLFelling <- function() {

  # Create a data.frame for logging in Natural Forest (lnf); taking data from
  # 'lnf_logging'
  # Volumes logged [m^3] in Natural Forest
  if (debug_frl) print(lnf_volume)
  # Average annual volume extracted from Natural Forest [m^3]
  (avg_vol_lnf <- mean(lnf_volume$volume))

  # To 'translate' volumes extracted to carbon loss, the 'Total Emissions Factor' from
  # Haas (2015) was used.

  # Compute CO2 loss for the different years
  co2eloss_t <- sapply(lnf_volume$volume,function(v) CalcEstEmFell(FRLParams$tef,v))

  # Average annual gross emissions from forest degradation
  fd_lg_aae <- mean(co2eloss_t)

  # MC simulation to estimate the uncertainty in estimated emissions from logging in
  # Natural Forest (source 'forest degradation')
  fdaacl <- vector()

  # Run MC simulation
  for (i in 1:FRLParams$runs) { # i <- 1
    # Random TEF
    TEFi <- rtriangle(1,
      theta = FRLParams$tef,
      lower = FRLParams$tef - FRLParams$tef * FRLParams$errtef,
      upper = FRLParams$tef + FRLParams$tef * FRLParams$errtef
    )
    # Average annual CO2 loss caused by logging in Natural Forest
    fdaacl[i] <- CalcEstEmFell(TEFi,avg_vol_lnf)
  }

  # Removals
  #TODO: Need to handle removals being negative
  # Above-ground carbon (AGC) accumulated over the Reference Period. This gives
  # the CO2 accumulation OVER the Reference Period.
  co2egain_t <- FRLParams$deltaT * sapply(lnf_area$area_harvested_total_ha,function(v) CalcEstRemFell(FRLParams$maiclnf,v)) * -1

  # Average annual CO2 removals over the Reference Period from forest degradation
  # tCO_2e (removals)
  fd_lg_aar <- mean(co2egain_t)

  # Uncertainty assessment (MC simulation): gross removals after logging
  fdaacg <- vector() # Vector that collects the results

  # Run MC simulation
  for (i in 1:FRLParams$runs) { # i <- 1
    # Random MAI for CO2 accumulation
    maicli <- rtriangle(
      n = 1, theta = FRLParams$maiclnf,
      lower = FRLParams$maiclnf - FRLParams$maiclnf * FRLParams$errmaiclnf,
      upper = FRLParams$maiclnf + FRLParams$maiclnf * FRLParams$errmaiclnf
    )

    # Random sample of areas harvested in year t
    v_area_harvested_total_ha <-
      rtriangle(
        n = 11, theta = lnf_area$area_harvested_total_ha,
        lower = lnf_area$area_harvested_total_ha -
          lnf_area$area_harvested_total_ha * FRLParams$erralnf,
        upper = lnf_area$area_harvested_total_ha +
          lnf_area$area_harvested_total_ha * FRLParams$erralnf
      )

    # CO2 accumulation (over the Reference Period) on areas harvested in year t
    agcrlt <- FRLParams$deltaT * sapply(v_area_harvested_total_ha,function(v) CalcEstRemFell(maicli,v)) * -1
    # Collect results
    fdaacg[i] <- mean(agcrlt)
  }

  # Upper and lower 90%-confidence bounds of emissions from forest degradation (logging)
  lcifdaae <- quantile(fdaacl, probs = FRLParams$qlci)
  ucifdaae <- quantile(fdaacl, probs = FRLParams$quci)
  # MC emission estimates (forest degradation; logging)
  v_fd_lg_aae <- fdaacl

  # Upper and lower 90%-confidence bounds of removals from forest degradation
  lcifdaar <- quantile(fdaacg, probs = FRLParams$qlci)
  ucifdaar <- quantile(fdaacg, probs = FRLParams$quci)
  # MC removal estimates (forest degradation)
  v_fd_lg_aar <- fdaacg

  # Average annual net emissions from forest degradation (logging)
  fd_lg_aane <- (fd_lg_aae - fd_lg_aar)
  # Upper and lower 90%-confidence bounds
  lcifdaane <- quantile((fdaacl - fdaacg), probs = FRLParams$qlci)
  ucifdaane <- quantile((fdaacl - fdaacg), probs = FRLParams$quci)
  # MC net emissions estimates
  v_fd_lg_aane <- (fdaacl - fdaacg)

  # Forest degradation annual data
  # Summary table for forest degradation (values per year)
  fd <- data.frame(
    year             = lnf_volume$year,
    volume_logged_m3 = lnf_volume$volume,
    area_logged_ha   = lnf_area[, "area_harvested_total_ha"],
    emissions_tco2e  = co2eloss_t,
    removals_tco2e   = co2egain_t * -1
  )

  # Show annual data
  if (debug_frl) print(fd)

  # Results (net emissions from logging in Natural Forest; source 'forest degradation'
  # Create nice result table (annual average)
  rs_fd_lg <- data.frame(sourcesink = c(
    "FD gross emissions",
    "FD gross removals",
    "FD net emissions"
  ))
  rs_fd_lg$em <- c(fd_lg_aae, fd_lg_aar * -1, fd_lg_aane)

  # TODO: Fix hack order because quantile is done on positive number, but then * -1
  # reverses order of Upper and Lower CI
  rs_fd_lg$lci <- c(lcifdaae, ucifdaar * -1, lcifdaane)
  rs_fd_lg$uci <- c(ucifdaae, lcifdaar * -1, ucifdaane)

  result <- list()
  result$rs_fd_lg <- rs_fd_lg
  result$fd_lg_aane <- fd_lg_aane
  result$v_fd_lg_aae <- v_fd_lg_aae
  result$v_fd_lg_aar <- v_fd_lg_aar
  result$v_fd_lg_aane <- v_fd_lg_aane

  return(result)
}

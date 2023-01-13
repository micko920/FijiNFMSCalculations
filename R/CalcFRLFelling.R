
#' @export
calcFRLFelling <- function() {

  # Create a data.frame for logging in Natural Forest (lnf); taking data from
  # 'lnf_logging'
  # Volumes logged [m^3] in Natural Forest
  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLFelling.R", ":9"))
    print(lnf_volume)
  }
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
  co2egain_t <- FRLParams$deltaT * sapply(lnf_area$area_harvested_total_ha,function(v) CalcEstRemFell(v, FRLParams$maiclnf,c(1))) * -1

  # Average annual CO2 removals over the Reference Period from forest degradation
  # tCO_2e (removals)
  fd_lg_aar <- mean(co2egain_t)

  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLFelling.R", ":51"))
    lgcstock <- growthTotals(
      FRLParams$Ty, lnf_area$area_harvested_total_ha,
      sapply(
        growthMatrix(FRLParams$Ty, lnf_area$area_harvested_total_ha, FRLParams$rdeltaT),
        function(v) {
          sapply(v, CalcEstRemFell, FRLParams$maiclnf,c(1))
        }
      )
    )
    print(lgcstock)
  }
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
    agcrlt <- FRLParams$deltaT * sapply(v_area_harvested_total_ha,function(v) CalcEstRemFell(v, maicli,c(1))) * -1
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
  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLFelling.R", ":110"))
    print(fd)
  }

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

  # Growth tables projection rather than average yearly growth
  yearly_growth <- c(0.5,rep_len(1,FRLParams$Tl-1))
  area <- lnf_area$area_harvested_total_ha

  lnf_planted_cstock <- growthTotals(
        FRLParams$Ty, area,
        sapply(
          growthMatrix(FRLParams$Ty,
                       area,
                       yearly_growth,
                       projection=7, offset = 14),
          function(v) {
            sapply(v, CalcEstRemFell, FRLParams$maiclnf,c(1))
          }
        ),
        projection=7, offset = 14)

  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLHardwoodPlantations", ":204"))
    print(lnf_planted_cstock)
  }

  # Uncertainty analysis
  vlnfcstock <- matrix(nrow=0,ncol=length(FRLParams$Ty)+4)
  gm <- growthMatrix(FRLParams$Ty,rep_len(1,length(FRLParams$Ty)),c(0.5,rep_len(1,length(FRLParams$Ty)-1)), projection=7, offset = 14)

  mArea  <- mean(area)
  # MC simulation
  for (i in 1:FRLParams$runs) { # i <- 1
    # Random MAI for CO2 accumulation
    maicli <- rtriangle(
      n = 1, theta = FRLParams$maiclnf,
      lower = FRLParams$maiclnf - FRLParams$maiclnf * FRLParams$errmaiclnf,
      upper = FRLParams$maiclnf + FRLParams$maiclnf * FRLParams$errmaiclnf
    )

    # Random sample of areas harvested in year t
    vAreas <-
      rtriangle(
        n = 1, theta = mArea,
        lower = mArea - mArea * FRLParams$erralnf,
        upper = mArea + mArea * FRLParams$erralnf
      )
    r <- apply(gm*vAreas, 2, CalcEstRemFell, maicli,1)
    if (i==1) vlnfcstock <- r
    else vlnfcstock <- rbind(vlnfcstock, r)
  }
  colnames(vlnfcstock) <- colnames(gm)

  # Yearly removals
  ec_lnf_cstock <- lnf_planted_cstock[c(-1,-2)]# Estimate
  mucstock <- apply(vlnfcstock,2, mean)
  lcilnfcstock <- apply(vlnfcstock,2, quantile, probs = FRLParams$qlci) # Lower confidence limit
  ucilnfcstock <- apply(vlnfcstock,2, quantile, probs = FRLParams$quci) # Upper confidence limit
  v_ec_lnf_cstock <- vlnfcstock# MC estimates
  # Result table CStock
  rs_ec_lnf_cstock <- data.frame(
    rbind(
      removals_tco2e_yr = ec_lnf_cstock,
      lci_removals_tco2e_yr = lcilnfcstock,
      uci_removals_tco2e_yr = ucilnfcstock,
      mu_removals_tco2e_yr = mucstock),
    check.names = F
  )
  result <- list()
  result$rs_fd_lg <- rs_fd_lg
  result$fd_lg_area <- lnf_area$area_harvested_total_ha
  result$fd_lg_aane <- fd_lg_aane
  result$v_fd_lg_aae <- v_fd_lg_aae
  result$v_fd_lg_aar <- v_fd_lg_aar
  result$v_fd_lg_aane <- v_fd_lg_aane
  result$rs_ec_lnf_cstock <- rs_ec_lnf_cstock
  result$v_ec_lnf_cstock <- v_ec_lnf_cstock

  return(result)
}

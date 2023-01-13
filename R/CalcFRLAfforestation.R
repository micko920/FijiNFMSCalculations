


#' @export
calcFRLAfforestation <- function() {

  # Uncertainty attached to the estimated total carbon increment for AR
  varmaic <- rtriangle(
    # Random mean annual carbon increment
    n = FRLParams$runs,
    theta = FRLParams$maicar,
    lower = FRLParams$maicar - FRLParams$maicar * FRLParams$errmaicar,
    upper = FRLParams$maicar + FRLParams$maicar * FRLParams$errmaicar
  ) * # Uncertainty attached to root-to-shoot ratio (tropical rainforest)
    (1 + rtriangle(
      n = FRLParams$runs,
      theta = FRLParams$Rlwk,
      lower = FRLParams$Rlwk - FRLParams$Rlwk * FRLParams$errRlwk,
      upper = FRLParams$Rlwk + FRLParams$Rlwk * FRLParams$errRlwk
    ))

  # Adding below-ground carbon
  # Carbon gains on areas afforested/reforested in year t (over the Reference Period)
  arcgainst <- FRLParams$deltaT * AdjustedAreas$ARareas * FRLParams$maicar * (1 + FRLParams$Rlwk)

  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLAfforestation.R", ":28"))
    # Create a data frame of C gains over the Reference Period
    arcgains <- data.frame(
      interval = as.character(FRLParams$Ty),
      C_gain_t = arcgainst
    )
    print(arcgains)
  }

  # Average annual C gains (AR) over the Reference Period
  araacg <- sum(arcgainst) / FRLParams$Tl
  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLAfforestation.R", ":50"))
    print(araacg)
  }

  # Uncertainty analysis
  # Create vector
  varaacg <- vector()

  # MC simulation
  for (i in 1:FRLParams$runs) { # i <- 1
    varaacg[i] <- (sum(FRLParams$deltaT * # Time available for growth...
      sum(AdjustedAreas$MCaaafor[i, ]) * # Average annual area of AR
      varmaic[i]) # Random increment
    ) / FRLParams$Tl # Length of the FRL Reference Period
  }

  # Average annual removals from afforestation/reforestation (AR)
  ec_ar_aar <- araacg * FRLParams$etacc # Estimate
  lciaraar <- quantile(varaacg * FRLParams$etacc, probs = FRLParams$qlci) # Lower confidence limit
  uciaraar <- quantile(varaacg * FRLParams$etacc, probs = FRLParams$quci) # Upper confidence limit
  v_ec_ar_aar <- varaacg * FRLParams$etacc # MC estimates

  # Result table AR (estimates are multiplied by -1, because removals always have
  # a negative sign)
  rs_ec_ar <- data.frame(
    aa_removals_tco2e_yr = ec_ar_aar * -1,
    lci_aa_removals_tco2e_yr = uciaraar * -1,
    uci_aa_removals_tco2e_yr = lciaraar * -1
  )

  row.names(rs_ec_ar) <- "1"
  # Show result table
  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLAfforestation.R", ":83"))
    print(rs_ec_ar)
  }

  # Growth tables projection rather than average yearly growth
  yearly_growth <- c(0.5,rep_len(1,FRLParams$Tl-1))

  arcstock <- growthTotals(
        FRLParams$Ty,
        rep(AdjustedAreas$ARareas, FRLParams$Tl),
        growthMatrix(FRLParams$Ty,
                     rep(AdjustedAreas$ARareas, FRLParams$Tl),
                     yearly_growth, projection=7, offset = 14) * FRLParams$maicar * (1 + FRLParams$Rlwk),
        projection=7, offset = 14
  )

  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLAfforestation.R", ":76"))
    print(arcstock)
  }

  # Uncertainty analysis
  varcstock <- matrix(nrow=0,ncol=length(FRLParams$Ty)+4)
  gm <- growthMatrix(FRLParams$Ty,rep_len(1,length(FRLParams$Ty)),c(0.5,rep_len(1,length(FRLParams$Ty)-1)), projection=7, offset = 14)

  # MC simulation
  for (i in 1:FRLParams$runs) { # i <- 1
    vAreas <-  sum(AdjustedAreas$MCaaafor[i, ]) # Up and Low land
    varmai <- rtriangle(
      # Random mean annual carbon increment
      n = FRLParams$Ty+7,
      theta = FRLParams$maicar,
      lower = FRLParams$maicar - FRLParams$maicar * FRLParams$errmaicar,
      upper = FRLParams$maicar + FRLParams$maicar * FRLParams$errmaicar
    )
    # Uncertainty attached to root-to-shoot ratio (tropical rainforest)
    varRlw <- (1 + rtriangle(
      n = FRLParams$Ty+7,
      theta = FRLParams$Rlwk,
      lower = FRLParams$Rlwk - FRLParams$Rlwk * FRLParams$errRlwk,
      upper = FRLParams$Rlwk + FRLParams$Rlwk * FRLParams$errRlwk
    ))
    r <- colSums(gm * vAreas * varmai * varRlw)
    if (i==1) varcstock <- r
    else varcstock <- rbind(varcstock, r)
  }
  colnames(varcstock) <- colnames(gm)

  # Yearly removals from afforestation/reforestation (AR)
  ec_ar_cstock <- arcstock[c(-1,-2)] * FRLParams$etacc# Estimate
  mucstock <- apply(varcstock * FRLParams$etacc,2, mean)
  lciarcstock <- apply(varcstock * FRLParams$etacc,2, quantile, probs = FRLParams$qlci) # Lower confidence limit
  uciarcstock <- apply(varcstock * FRLParams$etacc,2, quantile, probs = FRLParams$quci) # Upper confidence limit
  v_ec_ar_cstock <- varcstock* FRLParams$etacc # MC estimates
  # Result table AR CStock
  rs_ec_ar_cstock <- data.frame(
    rbind(
      removals_tco2e_yr = ec_ar_cstock * -1,
      lci_removals_tco2e_yr = lciarcstock * -1,
      uci_removals_tco2e_yr = uciarcstock * -1,
      mu_removals_tco2e_yr = mucstock * -1),
    check.names = F
  )

  result <- list()
  result$rs_ec_ar <- rs_ec_ar
  result$v_ec_ar_aar <- v_ec_ar_aar
  result$ar_aa_area <- AdjustedAreas$ARareas
  result$rs_ec_ar_cstock <- rs_ec_ar_cstock
  result$v_ec_ar_cstock <- v_ec_ar_cstock
  return(result)
}

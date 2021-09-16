
#' @export
calcFRLPlantations <- function() {

  # Confidence interval bounds
  # Gross emissions Forest Plantations (Hard- and Softwood)
  lciv_ec_hwsw_aae <- quantile(FRLSoftwoodPlantations$v_ec_sw_aae + # Emissions Softwood
    FRLHardwoodPlantations$v_ec_hw_aae, # Emissions Hardwood
  probs = FRLParams$qlci
  )
  uciv_ec_hwsw_aae <- quantile(FRLSoftwoodPlantations$v_ec_sw_aae +
    FRLHardwoodPlantations$v_ec_hw_aae,
  probs = FRLParams$quci
  )
  # Gross removals Forest Plantations (Hard- and Softwood)
  lciv_ec_hwsw_aar <- quantile(FRLSoftwoodPlantations$v_ec_sw_aar + # Removals Softwood
    FRLHardwoodPlantations$v_ec_hw_aar, # Removals Hardwood
  probs = FRLParams$qlci
  )
  uciv_ec_hwsw_aar <- quantile(FRLSoftwoodPlantations$v_ec_sw_aar +
    FRLHardwoodPlantations$v_ec_hw_aar,
  probs = FRLParams$quci
  )
  # Net emissions Forest Plantations (Hard- and Softwood)
  lciv_ec_hwsw_aane <- quantile((FRLSoftwoodPlantations$v_ec_sw_aae + FRLHardwoodPlantations$v_ec_hw_aae) - # Emissions Plantations
    (FRLSoftwoodPlantations$v_ec_sw_aar + FRLHardwoodPlantations$v_ec_hw_aar), # Removals Plantations
  probs = FRLParams$qlci
  )
  uciv_ec_hwsw_aane <- quantile((FRLSoftwoodPlantations$v_ec_sw_aae + FRLHardwoodPlantations$v_ec_hw_aae) -
    (FRLSoftwoodPlantations$v_ec_sw_aar + FRLHardwoodPlantations$v_ec_hw_aar),
  probs = FRLParams$quci
  )

  rs_ec_pl <- data.frame(
      plantations = c(
        " Gross em. Hardw.", # Gross emissions Hardwood Plantations
        " Gross em. Softw.", # Gross emissions Softwood Plantations
        " Gross em. Plant.", # Gross emissions Forest Plantations
        " Gross rem. Hardw.", # Gross removals Hardwood Plantations
        " Gross rem. Softw.", # Gross removals Softwood Plantations
        " Gross rem. Plant.", # Gross removals Forest Plantations
        " Net em. Hardw.", # Net emissions Hardwood Plantations
        " Net em. Softw.", # Net emissions Softwood Plantations
        " Net em. Plant." # Net emissions Forest Plantations
      ),
      aa_em_tco2e_yr = c(
        FRLHardwoodPlantations$ec_hw_aae,
        FRLSoftwoodPlantations$ec_sw_aae,
        FRLSoftwoodPlantations$ec_sw_aae + FRLHardwoodPlantations$ec_hw_aae,
        FRLHardwoodPlantations$ec_hw_aar * -1,
        FRLSoftwoodPlantations$ec_sw_aar * -1,
        (FRLSoftwoodPlantations$ec_sw_aar + FRLHardwoodPlantations$ec_hw_aar) * -1,
        (FRLHardwoodPlantations$ec_hw_aae - FRLHardwoodPlantations$ec_hw_aar),
        FRLSoftwoodPlantations$ec_sw_aae - FRLSoftwoodPlantations$ec_sw_aar,
        (FRLSoftwoodPlantations$ec_sw_aae + FRLHardwoodPlantations$ec_hw_aae) - (FRLSoftwoodPlantations$ec_sw_aar + FRLHardwoodPlantations$ec_hw_aar)
      ),
      lci_aa_em_tco2e_yr = c(
        FRLHardwoodPlantations$lci_ec_hw_aae,
        FRLSoftwoodPlantations$lci_ec_sw_aae,
        lciv_ec_hwsw_aae,
        FRLHardwoodPlantations$uci_ec_hw_aar * -1,
        FRLSoftwoodPlantations$uci_ec_sw_aar * -1,
        uciv_ec_hwsw_aar * -1,
        FRLHardwoodPlantations$lciv_ec_hw_aane,
        FRLSoftwoodPlantations$lciv_ec_sw_aane,
        lciv_ec_hwsw_aane
      ),
      uci_aa_em_tco2e_yr = c(
        FRLHardwoodPlantations$uci_ec_hw_aae,
        FRLSoftwoodPlantations$uci_ec_sw_aae,
        uciv_ec_hwsw_aae,
        FRLHardwoodPlantations$lci_ec_hw_aar * -1,
        FRLSoftwoodPlantations$lci_ec_sw_aar * -1,
        lciv_ec_hwsw_aar * -1,
        FRLHardwoodPlantations$uciv_ec_hw_aane,
        FRLSoftwoodPlantations$uciv_ec_sw_aane,
        uciv_ec_hwsw_aane
      )

  )
  row.names(rs_ec_pl) <- c("2","1","3","21","11","31","22","12","32")

  # Show result table for Forest Plantations
  if (debug_frl) print(rs_ec_pl)

  result <- list()
  result$rs_ec_pl <- rs_ec_pl
  return(result)
}

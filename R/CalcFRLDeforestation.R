
#' @export
calcFRLDeforestation <- function() {
  # Average annual carbon loss in Low- and Upland Natural Forest
  # Lowland
  df_L_aae <- CalcEstEmDefor(AdjustedAreas$areaLoss[1], EmissionFactors$df_ef[1, "ef_tco2e_ha"])
  # Upland
  df_U_aae <- CalcEstEmDefor(AdjustedAreas$areaLoss[2], EmissionFactors$df_ef[2, "ef_tco2e_ha"])

  # Average annual emissions from deforestation
  # Low- and Upland Natural Forest
  df_aae <- CalcGrossEmDefor(df_U_aae, df_L_aae)

  # MC estimates of average annual emissions from Lowland Natural Forest
  v_df_L_aae <- AdjustedAreas$MCaadeforL * EmissionFactors$v_dc[, 2] * FRLParams$etacc
  # MC estimates of average annual emissions from Upland Natural  Forest
  v_df_U_aae <- AdjustedAreas$MCaadeforU * EmissionFactors$v_dc[, 3] * FRLParams$etacc

  # Quantiles of MC average annual emissions from deforestation
  # Lowland Natural Forest
  lcidfaaeL <- quantile(v_df_L_aae, probs = FRLParams$qlci)
  ucidfaaeL <- quantile(v_df_L_aae, probs = FRLParams$quci)
  # Upland Natural Forest
  lcidfaaeU <- quantile(v_df_U_aae, probs = FRLParams$qlci)
  ucidfaaeU <- quantile(v_df_U_aae, probs = FRLParams$quci)
  # Low- and Upland Natural Forest
  lcidfaaeLU <- quantile(v_df_aae <- v_df_U_aae + v_df_L_aae, probs = FRLParams$qlci)
  ucidfaaeLU <- quantile(v_df_U_aae + v_df_L_aae, FRLParams$quci)

  # Result table: emissions from deforestation
  rs_df <- data.frame(
    stratum = c("Lowland", "Upland", "Total"),
    # Average annual emissions from deforestation
    aa_em_tco2e_yr = c(df_L_aae, df_U_aae, df_aae),
    # Lower confidence interval bound
    lci_aa_em_tco2e_yr = c(lcidfaaeL, lcidfaaeU, lcidfaaeLU),
    # Upper confidence interval bound
    uci_aa_em_tco2e_yr = c(ucidfaaeL, ucidfaaeU, ucidfaaeLU)
  )

  rs_df_all <- rs_df # Strata and aggregated strata
  rs_df_strata <- rs_df[-3, ] # Strata only
  rs_df <- rs_df[3, -1] # Aggregated strata only
  # Show result table
  if (debug_frl) print(rs_df_all)

  result <- list()
  result$rs_df <- rs_df
  result$v_df_L_aae <- v_df_L_aae
  result$v_df_U_aae <- v_df_U_aae
  return(result)
}


#' @export
calcFRLDegradation <- function() {
  # Result table for the net source 'forest degradation'
  rs_fd <- data.frame(
    source = c("FD_Logging_net", "FD_Biomass_burning", "FD_NaturalForest", "FD_total"),
    # Estimates (net emissions from logging in Natural Forest, emissions from
    # biomass burning in Softwood Plantations, Natural Forest Degradation and total)
    est = c(
      FRLFelling$fd_lg_aane, # Net emissions from logging (FD)
      FRLBurning$fd_bb_aae, # Emissions from biomass burning (FD)
      FRLNaturalForestDegradation$fd_nf_aae, # Emissions from Natural forest degradation (FD)
      FRLFelling$fd_lg_aane + FRLBurning$fd_bb_aae + FRLNaturalForestDegradation$fd_nf_aae # All
    ),
    # Lower 90%-confidence limits
    lci = c(
      quantile(FRLFelling$v_fd_lg_aane, # MC estimates net em. logging
        probs = FRLParams$qlci
      ),
      quantile(FRLBurning$v_fd_bb_aae, # MC estimates fire
        probs = FRLParams$qlci
      ),
      quantile(FRLNaturalForestDegradation$v_fd_nf_aae, # MC estimates Natural Forest degradation
        probs = FRLParams$qlci
      ),
      quantile(FRLFelling$v_fd_lg_aane + # MC estimates net em. logging
        FRLBurning$v_fd_bb_aae + # and MC estimates fire
        FRLNaturalForestDegradation$v_fd_nf_aae, # and MC estimates natural forest degradation
      probs = FRLParams$qlci
      )
    ),
    # Upper 90%-confidence limits
    uci = c(
      quantile(FRLFelling$v_fd_lg_aane, # MC estimates net em. logging
        probs = FRLParams$quci
      ),
      quantile(FRLBurning$v_fd_bb_aae, # MC estimates fire
        probs = FRLParams$quci
      ),
      quantile(FRLNaturalForestDegradation$v_fd_nf_aae, # MC estimates Natural Forest degradation
        probs = FRLParams$quci
      ),
      quantile(FRLFelling$v_fd_lg_aane + # MC estimates net em. logging
        FRLBurning$v_fd_bb_aae + # and MC estimates fire
        FRLNaturalForestDegradation$v_fd_nf_aae, # and MC estimates natural forest degradation
      probs = FRLParams$quci
      )
    )
  )
  # Show results: forest degaradtion
  if (debug_frl) {
    print(paste0("==== debug: ", "CalcFRLDegradation.R", ":52"))
    print(rs_fd)
  }
  result <- list()
  result$rs_fd <- rs_fd
  return(result)
}

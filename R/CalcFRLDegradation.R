
#' @export
calcFRLDegradation <- function() {
  # Result table for the net source 'forest degradation'
  rs_fd <- data.frame(
    source = c("FD_Logging_net", "FD_Biomass_burning", "FD_total"),
    # Estimates (net emissions from logging in Natural Forest, emissions from
    # biomass burning in Softwood Plantations and total)
    est = c(
      FRLFelling$fd_lg_aane, # Net emissions from logging (FD)
      FRLBurning$fd_bb_aae, # Emissions from biomass burning (FD)
      FRLFelling$fd_lg_aane + FRLBurning$fd_bb_aae # Both
    ),
    # Lower 90%-confidence limits
    lci = c(
      quantile(FRLFelling$v_fd_lg_aane, # MC estimates net em. logging
        probs = FRLParams$qlci
      ),
      quantile(FRLBurning$v_fd_bb_aae, # MC estimates fire
        probs = FRLParams$qlci
      ),
      quantile(FRLFelling$v_fd_lg_aane + # MC estimates net em. logging
        FRLBurning$v_fd_bb_aae, # and MC estimates fire
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
      quantile(FRLFelling$v_fd_lg_aane + # MC estimates net em. logging
        FRLBurning$v_fd_bb_aae, # and MC estimates fire
      probs = FRLParams$quci
      )
    )
  )
  # Show results: forest degaradtion
  if (debug_frl) print(rs_fd)
  result <- list()
  result$rs_fd <- rs_fd
  return(result)
}


#' @export
calcFRLContributions <- function() {
  # Contributions of the different sources and sinks .....................................
  # Contributions to gross emissions
  contributione <- round(frl_table$aa_emissions_tco2e_yr[-6] /
    frl_table$aa_emissions_tco2e_yr[6] * 100, 2)
  names(contributione) <- c("DF", "FDL", "FDF", "ECAR", "ECHS")
  contributione <- data.frame(contributione)

  # Contributions to gross removals
  contributionr <- round(frl_table$aa_removals_tco2e_yr[-6] /
    frl_table$aa_removals_tco2e_yr[6] * 100, 2)
  names(contributionr) <- c("DF", "FDL", "FDF", "ECAR", "ECHS")
  contributionr <- data.frame(contributionr)

  # Contributions to net emissions
  contributionn <- round(abs(frl_table$aa_net_emissions_tco2e_yr)[-6] /
    sum(abs(frl_table$aa_net_emissions_tco2e_yr)[-6]) * 100, 2)
  names(contributionn) <- c("DF", "FDL", "FDF", "ECAR", "ECHS")
  contributionn <- data.frame(contributionn)

  # Summary table of contributions (excluding fuelwood)
  contributions <- data.frame(
    sourceSink = c("DF", "FDL", "FDF", "ECAR", "ECHS"),
    gross_emissions = contributione[, 1],
    gross_removals = contributionr[, 1],
    net_emissions = contributionn[, 1]
  )

  contributionsexfuel <- contributions

  # Contributions of the different sources and sinks .....................................
  # Contributions to gross emissions
  contributione <- round(frl_table$aa_emissions_tco2e_yr[-7] /
    frl_table$aa_emissions_tco2e_yr[7] * 100, 2)
  names(contributione) <- c("DF", "FDL", "FDF", "FUEL", "ECAR", "ECHS")
  contributione <- data.frame(contributione)

  # Contributions to gross removals
  contributionr <- round(frl_table$aa_removals_tco2e_yr[-7] /
    frl_table$aa_removals_tco2e_yr[7] * 100, 2)
  names(contributionr) <- c("DF", "FDL", "FDF", "FUEL", "ECAR", "ECHS")
  contributionr <- data.frame(contributionr)

  # Contributions to net emissions
  contributionn <- round(abs(frl_table$aa_net_emissions_tco2e_yr)[-7] /
    sum(abs(frl_table$aa_net_emissions_tco2e_yr)[-7]) * 100, 2)
  names(contributionn) <- c("DF", "FDL", "FDF", "FUEL", "ECAR", "ECHS")
  contributionn <- data.frame(contributionn)

  # Summary of contributions
  contributions <- data.frame(
    sourceSink = c("DF", "FDL", "FDF", "FUEL", "ECAR", "ECHS"),
    gross_emissions = contributione[, 1],
    gross_removals = contributionr[, 1],
    net_emissions = contributionn[, 1]
  )

  # Contributions (including emissions from fuelwood consumption)
  contributionsfuel <- contributions

  result <- list()
  result$exFuel <- contributionsexfuel
  result$all <- contributionsfuel

  return(result)

}

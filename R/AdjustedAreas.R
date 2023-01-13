

formatDecimal <- function(x) {
  return(format(round(x, 4), nsmall = 4))
}


#' @export
aaboot <- function( # A data.frame. The original accuracy assessment sample. First
                   # column gives the predicted class, second column gives the observed
                   # ('true') class. Must contain levels for all possible
                   # classes even if not present
                   aa_sample = NULL,
                   # A data.frame. First column gives the class codes, second column
                   # gives the total area mapped of the class.
                   areas_mapped = NULL,
                   # Number of bootstrap runs (i.e., iterations)
                   iterations = 1000) {
  # Check if all necessary data are provided. If not, stop!
  if (is.null(aa_sample)) {
    stop("Accuracy assessment sample is missing.")
  }
  if (is.null(areas_mapped)) {
    stop("Total areas missing (provide 'areas_mapped').")
  }

  # AA sample size in strata (strata = change class)
  n1 <- table(aa_sample[,1])
  # Sort AA sample by change class (mapped class = predicted)
  names(aa_sample) <- c("predicted", "observed")
  aa_sample <- with(aa_sample, aa_sample[order(predicted), ])

  # Total area mapped and area weight of class i
  names(areas_mapped) <- c("lcc_code", "area_mapped")
  areas_mapped <- with(areas_mapped, areas_mapped[order(lcc_code), ])
  total_area_mapped <- sum(areas_mapped[, 2])
  weight_class_i <- areas_mapped[, 2] / total_area_mapped


  # Create data.frame that collects the results of the bootstrap runs
  Ais1 <- rep(0, length(levels(aa_sample[,2])))
  names(Ais1) <- paste0("class_", sort(levels(aa_sample[, 2])))

  # Vector to select rows from the AA sample (see 'n1' above)
  ns <- c(rbind(c(1, cumsum(n1) + 1)[-(length(n1) + 1)], cumsum(n1)))

  # Start bootstrap...
  for (i in 1:iterations) {
    for (k in 1:length(n1)) {
      # Simple random sample with replacement from stratum k
      if (k == 1) {
        rsi <- aa_sample[sample(ns[k]:ns[k + 1], n1[k], replace = TRUE), ]
      } else {
        # Simple random sample with replacement from stratum k + 1
        rsi <- rbind(
          rsi,
          aa_sample[sample(ns[k + k - 1]:ns[k + k], n1[k],
            replace = TRUE
          ), ]
        )
      }
    }

    # Compute error matrix
    emi <- table(rsi[, 1], rsi[, 2])

    # Compute error matrix with estimated area proportions
    empi <- rep(weight_class_i, length.out = length(levels(aa_sample$predicted)) * length(levels(aa_sample$observed))) *
      (emi / rowSums(emi))

    # Estimate bias-adjusted areas for run i
    Aisi <- total_area_mapped * colSums(empi, na.rm = TRUE)

    # Bind results together
    Ais1 <- rbind(Ais1, Aisi)
  }

  Ais1 <- Ais1[-1, ] # Remove first dummy row
  aab <- data.frame(Ais1) # Rename to aab
  row.names(aab) <- 1:nrow(aab) # Change row names (starting at 1)
  names(aab) <- sort(levels(aa_sample[, 2]))
  return(aab) # Return data frame
}

#' @export
CalcAdjustedAreas <- function(lcc_mapped_areas, aa_sample, aa_change_period, progress = function(...) {}) {
  lcc_mapped_areas <- lcc_mapped_areas[order(lcc_mapped_areas$class_code), ]
  aa_sample <- aa_sample[order(aa_sample$predicted), ]

  # Structure of 'lcc_mapped_areas'
  if (debug_er) print(str(lcc_mapped_areas))

  # Print object 'lcc_mapped_areas'
  if (debug_er) print(lcc_mapped_areas)

  # Structure of 'aa_sample'
  if (debug_er) print(str(aa_sample))

  # Head of 'aa_sample'
  if (debug_er) print(head(aa_sample))

  # Number of sample points in the mapped classes
  if (debug_er) print(table(aa_sample$predicted))


  # Get the total area mapped [ha]
  A_mapped <- sum(lcc_mapped_areas[, 2])

  # Extract the area mapped of class i
  A_mapped_i <- lcc_mapped_areas[, 2] # Extract area of class i
  names(A_mapped_i) <- lcc_mapped_areas[, 1] # Assign the class code

  # Compute the area proportion (mapped) of class i
  round(W_i <- A_mapped_i / A_mapped, 5)


  reference_codes <- c("111","112","171","172","711","712","777")
  mapped_class <- c("111","112","171","172","555", "711","712","777", "1115", "1125", "7775")
  aa_sample$predicted <- factor(aa_sample$predicted )
  aa_sample$observed <- factor(aa_sample$observed,levels = reference_codes )


  # Compute the sample error matrix (counts); map class in rows, reference class in columns
  err <- with(aa_sample, table(predicted, observed))


  # Compute the sample error matrix (area proportions); map class in rows, reference class in columns
  errp <- rep(W_i, length.out = length(levels(aa_sample$predicted)) * length(levels(aa_sample$observed))) * (err / rowSums(err))


  if (debug_er) {
    print(err)
    print(round(errp, 5))
  }

  # Estimate class areas [ha]
  aa_est_areas <- A_mapped * colSums(errp)

  runs <- 1000
  if (exists("MCRuns")) {
    runs <- MCRuns
  }
  # Take bootstrap sample and estimate areas
  aa_boot <- aaboot(
    aa_sample = aa_sample, # AA sample
    areas_mapped = lcc_mapped_areas, # Mapped areas of change
    iterations = runs # Number of iterations
  )

  # Output of the function 'aaboot' with iterations
  if (debug_er) {
    cbind(
      do.call(rbind, lapply(aa_boot, quantile, c(0.5, QLCI, QUCI))),
      do.call(rbind, lapply(aa_boot, mean))
    )
  }

  # Results of the accuracy assessment
  rs_AA <- data.frame(
    # Change class code
    class_code = lcc_mapped_areas[match(reference_codes, lcc_mapped_areas$class_code), 1],
    # Class description
    class_desc = c(
      "Stable LF", # LF = Lowland Natural Forest
      "Stable UF", # UF = Upland Natural Forest
      "DF Lowland", # DF = deforestation
      "DF Upland",
      "AR Lowland", # AR = afforestation/reforestation
      "AR Upland",
      "Stable NF" # NF = Non-Forest
    ),

    # Mapped areas of change classes [ha]
    area_mapped_ha = lcc_mapped_areas[match(reference_codes, lcc_mapped_areas$class_code), 2],
    # Estimated areas of change classes [ha]
    area_est_ha = aa_est_areas,
    # Mean of aa boot
    aaboot_mean = do.call(rbind, lapply(aa_boot[,reference_codes ], mean)),
    # Lower limit of the 90%-confidence interval
    lci_area_ha = apply(aa_boot[,reference_codes ], 2, function(x) quantile(x, probs = QLCI)),
    # Upper limit of the 90%-confidence interval
    uci_area_ha = apply(aa_boot[,reference_codes ], 2, function(x) quantile(x, probs = QUCI))
  )
  # Rename rows
  row.names(rs_AA) <- 1:nrow(rs_AA)
  if (debug_er) print(rs_AA) # Print results

  # Extract change classes (remove stable classes)
  rs_AA_annual <- rs_AA[3:7, ]
  # Compute annual average of change classes
  rs_AA_annual[, 3:7] <- rs_AA_annual[, 3:7] / aa_change_period
  # Rename rows
  row.names(rs_AA_annual) <- 1:nrow(rs_AA_annual)
  if (debug_er) print(rs_AA_annual) # Print

  # Result table for deforestation (AD)
  rs_AA_annual_df <- rs_AA_annual[1:2, c(2, 4:7)]
  # Create row for the total (sum of Low- and Upland Natural Forest)
  rs_AA_annual_df_total <- c(
    NA,
    sum(rs_AA_annual_df[, 2]),
    sum(rs_AA_annual_df[, 3]),
    quantile(aa_boot[, 3] + aa_boot[, 4], probs = QLCI) / aa_change_period,
    quantile(aa_boot[, 3] + aa_boot[, 4], probs = QUCI) / aa_change_period
  )
  # Merge results
  rs_AA_annual_df <- rbind(rs_AA_annual_df, rs_AA_annual_df_total)
  rs_AA_annual_df[, 1] <- as.character(rs_AA_annual_df[, 1])
  rs_AA_annual_df[3, 1] <- "Total"
  if (debug_er) print(rs_AA_annual_df) # Print



  areaLoss <- rs_AA_annual_df[1:2, 2]

  # Average annual area of deforestation in Low- and Upland Natural Forest
  (resAADefor <- data.frame(
    stratum = c("Lowland", "Upland"),
    areaLoss = areaLoss
  ))

  # AA results for afforestation/reforestation (AR); see Chapter on 'deforestation' (AD)
  rs_AA_annual_ar <- rs_AA_annual[3:4, c(2, 4:7)]
  # Create row for the total (sum of Low- and Upland Natural Forest)
  rs_AA_annual_ar_total <- c(
    NA,
    sum(rs_AA_annual_ar[, 2]),
    sum(rs_AA_annual_ar[, 3]),
    quantile(aa_boot[, 5] + aa_boot[, 6], probs = QLCI) / aa_change_period,
    quantile(aa_boot[, 5] + aa_boot[, 6], probs = QUCI) / aa_change_period
  )
  # Merge results
  rs_AA_annual_ar <- rbind(rs_AA_annual_ar, rs_AA_annual_ar_total)
  rs_AA_annual_ar[, 1] <- as.character(rs_AA_annual_ar[, 1])
  rs_AA_annual_ar[3, 1] <- "Total"


  if (debug_er) print(rs_AA_annual_ar) # Print

  # Estimated average annual area of afforestation/reforestation
  ARareas <- sum(aa_est_areas[5:6]) / aa_change_period

  # Average annual area of deforestation in Low- and Upland Natural Forest (MC estimates)
  MCaadeforL <- aa_boot[, 3] / aa_change_period # Lowland
  MCaadeforU <- aa_boot[, 4] / aa_change_period # Upland
  MCaaafor <- aa_boot[, 5:6] / aa_change_period # Average annual area of AR
  MCaaaforMean <- sum(apply(aa_boot[, 5:6] / aa_change_period, 2, mean)) # ARareas via aaboot mean

  result <- list()
  result$errorMatrix <- err
  result$errorProportions <- errp
  result$rs_AA <- rs_AA
  result$rs_AA_annual_df <- rs_AA_annual_df
  result$rs_AA_annual_ar <- rs_AA_annual_ar
  result$areaLoss <- areaLoss
  result$MCaadeforL <- MCaadeforL
  result$MCaadeforU <- MCaadeforU
  result$ARareas <- ARareas
  result$MCaaaforMean <- MCaaaforMean
  result$MCaaafor <- MCaaafor

  # print("**** END OF CalcAdjustedAreas ****")
  return(result)
}

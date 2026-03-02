# Tables Creation Functions for ER Monitoring Report. 4.2, 4.3, 5.2.2, 7 and 8

formatPercent <- function(x) {
  return(ifelse(is.na(x),"--", paste(format(round(x * 100, 2), nsmall = 2), "%")))
}

formatMaxPercent <- function(x) {
  return(ifelse(x>1,">100%",formatPercent(x)))
}

formatNumber <- function(x) {
  return(format(round(x, 0), big.mark = ",", digits= 4, nsmall = 0))
}

formatDecimal <- function(x) {
  return(format(round(x, 4), nsmall = 4))
}


#' @export
createTable_4_1_ReferenceLevel <- function(MR) {
  Table_4_1_ReferenceLevel <- data.frame(
    Year = c(MR$year1$year, MR$year2$year, "Total"),
    Deforestation = c( 
      formatNumber(MR$year1$EstFRLDefor),
      formatNumber(MR$year2$EstFRLDefor),
      formatNumber(MR$MpEstFRLDefor)
    ),
    Degradation = c( 
      formatNumber(MR$year1$EstFRLDegradation),
      formatNumber(MR$year2$EstFRLDegradation),
      formatNumber(MR$MpEstFRLDegradation)
    ),
    RemovalsAfor = c( 
      formatNumber(MR$year1$EstFRLARefor),
      formatNumber(MR$year2$EstFRLARefor),
      formatNumber(MR$MpEstFRLARefor)
    ),
    RemovalsNonAfor = c( 
      formatNumber(MR$year1$EstFRLFPln),
      formatNumber(MR$year2$EstFRLFPln),
      formatNumber(MR$MpEstFRLFPln)
    ), 
    Removals = c( 
      formatNumber(MR$year1$EstFRLEnh),
      formatNumber(MR$year2$EstFRLEnh),
      formatNumber(MR$MpEstFRLEnh)
    ),
    Adjustments = c( 
      "0",
      "0",
      "0"
    ),
    NetEmissionRemovals = c( #new Nov 2025 MGG TODO
      formatNumber(MR$year1$EstFRL),
      formatNumber(MR$year2$EstFRL),
      formatNumber(MR$MpEstFRL)
    )
  )
  names(Table_4_1_ReferenceLevel) <- c(
    "Year of Monitoring/Reporting Period t",
    "Emissions from deforestation (tCO2-e/yr)",
    "Emissions from forest degradation (tCO2-e/yr)",
    "Enhanced Removals A/R (tCO2-e/yr)", #new Nov 2025
    "Enhanced Removals other activities (tCO2-e/yr)", #new Nov 2025
    "Removals by sinks (tCO2-e/yr)",
    "Adjustment, if applicable (tCO2-e/yr)", #new Nov 2025
    "Reference Level (tCO2-e/yr)"
  )
  
  return(Table_4_1_ReferenceLevel)
}






#' @export
createTable_4_2 <- function(MR) {
  Table4_2 <- data.frame(
    Year = c(MR$year1$year, MR$year2$year, "Total"),
    Deforestation = c(
      formatNumber(MR$year1$GrossEmDefor),
      formatNumber(MR$year2$GrossEmDefor),
      formatNumber(MR$MpGrossEmDefor)
    ),
    Degradation = c(
      formatNumber(MR$year1$EstEmRemsDegradation),
      formatNumber(MR$year2$EstEmRemsDegradation),
      formatNumber(MR$MpEstEmRemsDegradation)
    ),
    RemovalsAfor = c(
      formatNumber(MR$year1$EstRemARefor),
      formatNumber(MR$year2$EstRemARefor),
      formatNumber(MR$MpEstEmRemsARefor)
    ),
    RemovalsNonAfor = c( 
      formatNumber(MR$year1$NetEmRemsFPln),
      formatNumber(MR$year2$NetEmRemsFPln),
      formatNumber(MR$MpNetEmRemsFPln)
    ), 
    Removals = c(
      formatNumber(MR$year1$EstEmRemsEnh),
      formatNumber(MR$year2$EstEmRemsEnh),
      formatNumber(MR$MpEstEmRemsEnh)
    ),
    NetEmissionRemovals = c(
      formatNumber(MR$year1$NetEmRems),
      formatNumber(MR$year2$NetEmRems),
      formatNumber(MR$MpNetEmRems)
    )
  )
  names(Table4_2) <- c(
    "Year of Monitoring/Reporting Period t",
    "Emissions from deforestation (tCO2-e/yr)",
    "Emissions from forest degradation (tCO2-e/yr) ",
    "Enhanced Removals from A/R (tCO2-e/yr)", #new Nov 2025
    "Enhanced Removals from other activities besides A/R (tCO2-e/yr)", #new Nov 2025
    "Removals by sinks (tCO2-e/yr)",
    "Net Emissions and Removals (tCO2-e/yr)"
  )

  return(Table4_2)
}


#' @export
createTable_4_3 <- function(MR, MRparams) {
  if (MRparams$period$IsRpEqualToMp) {
    Table4_3 <- data.frame(
      row.names = c(
        "Total Reference Level emissions during the Reporting Period (tCO2e)",
        "Net emissions and removals under the ER Program during the Reporting Period (tCO2e)",
        "Emission Reductions during the Reporting Period (tCO2e)"
      ),
      EqualDefEnh = c(
        formatNumber(MR$RpEstFRLDefEnh),
        formatNumber(MR$RpEstEmRemsDefEnh),
        formatNumber(MR$RpEstERsDefEnh) #  Monitoring Period length == Reporting Period Length
      ),
      EqualDegradation = c(
        formatNumber(MR$RpEstFRLDegradation),
        formatNumber(MR$RpEstEmRemsDegradation),
        formatNumber(MR$RpEstERsDegradation) #  Monitoring Period length == Reporting Period Length
      )
    )
  } else {
    Table4_3 <- data.frame(
      row.names = c(
        "Total Reference Level emissions during the Monitoring Period (tCO2e)",
        "Net emissions and removals under the ER Program during the Monitoring Period (tCO2e)",
        "Emission Reductions during the Monitoring Period (tCO2e)",
        "Length of the Reporting Period / Length of the Monitoring Period (#days/# days)",
        "Emission Reductions during the Reporting Period (tCO2e)"
      ),
      NotEqualDefEnh = c(
        formatNumber(MR$MpEstFRLDefEnh),
        formatNumber(MR$MpEstEmRemsDefEnh),
        formatNumber(MR$MpEstERsDefEnh),
        paste(formatNumber(MRparams$period$RpDays),"/",formatNumber(MRparams$period$MpDays)), #  Monitoring Period length != Reporting Period Length
        formatNumber(MR$RpEstERsDefEnh)
      ),
      NotEqualDegradation = c(
        formatNumber(MR$MpEstFRLDegradation),
        formatNumber(MR$MpEstEmRemsDegradation),
        formatNumber(MR$MpEstERsDegradation),
        paste(formatNumber(MRparams$period$RpDays),"/",formatNumber(MRparams$period$MpDays)), #  Monitoring Period length != Reporting Period Length
        formatNumber(MR$RpEstERsDegradation)
      )
    )
  }
  # The * is to reference a note about the forest degradation is excluded from
  # the Total value as it is calculated by proxy methods.
  names(Table4_3) <- c(
    "Total Emissions Reductions*",
    "Forest Degradation"
  )

  return(Table4_3)
}

#' @export
createTable_4_3_t1 <- function(MR, MRparams) {
  Table4_3 <- data.frame(
    row.names = c(
      "Emission or removals in the Reference Level (tCO2-e)",
      "Emissions or removals under the ER Program during the Monitoring Period (tCO2-e)",
      "Emission Reductions during the Monitoring Period (tCO2-e)",
      "Length of the Reporting period / Length of the Monitoring Period (# days/# days)",
      "Emission Reductions during the Reporting Period (tCO2-e)"
    ),
    A = c(
      formatNumber(MR$MpEstFRLDefor),
      formatNumber(MR$MpGrossEmDefor),
      formatNumber(MR$MpEstERsDefor),
      paste(
        formatNumber(MRparams$period$RpDays),
        "/",
        formatNumber(MRparams$period$MpDays)
      ),
      formatNumber(MR$RpEstERsDefor)
    ),
    B = c(
      formatNumber(MR$MpEstFRLDegradation),
      formatNumber(MR$MpEstEmRemsDegradation),
      formatNumber(MR$MpEstERsDegradation),
      paste(
        formatNumber(MRparams$period$RpDays),
        "/",
        formatNumber(MRparams$period$MpDays)
      ),
      formatNumber(MR$RpEstERsDegradation)
    ),
    C = c(
      formatNumber(MR$MpEstFRLARefor),
      formatNumber(MR$MpEstEmRemsARefor),
      formatNumber(MR$MpEstERsARefor),
      paste(
        formatNumber(MRparams$period$RpDays),
        "/",
        formatNumber(MRparams$period$MpDays)
      ),
      formatNumber(MR$RpEstERsARefor)
    ),
    D = c(
      formatNumber(MR$MpEstFRLFPln),
      formatNumber(MR$MpNetEmRemsFPln),
      formatNumber(MR$MpEstERsFPln),
      paste(
        formatNumber(MRparams$period$RpDays),
        "/",
        formatNumber(MRparams$period$MpDays)
      ),
      formatNumber(MR$RpEstERsFPln)
    ),
    Adjustments = c( 
      "0",
      "0",
      "0",
      "0",
      "0"
    ),
    E = c(
      formatNumber(MR$MpEstFRLDefor + MR$MpEstFRLDegradation +
        MR$MpEstFRLARefor + MR$MpEstFRLFPln),
      formatNumber(MR$MpGrossEmDefor + MR$MpEstEmRemsDegradation + 
        MR$MpEstEmRemsARefor + MR$MpNetEmRemsFPln),
      formatNumber(MR$MpEstERsDefor + MR$MpEstERsDegradation +
        MR$MpEstERsARefor + MR$MpEstERsFPln),
      paste(
        formatNumber(MRparams$period$RpDays),
        "/",
        formatNumber(MRparams$period$MpDays)
      ),
      formatNumber(MR$RpEstERsDefor + MR$RpEstERsDegradation +
                     MR$RpEstERsARefor + MR$RpEstERsFPln)
    )
  )
  names(Table4_3) <- c(
    "Deforestation",    # A
    "Forest degradation",    # B
    "Enhanced removals from afforestation/ reforestation (A/R)",    # C
    "Enhanced removals from other activities besides A/R*",    # D
    "Adjustments",
    "Total (tCO2-e)" # E
  )
  return(Table4_3)
}

#' @export
createTable_4_3_t2 <- function(MR, MRparams) {
  Table4_3 <- data.frame(
    row.names = c(
      "Year",
      "Emission or removals in the Reference Level (tCO2-e)",
      "Emissions or removals under the ER Program during the Monitoring Period (tCO2-e)",
      "Emission Reductions during the Monitoring Period (tCO2-e)",
      "Length of the Reporting period / Length of the Monitoring Period (# days/# days)",
      "Emission Reductions during the Reporting Period (tCO2-e)"
    ),
    A_y1 = c(
      MR$year1$year,
      formatNumber(MR$year1$EstFRLDefor),
      formatNumber(MR$year1$GrossEmDefor),
      formatNumber(MR$year1$EstERsDefor),
      paste(
        formatNumber(MRparams$period$RpDays),
        "/",
        formatNumber(MRparams$period$MpDays)
      ),
      formatNumber(MR$year1$RpEstERsDefor)
    ),
    A_y2 = c(
      MR$year2$year,
      formatNumber(MR$year2$EstFRLDefor),
      formatNumber(MR$year2$GrossEmDefor),
      formatNumber(MR$year2$EstERsDefor),
      paste(
        formatNumber(MRparams$period$RpDays),
        "/",
        formatNumber(MRparams$period$MpDays)
      ),
      formatNumber(MR$year2$RpEstERsDefor)
    ),
    B_y1 = c(
      MR$year1$year,
      formatNumber(MR$year1$EstFRLDegradation),
      formatNumber(MR$year1$EstEmRemsDegradation),
      formatNumber(MR$year1$EstERsDegradation),
      paste(
        formatNumber(MRparams$period$RpDays),
        "/",
        formatNumber(MRparams$period$MpDays)
      ),
      formatNumber(MR$year1$RpEstERsDegradation)
    ),
    B_y2 = c(
      MR$year2$year,
      formatNumber(MR$year2$EstFRLDegradation),
      formatNumber(MR$year2$EstEmRemsDegradation),
      formatNumber(MR$year2$EstERsDegradation),
      paste(
        formatNumber(MRparams$period$RpDays),
        "/",
        formatNumber(MRparams$period$MpDays)
      ),
      formatNumber(MR$year2$RpEstERsDegradation)
    ),
    C_y1 = c(
      MR$year1$year,
      formatNumber(MR$year1$EstFRLARefor),
      formatNumber(MR$year1$EstRemARefor),
      formatNumber(MR$year1$EstERsARefor),
      paste(
        formatNumber(MRparams$period$RpDays),
        "/",
        formatNumber(MRparams$period$MpDays)
      ),
      formatNumber(MR$year1$RpEstERsARefor)
    ),
    C_y2 = c(
      MR$year2$year,
      formatNumber(MR$year2$EstFRLARefor),
      formatNumber(MR$year2$EstRemARefor),
      formatNumber(MR$year2$EstERsARefor),
      paste(
        formatNumber(MRparams$period$RpDays),
        "/",
        formatNumber(MRparams$period$MpDays)
      ),
      formatNumber(MR$year2$RpEstERsARefor)
    ),
    D_y1 = c(
      MR$year1$year,
      formatNumber(MR$year1$EstFRLFPln),
      formatNumber(MR$year1$NetEmRemsFPln),
      formatNumber(MR$year1$EstERsFPln),
      paste(
        formatNumber(MRparams$period$RpDays),
        "/",
        formatNumber(MRparams$period$MpDays)
      ),
      formatNumber(MR$year1$RpEstERsFPln)
    ),
    D_y2 = c(
      MR$year2$year,
      formatNumber(MR$year2$EstFRLFPln),
      formatNumber(MR$year2$NetEmRemsFPln),
      formatNumber(MR$year2$EstERsFPln),
      paste(
        formatNumber(MRparams$period$RpDays),
        "/",
        formatNumber(MRparams$period$MpDays)
      ),
      formatNumber(MR$year2$RpEstERsFPln)
    ),
    Adjustments_y1 = c( 
      MR$year1$year,
      "0",
      "0",
      "0",
      "0",
      "0"
    ),
    Adjustments_y2 = c( 
      MR$year2$year,
      "0",
      "0",
      "0",
      "0",
      "0"
    ),
    E_y1 = c(
      MR$year1$year,
      formatNumber(MR$year1$EstFRLDefor + MR$year1$EstFRLDegradation +
                     MR$year1$EstFRLARefor + MR$year1$EstFRLFPln),
      formatNumber(MR$year1$GrossEmDefor + MR$year1$EstEmRemsDegradation + 
                     MR$year1$EstRemARefor + MR$year1$NetEmRemsFPln),
      formatNumber(MR$year1$EstERsDefor + MR$year1$EstERsDegradation +
                     MR$year1$EstERsARefor + MR$year1$EstERsFPln),
      paste(
        formatNumber(MRparams$period$RpDays),
        "/",
        formatNumber(MRparams$period$MpDays)
      ),
      formatNumber(MR$year1$RpEstERsDefor + MR$year1$RpEstERsDegradation +
                     MR$year1$RpEstERsARefor + MR$year1$RpEstERsFPln)
    ),
    E_y2 = c(
      MR$year2$year,
      formatNumber(MR$year2$EstFRLDefor + MR$year2$EstFRLDegradation +
                     MR$year2$EstFRLARefor + MR$year2$EstFRLFPln),
      formatNumber(MR$year2$GrossEmDefor + MR$year2$EstEmRemsDegradation + 
                     MR$year2$EstRemARefor + MR$year2$NetEmRemsFPln),
      formatNumber(MR$year2$EstERsDefor + MR$year2$EstERsDegradation +
                     MR$year2$EstERsARefor + MR$year2$EstERsFPln),
      paste(
        formatNumber(MRparams$period$RpDays),
        "/",
        formatNumber(MRparams$period$MpDays)
      ),
      formatNumber(MR$year2$RpEstERsDefor + MR$year2$RpEstERsDegradation +
                     MR$year2$RpEstERsARefor + MR$year2$RpEstERsFPln)
    )
  )
  names(Table4_3) <- NULL
  return(Table4_3)
}

#' @export
createTable_4_3_t5 <- function(MR, MRparams) {
  total <- c(MR$year1$RpEstERsDefor + MR$year1$RpEstERsDegradation +
                 MR$year1$RpEstERsARefor + MR$year1$RpEstERsFPln,
             MR$year2$RpEstERsDefor + MR$year2$RpEstERsDegradation +
                            MR$year2$RpEstERsARefor + MR$year2$RpEstERsFPln)
  Table4_3 <- data.frame(
    row.names = c(
      "Total",
      "Emission Reductions from enhanced Removals from afforestation/reforestation as a percentage of the total FCPF ERs (%)",
      "Enhanced Removals from other activities besides A/R (%)",
      "Forest degradation (%)",
      "Deforestation (%)",
      "Adjustment (%)"
    ),
    total_y1 = c(
      formatNumber(total[1]),
      formatPercent(MR$year1$RpEstERsARefor / total[1]),
      formatPercent(MR$year1$RpEstERsFPln / total[1]),
      formatPercent(MR$year1$RpEstERsDegradation / total[1]),
      formatPercent(MR$year1$RpEstERsDefor / total[1]),
      "0"
    ),
    total_y2 = c(
      formatNumber(total[2]),
      formatPercent(MR$year2$RpEstERsARefor / total[2]),
      formatPercent(MR$year2$RpEstERsFPln / total[2]),
      formatPercent(MR$year2$RpEstERsDegradation / total[2]),
      formatPercent(MR$year2$RpEstERsDefor / total[2]),
      "0"
    )
  )
  names(Table4_3) <- c(MR$year1$year, MR$year2$year)
  return(Table4_3)
}

# Table in section 5.2 Quantification of the uncertainty of the estimate of Emission Reductions


#' @export
createTable_5_2_2 <- function(MR) {
  Table5_2_2 <- data.frame(
    row.names = c(
      "Median",
      "Upper Bound 90% CI",
      "Lower Bound 90% CI",
      "Half Width Confidence Interval at 90%",
      "Relative margin",
      "Uncertainty discount"
    ),
    totalEmissions = c(
      formatNumber(MR$McMpEstERsDefEnh$UCModel$median),
      formatNumber(MR$McMpEstERsDefEnh$UCModel$UCI),
      formatNumber(MR$McMpEstERsDefEnh$UCModel$LCI),
      formatNumber(MR$McMpEstERsDefEnh$UCModel$halfWidth),
      formatMaxPercent(MR$McMpEstERsDefEnh$UCModel$relativeMargin),
      formatPercent(MR$McMpEstERsDefEnh$UCModel$conserFactor)
    ),
    forestDeg = c(
      formatNumber(MR$McMpEstERsFDeg$UCModel$median),
      formatNumber(MR$McMpEstERsFDeg$UCModel$UCI),
      formatNumber(MR$McMpEstERsFDeg$UCModel$LCI),
      formatNumber(MR$McMpEstERsFDeg$UCModel$halfWidth),
      formatMaxPercent(MR$McMpEstERsFDeg$UCModel$relativeMargin),
      formatPercent(MR$McMpEstERsFDeg$UCModel$conserFactor)
    )
  )
  # The * is to reference a note about the forest degradation is excluded from
  # the Total value as it is calculated by proxy methods.
  names(Table5_2_2) <- c(
    "Total Emissions Reductions*",
    "Forest Degradation"
  )
  return(Table5_2_2)
}


#' @export
createTable_7_2 <- function(MR, MRparams) {
  Table7_2 <- data.frame(
    row.names = c(
      "A",
      "B",
      "C",
      "D",
      "E",
      "F",
      "G",
      "H",
      "I",
      "J",
      "K"
    ),
    Titles = c(
      "ER Program Refrence Level for this Reporting Period",
      "ER Program Reference Level for all previous Reporting Periods in the ERPA",
      "Cumulative Reference Level Emissions for all Reporting Periods",
      "Estimation of emissions by sources and removals by sinks for this Reporting Period",
      "Estimation of emissions by sources and removals by sinks for all previous Reporting Periods in the ERPA",
      "Cumulative emissions by sources and removals by sinks including the current reporting period",
      "Cumulative quantity of Total ERs estimated including the current reporting period",
      "Cumulative quantity of Total ERs estimated for prior reporting periods",
      "Available ERs this Reporting Period",
      "Amount of ERs that have been previously transfered to the Carbon Fund as Contract ERs and Additional ERs",
      "Quantity of Buffer ERs to be cancelled from the Reversal Buffer account"
    ),
    Values = c(
      # A
      formatNumber(MR$RpEstFRL),
      # B
      formatNumber(MRparams$ErpaPreviousFRL),
      # C
      formatNumber(MR$ErpaCurrentFRL), #   A + B
      # D
      formatNumber(MR$RpNetEmRems),
      # E
      formatNumber(MRparams$ErpaPreviousEmRems),
      # F
      formatNumber(MR$ErpaCurrentEmRems), # D + E
      # G
      formatNumber(MR$ErpaCurrentERs), # C - F
      # H
      formatNumber(MRparams$ErpaPreviousERs),
      # I
      formatNumber(MR$ErpaCurrentBalance), # G - H
      # J
      ifelse(MR$IsReversal, formatNumber(MRparams$ErpaTransferredERs), "0"),
      # k
      ifelse(MR$IsReversal, formatNumber(MR$RpCanceledERs), "0") # J / (H * (H - G))
    )
  )
}

#' @export
createTable_8 <- function(MR, MRparams) {
  Table <- data.frame(
    row.names = c(
      "A",
      "B",
      "C",
      "D",
      "E",
      "F",
      "G",
      "H",
      "I",
      "J",
      "K",
      "L"
    ),
    Titles = c(
      "Emission Reductions during the Reporting period (tCO2-e)",
      "If applicable, number of Emission Reductions from reducing forest degradation that have been estimated using proxy-based estimation approaches (use zero if not applicable)",
      "Number of Emission Reductions estimated using measurement approaches (A-B)",
      "Percentage of ERs (A) for which the ability to transfer Title to ERs is clear or uncontested",
      "ERs sold, assigned or otherwise used by any other entity for sale, public relations, compliance or any other purpose including ERs accounted separately under other GHG accounting schemes or ERs that have been set-aside to meet Reversal management requirements under other GHG accounting schemes",
      "Total ERs (B+C)*D-E",
      "Conservativeness Factor to reflect the level of uncertainty from non-proxy based approaches associated with the estimation of ERs during the Crediting Period",
      "Quantity of ERs to be allocated to the Uncertainty Buffer (0.15*B/A*F)+(G*C/A*F)",
      "Total reversal risk set-aside percentage applied to the ER program",
      "Quantity of ERs to allocated to the Reversal Buffer (F-H)*(I-5%)",
      "Quantity of ERs to be allocated to the Pooled Reversal Buffer (F-H)*5%",
      "Number of FCPF ERs  (F- H – J – K)"
    ),
    year1 = c(
      # A
      formatNumber(MR$year1$RpEstERs),
      # B
      formatNumber(MR$year1$RpEstERsFDeg),
      # C
      formatNumber(MR$year1$RpEstERsDefEnh), #  A - B
      # D
      formatPercent(MR$year1$ErpaPercentageClearERs), # Uncontested ERs = 1 - (Contested / ERs)
      # E
      formatNumber(MRparams$year1$ErpaSoldERs),
      # F
      formatNumber(MR$year1$RpAdjERs), #  (B + C) * D - E
      # G  from Table5_2 conservativeness factor.
      formatPercent(MR$McMpEstERsDefEnh$UCModel$conserFactor),
      # H
      formatNumber(MR$year1$RpSetaside), # (((0.15 * B) / A) * F) + (((G * C) / A) * F), 0.15 is default ConserFactor for FDeg
      # I
      formatPercent(MRparams$ErpaRiskSetaside),
      # J
      formatNumber(MR$year1$RpBufferedERs), # (F - H)*(I - 5%)
      # K
      formatNumber(MR$year1$RpPooledBufferedERs), # (F - H)*5%
      # L
      formatNumber(MR$year1$RpERs) # (F - H - J - K)
    ),
    year2 = c(
      # A
      formatNumber(MR$year2$RpEstERs),
      # B
      formatNumber(MR$year2$RpEstERsFDeg),
      # C
      formatNumber(MR$year2$RpEstERsDefEnh), #  A - B
      # D
      formatPercent(MR$year2$ErpaPercentageClearERs), # Uncontested ERs = 1 - (Contested / ERs)
      # E
      formatNumber(MRparams$year2$ErpaSoldERs),
      # F
      formatNumber(MR$year2$RpAdjERs), #  (B + C) * D - E
      # G  from Table5_2 conservativeness factor.
      formatPercent(MR$McMpEstERsDefEnh$UCModel$conserFactor),
      # H
      formatNumber(MR$year2$RpSetaside), # (((0.15 * B) / A) * F) + (((G * C) / A) * F), 0.15 is default ConserFactor for FDeg
      # I
      formatPercent(MRparams$ErpaRiskSetaside),
      # J
      formatNumber(MR$year2$RpBufferedERs), # (F - H)*(I - 5%)
      # K
      formatNumber(MR$year2$RpPooledBufferedERs), # (F - H)*5%
      # L
      formatNumber(MR$year2$RpERs) # (F - H - J - K)
    ),
    Totals = c(
      # A
      formatNumber(MR$RpEstERs),
      # B
      formatNumber(MR$RpEstERsFDeg),
      # C
      formatNumber(MR$RpEstERsDefEnh), #  A - B
      # D
      formatPercent(MR$ErpaPercentageClearERs), # Uncontested ERs = 1 - (Contested / ERs)
      # E
      formatNumber(MRparams$ErpaSoldERs),
      # F
      formatNumber(MR$RpAdjERs), #  (B + C) * D - E
      # G  from Table5_2 conservativeness factor.
      formatPercent(MR$McMpEstERsDefEnh$UCModel$conserFactor),
      # H
      formatNumber(MR$RpSetaside), # (((0.15 * B) / A) * F) + (((G * C) / A) * F), 0.15 is default ConserFactor for FDeg
      # I
      formatPercent(MRparams$ErpaRiskSetaside),
      # J
      formatNumber(MR$RpBufferedERs), # (F - H)*(I - 5%)
      # K
      formatNumber(MR$RpPooledBufferedERs), # (F - H)*5%
      # L
      formatNumber(MR$RpERs) # (F - H - J - K)
    )
  )
  names(Table) <- c("Titles",as.character(MRparams$period$start), as.character(MRparams$period$end), "Totals")
  return(Table)
}

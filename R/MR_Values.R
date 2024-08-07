
#' @export
create_EstMRValues <- function(UC_ER, ER, EmRems, MV, MRparams) {
  MR <- list()
  MR$year1 <- list()
  MR$year2 <- list()

  # Table4_2
  MR$year1$year <- MV$year1$year
  MR$year2$year <- MV$year2$year

  MR$year1$GrossEmDefor <- EmRems$year1$GrossEmDefor
  MR$year2$GrossEmDefor <- EmRems$year2$GrossEmDefor

  MR$MpGrossEmDefor <- ER$MpGrossEmDefor

  MR$year1$EstEmRemsFDeg <- EmRems$year1$EstEmRemsFDeg
  MR$year2$EstEmRemsFDeg <- EmRems$year2$EstEmRemsFDeg
  MR$MpEstEmRemsFDeg <- ER$MpEstEmRemsFDeg

  MR$year1$EstEmRemsEnh <- EmRems$year1$EstEmRemsEnh
  MR$year2$EstEmRemsEnh <- EmRems$year2$EstEmRemsEnh
  MR$MpEstEmRemsEnh <- ER$MpEstEmRemsEnh

  MR$year1$NetEmRems <- EmRems$year1$NetEmRems
  MR$year2$NetEmRems <- EmRems$year2$NetEmRems

  # Table4_2, Table 4_3
  MR$MpNetEmRems <- ER$MpNetEmRems
  MR$RpNetEmRems <- ER$MpNetEmRems * MRparams$RpMpRatio

  MR$MpEstFRL <- ER$MpEstFRL
  MR$RpEstFRL <- ER$MpEstFRL * MRparams$RpMpRatio

  MR$MpEstERs <- ER$MpEstERs
  MR$RpEstERs <- ER$MpEstERs * MRparams$RpMpRatio

  # Extended table 4.3 which has separated Forest Degradation from Defor & Enh
  MR$MpEstEmRemsFDeg <- ER$MpEstEmRemsFDeg
  MR$RpEstEmRemsFDeg <- ER$MpEstEmRemsFDeg * MRparams$RpMpRatio

  MR$MpEstFRLFDeg <- ER$MpEstFRLFDeg
  MR$RpEstFRLFDeg <- ER$MpEstFRLFDeg * MRparams$RpMpRatio

  MR$MpEstERsFDeg <- ER$MpEstERsFDeg
  MR$RpEstERsFDeg <- ER$MpEstERsFDeg * MRparams$RpMpRatio
  MR$McMpEstERsFDeg <- ER$McMpEstERsFDeg

  MR$MpEstEmRemsDefEnh <- ER$MpEstEmRemsDefEnh
  MR$RpEstEmRemsDefEnh <- ER$MpEstEmRemsDefEnh * MRparams$RpMpRatio

  MR$MpEstFRLDefEnh <- ER$MpEstFRLDefEnh
  MR$RpEstFRLDefEnh <- ER$MpEstFRLDefEnh * MRparams$RpMpRatio

  MR$MpEstERsDefEnh <- ER$MpEstERsDefEnh
  MR$RpEstERsDefEnh <- ER$MpEstERsDefEnh * MRparams$RpMpRatio
  MR$McMpEstERsDefEnh <- ER$McMpEstERsDefEnh
  return(MR)
}


#' @export
create_MRValues <- function(UC_ER, ER, EmRems, MV, MRparams) {
  MR <- create_EstMRValues(UC_ER, ER, EmRems, MV, MRparams)

  # Table5_2_2
  MR$McMpEstERsDefEnh <- UC_ER$McMpEstERsDefEnh
  MR$McMpEstERsFDeg <- UC_ER$McMpEstERsFDeg

  # Table7_2
  # A - MR$RpEstFRL
  # B - MRparams$ErpaPreviousFRL
  # C
  MR$ErpaCurrentFRL <- MR$RpEstFRL + MRparams$ErpaPreviousFRL
  # D - MR$RpNetEmRems
  # E - MRparams$ErpaPreviousEmRems
  # F
  MR$ErpaCurrentEmRems <- MR$RpNetEmRems + MRparams$ErpaPreviousEmRems # D + E
  # G
  MR$ErpaCurrentERs <- MR$ErpaCurrentFRL - MR$ErpaCurrentEmRems # C - F
  # H - MRparams$ErpaPreviousERs
  # I
  MR$ErpaCurrentBalance <- MR$ErpaCurrentERs - MRparams$ErpaPreviousERs # G - H
  # J -  MRparams$ErpaTransferredERs
  # k
  MR$IsReversal <- MR$ErpaCurrentBalance < 0
  MR$RpCanceledERs <- ifelse(MR$IsReversal,
    # J / (H * (H - G))
    MRparams$ErpaTransferredERs / (MRparams$ErpaPreviousERs * (MRparams$ErpaPreviousERs - MR$ErpaCurrentERs)),
    0
  )


  # Table8
  # The Reporting Period Pro-rata is applied at this level to the Monitoring Period Numbers
  # A - MR$RpEstERs
  # B - MR$RpEstERsFDeg
  # C - MR$RpEstERsDefEnh , A - B
  # D - Uncontested ERs = 1 - (Contested / ERs)
  MR$ErpaPercentageClearERs <- 1 - (MRparams$ErpaContestedERs / MR$RpEstERs)
  # E - MRparams$ErpaSoldERs
  # F - ((B + C) * D) - E
  MR$RpAdjERs <- (MR$RpEstERs * MR$ErpaPercentageClearERs) - MRparams$ErpaSoldERs
  # G - MR$McMpEstERsDefEnh$UCModel$conserFactor
  # H - (((0.15 * B) / A) * F) + (((G * C) / A) * F), 0.15 is default ConserFactor for FDeg
  MR$RpSetaside <- (((MR$RpEstERsFDeg   * MR$McMpEstERsFDeg$UCModel$conserFactor) / MR$RpEstERs) * MR$RpAdjERs) + (((MR$RpEstERsDefEnh * MR$McMpEstERsDefEnh$UCModel$conserFactor) / MR$RpEstERs) * MR$RpAdjERs)
  # I - MRparams$ErpaRiskSetaside
  # J - (F - H)*(I - 5%)
  MR$RpPotentialERs <- MR$RpAdjERs - MR$RpSetaside
  MR$RpBufferedERs <- MR$RpPotentialERs * (MRparams$ErpaRiskSetaside - 0.05)
  # K - (F - H) * 5%
  MR$RpPooledBufferedERs <- MR$RpPotentialERs * 0.05
  # L,  (F - H - J - K)
  MR$RpERs <- MR$RpPotentialERs - MR$RpBufferedERs - MR$RpPooledBufferedERs

  return(MR)
}


#' @export
create_EstMRValues <- function(UC_ER, ER, EmRems, MV, MRparams) {
  MR <- list()
  MR$year1 <- list()
  MR$year2 <- list()

  # Table4_2
  MR$year1$year <- MV$year1$year
  MR$year2$year <- MV$year2$year

  MR$year1$EstFRLDefor <- ER$year1$EstFRLDefor
  MR$year2$EstFRLDefor <- ER$year2$EstFRLDefor
  MR$year1$GrossEmDefor <- EmRems$year1$GrossEmDefor
  MR$year2$GrossEmDefor <- EmRems$year2$GrossEmDefor
  MR$year1$EstERsDefor <- ER$year1$EstERsDefor
  MR$year2$EstERsDefor <- ER$year2$EstERsDefor
  MR$year1$RpEstERsDefor <- MR$year1$EstERsDefor * MRparams$period$RpMpRatio
  MR$year2$RpEstERsDefor <- MR$year2$EstERsDefor * MRparams$period$RpMpRatio
  
  MR$MpGrossEmDefor <- ER$MpGrossEmDefor
  
  MR$year1$EstFRLARefor <- ER$year1$EstFRLARefor
  MR$year2$EstFRLARefor <- ER$year2$EstFRLARefor
  MR$year1$EstRemARefor <- EmRems$year1$EstRemARefor
  MR$year2$EstRemARefor <- EmRems$year2$EstRemARefor
  MR$year1$EstERsARefor <- ER$year1$EstERsARefor
  MR$year2$EstERsARefor <- ER$year2$EstERsARefor
  MR$year1$RpEstERsARefor <- MR$year1$EstERsARefor * MRparams$period$RpMpRatio
  MR$year2$RpEstERsARefor <- MR$year2$EstERsARefor * MRparams$period$RpMpRatio
  
  MR$MpEstEmRemsARefor <- ER$MpEstEmRemsARefor

  MR$year1$EstEmRemsFDeg <- EmRems$year1$EstEmRemsFDeg
  MR$year2$EstEmRemsFDeg <- EmRems$year2$EstEmRemsFDeg
  MR$MpEstEmRemsFDeg <- ER$MpEstEmRemsFDeg
  
  MR$year1$EstFRLDegradation <- ER$year1$EstFRLDegradation
  MR$year2$EstFRLDegradation <- ER$year2$EstFRLDegradation
  MR$year1$EstEmRemsDegradation <- EmRems$year1$EstEmRemsDegradation
  MR$year2$EstEmRemsDegradation <- EmRems$year2$EstEmRemsDegradation
  MR$year1$EstERsDegradation <- ER$year1$EstERsDegradation
  MR$year2$EstERsDegradation <- ER$year2$EstERsDegradation
  MR$year1$RpEstERsDegradation <- MR$year1$EstERsDegradation * MRparams$period$RpMpRatio
  MR$year2$RpEstERsDegradation <- MR$year2$EstERsDegradation * MRparams$period$RpMpRatio
  
  MR$MpEstEmRemsDegradation <- ER$MpEstEmRemsDegradation
  
  MR$year1$EstFRLFPln <- ER$year1$EstFRLFPln
  MR$year2$EstFRLFPln <- ER$year2$EstFRLFPln
  MR$year1$NetEmRemsFPln <- EmRems$year1$NetEmRemsFPln
  MR$year2$NetEmRemsFPln <- EmRems$year2$NetEmRemsFPln
  MR$year1$EstERsFPln <- ER$year1$EstERsFPln
  MR$year2$EstERsFPln <- ER$year2$EstERsFPln
  MR$year1$RpEstERsFPln <- MR$year1$EstERsFPln * MRparams$period$RpMpRatio
  MR$year2$RpEstERsFPln <- MR$year2$EstERsFPln * MRparams$period$RpMpRatio
  
  MR$MpNetEmRemsFPln <- ER$MpNetEmRemsFPln

  MR$year1$EstEmRemsFDegNonProxy <- EmRems$year1$EstEmRemsFDegNonProxy
  MR$year2$EstEmRemsFDegNonProxy <- EmRems$year2$EstEmRemsFDegNonProxy
  MR$MpEstEmRemsFDegNonProxy <- ER$MpEstEmRemsFDegNonProxy
  
  MR$year1$EstFRLEnh <- ER$year1$EstFRLEnh
  MR$year2$EstFRLEnh <- ER$year2$EstFRLEnh
  MR$year1$EstEmRemsEnh <- EmRems$year1$EstEmRemsEnh
  MR$year2$EstEmRemsEnh <- EmRems$year2$EstEmRemsEnh
  MR$year1$EstERsEnh <- ER$year1$EstERsEnh
  MR$year2$EstERsEnh <- ER$year2$EstERsEnh
  MR$year1$RpEstERsEnh <- MR$year1$EstERsEnh * MRparams$period$RpMpRatio
  MR$year2$RpEstERsEnh <- MR$year2$EstERsEnh * MRparams$period$RpMpRatio
  
  MR$MpEstEmRemsEnh <- ER$MpEstEmRemsEnh

  MR$year1$NetEmRems <- EmRems$year1$NetEmRems
  MR$year2$NetEmRems <- EmRems$year2$NetEmRems
  
  # Table4_2, Table 4_3
  MR$MpNetEmRems <- ER$MpNetEmRems
  MR$RpNetEmRems <- ER$MpNetEmRems * MRparams$period$RpMpRatio

  MR$year1$EstFRL <- ER$year1$EstFRL
  MR$year2$EstFRL <- ER$year2$EstFRL
  MR$MpEstFRL <- ER$MpEstFRL
  MR$RpEstFRL <- ER$MpEstFRL * MRparams$period$RpMpRatio

  MR$year1$RpEstERs <- ER$year1$EstERs * MRparams$period$RpMpRatio
  MR$year2$RpEstERs <- ER$year2$EstERs * MRparams$period$RpMpRatio
  MR$MpEstERs <- ER$MpEstERs
  MR$RpEstERs <- ER$MpEstERs * MRparams$period$RpMpRatio
  
  # Extended table 4.3 which has separated Forest Degradation from Defor & Enh
  MR$MpEstEmRemsDegradation <- ER$MpEstEmRemsDegradation
  MR$RpEstEmRemsDegradation <- ER$MpEstEmRemsDegradation * MRparams$period$RpMpRatio
  
  MR$MpEstEmRemsFDeg <- ER$MpEstEmRemsFDeg
  MR$RpEstEmRemsFDeg <- ER$MpEstEmRemsFDeg * MRparams$period$RpMpRatio

  MR$MpEstEmRemsFDegNonProxy <- ER$MpEstEmRemsFDegNonProxy
  MR$RpEstEmRemsFDegNonProxy <- ER$MpEstEmRemsFDegNonProxy * MRparams$period$RpMpRatio
  
  MR$MpEstEmRemsARefor <- ER$MpEstRemARefor #no Em
  MR$RpEstEmRemsARefor <- ER$MpEstRemARefor * MRparams$period$RpMpRatio
  
  MR$MpEstFRLDefor <- ER$MpEstFRLDefor
  MR$RpEstFRLDefor <- ER$MpEstFRLDefor * MRparams$period$RpMpRatio
  
  MR$MpEstERsDefor <- ER$MpEstERsDefor
  MR$RpEstERsDefor <- ER$MpEstERsDefor * MRparams$period$RpMpRatio
  
  MR$MpEstFRLDegradation <- ER$MpEstFRLDegradation
  MR$RpEstFRLDegradation <- ER$MpEstFRLDegradation * MRparams$period$RpMpRatio
  
  MR$MpEstERsDegradation <- ER$MpEstERsDegradation  
  MR$RpEstERsDegradation <- ER$MpEstERsDegradation * MRparams$period$RpMpRatio
  
  MR$MpEstFRLARefor <- ER$MpEstFRLARefor
  MR$RpEstFRLARefor <- ER$MpEstFRLARefor * MRparams$period$RpMpRatio
  
  MR$MpEstERsARefor <- ER$MpEstERsARefor
  MR$RpEstERsARefor <- ER$MpEstERsARefor * MRparams$period$RpMpRatio
  
  MR$MpEstFRLFPln <- ER$MpEstFRLFPln
  MR$RpEstFRLFPln <- ER$MpEstFRLFPln * MRparams$period$RpMpRatio
  
  MR$MpEstERsFPln <- ER$MpEstERsFPln
  MR$RpEstERsFPln <- ER$MpEstERsFPln * MRparams$period$RpMpRatio
  
  MR$MpEstFRLFDeg <- ER$MpEstFRLFDeg
  MR$RpEstFRLFDeg <- ER$MpEstFRLFDeg * MRparams$period$RpMpRatio
  
  MR$year1$RpEstERsFDeg <- ER$year1$EstERsFDeg * MRparams$period$RpMpRatio
  MR$year2$RpEstERsFDeg <- ER$year2$EstERsFDeg * MRparams$period$RpMpRatio
  MR$MpEstERsFDeg <- ER$MpEstERsFDeg
  MR$RpEstERsFDeg <- ER$MpEstERsFDeg * MRparams$period$RpMpRatio
  MR$McMpEstERsFDeg <- ER$McMpEstERsFDeg
  
  
  MR$MpEstFRLFDegNonProxy <- ER$MpEstFRLFDegNonProxy
  MR$RpEstFRLFDegNonProxy <- ER$MpEstFRLFDegNonProxy * MRparams$period$RpMpRatio

  MR$MpEstERsFDegNonProxy <- ER$MpEstERsFDegNonProxy
  MR$RpEstERsFDegNonProxy <- ER$MpEstERsFDegNonProxy * MRparams$period$RpMpRatio
  MR$McMpEstERsFDegNonProxy <- ER$McMpEstERsFDegNonProxy
  
  MR$MpEstFRLEnh <- ER$MpEstFRLEnh
  MR$RpEstFRLEnh <- ER$MpEstFRLEnh * MRparams$period$RpMpRatio
  
  MR$MpEstEmRemsDefEnh <- ER$MpEstEmRemsDefEnh
  MR$RpEstEmRemsDefEnh <- ER$MpEstEmRemsDefEnh * MRparams$period$RpMpRatio

  MR$MpEstFRLDefEnh <- ER$MpEstFRLDefEnh
  MR$RpEstFRLDefEnh <- ER$MpEstFRLDefEnh * MRparams$period$RpMpRatio

  MR$year1$RpEstERsDefEnh <- ER$year1$EstERsDefEnh * MRparams$period$RpMpRatio
  MR$year2$RpEstERsDefEnh <- ER$year2$EstERsDefEnh * MRparams$period$RpMpRatio
  MR$MpEstERsDefEnh <- ER$MpEstERsDefEnh
  MR$RpEstERsDefEnh <- ER$MpEstERsDefEnh * MRparams$period$RpMpRatio
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
  MR$year1$ErpaPercentageClearERs <- 1 - (MRparams$year1$ErpaContestedERs / MR$year1$RpEstERs)
  MR$year2$ErpaPercentageClearERs <- 1 - (MRparams$year2$ErpaContestedERs / MR$year2$RpEstERs)
  MR$ErpaPercentageClearERs <- 1 - (MRparams$ErpaContestedERs / MR$RpEstERs)
  # E - MRparams$ErpaSoldERs
  # F - ((B + C) * D) - E
  MR$year1$RpAdjERs <- (MR$year1$RpEstERs * MR$year1$ErpaPercentageClearERs) - MRparams$year1$ErpaSoldERs
  MR$year2$RpAdjERs <- (MR$year2$RpEstERs * MR$year2$ErpaPercentageClearERs) - MRparams$year2$ErpaSoldERs
  MR$RpAdjERs <- (MR$RpEstERs * MR$ErpaPercentageClearERs) - MRparams$ErpaSoldERs
  # G - MR$McMpEstERsDefEnh$UCModel$conserFactor
  # H - (((0.15 * B) / A) * F) + (((G * C) / A) * F), 0.15 is default ConserFactor for FDeg
  MR$year1$RpSetaside <- (((MR$year1$RpEstERsFDeg   * MR$McMpEstERsFDeg$UCModel$conserFactor) / MR$year1$RpEstERs) * MR$year1$RpAdjERs) + (((MR$year1$RpEstERsDefEnh * MR$McMpEstERsDefEnh$UCModel$conserFactor) / MR$year1$RpEstERs) * MR$year1$RpAdjERs)
  MR$year2$RpSetaside <- (((MR$year2$RpEstERsFDeg   * MR$McMpEstERsFDeg$UCModel$conserFactor) / MR$year2$RpEstERs) * MR$year2$RpAdjERs) + (((MR$year2$RpEstERsDefEnh * MR$McMpEstERsDefEnh$UCModel$conserFactor) / MR$year2$RpEstERs) * MR$year2$RpAdjERs)
  MR$RpSetaside <- (((MR$RpEstERsFDeg   * MR$McMpEstERsFDeg$UCModel$conserFactor) / MR$RpEstERs) * MR$RpAdjERs) + (((MR$RpEstERsDefEnh * MR$McMpEstERsDefEnh$UCModel$conserFactor) / MR$RpEstERs) * MR$RpAdjERs)
  # I - MRparams$ErpaRiskSetaside
  # J - (F - H)*(I - 5%)
  MR$year1$RpPotentialERs <- MR$year1$RpAdjERs - MR$year1$RpSetaside
  MR$year2$RpPotentialERs <- MR$year2$RpAdjERs - MR$year2$RpSetaside
  MR$RpPotentialERs <- MR$RpAdjERs - MR$RpSetaside
  MR$year1$RpBufferedERs <- MR$year1$RpPotentialERs * (MRparams$ErpaRiskSetaside - 0.05)
  MR$year2$RpBufferedERs <- MR$year2$RpPotentialERs * (MRparams$ErpaRiskSetaside - 0.05)
  MR$RpBufferedERs <- MR$RpPotentialERs * (MRparams$ErpaRiskSetaside - 0.05)
  # K - (F - H) * 5%
  MR$year1$RpPooledBufferedERs <- MR$year1$RpPotentialERs * 0.05
  MR$year2$RpPooledBufferedERs <- MR$year2$RpPotentialERs * 0.05
  MR$RpPooledBufferedERs <- MR$RpPotentialERs * 0.05
  # L,  (F - H - J - K)
  MR$year1$RpERs <- MR$year1$RpPotentialERs - MR$year1$RpBufferedERs - MR$year1$RpPooledBufferedERs
  MR$year2$RpERs <- MR$year2$RpPotentialERs - MR$year2$RpBufferedERs - MR$year2$RpPooledBufferedERs
  MR$RpERs <- MR$RpPotentialERs - MR$RpBufferedERs - MR$RpPooledBufferedERs

  return(MR)
}

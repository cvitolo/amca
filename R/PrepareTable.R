#' Prepare a table containing Model ID numbers, model options and parameter in use.
#'
#' @param reduced boolean value. If FALSE the entire model list is taken into account. If FALSE, a reduced version is selected.
#'
#' @return table containing Model ID numbers, model options and parameter in use
#'
#' @examples
#' # PrepareTable()
#'

PrepareTable <- function(reduced=FALSE){

  modlist <- NULL
  load(system.file("data/modlist.rda", package = "fuse"))

  if (reduced == TRUE) {

    #Rainfall error is not included in the inference, therefore we remove models with q_tdh = 81.
    modlist <- modlist[which(modlist[,"rferr"] == 12),]

    #Routing is always allowed, therefore we remove models with q_tdh = 81.
    modlist <- modlist[which(modlist[,"q_tdh"] == 82),]

    #Some model combinations are physically unrealistic, therefore we delete them.
    # modlist <- modlist[-which(modlist[,"arch2"] == 33 & modlist[,"qperc"] == 52),]
    # modlist <- modlist[-which(modlist[,"arch2"] == 34 & modlist[,"qperc"] == 52),]
    modlist <- modlist[-which((modlist[,"arch2"] == 33 | modlist[,"arch2"] == 34) & modlist[,"qperc"] == 52),]

    # The effect of the evaporation scheme is negligible
    # modlist <- modlist[-which(modlist[,"esoil"] == 61),]

    # The effect of the interflow is negligible
    # modlist <- modlist[-which(modlist[,"qintf"] == 72),]

  }

  NumberOfColumns <- dim(modlist)[2]

  # The number of models to take into consideration reduces to 60 # considering evap and interflow they are 240
  # We add a column for the mid counter (mcounter)
  # modlist$mcounter <- seq(1:dim(modlist)[1])

  #Tidy up
  row.names(modlist) <- NULL
  # modlist <- modlist[,c("mcounter","mid","rferr","arch1","arch2","qsurf","qperc","esoil","qintf","q_tdh")]
  modlist <- modlist[,c("mid","rferr","arch1","arch2","qsurf","qperc","esoil","qintf","q_tdh")]

  #We add a column for each parameter
  modlist$rferr_add <- NA
  modlist$rferr_mlt <- NA
  modlist$maxwatr_1 <- NA
  modlist$maxwatr_2 <- NA
  modlist$fracten   <- NA
  modlist$frchzne   <- NA
  modlist$fprimqb   <- NA
  modlist$rtfrac1   <- NA
  modlist$percrte   <- NA
  modlist$percexp   <- NA
  modlist$sacpmlt   <- NA
  modlist$sacpexp   <- NA
  modlist$percfrac  <- NA
  modlist$iflwrte   <- NA
  modlist$baserte   <- NA
  modlist$qb_powr   <- NA
  modlist$qb_prms   <- NA
  modlist$qbrate_2a <- NA
  modlist$qbrate_2b <- NA
  modlist$sareamax  <- NA
  modlist$axv_bexp  <- NA
  modlist$loglamb   <- NA
  modlist$tishape   <- NA
  modlist$timedelay <- NA

  NewNumberOfColumns <- dim(modlist)[2]

  #Create a dummy parameter set, where all the elements are 999.
  #Where the parameter is used, the cell contains 999, otherwise it contains NA.

  dummyParameterSet <- data.frame(matrix(999, ncol=NewNumberOfColumns-NumberOfColumns, nrow=1))
  names(dummyParameterSet) <- names(modlist)[(NumberOfColumns+1):NewNumberOfColumns]

  for (row in 1:dim(modlist)[1]){

    # (1) rainfall errors
    # additive_e
    if(modlist[row,"rferr"] == 11) modlist[row,"rferr_add"] <- dummyParameterSet$rferr_add
    # multiplc_e
    if(modlist[row,"rferr"] == 12) modlist[row,"rferr_mlt"] <- dummyParameterSet$rferr_mlt

    # (2) upper-layer architecture
    # onestate_1, tension1_1 (need to define tension and free storage -- even if one state)
    if(modlist[row,"arch1"] == 21 || modlist[row,"arch1"] == 22) {
      modlist[row,"fracten"]   <- dummyParameterSet$fracten   # frac total storage as tension storage (-)
      modlist[row,"maxwatr_1"] <- dummyParameterSet$maxwatr_1 # maximum total storage in layer1 (mm)
    }

    # tension2_1 (tension storage sub-divided into recharge and excess)
    if(modlist[row,"arch1"] == 23) {
      modlist[row,"frchzne"]   <- dummyParameterSet$frchzne   # PRMS: frac tension storage in recharge zone (-)
      modlist[row,"fracten"]   <- dummyParameterSet$fracten   # frac total storage as tension storage (-)
      modlist[row,"maxwatr_1"] <- dummyParameterSet$maxwatr_1 # maximum total storage in layer1 (mm)
      #fraclowz  <- dummyParameterSet$fraclowz # fraction of soil excess to lower zone (-) # NOT USED
    }

    # (3) lower-layer architecture / baseflow
    # fixedsiz_2 (power-law relation (no parameters needed for the topo index distribution))
    if(modlist[row,"arch2"] == 31) {
      modlist[row,"maxwatr_2"] <- dummyParameterSet$maxwatr_2
      modlist[row,"baserte"]   <- dummyParameterSet$baserte
      modlist[row,"qb_powr"]   <- dummyParameterSet$qb_powr
    }

    # tens2pll_2 (tension reservoir plus two parallel tanks)
    if(modlist[row,"arch2"] == 32) {
      modlist[row,"percfrac"]  <- dummyParameterSet$percfrac   # fraction of percolation to tension storage (-)
      modlist[row,"fprimqb"]   <- dummyParameterSet$fprimqb    # SAC: fraction of baseflow in primary resvr (-)
      modlist[row,"maxwatr_2"] <- dummyParameterSet$maxwatr_2  # maximum total storage in layer2 (mm)
      modlist[row,"qbrate_2a"] <- dummyParameterSet$qbrate_2a  # baseflow depletion rate for primary resvr (day-1)
      modlist[row,"qbrate_2b"] <- dummyParameterSet$qbrate_2b  # baseflow depletion rate for secondary resvr (day-1)
    }

    # unlimfrc_2 (baseflow resvr of unlimited size (0-huge), frac rate)
    if(modlist[row,"arch2"] == 33) {
      modlist[row,"maxwatr_2"] <- dummyParameterSet$maxwatr_2   # maximum total storage in layer2 (mm)
      modlist[row,"qb_prms"]   <- dummyParameterSet$qb_prms     # baseflow depletion rate (day-1)
    }

    # unlimpow_2 (topmodel option = power-law transmissivity profile)
    if(modlist[row,"arch2"] == 34) {
      modlist[row,"maxwatr_2"] <- dummyParameterSet$maxwatr_2   # maximum total storage in layer2 (mm)
      modlist[row,"baserte"]   <- dummyParameterSet$baserte     # baseflow rate (mm day-1)
      # the params below are always used to calculate qbsat!
      # modlist[row,"loglamb"]   <- dummyParameterSet$loglamb     # mean value of the log-transformed topographic index (m)
      # modlist[row,"tishape"]   <- dummyParameterSet$tishape     # shape parameter for the topo index Gamma distribution (-)
      # modlist[row,"qb_powr"]   <- dummyParameterSet$qb_powr     # baseflow exponent (-)
    }

    # the params below are always used to calculate qbsat!
    # this also mean that loglamb and tishape are always calculated!
    modlist[row,"loglamb"]   <- dummyParameterSet$loglamb     # mean value of the log-transformed topographic index (m)
    modlist[row,"tishape"]   <- dummyParameterSet$tishape     # shape parameter for the topo index Gamma distribution (-)
    modlist[row,"qb_powr"]   <- dummyParameterSet$qb_powr     # baseflow exponent (-)

    # (4) surface runoff
    # arno_x_vic = arno/xzang/vic parameterization (upper zone control)
    if(modlist[row,"qsurf"] == 41) modlist[row,"axv_bexp"] <- dummyParameterSet$axv_bexp

    # prms_varnt = prms variant (fraction of upper tension storage)
    if(modlist[row,"qsurf"] == 42) modlist[row,"sareamax"] <- dummyParameterSet$sareamax

    # tmdl_param = topmodel parameterization
    if(modlist[row,"qsurf"] == 43) {
      # need the topographic index if we don't have it for baseflow
      if(modlist[row,"arch2"] == 32 || modlist[row,"arch2"] == 33 || modlist[row,"arch2"] == 31) {
        modlist[row,"loglamb"]   <- dummyParameterSet$loglamb
        modlist[row,"tishape"]   <- dummyParameterSet$tishape
      }
      # need the topmodel power if we don't have it for baseflow # baseflow exponent (-), used to modify the topographic
      if(modlist[row,"arch2"] == 32 || modlist[row,"arch2"] == 33 || modlist[row,"arch2"] == 35) modlist[row,"qb_powr"]   <- dummyParameterSet$qb_powr
    }

    # (5) percolation
    # perc_f2sat, perc_w2sat = standard equation k(theta)**c
    if(modlist[row,"qperc"] == 51||modlist[row,"qperc"] == 53) {
      modlist[row,"percrte"]   <- dummyParameterSet$percrte             # percolation rate (mm day-1)
      modlist[row,"percexp"]   <- dummyParameterSet$percexp             # percolation exponent (-)
    }

    # perc_lower = perc defined by moisture content in lower layer (sac)
    if(modlist[row,"qperc"] == 52) {
      modlist[row,"sacpmlt"]   <- dummyParameterSet$sacpmlt             # multiplier in the SAC model for dry lower layer (-)
      modlist[row,"sacpexp"]   <- dummyParameterSet$sacpexp             # exponent in the SAC model for dry lower layer (-)
    }

    # (6) evaporation
    # fraction of roots in the upper layer (-)
    if(modlist[row,"esoil"] == 61) modlist[row,"rtfrac1"] <- dummyParameterSet$rtfrac1

    # (7) interflow
    # interflow rate (mm day-1)
    if(modlist[row,"qintf"] == 72) modlist[row,"iflwrte"] <- dummyParameterSet$iflwrte

    # (8) routing
    # timedelay (day)
    if(modlist[row,"q_tdh"] == 82) modlist[row,"timedelay"] <- dummyParameterSet$timedelay
  }

  #length(na.omit(v))
  row.names(modlist) <- NULL

  return(modlist)

}

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

  load(system.file("data/modlist.rda", package = "fuse"))
  modlist <- modlist
  damachModelList <- modlist

  if (reduced == TRUE) {

    #Rainfall error is not included in the inference, therefore remove all the models with even MID.
    damachModelList <- damachModelList[which(damachModelList[,"mid"] %% 2 != 0),]

    #Routing is always allowed, therefore we remove models with q_tdh = 81.
    damachModelList <- damachModelList[which(damachModelList[,"q_tdh"] == 82),]

    #Some model combinations are physically unrealistic, therefore we delete them.
    # damachModelList <- damachModelList[-which(damachModelList[,"arch2"] == 33 & damachModelList[,"qperc"] == 52),]
    # damachModelList <- damachModelList[-which(damachModelList[,"arch2"] == 34 & damachModelList[,"qperc"] == 52),]
    damachModelList <- damachModelList[-which((damachModelList[,"arch2"] == 33 | damachModelList[,"arch2"] == 34) & damachModelList[,"qperc"] == 52),]

    # The effect of the evaporation scheme is negligible
    # damachModelList <- damachModelList[-which(damachModelList[,"esoil"] == 61),]

    # The effect of the interflow is negligible
    # damachModelList <- damachModelList[-which(damachModelList[,"qintf"] == 72),]

  }

  NumberOfColumns <- dim(damachModelList)[2]

  # The number of models to take into consideration reduces to 60 # considering evap and interflow they are 240
  # We add a column for the mid counter (mcounter)
  # damachModelList$mcounter <- seq(1:dim(damachModelList)[1])

  #Tidy up
  row.names(damachModelList) <- NULL
  # damachModelList <- damachModelList[,c("mcounter","mid","rferr","arch1","arch2","qsurf","qperc","esoil","qintf","q_tdh")]
  damachModelList <- damachModelList[,c("mid","rferr","arch1","arch2","qsurf","qperc","esoil","qintf","q_tdh")]

  #We add a column for each parameter
  damachModelList$rferr_add <- NA
  damachModelList$rferr_mlt <- NA
  damachModelList$maxwatr_1 <- NA
  damachModelList$maxwatr_2 <- NA
  damachModelList$fracten   <- NA
  damachModelList$frchzne   <- NA
  damachModelList$fprimqb   <- NA
  damachModelList$rtfrac1   <- NA
  damachModelList$percrte   <- NA
  damachModelList$percexp   <- NA
  damachModelList$sacpmlt   <- NA
  damachModelList$sacpexp   <- NA
  damachModelList$percfrac  <- NA
  damachModelList$iflwrte   <- NA
  damachModelList$baserte   <- NA
  damachModelList$qb_powr   <- NA
  damachModelList$qb_prms   <- NA
  damachModelList$qbrate_2a <- NA
  damachModelList$qbrate_2b <- NA
  damachModelList$sareamax  <- NA
  damachModelList$axv_bexp  <- NA
  damachModelList$loglamb   <- NA
  damachModelList$tishape   <- NA
  damachModelList$timedelay <- NA

  NewNumberOfColumns <- dim(damachModelList)[2]

  #Create a dummy parameter set, where all the elements are 999.
  #Where the parameter is used, the cell contains 999, otherwise it contains NA.

  dummyParameterSet <- data.frame(matrix(999, ncol=NewNumberOfColumns-NumberOfColumns, nrow=1))
  names(dummyParameterSet) <- names(damachModelList)[(NumberOfColumns+1):NewNumberOfColumns]

  for (row in 1:dim(damachModelList)[1]){

    # (1) rainfall errors
    # additive_e
    if(damachModelList[row,"rferr"] == 11) damachModelList[row,"rferr_add"] <- dummyParameterSet$rferr_add
    # multiplc_e
    if(damachModelList[row,"rferr"] == 12) damachModelList[row,"rferr_mlt"] <- dummyParameterSet$rferr_mlt

    # (2) upper-layer architecture
    # onestate_1, tension1_1 (need to define tension and free storage -- even if one state)
    if(damachModelList[row,"arch1"] == 21 || damachModelList[row,"arch1"] == 22) {
      damachModelList[row,"fracten"]   <- dummyParameterSet$fracten   # frac total storage as tension storage (-)
      damachModelList[row,"maxwatr_1"] <- dummyParameterSet$maxwatr_1 # maximum total storage in layer1 (mm)
    }

    # tension2_1 (tension storage sub-divided into recharge and excess)
    if(damachModelList[row,"arch1"] == 23) {
      damachModelList[row,"frchzne"]   <- dummyParameterSet$frchzne   # PRMS: frac tension storage in recharge zone (-)
      damachModelList[row,"fracten"]   <- dummyParameterSet$fracten   # frac total storage as tension storage (-)
      damachModelList[row,"maxwatr_1"] <- dummyParameterSet$maxwatr_1 # maximum total storage in layer1 (mm)
      #fraclowz  <- dummyParameterSet$fraclowz # fraction of soil excess to lower zone (-) # NOT USED
    }

    # (3) lower-layer architecture / baseflow
    # fixedsiz_2 (power-law relation (no parameters needed for the topo index distribution))
    if(damachModelList[row,"arch2"] == 31) {
      damachModelList[row,"maxwatr_2"] <- dummyParameterSet$maxwatr_2
      damachModelList[row,"baserte"]   <- dummyParameterSet$baserte
      damachModelList[row,"qb_powr"]   <- dummyParameterSet$qb_powr
    }

    # tens2pll_2 (tension reservoir plus two parallel tanks)
    if(damachModelList[row,"arch2"] == 32) {
      damachModelList[row,"percfrac"]  <- dummyParameterSet$percfrac   # fraction of percolation to tension storage (-)
      damachModelList[row,"fprimqb"]   <- dummyParameterSet$fprimqb    # SAC: fraction of baseflow in primary resvr (-)
      damachModelList[row,"maxwatr_2"] <- dummyParameterSet$maxwatr_2  # maximum total storage in layer2 (mm)
      damachModelList[row,"qbrate_2a"] <- dummyParameterSet$qbrate_2a  # baseflow depletion rate for primary resvr (day-1)
      damachModelList[row,"qbrate_2b"] <- dummyParameterSet$qbrate_2b  # baseflow depletion rate for secondary resvr (day-1)
    }

    # unlimfrc_2 (baseflow resvr of unlimited size (0-huge), frac rate)
    if(damachModelList[row,"arch2"] == 33) {
      damachModelList[row,"maxwatr_2"] <- dummyParameterSet$maxwatr_2   # maximum total storage in layer2 (mm)
      damachModelList[row,"qb_prms"]   <- dummyParameterSet$qb_prms     # baseflow depletion rate (day-1)
    }

    # unlimpow_2 (topmodel option = power-law transmissivity profile)
    if(damachModelList[row,"arch2"] == 34) {
      damachModelList[row,"maxwatr_2"] <- dummyParameterSet$maxwatr_2   # maximum total storage in layer2 (mm)
      damachModelList[row,"baserte"]   <- dummyParameterSet$baserte     # baseflow rate (mm day-1)
      damachModelList[row,"loglamb"]   <- dummyParameterSet$loglamb     # mean value of the log-transformed topographic index (m)
      damachModelList[row,"tishape"]   <- dummyParameterSet$tishape     # shape parameter for the topo index Gamma distribution (-)
      damachModelList[row,"qb_powr"]   <- dummyParameterSet$qb_powr     # baseflow exponent (-)
    }

    # (4) surface runoff
    # arno_x_vic = arno/xzang/vic parameterization (upper zone control)
    if(damachModelList[row,"qsurf"] == 41) damachModelList[row,"axv_bexp"] <- dummyParameterSet$axv_bexp

    # prms_varnt = prms variant (fraction of upper tension storage)
    if(damachModelList[row,"qsurf"] == 42) damachModelList[row,"sareamax"] <- dummyParameterSet$sareamax

    # tmdl_param = topmodel parameterization
    if(damachModelList[row,"qsurf"] == 43) {
      # need the topographic index if we don't have it for baseflow
      if(damachModelList[row,"arch2"] == 32 || damachModelList[row,"arch2"] == 33 || damachModelList[row,"arch2"] == 31) {
        damachModelList[row,"loglamb"]   <- dummyParameterSet$loglamb
        damachModelList[row,"tishape"]   <- dummyParameterSet$tishape
      }
      # need the topmodel power if we don't have it for baseflow # baseflow exponent (-), used to modify the topographic
      if(damachModelList[row,"arch2"] == 32 || damachModelList[row,"arch2"] == 33 || damachModelList[row,"arch2"] == 35) damachModelList[row,"qb_powr"]   <- dummyParameterSet$qb_powr
    }

    # (5) percolation
    # perc_f2sat, perc_w2sat = standard equation k(theta)**c
    if(damachModelList[row,"qperc"] == 51||damachModelList[row,"qperc"] == 53) {
      damachModelList[row,"percrte"]   <- dummyParameterSet$percrte             # percolation rate (mm day-1)
      damachModelList[row,"percexp"]   <- dummyParameterSet$percexp             # percolation exponent (-)
    }

    # perc_lower = perc defined by moisture content in lower layer (sac)
    if(damachModelList[row,"qperc"] == 52) {
      damachModelList[row,"sacpmlt"]   <- dummyParameterSet$sacpmlt             # multiplier in the SAC model for dry lower layer (-)
      damachModelList[row,"sacpexp"]   <- dummyParameterSet$sacpexp             # exponent in the SAC model for dry lower layer (-)
    }

    # (6) evaporation
    # fraction of roots in the upper layer (-)
    if(damachModelList[row,"esoil"] == 61) damachModelList[row,"rtfrac1"] <- dummyParameterSet$rtfrac1

    # (7) interflow
    # interflow rate (mm day-1)
    if(damachModelList[row,"qintf"] == 72) damachModelList[row,"iflwrte"] <- dummyParameterSet$iflwrte

    # (8) routing
    # timedelay (day)
    if(damachModelList[row,"q_tdh"] == 82) damachModelList[row,"timedelay"] <- dummyParameterSet$timedelay
  }

  #length(na.omit(v))
  row.names(damachModelList) <- NULL

  return(damachModelList)

}

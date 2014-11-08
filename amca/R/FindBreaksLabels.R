#' Utility function to define breaks for plotting modelling options
#'
#' @param modelOption modelling option
#'
#' @return breaks
#'
#' @examples
#' # FindBreaksLabels(modelOption)
#'

FindBreaksLabels <- function(modelOption){

  if (modelOption == "rferr") {
    myBreaks <- c("11","12")
    myLabels <- c("additive_e","multiplc_e")
    myTitle <- "Rainfall error"
  }

  if (modelOption == "arch1") {
    myBreaks <- c("21","22","23")
    myLabels <- c("onestate_1","tension1_1","tension2_1")
    myTitle <- "Upper layer architecture"
  }

  if (modelOption == "arch2") {
    myBreaks <- c("31","32","33","34")
    myLabels <- c("fixedsiz_2","tens2pll_2","unlimfrc_2","unlimpow_2")
    myTitle <- "Lower layer architecture"
  }

  if (modelOption == "qsurf") {
    myBreaks <- c("41","42","43")
    myLabels <- c("arno_x_vic","prms_varnt","tmdl_param")
    myTitle <- "Runoff"
  }

  if (modelOption == "qperc") {
    myBreaks <- c("51","52","53")
    myLabels <- c("perc_f2sat","perc_lower","perc_w2sat")
    myTitle <- "Percolation"
  }

  if (modelOption == "esoil") {
    myBreaks <- c("61","62")
    myLabels <- c("rootweight","sequential")
    myTitle <- "Evaporation"
  }

  if (modelOption == "qintf") {
    myBreaks <- c("71","72")
    myLabels <- c("intflwnone","intflwsome")
    myTitle <- "Interflow"
  }

  if (modelOption == "q_tdh") {
    myBreaks <- c("81","82")
    myLabels <- c("no_routing","rout_gamma")
    myTitle <- "Routing"
  }

  return(list(breaks=myBreaks,
              labels=myLabels,
              title=myTitle))

}

FindBreaksLabels <- function(myOption){

  if (myOption == "rferr") {
    myBreaks <- c("11","12")
    myLabels <- c("additive_e","multiplc_e")
    myTitle <- "Rainfall error"
  }

  if (myOption == "arch1") {
    myBreaks <- c("21","22","23")
    myLabels <- c("onestate_1","tension1_1","tension2_1")
    myTitle <- "Upper layer architecture"
  }

  if (myOption == "arch2") {
    myBreaks <- c("31","32","33","34")
    myLabels <- c("fixedsiz_2","tens2pll_2","unlimfrc_2","unlimpow_2")
    myTitle <- "Lower layer architecture"
  }

  if (myOption == "qsurf") {
    myBreaks <- c("41","42","43")
    myLabels <- c("arno_x_vic","prms_varnt","tmdl_param")
    myTitle <- "Runoff"
  }

  if (myOption == "qperc") {
    myBreaks <- c("51","52","53")
    myLabels <- c("perc_f2sat","perc_lower","perc_w2sat")
    myTitle <- "Percolation"
  }

  if (myOption == "esoil") {
    myBreaks <- c("61","62")
    myLabels <- c("rootweight","sequential")
    myTitle <- "Evaporation"
  }

  if (myOption == "qintf") {
    myBreaks <- c("71","72")
    myLabels <- c("intflwnone","intflwsome")
    myTitle <- "Interflow"
  }

  if (myOption == "q_tdh") {
    myBreaks <- c("81","82")
    myLabels <- c("no_routing","rout_gamma")
    myTitle <- "Routing"
  }

  return(list(breaks=myBreaks,
              labels=myLabels,
              title=myTitle))

}

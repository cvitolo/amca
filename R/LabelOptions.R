#' Utility function to define labels for plotting modelling options
#'
#' @param numericalOption numerical option defining the compone component
#'
#' @return labels
#' 
#' @export
#'
#' @examples
#' # LabelOptions(numericalOption)
#'

LabelOptions <- function(numericalOption){

  if (numericalOption == "11") label <- "additive_e"
  if (numericalOption == "12") label <- "multiplc_e"

  if (numericalOption == "21") label <- "onestate_1"
  if (numericalOption == "22") label <- "tension1_1"
  if (numericalOption == "23") label <- "tension2_1"

  if (numericalOption == "31") label <- "fixedsiz_2"
  if (numericalOption == "32") label <- "tens2pll_2"
  if (numericalOption == "33") label <- "unlimfrc_2"
  if (numericalOption == "34") label <- "unlimpow_2"

  if (numericalOption == "41") label <- "arno_x_vic"
  if (numericalOption == "42") label <- "prms_varnt"
  if (numericalOption == "43") label <- "tmdl_param"

  if (numericalOption == "51") label <- "perc_f2sat"
  if (numericalOption == "52") label <- "perc_lower"
  if (numericalOption == "53") label <- "perc_w2sat"

  if (numericalOption == "61") label <- "rootweight"
  if (numericalOption == "62") label <- "sequential"

  if (numericalOption == "71") label <- "intflwnone"
  if (numericalOption == "72") label <- "intflwsome"

  if (numericalOption == "81") label <- "no_routing"
  if (numericalOption == "82") label <- "rout_gamma"

  return(label)

}

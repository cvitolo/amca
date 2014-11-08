LabelOptions <- function(option){

  if (option == "11") label <- "additive_e"
  if (option == "12") label <- "multiplc_e"

  if (option == "21") label <- "onestate_1"
  if (option == "22") label <- "tension1_1"
  if (option == "23") label <- "tension2_1"

  if (option == "31") label <- "fixedsiz_2"
  if (option == "32") label <- "tens2pll_2"
  if (option == "33") label <- "unlimfrc_2"
  if (option == "34") label <- "unlimpow_2"

  if (option == "41") label <- "arno_x_vic"
  if (option == "42") label <- "prms_varnt"
  if (option == "43") label <- "tmdl_param"

  if (option == "51") label <- "perc_f2sat"
  if (option == "52") label <- "perc_lower"
  if (option == "53") label <- "perc_w2sat"

  if (option == "61") label <- "rootweight"
  if (option == "62") label <- "sequential"

  if (option == "71") label <- "intflwnone"
  if (option == "72") label <- "intflwsome"

  if (option == "81") label <- "no_routing"
  if (option == "82") label <- "rout_gamma"

  return(label)

}

#' Probabilistic Nash-Sutcliffe efficiency
#'
#' @description This is an implementation of the probabilistic Nash-Sutcliffe efficiency defined by Bulygina et al. (2009).
#'
#' @param Qobs vector of observed values
#' @param discharges is a matrix of simulations
#'
#' @return probabilistic efficiency
#' 
#' @export
#'
#' @examples
#' # pNS(Qobs, discharges)
#'

pNS <- function (Qobs, discharges){

  meanD <- apply(discharges, 2, mean)
  varD <- apply(discharges, 2, var)

  EF <- ( 1 - sum((meanD - Qobs)^2)/sum((Qobs - mean(Qobs))^2) ) -
    ( sum(varD) / sum((Qobs - mean(Qobs))^2)^2 )

  return(EF)

}

#' Calculate precision/sharpness
#'
#' @description Precision definition available in Yadav et al. (2007)
#'
#' @param lowerBound1 vector containing the minimum or the lower quantile bound for the first ensemble
#' @param upperBound1 vector containing the maximum or the upper quantile bound for the first ensemble
#' @param lowerBound2 vector containing the minimum or the lower quantile bound for the second ensemble
#' @param upperBound2 vector containing the maximum or the upper quantile bound for the second ensemble
#'
#' @return Returns precision, in the range [0,100], percentage of closeness to a line.
#'
#' @examples
#' # precision(lowerBound1, upperBound1, lowerBound2, upperBound2)
#'

precision <- function(lowerBound1, upperBound1, lowerBound2, upperBound2){

  # baseline spread
  baseSpread <- mean(upperBound1 - lowerBound1)

  # RE spread
  newSpread <- mean(upperBound2 - lowerBound2)
  precisionRE <- round((baseSpread - newSpread)/baseSpread*100,0)

  return(precisionRE)

}

#' Calculate accuracy
#'
#' @description Accuracy definition available in Yadav et al. (2007)
#'
#' @param Qobs vector containing the observed flow
#' @param lowerBound vector containing the minimum or the lower quantile bound
#' @param upperBound vector containing the maximum or the upper quantile bound
#'
#' @return Returns accuracy, in the range [0,100], percentage of time steps for which the observation is within the bounds.
#'
#' @examples
#' # accuracy(Qobs, lowerBound, upperBound)
#'

accuracy <- function(Qobs, lowerBound, upperBound){

  indicator1 <- rep(NA,length(Qobs))

  for (t in 1:length(Qobs)){
    indicator1[t] <- ifelse(Qobs[t] <= upperBound[t] &&
                            Qobs[t] >= lowerBound[t],1,0)
  }
  accuracyRE <- round(sum(indicator1)/length(Qobs)*100,0)

  return(accuracyRE)

}

#' Summarise performance coefficients
#'
#' @param IE Initial Ensemble bounds
#' @param RE Reduced Ensemble bounds
#'
#' @return Returns accuracy, precision and statistical reliability
#'
#' @examples
#' # review(IE, RE)
#'

review <- function(IE, RE){

  spread1 <- mean(IE$uQ - IE$lQ)

  # Calculate ACCURACY / RELIABILITY (YADAV et al. 2007)
  indicator1 <- rep(NA,dim(IE$bounds)[1])
  for (t in 1:dim(IE$bounds)[1]){
    indicator1[t] <- ifelse(IE$Qobs[t] <= IE$uQ[t] && IE$Qobs[t] >= IE$lQ[t],1,0)
  }
  accuracyIE <- sum(indicator1)/dim(IE)[1]
  message(paste("Accuracy of initial ensemble =",round(accuracyIE*100,0), "%"))

  #*****************************************************************************

  spread2 <- mean(RE$uQ - RE$lQ)

  indicator2 <- rep(NA,dim(RE$bounds)[1])
  for (t in 1:dim(RE$bounds)[1]){
    if (IE$Qobs[t] <= RE$uQ[t] && IE$Qobs[t] >= RE$lQ[t]) {
      indicator2[t] <- 1
    }else{
      indicator2[t] <- 0
    }
  }

  accuracyRE <- sum(indicator2)/dim(RE$bounds)[1]
  message(paste("Accuracy of reduced ensemble =", round(accuracyRE*100,0), "%"))

  #*****************************************************************************

  # Calculate PRECISION / SHARPNESS
  precisionRE <- (spread1 - spread2)/spread1

  message(paste("Precision of reduced ensemble =",round(precisionRE*100,0),"%"))

  #*****************************************************************************

  reliabilityRE <- EnsembleForecast(RE$discharges, IE$Qobs)

  message(paste("Statistical reliability = ",round(reliabilityRE*100,0),"%"))

  return(as.list(c("accuracyIE" = accuracyIE,
                   "accuracyRE" = accuracyRE,
                   "precisionRE" = precisionRE,
                   "reliabilityRE" = reliabilityRE)))

}


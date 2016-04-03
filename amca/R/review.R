#' Summarise performance coefficients
#'
#' @param IE Initial Ensemble bounds
#' @param RE Reduced Ensemble bounds
#' @param rel reliability is calculated is rel is set to TRUE
#'
#' @return Returns accuracy, precision and statistical reliability
#'
#' @examples
#' # review(IE, RE)
#'

review <- function(IE, RE, rel = TRUE, IE90PI = FALSE){

  # IE min-max
  spreadMax <- mean(IE$maxQ - IE$minQ)
  # Calculate ACCURACY / RELIABILITY (YADAV et al. 2007)
  indicator1 <- rep(NA,dim(IE)[1])
  for (t in 1:dim(IE)[1]){
    indicator1[t] <- ifelse(IE$Qobs[t] <= IE$maxQ[t] &&
                              IE$Qobs[t] >= IE$minQ[t],1,0)
  }
  accuracyIEmax <- round(sum(indicator1)/dim(IE)[1]*100,0)
  message(paste("Accuracy of initial ensemble = ", accuracyIEmax, "%", sep =""))

  #*****************************************************************************

  # IE 90% prediction interval
  if (IE90PI) {
    spread1 <- mean(IE$uQ - IE$lQ)
    precisionIE <- (spreadMax - spread1)/spreadMax
    # Calculate ACCURACY / RELIABILITY (YADAV et al. 2007)
    indicator1 <- rep(NA,dim(IE)[1])
    for (t in 1:dim(IE)[1]){
      indicator1[t] <- ifelse(IE$Qobs[t] <= IE$uQ[t] && IE$Qobs[t] >= IE$lQ[t],1,0)
    }
    accuracyIE <- sum(indicator1)/dim(IE)[1]
    message(paste("Accuracy of initial ensemble (90%) = ",
                  round(accuracyIE*100,0), "%", sep =""))
  }else{
    accuracyIE <- NA
    precisionIE <- NA
  }

  #*****************************************************************************

  # RE 90% prediction interval
  spread2 <- mean(RE$bounds$uQ - RE$bounds$lQ)
  indicator2 <- rep(NA,dim(RE$bounds)[1])
  for (t in 1:dim(RE$bounds)[1]){
    if (RE$bounds$Qobs[t] <= RE$bounds$uQ[t] &
        RE$bounds$Qobs[t] >= RE$bounds$lQ[t]) {
      indicator2[t] <- 1
    }else{
      indicator2[t] <- 0
    }
  }

  accuracyRE <- sum(indicator2)/dim(RE$bounds)[1]
  message(paste("Accuracy of reduced ensemble =", round(accuracyRE*100,0), "%"))

  #*****************************************************************************

  # Calculate PRECISION / SHARPNESS
  precisionRE <- (spreadMax - spread2)/spreadMax

  message(paste("Precision of reduced ensemble =",round(precisionRE*100,0),"%"))

  #*****************************************************************************

  # RE STATISTICAL RELIABILITY

  if (rel) {
    reliabilityRE <- EnsembleForecast(RE$discharges, IE$Qobs)
    message(paste("Statistical reliability = ",round(reliabilityRE*100,0),"%"))
  }else{
    reliabilityRE <- NA
  }

  #*****************************************************************************

  # RE probabilistic NS
  REpNS <- pNS(df$Qobs, discharges)

  #*****************************************************************************

  return(as.list(c("accuracyIEmax" = accuracyIEmax,
                   "accuracyIE" = accuracyIE,
                   "accuracyRE" = accuracyRE,
                   "precisionRE" = precisionRE,
                   "reliabilityRE" = reliabilityRE,
                   "probabilisticNSRE" = REpNS)))

}

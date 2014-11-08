#' Summarise performance coefficients
#'
#' @param IE Initial Ensemble bounds
#' @param RE Reduced Ensemble bounds and discharges
#' @param observedQ observed discharge (warmup removed)
#' @param type if equal to "B", only the max/min bounds are used. If equal to "P", it uses the upper/lower percentiles.
#'
#' @return Returns accuracy, precision and statistical reliability
#'
#' @examples
#' # review(IE, RE)
#'

review <- function(IE, RE, observedQ, type = "B"){

  #*****************************************************************************
  message("CALCULATING ACCURACY, PRECISION AND RELIABILITY...")
  #*****************************************************************************
  Q1 <- IE$bounds[,"Qobs"]

  if (!is.na(IE$bounds[1,"UP"])){
    if (type == "B") UB1 <- IE$bounds[,"UB"]
    if (type == "P") UB1 <- IE$bounds[,"UP"]
    if (type == "B") LB1 <- IE$bounds[,"LB"]
    if (type == "P") LB1 <- IE$bounds[,"LP"]
  }else{
    UB1 <- IE$bounds[,"UB"]
    LB1 <- IE$bounds[,"LB"]
  }

  spread1 <- mean(UB1 - LB1)

  # Calculate ACCURACY / RELIABILITY (YADAV et al. 2007)
  indicator1 <- rep(NA,dim(IE$bounds)[1])
  for (t in 1:dim(IE$bounds)[1]){
    indicator1[t] <- ifelse(Q1[t] <= UB1[t] && Q1[t] >= LB1[t],1,0)
  }
  accuracyIE <- sum(indicator1)/dim(IE$bounds)[1]
  message(paste("Accuracy of initial ensemble =",round(accuracyIE*100,0), "%"))

  #*****************************************************************************

  Q2 <- IE$bounds[,"Qobs"]
  if (type == "B") UB2 <- RE$bounds[,"UB"]
  if (type == "P") UB2 <- RE$bounds[,"UP"]
  if (type == "B") LB2 <- RE$bounds[,"LB"]
  if (type == "P") LB2 <- RE$bounds[,"LP"]

  spread2 <- mean(UB2 - LB2)

  indicator2 <- rep(NA,dim(RE$bounds)[1])
  for (t in 1:dim(RE$bounds)[1]){
    if (Q2[t] <= UB2[t] && Q2[t] >= LB2[t]) {
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

  reliabilityRE <- EnsembleForecast(RE$discharges, observedQ)

  message(paste("Statistical reliability = ",round(reliabilityRE*100,0),"%"))

  return(as.list(c("accuracyIE"=accuracyIE,
                   "accuracyRE"=accuracyRE,
                   "precisionRE"=precisionRE,
                   "reliabilityRE"=reliabilityRE)))

}


#' Calculate 5 Model Performance Indices (MPIs)
#'
#' @param Po observed precipitation [mm/day]
#' @param Qo observed streamflow discharge [mm/day]
#' @param Qs simulated streamflow discharge [mm/day]
#'
#' @details LAGTIME is calculated from the tiger::lagtime function. MAE is the mean absolute error. NSHF is the complement to 1 of Nash-Sutcliffe efficiency for high flows. NSLF is the complement to 1 of Nash-Sutcliffe efficiency for low flows. RR is the Rainfall-Runoff coefficient. All the indices above tend to zero (best performance).
#'
#' @return A list of 5 indices: LAGTIME, MAE, NSHF, NSLF, RR.
#'
#' @examples
#' # PerformanceIndices(Po,Qo,Qs)

######################

PerformanceIndices <- function(Po,Qo,Qs){

  LAGTIME <- lagtime(Qo,Qs)

  MAE <- mean(Qs - Qo, na.rm = TRUE)

  NSHF <- 1-EF(Qo,Qs)

  NSLF <- 1-EF(log(Qo),log(Qs))

  RR <- sum(Qs)/sum(Po)

  return( list("LAGTIME" = LAGTIME,
               "MAE"     = MAE,
               "NSHF"    = NSHF,
               "NSLF"    = NSLF,
               "RR"      = RR) )

}

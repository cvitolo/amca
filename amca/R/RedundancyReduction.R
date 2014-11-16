#' Redundancy reduction process using Dynamic Time Warping.
#'
#' @param ParetoFront Pareto Frontier
#' @param DATA this is the data frame containing the observations
#' @param the.som som map
#' @param ParameterSet is the table of parameters
#' @param observedQ observed discharge (warmup removed)
#' @param deltim time step in days
#' @param pperiod is the vector of time step to take into consideration when calculating performances
#' @param verbose if set to TRUE it prints running information (default = FALSE)
#'
#' @return A subset of the table containing the filtered configurations.
#'
#' @details This function removes redundant realizations by extracting the realization with the best DTW score from each cluster.
#'
#' @examples
#' # RedundancyReduction(ParetoFrontTable,DATA,the.som,parameters,observedQ,deltim,pperiod)
#'
RedundancyReduction <- function(ParetoFront,DATA,the.som,
                                ParameterSet,observedQ,deltim,pperiod,verbose=FALSE){

  DTWtable <- ParetoFront
  DTWtable$ClusterX <- the.som$visual[,1]
  DTWtable$ClusterY <- the.som$visual[,2]
  DTWtable$dtw_score <- NA

  discharges <- matrix(NA,ncol=length(pperiod),nrow=dim(DTWtable)[1])

  for ( i in 1:dim(DTWtable)[1] ){

    mid <- as.numeric(as.character(DTWtable[i,"mid"]))
    pid <- as.numeric(as.character(DTWtable[i,"pid"]))

    if (verbose == TRUE) print(paste("Calculating DTW for simulation",i,"out of",dim(DTWtable)[1],
                                     "model",mid,"param",pid))

    simulatedQ <- as.numeric(as.character(RunFUSE(DATA, ParameterSet[pid,],
                                                  deltim, mid)[pperiod]))

    if (any(is.na(simulatedQ))) print("NAs found!")

    # DTWtable$dtw_score[i] <- dtw(observedQ, simulatedQ)$distance
    DTWtable$dtw_score[i] <- dtw(observedQ, simulatedQ,distance.only=TRUE)$normalizedDistance
    discharges[i,] <- simulatedQ

  }

  uniqueClusters <- unique(DTWtable[,c("ClusterX","ClusterY")])
  reducedEnsemble <- data.frame(matrix(NA,
                                       nrow=dim(uniqueClusters)[1],
                                       ncol=dim(DTWtable)[2]))
  names(reducedEnsemble) <- names(DTWtable)

  allRows <- c()
  for (clusternumber in 1:dim(uniqueClusters)[1]){

    # print(paste("Cluster number",clusternumber))
    rows <- which(DTWtable[,"ClusterX"]==uniqueClusters[clusternumber,"ClusterX"] &
                  DTWtable[,"ClusterY"]==uniqueClusters[clusternumber,"ClusterY"])

    allRows <- append(allRows,
                      which(DTWtable$dtw_score==min(DTWtable$dtw_score[rows])) )

  }

  reducedEnsemble <- list("table" = DTWtable[sort(allRows),],
                          "discharges" = discharges)

  return(reducedEnsemble)

}

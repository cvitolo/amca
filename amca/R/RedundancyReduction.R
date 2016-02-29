#' Redundancy reduction process using SOM clustering and Dynamic Time Warping.
#'
#' @param ParetoFront Pareto Frontier
#' @param observedQ observed discharge (warmup removed)
#' @param ResultsFolder Path to the folder containing results from MC simulations.
#' @param ObsIndicesNames is the list of indices names.
#' @param verbose if set to TRUE it prints running information (default = FALSE)
#'
#' @return A subset of the table containing the filtered configurations.
#'
#' @details This function removes redundant realizations by extracting the realization with the best DTW score from each cluster.
#'
#' @examples
#' # RedundancyReduction(ParetoFrontTable,DATA,the.som,parameters,observedQ,deltim,pperiod)
#'
RedundancyReduction <- function(ParetoFront, observedQ, ResultsFolder,
                                ObsIndicesNames, verbose=FALSE){

  # Cluster non-dominated simulations
  # library(som)

  # observedQ <- DATA$Q[pperiod]

  dimX <- ceiling(sqrt(dim(PF)[1]))
  dimY <- ceiling(dim(PF)[1]/dimX)

  PFnumeric <- apply(PF[,ObsIndicesNames], 2, as.numeric)

  the.som <- som(PFnumeric,
                 xdim=dimX, ydim=dimY,
                 init="linear",neigh="gaussian",topol="rect")

  PF$ClusterX <- the.som$visual[,1]
  PF$ClusterY <- the.som$visual[,2]
  PF$dtw_score <- NA

  # library(dtw)
  for ( i in 1:dim(PF)[1] ){

    mid <- PF$mid[i]
    pid <- PF$pid[i]

    if (verbose) {
      print(paste(i, "out of ", dim(PF)[1], "- MID", mid, "PID", pid))
    }

    discharges <- NULL
    load(paste(ResultsFolder, "MID_", mid, ".Rdata", sep=""))

    if(i == 1){
      dischargesDTW <- discharges[pid,]
    }else{
      dischargesDTW <- rbind(dischargesDTW, discharges[pid,])
    }

    PF$dtw_score[i] <- dtw(observedQ, discharges[pid,],
                           distance.only=TRUE)$normalizedDistance

  }

  uniqueClusters <- unique(PF[,c("ClusterX","ClusterY")])

  allRows <- c()
  for (clusternumber in 1:dim(uniqueClusters)[1]){

    if (verbose) {
      print(paste("Cluster number",clusternumber))
    }

    rows <- which(PF[,"ClusterX"] == uniqueClusters[clusternumber,"ClusterX"] &
                    PF[,"ClusterY"] == uniqueClusters[clusternumber,"ClusterY"])

    allRows <- c(allRows, which(PF$dtw_score==min(PF$dtw_score[rows])) )

  }

  reducedEnsemble <- PF[sort(allRows),]

  return(reducedEnsemble)

}

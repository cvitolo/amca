#' Redundancy reduction process using SOM clustering and Dynamic Time Warping.
#'
#' @param DATA This is a data.frame containing the observed time series (zoo objects). It is structured into three columns: P containing precipitation, E containing potential evapotranspiration and Q containing streamflow discharge.
#' @param PF Pareto Frontier
#' @param observedQ observed discharge (warmup removed)
#' @param parameters This is a named data frame containing the parameter table, where each column corresponds to a parameter and each row to a realization.
#' @param deltim time step for FUSE simulations
#' @param warmup Percentage of initial time steps to ignore (default is 0.5 that corresponds to 50 percent of the full length).
#' @param ResultsFolder Path to the folder containing results from MC simulations.
#' @param ObsIndicesNames is the list of indices names.
#' @param verbose if set to TRUE it prints running information (default = FALSE)
#'
#' @return A subset of the table containing the filtered configurations.
#'
#' @details This function removes redundant realizations by extracting the realization with the best DTW score from each cluster.
#'
#' @export
#' 
#' @examples
#' # RedundancyReduction(PF, observedQ, ResultsFolder, ObsIndicesNames)
#'
RedundancyReduction <- function(DATA, PF, observedQ, parameters, deltim, warmup,
                                ResultsFolder, ObsIndicesNames, verbose=FALSE){

  # Cluster non-dominated simulations
  pperiod <- (warmup + 1):dim(DATA)[1]
  dimX <- ceiling(sqrt(dim(PF)[1]))
  dimY <- ceiling(dim(PF)[1]/dimX)

  PFnumeric <- apply(PF[,ObsIndicesNames], 2, as.numeric)

  the.som <- som::som(PFnumeric,
                      xdim = dimX,
                      ydim = dimY,
                      init = "linear",
                      neigh = "gaussian",
                      topol = "rect")

  PF$ClusterX <- the.som$visual[,1]
  PF$ClusterY <- the.som$visual[,2]
  PF$dtw_score <- NA

  for ( i in 1:dim(PF)[1] ){

    mid <- PF$mid[i]
    pid <- PF$pid[i]

    if (verbose) {
      print(paste(i, "out of", dim(PF)[1], "- MID", mid, "PID", pid))
    }

    simulatedQ <- zoo::coredata(fuse(DATA, mid, deltim,
                                     ParameterSet = parameters[pid,])[pperiod])

    PF$dtw_score[i] <- dtw::dtw(observedQ, simulatedQ,
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

    allRows <- c(allRows, rows[which.min(PF$dtw_score[rows])] )

  }

  reducedEnsemble <- PF[sort(allRows),]

  return(reducedEnsemble)

}

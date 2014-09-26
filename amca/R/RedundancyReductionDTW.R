#' Redundancy reduction process using Dynamic Time Warping.
#'
#' @param DTWtable pareto frontier of the DTW realisations
#'
#' @return A subset of the table containing the Pareto frontier.
#'
#' @details This function removes redundant realizations by extracting the realization with the best DTW score from each cluster.
#'
#' @examples
#' # ParetoFrontier(indices[,,selected_models[,"row"]],selected_models,parameters,SelectedIndices)
#'
RedundancyReductionDTW <- function(DTWtable){

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

  reducedEnsemble <- DTWtable[sort(allRows),]

  return(reducedEnsemble)

}

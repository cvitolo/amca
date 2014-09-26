#' Add columns with cluster information to the Pareto Front
#'
#' @param ParetoFront Pareto Frontier
#' @param DATA this is the data frame containing the observations
#' @param the.som som map
#' @param SimulationFolder path to the folder where the function saves the results
#' @param observedQ observed discharge (warmup removed)
#'
#' @return A data.frame with 4 columns: date&time (Dates), observed discharge (Qobs), lower bound (LB), upper bound (UB)
#'
#' @details The table containing the pareto frontier is now extended with clustering information.
#'
#' @examples
#' # ParetoFrontierDTW(ParetoFront,DATA,the.som,SimulationFolder,observedQ)
#'

ParetoFrontierDTW <- function(ParetoFront,DATA,the.som,SimulationFolder,
                              observedQ){

  DTWtable <- ParetoFront
  DTWtable$ClusterX <- the.som$visual[,1]
  DTWtable$ClusterY <- the.som$visual[,2]
  DTWtable$dtw_score <- NA

  for ( i in 1:dim(DTWtable)[1] ){
    # print(paste("Scenario",i,"of",dim(DTWtable)[1]))
    scenarios <- ScenarioPlotDTW("mid"=DTWtable[i,"mid"],
                                 "pid"=DTWtable[i,"pid"],SimulationFolder)
    temp <- as.numeric(as.character(scenarios))
    simulatedQ <- as.numeric(as.character(temp))
    simulatedQ <- simulatedQ[1:length(simulatedQ)]
    DTWtable$dtw_score[i] <- dtw(observedQ, simulatedQ)$distance
  }

  return(DTWtable)
}

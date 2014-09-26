#' Generate a table containing only non-dominated realisations based on MPIs and their Pareto Frontier.
#'
#' @param Indices 3d array containing indices (1000 parameter sets x 10 perf. measures x 252 model structures)
#' @param realisations list of model structures to take into account
#' @param ObsIndices this is the list of model performance indices calculated from observations.
#'
#' @return a subset of the initial dataset containing only non-dominated realisations
#'
#' @examples
#' # ParetoFrontier(Indices, PreSelTable, ObsIndices)
#'

ParetoFrontier <- function(Indices,realisations,ObsIndices){

  mat <- matrix(as.numeric(as.character(unlist(realisations[,names(ObsIndices)],
                                               use.names = FALSE))),
                ncol = dim(Indices)[2], byrow = FALSE)

  # matrix with points
  pf  <- t(nondominated_points(t( mat )))

  x <- RowMatch(realisations[,names(ObsIndices)],pf)

  pf_extended <- realisations[x,]

  rows2remove <- c()
  for (r in 2:dim(pf_extended)[1]){
    if (pf_extended[r,"mid"]==pf_extended[r-1,"mid"] & pf_extended[r,"pid"]==pf_extended[r-1,"pid"]){
      rows2remove <- append(rows2remove,r)
    }
  }
  if (length(rows2remove)>0){
    paretoFront <- pf_extended[-rows2remove,]
  }else{
    paretoFront <- pf_extended
  }

  return(paretoFront)
}

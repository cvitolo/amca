#' Generate a table containing only non-dominated realisations based on MPIs and their Pareto Frontier.
#'
#' @param realisations list of model structures to take into account
#' @param ObsIndices this is the list of model performance indices calculated from observations.
#'
#' @return a subset of the initial dataset containing only non-dominated realisations
#'
#' @examples
#' # ParetoFrontier(PreSelTable, ObsIndices)
#'

ParetoFrontier <- function(realisations,ObsIndices){

  mat <- data.matrix(realisations[,names(ObsIndices)])

  # remove NA from mat
  mat <- na.omit(mat[,names(ObsIndices)])

  # matrix with points
  pf  <- t(nondominated_points(t( mat )))

  myRows <- c()
  counter <- 0
  indices <- realisations[,names(ObsIndices)]
  for (r in 1:dim(pf)[1]){
    counter <- counter + 1
    temp <- RowMatch(indices, pf[r,])
    if (!(temp %in% myRows)) {
      myRows <- c(myRows,temp)
      indices[temp,] <- NA
    }
  }

  pf_extended <- realisations[myRows,]

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

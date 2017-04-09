#' Generate a table containing only non-dominated realisations based on MPIs and their Pareto Frontier.
#'
#' @param realisations list of model structures to take into account
#' @param ObsIndicesNames this is a vector containing the names of the indices.
#'
#' @return a subset of the initial dataset containing only non-dominated realisations
#' 
#' @export
#'
#' @examples
#' # ParetoFrontier(PreSelTable)
#'

ParetoFrontier <- function(realisations, ObsIndicesNames){

  mat <- t(realisations[, ObsIndicesNames])
  mat <- apply(mat, 2, as.numeric)

  # matrix with points
  RowsOfNonDominatedPoints <- which(!emoa::is_dominated(mat))
  pf  <- realisations[RowsOfNonDominatedPoints, ]

  return(pf)
}

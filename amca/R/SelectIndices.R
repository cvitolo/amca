#' Finds Indices for selected realisations.
#'
#' @param realisations table containing at least 2 columns: mid (list of model structures) and pid (parameter sets)
#' @param Indices this is the array containing the rescaled Model Performance.
#' @param ModelList this is the list of model structures.
#' @param ObsIndicesNames this is the list of model performance indices calculated from observations.
#'
#' @return Extended data.frame
#'
#' @examples
#' # SelectIndices(realisations)
#'

SelectIndices <- function(realisations, Indices, ModelList, ObsIndicesNames){

  colNames <- c(names(realisations), unlist(ObsIndicesNames))

  tableX <- matrix(NA, ncol=length(colNames), nrow=dim(realisations)[1])
  tableX[,1:dim(realisations)[2]] <- as.matrix(realisations)

  MIDs <- as.numeric(as.character(unique(realisations$mid)))

  for (mid in MIDs) {

    rows <- which(tableX[,which(colNames=="mid")]==mid)
    counterMID <- which(ModelList$mid %in% mid)

    colsMod <- which(colNames %in% ObsIndicesNames)
    counterPIDs <- as.numeric(tableX[rows,which(colNames=="pid")])
    tableX[rows,colsMod] <- Indices[counterPIDs,,counterMID]

  }

  tableX <- data.frame(tableX)
  names(tableX) <- colNames

  return(tableX)

}

#' Extends the table of realisations to include the indices, model building options and parameters.
#'
#' @param realisations table containing at least 2 columns: mid (list of model structures) and pid (parameter sets)
#' @param ModelList a table that contains at list 1 colum named "mid" (list of model structures). Other informations can be added as additional columns but will be ignored.
#' @param Indices this is the array containing the Model Performance Indices rescaled in the range [0,1], see \code{RescaleIndices}.
#' @param parameters This is a named data frame containing the parameter table, where each column corresponds to a parameter and each row to a realization.
#' @param ObsIndicesNames this is the list of model performance indices calculated from observations.
#' @param verbose if set to TRUE it prints running information
#' @param onlyIndices if set to TRUE to extends the table of realisations only with indices.
#'
#' @return Extended data.frame
#' 
#' @export
#'
#' @examples
#' # ExtendTable(PreSel,ModelList,Indices,parameters,ObsIndicesNames)
#'

ExtendTable <- function(realisations, ModelList, Indices, parameters,
                        ObsIndicesNames, verbose=FALSE, onlyIndices = FALSE){

  if (onlyIndices == TRUE){

    colNames <- c(names(realisations), ObsIndicesNames)
    tableX <- matrix(NA, ncol=length(colNames), nrow=dim(realisations)[1])
    tableX[,1:dim(realisations)[2]] <- as.matrix(realisations)
    MIDs <- as.numeric(as.character(unique(realisations$mid)))
    PIDs <- as.numeric(as.character(unique(realisations$pid)))

    for (mid in MIDs) {

      if (verbose == TRUE) {
        print( paste("FUN: ExtendTable - MID",mid,
                     "out of",max(as.numeric(as.character(MIDs)))) )
      }

      rows <- which(tableX[,which(colNames=="mid")]==mid)
      counterMID <- which(ModelList$mid %in% mid)
      colsMod <- which(colNames %in% ObsIndicesNames)
      counterPIDs <- as.numeric(tableX[rows,which(colNames=="pid")])
      tableX[rows,colsMod] <- Indices[counterPIDs,,counterMID]

    }

    tableX <- data.frame(tableX)
    names(tableX) <- colNames

  }else{

    colNamesModels <- c("rferr","arch1","arch2","qsurf",
                        "qperc","esoil","qintf","q_tdh")

    colNames <- c(names(realisations),
                  ObsIndicesNames,
                  colNamesModels,
                  names(parameters))

    tableX <- matrix(NA, ncol=length(colNames), nrow=dim(realisations)[1])

    tableX[,1:dim(realisations)[2]] <- as.matrix(realisations)

    MIDs <- as.numeric(as.character(unique(realisations$mid)))

    PIDs <- as.numeric(as.character(unique(realisations$pid)))

    colsPar <- which(colNames %in% names(parameters))

    for (pid in PIDs) {

      if (verbose == TRUE) {
        print( paste("FUN: ExtendTable - PID",pid,
                     "out of",max(as.numeric(as.character(PIDs)))) )
      }

      rows <- which(tableX[,which(colNames=="pid")]==pid)

      tableX[rows,colsPar] <- t(matrix(as.numeric(parameters[pid,]),
                                       nrow=length(colsPar), ncol=length(rows)))

    }

    for (mid in MIDs) {

      if (verbose == TRUE) {
        print( paste("FUN: ExtendTable - MID",mid,
                     "out of",max(as.numeric(as.character(MIDs)))) )
      }

      rows <- which(tableX[,which(colNames=="mid")]==mid)
      counterMID <- which(ModelList$mid %in% mid)
      colsMod <- which(colNames %in% colNamesModels)
      tableX[rows,colsMod] <- t(matrix(as.numeric(ModelList[counterMID,
                                                            colNamesModels]),
                                       nrow=length(colsMod),ncol=length(rows)))

      colsMod <- which(colNames %in% ObsIndicesNames)
      counterPIDs <- as.numeric(tableX[rows,which(colNames=="pid")])
      tableX[rows,colsMod] <- Indices[counterPIDs,,counterMID]

      colsParNA <- which(is.na(ModelList[which(ModelList$mid==mid),
                                         names(parameters)]))
      startCol <- which(colNames %in% names(parameters)[1]) - 1
      tableX[rows,startCol+colsParNA] <- NA

    }

    tableX <- data.frame(tableX)
    names(tableX) <- colNames

  }

  return(tableX)

}

#' Extends the table of realisations to include the indices, model building options and parameters.
#'
#' @param realisations table containing at least 2 columns: mid (list of model structures) and pid (parameter sets)
#' @param ModelList a table that contains at list 1 colum named "mid" (list of model structures). Other informations can be added as additional columns but will be ignored.
#' @param Indices this is the array containing the Model Performance Indices rescaled in the range [0,1], see \code{RescaleIndices}.
#' @param parameters This is a named data frame containing the parameter table, where each column corresponds to a parameter and each row to a realization.
#' @param ObsIndices this is the list of model performance indices calculated from observations.
#' @param verbose if set to TRUE it prints running information
#'
#' @return Extended data.frame
#'
#' @examples
#' # ExtendTable(PreSel,ModelList,Indices,parameters,ObsIndices)
#'

# ExtendTable <- function(realisations,ModelList,Indices,parameters,
#                         ObsIndices, verbose){
#
#   colNamesModels <- c("rferr","arch1","arch2","qsurf",
#                       "qperc","esoil","qintf","q_tdh")
#
#   colNames <- c(names(realisations),
#                 names(ObsIndices),
#                 colNamesModels,
#                 names(parameters))
#
#   tableX <- matrix(NA, ncol=length(colNames), nrow=dim(realisations)[1])
#
#   tableX[,1:dim(realisations)[2]] <- as.matrix(realisations)
#
#   MIDs <- as.numeric(as.character(unique(realisations$mid)))
#
#   PIDs <- as.numeric(as.character(unique(realisations$pid)))
#
#   if (verbose == TRUE) message( "Selection based on mid" )
#
#   for (mid in MIDs) {
#
#     if (verbose == TRUE) print( paste("FUN: ExtendTable - MID",mid,"out of",max(as.numeric(as.character(MIDs)))) )
#
#     rows <- which(tableX[,which(colNames=="mid")]==mid)
#     counterMID <- which(ModelList$mid %in% mid)
#     cols <- which(colNames %in% colNamesModels)
#     tableX[rows,cols] <- t(matrix(as.numeric(ModelList[counterMID,colNamesModels]),nrow=length(cols),ncol=length(rows)))
#
#     cols <- which(colNames %in% names(ObsIndices))
#     counterPIDs <- as.numeric(tableX[rows,which(colNames=="pid")])
#     tableX[rows,cols] <- Indices[counterPIDs,,counterMID]
#
#   }
#
#   if (verbose == TRUE) message( "Selection based on pid" )
#
#   for (pid in PIDs) {
#
#     if (verbose == TRUE) print( paste("FUN: ExtendTable - PID",pid,"out of",max(as.numeric(as.character(PIDs)))) )
#
#     rows <- which(tableX[,which(colNames=="pid")]==pid)
#     cols <- which(colNames %in% names(parameters))
#
#     tableX[rows,cols] <- t(matrix(as.numeric(parameters[pid,]),nrow=length(cols),ncol=length(rows)))
#
#   }
#
#   tableX <- data.frame(tableX)
#   names(tableX) <- colNames
#
#   return(tableX)
#
# }

ExtendTable <- function(realisations,ModelList,Indices,parameters,
                        ObsIndices, verbose){

  colNamesModels <- c("rferr","arch1","arch2","qsurf",
                      "qperc","esoil","qintf","q_tdh")

  colNames <- c(names(realisations),
                names(ObsIndices),
                colNamesModels,
                names(parameters))

  tableX <- matrix(NA, ncol=length(colNames), nrow=dim(realisations)[1])

  tableX[,1:dim(realisations)[2]] <- as.matrix(realisations)

  MIDs <- as.numeric(as.character(unique(realisations$mid)))

  PIDs <- as.numeric(as.character(unique(realisations$pid)))

  colsPar <- which(colNames %in% names(parameters))

  if (verbose == TRUE) message( "Selection based on pid" )

  for (pid in PIDs) {

    if (verbose == TRUE) print( paste("FUN: ExtendTable - PID",pid,"out of",max(as.numeric(as.character(PIDs)))) )

    rows <- which(tableX[,which(colNames=="pid")]==pid)

    tableX[rows,colsPar] <- t(matrix(as.numeric(parameters[pid,]),nrow=length(colsPar),ncol=length(rows)))

  }

  if (verbose == TRUE) message( "Selection based on mid" )

  for (mid in MIDs) {

    if (verbose == TRUE) print( paste("FUN: ExtendTable - MID",mid,"out of",max(as.numeric(as.character(MIDs)))) )

    rows <- which(tableX[,which(colNames=="mid")]==mid)
    counterMID <- which(ModelList$mid %in% mid)
    colsMod <- which(colNames %in% colNamesModels)
    tableX[rows,colsMod] <- t(matrix(as.numeric(ModelList[counterMID,colNamesModels]),nrow=length(colsMod),ncol=length(rows)))

    colsMod <- which(colNames %in% names(ObsIndices))
    counterPIDs <- as.numeric(tableX[rows,which(colNames=="pid")])
    tableX[rows,colsMod] <- Indices[counterPIDs,,counterMID]

    colsParNA <- which(is.na(ModelList[which(ModelList$mid==mid),
                                       names(parameters)]))
    startCol <- which(colNames %in% names(parameters)[1]) - 1
    tableX[rows,startCol+colsParNA] <- NA

  }

  tableX <- data.frame(tableX)
  names(tableX) <- colNames

  return(tableX)

}

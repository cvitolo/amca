#' Automatically set a performance threshold.
#'
#' @param ModelList a table that contains at list 1 colum named "mid" (list of model structures)
#' @param Indices this is the array containing the rescaled Model Performance
#' @param selectM boolean value. If it is TRUE the preselection applies to model structures
#' @param selectP boolean value. If it is TRUE the preselection applies to parameter sets
#' @param type it can either be "dependent" or "independent"
#' Indices.
#' @param verbose if set to TRUE it prints running information and a plot showing the threshold.
#'
#' @return A number between 0 and 1
#' 
#' @export
#'
#' @examples
#' # SetThreshold(ModelList, Indices)
#'

SetThreshold <- function(ModelList, Indices, selectM=TRUE,selectP=TRUE,
                         type="independent", verbose){

  MIDs <- c()
  PIDs <- c()

  for (threshold in seq(1,0.1,-0.1)){

    if (verbose==TRUE) {
      print(paste("FUN: SetThreshold - calculating threshold",threshold))
    }

    PreSelRealisations <- PreSelection(ModelList,
                                       Indices,
                                       threshold,
                                       selectM,selectP,type)

    MIDs <- append(MIDs,length(unique(PreSelRealisations$mid)))
    PIDs <- append(PIDs,length(unique(PreSelRealisations$pid)))

  }

  if (selectP==FALSE){
    normalizedSUM <- MIDs
    threshold <- seq(1,0.1,-0.1)[which(normalizedSUM==min(normalizedSUM))][1]
  }else{
    normalizedSUM <- PIDs/1248 + MIDs/1000
    minNparams <- 1
    rows2remove <- c(which(MIDs <= minNparams),which(PIDs <= minNparams))
    if (length(rows2remove)>0){
      threshold <- seq(1,0.1,-0.1)[which(normalizedSUM==min(normalizedSUM[-rows2remove]))][1]
    }else{
      threshold <- seq(1,0.1,-0.1)[which(normalizedSUM==min(normalizedSUM))][1]
    }
  }

  return(threshold)

}

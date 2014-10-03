#' Automatically set a performance threshold.
#'
#' @param ModelList a table that contains at list 1 colum named "mid" (list of model structures)
#' @param Indices this is the array containing the rescaled Model Performance Indices.
#' @param verbose if set to TRUE it prints running information and a plot showing the threshold.
#' @param selectM boolean value. If it is TRUE the preselection applies to model structures
#' @param selectP boolean value. If it is TRUE the preselection applies to parameter sets
#' @param type it can either be "dependent" or "independent"
#'
#' @return A number between 0 and 1
#'
#' @examples
#' # SetThreshold(ModelList, Indices)
#'

SetThreshold <- function(ModelList, Indices, verbose,
                         selectM=TRUE,selectP=TRUE,type="independent"){

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

  if (verbose==TRUE) {

    x <- seq(1,0.1,-0.1)
    y <- normalizedSUM

    df <- data.frame(x, y)

    # Plot normalized sum of suitable models and parameters
    ggplot(data=df) +
      geom_line(aes(x = x, y = y)) +
      geom_point(x = threshold, y = min(normalizedSUM), color="red", size=3) +
      xlab("Threshold") + ylab("Model structures after pre-selection") + theme_bw()

  }

  return(threshold)

}

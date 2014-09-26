#' Perform the preliminary selection
#'
#' @param ModelList this is the list of model structures
#' @param Indices this is the array containing the Model Performance Indices rescaled in the range [0,1], see \code{RescaleIndices}.
#' @param threshold is a number between 0 and 1 setting the filtering threshold.
#' @param selectM boolean value. If it is TRUE the preselection applies to model structures
#' @param selectP boolean value. If it is TRUE the preselection applies to parameter sets
#' @param type it can either be "dependent" or "independent"
#'
#' @return list of two data.frames, each with two columns:
#' models = row number (row) and model identification number (mid);
#' params = row number (row) and model identification number (pid), by definition row and pid coincide.
#'
#' @examples
#' # PreSelection(ModelList, Indices, threshold)
#'

PreSelection <- function(ModelList,Indices,threshold,
                         selectM=TRUE,selectP=TRUE,type="independent"){

  SelectedMIDs <- ModelList[,"mid"]        # At the beginning all the models are selected
  SelectedPIDs <- seq(1:dim(Indices)[1])   # At the beginning all the parameter sets are selected

  for (index in 1:dim(Indices)[2]) {

    # print(index)

    if (selectM == TRUE){

      # Pre-selection of model structures
      ModelScores <-  data.frame("mid"=ModelList[,"mid"],
                                 "score"=apply(Indices[,index,],2,median,
                                               na.rm=TRUE))

      GoodModels <- ModelScores$mid[which(ModelScores$score<threshold)]
      SelectedMIDs <- intersect(GoodModels,SelectedMIDs)

    }

    if (type=="dependent") {
      mcounters <- which(ModelList$mid %in% SelectedMIDs)
      myIndices <- Indices[,,mcounters]
    }else{
      myIndices <- Indices
    }

    if (selectP == TRUE){

      # Pre-selection of parameter sets
      ParamScores <-  data.frame("pid"   = seq(1:dim(Indices)[1]),
                                 "score" = apply(Indices[,index,],
                                                 1, median,na.rm=TRUE))

      GoodParams <- ParamScores$pid[which(ParamScores$score<threshold)]
      SelectedPIDs <- intersect(GoodParams,SelectedPIDs)

    }

  }

  if (length(SelectedMIDs)==0) {

    SelectedMIDs <- ModelList[,"mid"]

  }

  if (length(SelectedPIDs)==0) {

    SelectedPIDs <- seq(1:dim(Indices)[1])

  }

  selectedRealisations <- data.frame(table("mid"=rep(SelectedMIDs,
                                                     length(SelectedPIDs)),
                                           "pid"=rep(SelectedPIDs,
                                                     length(SelectedMIDs)) ) )

  return(selectedRealisations[,1:2])

}

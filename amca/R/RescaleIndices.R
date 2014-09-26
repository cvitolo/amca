#' Rescale Model Performance Indices (MPIs) between 0 and 1.
#'
#' @param Indices this is the array containing the not-yet rescaled Model Performance Indices.
#' @param ObsIndices this is the list of model performance indices calculated from observations.
#'
#' @details The best performance always tends to zero.
#'
#' @return Rescaled array of Model Performance Indices
#'
#' @examples
#' # newIndices <- RescaleIndices(Indices, ObsIndices)
#'

RescaleIndices <- function(Indices,ObsIndices) {

  x <- array(NA,dim=c(dim(Indices)[1],dim(Indices)[2],dim(Indices)[3]))

  x[,1,] <- abs(Indices[,1,])                       # LAGTIME
  x[,2,] <- abs(Indices[,2,])                       # MAE
  x[,3,] <- Indices[,3,]                            # NSHF
  x[,4,] <- Indices[,4,]                            # NSLF
  x[,5,] <- abs(Indices[,5,] - ObsIndices$RR)       # RR

  # Normalize the values so that they vary between 0 and 1
  # note the use of unlist() to calculate the sum over the whole matrix
  for (f in 1:dim(Indices)[2]){
    maxx <- max(unlist(x[,f,]),na.rm=TRUE)
    minx <- min(unlist(x[,f,]),na.rm=TRUE)
    x[,f,] <- (x[,f,]-minx)/(maxx-minx)
  }

  return(x)

}

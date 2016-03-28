#' Rescale Model Performance Indices (MPIs) between 0 and 1.
#'
#' @param Indices this is the array containing the not-yet rescaled Model Performance Indices.
#'
#' @details The best performance always tends to zero.
#'
#' @return Rescaled array of Model Performance Indices
#'
#' @examples
#' # newIndices <- RescaleIndices(Indices)
#'

RescaleIndices <- function(Indices) {

  # library(scales)

  Indices <- abs(Indices)

  for (i in 1:dim(Indices)[2]){

    # print(i)
    x <- quantile(na.omit(Indices[,i,]), 0.99)
    Indices[,i,][Indices[,i,] > x] <- x
    Indices[,i,] <- rescale(Indices[,i,], to=c(0,1))

  }

  return(Indices)

}

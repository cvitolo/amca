#' Collate indices of various model structures in one 3-dimensional array.
#'
#' @param ModelList this is the reduced list of model structures (total number: 252)
#' @param ResultsFolder path to the folder where the function saves the results
#' @param nParams number of sampled parameter sets
#' @param nIndices number of Model Performance Indices (MPIs)
#' @param verbose if TRUE, it prints details of the current simulation
#' @param string name of the object containing the indices (this is "indices" by default)
#'
#' @return 3D arrays containing in the first dimension the parameters, in the second dimension the indices and in the third dimension the model structures.
#'
#' @export
#' 
#' @examples
#' # indices <- Simulations2Indices(ModelList,ResultsFolder,nParams, nIndices, verbose)
#'

Simulations2Indices <- function(ModelList, ResultsFolder,
                                nParams, nIndices, verbose,
                                string = "indices"){

  x <- array(0, dim=c(nParams,nIndices,dim(ModelList)[1]) )

  mcounter <- 0

  for (mid in ModelList[,"mid"]){

    if (verbose==TRUE) {
      print(paste("FUN: Simulations2Indices - Opening model results",mid))
    }

    indices <- NULL
    load(file.path(ResultsFolder, paste0("MID_", mid, ".Rdata")))
    # indices <- eval(parse(text = string))

    mcounter <- mcounter + 1
    x[,,mcounter] <- as.matrix(indices[1:nParams,])

  }

  return(x)

}

#' Extracts the ensemble bounds from a given a set of realisations.
#'
#' Results are generally divided in multiple files, therefore the function requires the simulations' folder path.
#'
#' @param observedQ this is the observed streamflows
#' @param SimulationFolder path to the folder where simulations are stored
#' @param MIDs list of model structures to use
#' @param PIDs list of parameter indices to use
#' @param lowerP lower probability (e.g. 0.05 means 5th percentile)
#' @param upperP upper probability (e.g. 0.95 means 95th percentile)
#' @param verbose if set to TRUE it prints running information
#' @param outputQ if set to TRUE, the funtion also returns the discharges matrix
#'
#' @return A data.frame with 6 columns: date&time (Dates), observed discharge (Qobs), lower bound (lQ), median (mQ), upper bound (uQ).
#'
#' @examples
#' # BuildEnsemble(observedQ, SimulationFolder, MIDs, PIDs)
#'

BuildEnsemble <- function(observedQ, SimulationFolder, MIDs, PIDs,
                          lowerP=0.05, upperP=0.95,
                          verbose=FALSE, outputQ = FALSE) {

  if (length(MIDs) >= 624 & length(PIDs) >= 10000){

    ### Build Initial Ensemble using bigmemory/biganalytics ####################
    # library(bigmemory)
    # library(biganalytics)

    # Create empty big.matrix (Initial Ensemble: A)
    A <- filebacked.big.matrix(nrow = length(PIDs) * length(MIDs),
                               ncol = length(observedQ),
                               type='double', init=NULL,
                               backingfile="dischargesSynthetic.bin",
                               descriptorfile="dischargesSynthetic.desc")
    # Fill the matrix
    rowStart <- 1
    rowEnd <- length(PIDs)
    for (mid in MIDs){

      if (verbose==TRUE) {
        print(paste("FUN: BuildEnsemble - Opening MID ", mid, sep=""))
      }

      discharges <- NULL
      load(paste(SimulationFolder, "MID_", mid, ".Rdata", sep=""))
      A[rowStart:rowEnd,] <- discharges
      rowStart <- rowEnd + 1
      rowEnd <- rowEnd + length(PIDs)
    }

    # Now force any modified information to be written to A (file-backed)
    # flush(A) # This is not necessary!

    # If I need to read previously created big.matrix:
    # A <-  attach.big.matrix(dget("dischargesSynthetic.desc"))

  }else{

    A <- matrix(NA, nrow=0, ncol=length(observedQ))

    for (mid in MIDs){

      if (verbose==TRUE) {
        print(paste("FUN: BuildEnsemble - Opening MID ", mid, sep=""))
      }

      discharges <- NULL
      load( paste(SimulationFolder,"/MID_",mid,".Rdata",sep="") )
      A <- rbind(A,discharges[PIDs,])

    }

  }

  lQ <- apply(A, 2, quantile, probs = lowerP)
  mQ <- apply(A, 2, quantile, probs = 0.50)
  uQ <- apply(A, 2, quantile, probs = upperP)

  bounds <- data.frame("Dates"= 1:length(lQ), "Qobs"=observedQ,
                   "lQ"=lQ, "mQ"=mQ, "uQ"=uQ)

  if (outputQ) {

    return(list("bounds" = bounds, "discharges" = A))

  }else{

    return(bounds)

  }

}

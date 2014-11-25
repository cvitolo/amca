#' Extracts the ensemble bounds from a given a set of realisations.
#'
#' Results are generally divided in multiple files, therefore the function requires the simulations' folder path.
#'
#' @param DATA this is the data frame containing the observations
#' @param warmup this is the warmup period
#' @param Realisations2Use list of the filtered realisations
#' @param SimulationFolder path to the folder where simulation results are stored
#' @param maxminOnly if set to FALSE it calculates max/min bounds and upper/lower percentile bounds, otherwise only the max/min bounds (default is FALSE).
#' @param lowerP lower probability (e.g. 0.05 means 5th percentile)
#' @param upperP upper probability (e.g. 0.95 means 95th percentile)
#' @param verbose if set to TRUE it prints running information
#'
#' @return A data.frame with 6 columns: date&time (Dates), observed discharge (Qobs), lower bound (LB), upper bound (UB), lower percentile (LP), upper percentile (UP)
#'
#' @examples
#' # BuildEnsemble(DATA, warmup, Realisations2Use, SimulationFolder)
#'

BuildEnsemble <- function( DATA,
                           warmup,
                           Realisations2Use,
                           SimulationFolder,
                           maxminOnly=TRUE,
                           lowerP=0.05, upperP=0.95,
                           verbose=FALSE) {

  pperiod <- (warmup + 1):dim(DATA)[1] # performance period

  p <- sort(as.numeric(as.character(unique(Realisations2Use$pid))))
  m <- sort(as.numeric(as.character(unique(Realisations2Use$mid))))

  allDischarges <- NA

  if (maxminOnly == TRUE) {

    minD <- matrix(NA,nrow=0,ncol=dim(DATA)[1]-warmup)
    maxD <- matrix(NA,nrow=0,ncol=dim(DATA)[1]-warmup)

    mcounter <- 0
    for (mid in m){

      mcounter <- mcounter + 1

      if (verbose==TRUE) {

        print(paste("FUN: BuildEnsemble - Opening MID ",
                    mid,". ",mcounter, " out of ",length(m), sep=""))

      }

      load( paste(SimulationFolder,"MID_",mid,".Rdata",sep="") )

      if ( exists("discharges") ){

        discharges <- discharges # to avoid NOTE from check

        minD <- apply(rbind(minD,discharges),2,min, na.rm=TRUE)
        maxD <- apply(rbind(maxD,discharges),2,max, na.rm=TRUE)

      }else{

        minQ <- minQ; maxQ <- maxQ # to avoid NOTE from check

        if ( mcounter == 1 ){
          minD <- minQ[pperiod]
          maxD <- maxQ[pperiod]
        }else{
          minD <- apply(rbind(minD,minQ[pperiod]),2,min, na.rm=TRUE)
          maxD <- apply(rbind(maxD,minQ[pperiod]),2,max, na.rm=TRUE)
        }
      }

    }

    bounds <- data.frame("Dates"=index(DATA$P)[(warmup+1):(dim(DATA)[1])],
                         "Qobs"=DATA$Q[(warmup+1):(dim(DATA)[1])],
                         "LB"=minD,
                         "UB"=maxD,
                         "LP"=rep(NA,dim(DATA)[1]-warmup),
                         "UP"=rep(NA,dim(DATA)[1]-warmup))

  }else{

    allDischarges <- matrix(NA,nrow=0,ncol=dim(DATA)[1]-warmup)

    mcounter <- 0
    for (mid in m){

      if (verbose==TRUE) {
        mcounter <- mcounter + 1
        print(paste("FUN: BuildEnsemble - Opening MID ",
                    mid," (",mcounter, " out of ",length(m),")", sep=""))
      }

      load( paste(SimulationFolder,"/MID_",mid,".Rdata",sep="") )
      allDischarges <- rbind(allDischarges,discharges[p,])

    }

    bounds <- data.frame("Dates"=index(DATA$P)[(warmup+1):(dim(DATA)[1])],
                         "Qobs"=DATA$Q[(warmup+1):(dim(DATA)[1])],
                         "LB"=apply(allDischarges, 2, min),
                         "UB"=apply(allDischarges, 2, max),
                         "LP"=apply(allDischarges, 2, quantile, probs = lowerP),
                         "UP"=apply(allDischarges, 2, quantile, probs = upperP))

  }

  return(list("discharges" = allDischarges, "bounds" = bounds))

}

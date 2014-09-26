#' Extract the ensemble from a given a set of realisations.
#'
#' Results are generally divided in multiple files, therefore the function requires the simulations' folder path.
#'
#' @param Realisations2Use list of the filtered realisations
#' @param SimulationFolder path to the folder where simulation results are stored
#' @param SimLength length of the simulation results, usually it is total length - warmup
#' @param warmup this is the warmup period
#'
#' @return predictions
#'
#' @examples
#' #
#'

PredictionsExtract <- function(Realisations2Use, SimulationFolder, SimLength, warmup){

  # extract info from files
  predictions <- matrix(0,ncol=SimLength,nrow=dim(Realisations2Use)[1])

  oldmid <- 0
  mids <- unique(Realisations2Use[,"mid"])
  start <- 1
  for (i in 1:length(unique(Realisations2Use[,"mid"]))){
    #print(paste("MID ",i," out of ",length(unique(Realisations2Use[,"mid"])),sep=""))
    mid <- mids[i]
    pids <- Realisations2Use[which(Realisations2Use[,"mid"]==mid),"pid"]
    if (oldmid != mid) {
      load(paste(SimulationFolder,"/MID_",mid,".Rdata",sep=""))
      discharges <- discharges
    }
    end <- start + length(pids)-1
    predictions[start:end,] <- unlist(discharges[pids,])
    start <- end + 1
    oldmid <- mid
  }

  return(predictions)

}

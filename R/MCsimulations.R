#' This function runs Monte Carlo simulations for a given number of realisations.
#'
#' @param DATA this is the data frame containing the observations
#' @param parameters table containing the parameter sets, see \code{GeneratePsetsFUSE}
#' @param deltim this is the time step (in days)
#' @param warmup this is the warmup period
#' @param ListOfModels this is the reduced list of model structures (default = 1:1248)
#' @param SimulationFolder path to the folder where the function saves the results
#' @param MPIs list of functions describing the Model performance Indices
#' @param verbose if TRUE, it prints details of the current simulation
#'
#' @return 2 arrays: R (discharges) and S (indices).
#' 
#' @export
#'
#' @examples
#' # MCsimulations(DATA,deltim,warmup,parameters,ListOfModels,SimulationFolder,MPIs)
#'

MCsimulations <- function(DATA,
                          parameters,
                          deltim = 1,
                          warmup = 0,
                          ListOfModels=1:1248,
                          SimulationFolder = getwd(),
                          MPIs = NULL,
                          verbose = TRUE){

  if (is.null(MPIs)) {

    message("Default MPIs are used, see documentation for more information")

    LAGTIME = function(x) tiger::lagtime(x$Qo, x$Qs)    
    MAE     = function(x) mean(x$Qs - x$Qo, na.rm = TRUE)            
    NSHF    = function(x) 1 - qualV::EF(x$Qo, x$Qs)           
    NSLF    = function(x) 1 - qualV::EF(log(x$Qo), log(x$Qs))           
    RR      = function(x) sum(x$Qs) / sum(x$Po)

    MPIs <- list("LAGTIME"=LAGTIME,"MAE"=MAE,"NSHF"=NSHF,"NSLF"=NSLF,"RR"=RR)

  }

  for (i in 1:length(ListOfModels)){

    mid <- ListOfModels[i]
    indices <- data.frame(matrix(NA,
                                 nrow=dim(parameters)[1],
                                 ncol=length(MPIs) ))
    discharges <- data.frame(matrix(NA,
                                    nrow=dim(parameters)[1],
                                    ncol=dim(DATA)[1]-warmup))

    for (pid in 1:dim(parameters)[1]){

      if (verbose == TRUE) {
        print(paste("Current configuration: model ",
                    mid," - parameter set ",pid))
      }

      ParameterSet <- as.list(parameters[pid,])

      q_routed <- fuse(DATA = DATA,
                       mid = mid,
                       deltim = deltim,
                       ParameterSet = ParameterSet)

      x <- data.frame( Po = DATA[(warmup + 1):dim(DATA)[1],"P"],
                       Qo = DATA[(warmup + 1):dim(DATA)[1],"Q"],
                       Qs = q_routed[(warmup + 1):dim(DATA)[1]] )

      y <- lapply(MPIs, function(f) sapply(list(x), function(d) f(d) ) )

      indices[pid,] <- as.numeric(as.character(y))

      discharges[pid,] <- q_routed[(warmup + 1):dim(DATA)[1]]

    }

    names(indices) <- names(MPIs)

    save(indices, discharges, file = paste(SimulationFolder,
                                           "/MID_",mid,".Rdata",sep=""))

  }

}


#' Run the AMCA algorithm.
#'
#' @param DATA This is a data.frame containing the observed time series (zoo objects). It is structured into three columns: P containing precipitation, E containing potential evapotranspiration and Q containing streamflow discharge.
#' @param ResultsFolder Path to the folder containing results from MC simulations.
#' @param selectedModels (OPTIONAL) This is a table that contains at list 1 column named \code{mid} (list of model structures). Other informations can be added as additional columns but will be ignored (default = NULL).
#' @param warmup Percentage of initial time steps to ignore (default is 0.5 that corresponds to 50 percent of the full length).
#' @param verbose if set to TRUE it prints running information (default is FALSE).
#' @param PreSel if set to FALSE the preliminary selection step is skipped (default is TRUE).
#'
#' @return A list of suggested model configurations.
#'
#' @examples
#' # results <- amca(DATA, ResultsFolder)
#'

amca <- function(DATA, ResultsFolder,
                 selectedModels=NULL, warmup=NULL, verbose=TRUE, PreSel=TRUE){

  # For testing:
  # DATA <- readRDS("~/amca/syntheticDATA.rds")
  # ResultsFolder <- "~/amca/synthetic/"
  # selectedModels <- seq(1,1248,2)
  # warmup=NULL; verbose=TRUE; PreSel=TRUE

  # Preparing forcing inputs ###################################################
  options(warn=-1) # do not print warnings

  if ( is.null(ResultsFolder) ) {
    message("Please specify ResultsFolder.")
    stop
  }

  if ( is.null(warmup) ) {
    # period to warmup the model
    fractionWarmUp <- 0.50 # 50%
    warmup <- ifelse(fractionWarmUp==0,0,round(dim(DATA)[1]*fractionWarmUp,0))
  }
  message(paste("Using",warmup,"time steps to warmup."))

  # performance period, period for which indices were calculated
  pperiod <- (warmup + 1):dim(DATA)[1]
  observedQ <- coredata(DATA[pperiod,"Q"])

  # load list of availabe models
  ModelList <- PrepareTable()
  if (!is.null(selectedModels)) {
    ModelList <- ModelList[which(ModelList$mid %in% selectedModels),]
  }

  # Find out how many parameter sets were used by looking at the first output
  discharges <- NULL
  load(dir(ResultsFolder)[1])
  numberOfParamSets <- dim(discharges)[1]

  # Build Initial Ensemble of MPIs #############################################
  Indices <- Simulations2Indices(ModelList, ResultsFolder, numberOfParamSets,
                                    nIndices=4, verbose=TRUE)

  ### BUILD ARRAY P ############################################################
  # Convert Infs and high numbers to NA
  Indices[is.infinite(Indices)] <- NA
  Indices[Indices > 1000000] <- NA

  # The ObsIndices should all be 0. There is no need to substract the true
  # (observed) indices from the raw one + absolute value.
  # Just calculate the absolute value, then rescale between 0 and 1.
  # library(scales)
  Indices[,1,] <- rescale(abs(Indices[,1,]), to=c(0,1))
  Indices[,2,] <- rescale(abs(Indices[,2,]), to=c(0,1))
  Indices[,3,] <- rescale(Indices[,3,], to=c(0,1))
  Indices[,4,] <- rescale(Indices[,4,], to=c(0,1))

  ### PRELIMINARY SELECTION ####################################################
  myThreshold <- SetThreshold(ModelList, Indices, verbose=TRUE)
  PreSelReal <- PreSelection(ModelList, Indices, threshold = myThreshold)

  PreSelTable <- SelectIndices(PreSelReal, Indices, ModelList)

  ### PARETO FRONTIER ##########################################################
  # library(emoa)
  PF <- ParetoFrontier(PreSelTable)

  ### REDUNDANCY REDUCTION #####################################################
  # library(som)
  # library(dtw)
  REtable <- RedundancyReduction(PF, observedQ = DATA$Q[pperiod], verbose=TRUE)

  # REVIEW #####################################################################
  IE <- BuildEnsemble(observedQ, ResultsFolder,
                      ModelList$mid, 1:length(numberOfParamSets))
  RE <- BuildEnsemble(observedQ, ResultsFolder, REtable$mid, REtable$PIDs)
  reviewCoefficients <- review(IE, RE)

  return(list("IE" = IE, "RE" = RE, "reviewCoefficients" = reviewCoefficients))

}

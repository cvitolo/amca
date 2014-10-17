#' Run the AMCA algorithm.
#'
#' @param DATA This is a data.frame containing the observed time series (zoo objects).
#'             It is structured into three columns:
#'             "P" containing precipitation,
#'             "E" containing evapo-transpiration and
#'             "Q" containing streamflow discharge.
#' @param parameters This is a named data frame containing the parameter table, where each column corresponds to a parameter and each row to a realization.
#' @param MPIs list of functions describing the Model performance Indices.
#' @param ResultsFolder Path to the folder containing results from MC simulations.
#' @param selectedModels (OPTIONAL) This is a table that contains at list 1 column named "mid" (list of model structures). Other informations can be added as additional columns but will be ignored (default = NULL).
#' @param warmup Number of initial time steps to ignore (default = 0).
#' @param verbose if set to TRUE it prints running information (default = FALSE).
#'  @param PreSel if set to FALSE the preliminary selection step is skipped (default = TRUE).
#'
#' @return A list of objects to infer.
#'
#' @examples
#' # results <- damach( DATA, parameters, MPIs, ResultsFolder )
#'

amca <- function(DATA, parameters, MPIs, ResultsFolder,
                 selectedModels=NULL, warmup=0, verbose=FALSE,
                 PreSel=TRUE){

  # options(warn=-1) # do not print warnings

  # load list of availabe models
  load(system.file("data/modlist.rda", package = "fuse"))
  modlist <- modlist # to remove NOTE in R CMD check
  ModelList <- PrepareTable() ; rm(modlist)

  if (!is.null(selectedModels)){

    selectedRows <- which(ModelList$mid %in% selectedModels)
    ModelList <- ModelList[selectedRows,]

  }else{

    if (all(parameters$rferr_add==0) & all(parameters$rferr_mlt==1)){
      selectedRows <- which(ModelList$rferr==11)
      ModelList <- ModelList[selectedRows,]
    }

  }

  pperiod <- (warmup + 1):dim(DATA)[1] # performance period

  observedQ <- coredata(DATA[pperiod,"Q"])

  #***************************************************************************
  # message("CALCULATING TRUE VALUES FOR INDICES...")
  #***************************************************************************
  x <- data.frame( Po = coredata(DATA[pperiod,"P"]),
                   Qo = observedQ,
                   Qs = observedQ )

  ObsIndices <- lapply(MPIs, function(f) sapply(list(x), function(d) f(d) ) )

  nParams <- dim(parameters)[1]        # number of parameters
  nModels <- dim(ModelList)[1]         # number of model structures
  nIndices <- length(ObsIndices)       # number of model performance indices

  AllRealisations <- data.frame(table("mid"=rep(ModelList[,"mid"], nParams),
                                      "pid"=rep(seq(1:nParams), nModels) ) )

  #***************************************************************************
  message("GENERATING THE INITIAL ENSEMBLE FROM THE RESULT SPACE...")
  #***************************************************************************

  arrayP <- Simulations2Indices(ModelList,
                                ResultsFolder,
                                nParams,
                                nIndices,
                                verbose)

  # TRANSFORM arrayP SO THAT ALL INDICES ARE 0-BASED IN THE RANGE [0,1]
  Indices <- RescaleIndices(arrayP, ObsIndices)

  BoundsIE <- BuildEnsemble(DATA,
                            warmup,
                            AllRealisations,
                            ResultsFolder,
                            maxminOnly = TRUE,
                            verbose)

  if ( PreSel == TRUE ){

    #***************************************************************************
    message("PRELIMINARY SELECTION...")
    #***************************************************************************
    # Pre-selection mode
    selectM <- TRUE
    selectP <- FALSE

    myThreshold <- SetThreshold(ModelList, Indices, verbose, selectM, selectP)

    message(paste("Automatically generated threshold:", myThreshold))

    PreSelRealisations <- PreSelection(ModelList, Indices, myThreshold,
                                       selectM, selectP)

    PreSelTable <- ExtendTable(PreSelRealisations, ModelList, Indices,
                               parameters, ObsIndices, verbose)

    message(paste("Selected models: ",
                  length(unique(PreSelTable$mid)),
                  " - Selected params: ",
                  length(unique(PreSelTable$pid)),sep=""))

    BoundsPreSel <-  BuildEnsemble(DATA,
                                   warmup,
                                   PreSelTable,
                                   ResultsFolder,
                                   maxminOnly=FALSE,
                                   lowerP=0.05, upperP=0.95,
                                   verbose)

  }else{

    myThreshold <- NULL

    PreSelTable <- ExtendTable(AllRealisations, ModelList,
                               Indices, parameters, ObsIndices, verbose)
    BoundsPreSel <- BoundsIE

  }

  #***************************************************************************
  message("PARETO FRONTIER...")
  #***************************************************************************
  ParetoFrontTable <- ParetoFrontier(Indices, PreSelTable, ObsIndices)

  message(paste("Selected models: ",
                length(unique(ParetoFrontTable$mid)),
                " - Selected params: ",
                length(unique(ParetoFrontTable$pid)),sep=""))

  BoundsPF <- BuildEnsemble(DATA,
                            warmup,
                            ParetoFrontTable,
                            ResultsFolder,
                            maxminOnly=FALSE,
                            lowerP=0.05, upperP=0.95,
                            verbose)

  if (dim(ParetoFrontTable)[1]==1){

    message("The Pareto Front is made of 1 realization: MID =",
            ParetoFrontTable[1], "and PID =", ParetoFrontTable[2])

    MySOM <- NA
    DTWTable <- NA
    RETable <- NA
    BoundsRE <- NA
    reviewCoefficients <- NA

  }else{

    #*************************************************************************
    message("CLUSTER ANALYSIS with SOMs...")
    #*************************************************************************

    dimX <- ceiling(sqrt(dim(ParetoFrontTable)[1]))
    dimY <- ceiling(dim(ParetoFrontTable)[1]/dimX)

    message(paste("Automatically generated SOM's dimensions: dimX =",
                  dimX,
                  " - dimY = ",
                  dimY,sep=""))

    unlistedPF <- unlist(ParetoFrontTable[,names(ObsIndices)])
    temp <- matrix(as.numeric(as.character(unlistedPF)),
                   nrow = dim(ParetoFrontTable)[1],
                   ncol = length(names(ObsIndices)),
                   byrow = TRUE)

    the.som <- som(temp,
                   xdim=dimX, ydim=dimY,
                   init="linear",neigh="gaussian",topol="rect")

    #*************************************************************************
    message("REDUNDANCY REDUCTION, SIMILARITY SEARCH with DTW...")
    #*************************************************************************

    # Calculate the Ensemble with a non recursive SOM method + DTW

    DTWTable <- ParetoFrontierDTW(ParetoFrontTable,
                                  DATA,
                                  the.som,
                                  ResultsFolder,
                                  warmup)

    #calculate Filtered Ensemble

    RETable <- RedundancyReductionDTW(DTWTable)

    message(paste("Selected models: ",
                  length(unique(RETable$mid)),
                  " - Selected params: ",
                  length(unique(RETable$pid)),sep=""))

    BoundsRE <- BuildEnsemble(DATA,
                              warmup,
                              RETable,
                              ResultsFolder,
                              maxminOnly=FALSE,
                              lowerP=0.05, upperP=0.95,
                              verbose)

  }

  #***************************************************************************
  # message("DaMACH algorithm terminated")
  #***************************************************************************

  #***************************************************************************
  message("Review coefficients:")
  #***************************************************************************

  reviewCoefficients <- review(BoundsIE, BoundsRE, observedQ, type = "P")

  return(list("arrayP"=arrayP,
              "Indices"=Indices,
              "AllRealisations"=AllRealisations,
              "BoundsIE"=BoundsIE,
              "threshold"=myThreshold,
              "PreSelTable" = PreSelTable,
              "BoundsPreSel"=BoundsPreSel,
              "ParetoFrontTable"=ParetoFrontTable,
              "BoundsPF"=BoundsPF,
              "MySOM"=the.som,
              "DTWTable"=DTWTable,
              "RETable"=RETable,
              "BoundsRE"=BoundsRE,
              "reviewCoefficients"=reviewCoefficients) )

}

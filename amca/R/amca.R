#' AMCA algorithm.
#'
#' @param DATA This is a data.frame containing the observed time series (zoo objects). It is structured into three columns containing: precipitation (P), potential evapo-transpiration (E) and streamflow discharge (Q).
#' @param parameters This is a named data frame containing the parameter table, where each column corresponds to a parameter and each row to a realization.
#' @param MPIs list of functions describing the Model performance Indices.
#' @param ResultsFolder Path to the folder containing results from MC simulations.
#' @param selectedModels (OPTIONAL) This is a table that contains at list 1 column named "mid" (list of model structures). Other informations can be added as additional columns but will be ignored (default = NULL).
#' @param warmup Number of initial time steps to ignore (default = 10 percent of the length of DATA).
#' @param verbose if set to TRUE it prints running information (default = FALSE).
#' @param PreSel if set to FALSE the preliminary selection step is skipped (default = TRUE).
#' @param allBounds if set to TRUE it calculates ensembles of intermediate steps, if set to FALSE only BoundIE and BoundsRE are calculated (default = FALSE). In this case the MC simulations should produce 2 ojects: indices and simulates discharge table.
#'
#' @return A list of objects to infer.
#'
#' @examples
#' # results <- damach( DATA, parameters, MPIs, ResultsFolder )
#'

amca <- function(DATA, parameters, MPIs, ResultsFolder,
                 selectedModels=NULL, warmup=NULL, verbose=FALSE,
                 PreSel=TRUE, allBounds=FALSE){

  #*****************************************************************************
  message("###################################################################")
  message("")
  #*****************************************************************************

  if ( any(is.na(DATA)) ) {
    message("DATA contains NA's, please remove them before running AMCA.")
    stop
  }

  options(warn=-1) # do not print warnings
  deltim <- as.numeric( difftime(index(DATA)[2], index(DATA)[1], units="days") )

  if ( is.null(warmup) ) {
    warmup <- round(dim(DATA)[1]/10,0) # default warmup = 10% of DATA's length
  }

  # load list of availabe models
  ModelList <- PrepareTable()

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

  #*****************************************************************************
  message("CALCULATING TRUE VALUES FOR INDICES")
  #*****************************************************************************
  x <- data.frame( Po = coredata(DATA[pperiod,"P"]),
                   Qo = observedQ,
                   Qs = observedQ )

  ObsIndices <- lapply(MPIs, function(f) sapply(list(x), function(d) f(d) ) )

  nParams <- dim(parameters)[1]        # number of parameters
  nModels <- dim(ModelList)[1]         # number of model structures
  nIndices <- length(ObsIndices)       # number of model performance indices

  AllRealisations <- data.frame(table("mid"=rep(ModelList[,"mid"], nParams),
                                      "pid"=rep(seq(1:nParams), nModels) ) )

  message(paste("True Indices: LAGTIME = ",ObsIndices[[1]],
                ", MAE = ", ObsIndices[[2]],
                ", NSHF = ", ObsIndices[[3]],
                ", NSLF = ", ObsIndices[[4]],
                ", RR = ", round(ObsIndices[[5]],2),
                sep=""))

  message("")

  #*****************************************************************************
  message("GENERATING THE INITIAL ENSEMBLE FROM THE RESULT SPACE")
  #*****************************************************************************

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
                            lowerP=0.05, upperP=0.95,
                            verbose)

  message(paste("IE's dimensions =", dim(Indices)[1], "x",
                dim(Indices)[2], "x",
                dim(Indices)[3]))
  message("")

  if ( PreSel == TRUE ){

    #***************************************************************************
    message("PRELIMINARY SELECTION")
    #***************************************************************************
    # Pre-selection mode
    selectM <- TRUE
    selectP <- FALSE

    myThreshold <- SetThreshold(ModelList, Indices, selectM, selectP,
                                type="independent", verbose)

    message(paste("Automatically generated threshold:", myThreshold))

    PreSelRealisations <- PreSelection(ModelList, Indices, myThreshold,
                                       selectM, selectP)

    temp <- ExtendTable(PreSelRealisations, ModelList, Indices,
                        parameters, ObsIndices, verbose)

    PreSelTable <- data.frame(lapply(temp, as.character),stringsAsFactors=FALSE)

    message(paste("Selected models: ",length(unique(PreSelTable$mid)),
                  " - Selected params: ",length(unique(PreSelTable$pid)),
                  sep=""))

    message("")

    if (allBounds == TRUE) {
      BoundsPreSel <-  BuildEnsemble(DATA,
                                     warmup,
                                     PreSelTable,
                                     ResultsFolder,
                                     maxminOnly=FALSE,
                                     lowerP=0.05, upperP=0.95,
                                     verbose)
    }else{
      BoundsPreSel <- NULL
    }

  }else{

    myThreshold <- NULL

    temp <- ExtendTable(AllRealisations, ModelList,
                        Indices, parameters, ObsIndices, verbose)

    PreSelTable <- data.frame(lapply(temp, as.character),
                              stringsAsFactors=FALSE)

    BoundsPreSel <- BoundsIE

  }

  #*****************************************************************************
  message("PARETO FRONTIER")
  #*****************************************************************************
  temp <- ParetoFrontier(PreSelTable, ObsIndices)

  ParetoFrontTable <- data.frame(lapply(temp, as.character),
                                 stringsAsFactors=FALSE)

  message(paste("Selected models: ",
                length(unique(ParetoFrontTable$mid)),
                " - Selected params: ",
                length(unique(ParetoFrontTable$pid)),sep=""))

  if (allBounds == TRUE) {
    BoundsPF <- BuildEnsemble(DATA,
                              warmup,
                              ParetoFrontTable,
                              ResultsFolder,
                              maxminOnly=FALSE,
                              lowerP=0.05, upperP=0.95,
                              verbose)
  }else{
    BoundsPF <- NULL
  }

  message("")

  if (dim(ParetoFrontTable)[1]==1){

    message("The Pareto Front is made of 1 realization: MID =",
            ParetoFrontTable[1], "and PID =", ParetoFrontTable[2])

    MySOM <- NA
    RETable <- NA
    BoundsRE <- NA
    reviewCoefficients <- NA

  }else{

    #***************************************************************************
    message("CLUSTER ANALYSIS with SOMs")
    #***************************************************************************

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
                   byrow = FALSE)

    the.som <- som(temp,
                   xdim=dimX, ydim=dimY,
                   init="linear",neigh="gaussian",topol="rect")

    message("")

    #***************************************************************************
    message("REDUNDANCY REDUCTION, SIMILARITY SEARCH with DTW")
    #***************************************************************************

    # Calculate the Filtered Ensemble with a non recursive SOM method + DTW

    temp <- RedundancyReduction(ParetoFrontTable,DATA,the.som,
                                parameters,observedQ,deltim,pperiod,verbose)

    RETable <- data.frame(lapply(temp$table, as.character),
                          stringsAsFactors=FALSE)

    bounds <- data.frame("Dates"=index(DATA$P)[pperiod],
                           "Qobs"=DATA$Q[pperiod],
                           "LB"=apply(temp$discharges, 2, min),
                           "UB"=apply(temp$discharges, 2, max),
                           "LP"=apply(temp$discharges, 2, quantile, probs = 0.05),
                           "UP"=apply(temp$discharges, 2, quantile, probs = 0.95))

    BoundsRE <- list("discharges" = temp$discharges, "bounds" = bounds)

    message(paste("Selected models: ", length(unique(RETable$mid)),
                  " - Selected params: ", length(unique(RETable$pid)),sep=""))
  }

  message("")

  #*****************************************************************************
  message("REVIEW COEFFICIENTS")
  #*****************************************************************************

  reviewCoefficients <- review(BoundsIE, BoundsRE, observedQ, type = "P")

  #*****************************************************************************
  message("")
  message("###################################################################")
  #*****************************************************************************

  return(list("arrayP"             = arrayP,
              "Indices"            = Indices,
              "AllRealisations"    = AllRealisations,
              "BoundsIE"           = BoundsIE,
              "threshold"          = myThreshold,
              "PreSelTable"        = PreSelTable,
              "BoundsPreSel"       = BoundsPreSel,
              "ParetoFrontTable"   = ParetoFrontTable,
              "BoundsPF"           = BoundsPF,
              "MySOM"              = the.som,
              "RETable"            = RETable,
              "BoundsRE"           = BoundsRE,
              "reviewCoefficients" = reviewCoefficients) )

}

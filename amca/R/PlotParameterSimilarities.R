#' Plot prior and posterior parameter distributions (for comparison).
#'
#' @param RealisationsTable list of the filtered realisations
#' @param parameters This is a named data frame containing the parameter table, where each column corresponds to a parameter and each row to a realization.
#' @param selectedParams is a list of pid to filter
#' @param synparameters is the synthetic parameter set (if available)
#'
#' @return A plot comparing prior and posterior parameter distributions.
#'
#' @examples
#' # Plot_ParameterSimilarities(RealisationsTable,selectedParams="",synparameters)
#'

PlotParameterSimilarities <- function(RealisationsTable,
                                      parameters,
                                      selectedParams=NULL,
                                      synparameters=NULL){

  # Colorblind-friendly palette with grey:
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  ParameterNames <- names(parameters)[3:24]

  # Chi-square test
  chi2IsUniform<-function(dataset,significance=0.05){
    chi2IsUniform=(chisq.test(dataset)$p.value>significance)
  }

  if (is.null(selectedParams)) {

    ParamTable <- RealisationsTable[,ParameterNames]

  }else{

    ParamTable <- RealisationsTable[,selectedParams]

  }

  NewParamTable <- data.frame(matrix(NA,ncol=dim(ParamTable)[2],nrow=dim(ParamTable)[1]))

  i <- 0
  for (col in 1:dim(ParamTable)[2]){
    print("****************************************")
    print("")
    print(paste("PARAMETER: ",names(ParamTable)[col],sep=""))
    Pdistr <- as.numeric(as.character(ParamTable[!is.na(ParamTable[,col]),col]))
    if ( length(Pdistr)<6 ) {
      print("Too many NA! This parameter distribution will be discarded.")
      NewParamTable <- NewParamTable[,1:dim(NewParamTable)[2]]
    }else{
      print(chisq.test(Pdistr))
      print(paste("uniform?",chi2IsUniform(Pdistr)))
      i <- i + 1
      NewParamTable[,i] <- as.numeric(as.character(ParamTable[,col]))
      names(NewParamTable)[i] <- names(ParamTable)[col]
    }
  }

  for (newCol in 1:dim(NewParamTable)[2]){
    if (all(is.na(NewParamTable[,newCol]))) NewParamTable <- NewParamTable[,-newCol]
  }

  #NewParamTable <- NewParamTable[,which(!is.na(NewParamTable[1,]))]
  newparams <- NewParamTable
  oldparams <- parameters[,names(NewParamTable)]

  newp <- cbind("Type"="POSTERIOR",newparams)   #,"exact"=NA)
  oldp <- cbind("Type"="PRIOR",oldparams)   # "exact"=NA)

  # To compare parameter distributions one-by-one:
  # compare_params(newparams,oldparams)

  # To compare parameter distributions together in a single plot:
  mergeparams <- melt( rbind(oldp,newp), id=c("Type") )

  Type <- NULL
  value <- NULL

  if (!is.null(synparameters)) {

    myValue <- as.numeric(synparameters[selectedParams])
    trueValues <- data.frame("variable" = selectedParams,
                             "value" = myValue)

    p <- ggplot( mergeparams, aes(value)) +
      geom_density(aes(value,fill=Type),alpha = 0.5) +
      facet_wrap(~variable, scales = "free") +   # labeller = label_parsed
      geom_point(data = trueValues, y = 0, color="red", size = 3) +
      xlab("") + ylab("") +
      scale_fill_manual( values=cbPalette ) +
      theme_bw() +
      theme( legend.title = element_blank(),
             legend.background = element_blank(),
             legend.position="bottom",
             axis.text.x = element_text(colour="grey20",size=11),
             axis.text.y = element_text(colour="grey20",size=13),
             legend.text = element_text(size=20),
             strip.text.x = element_text(size = 20),
             strip.text.y = element_text(size = 20) )

  }else{

    p <- ggplot( mergeparams, aes(value)) +
      geom_density(aes(value,fill=Type),alpha = 0.5) +
      facet_wrap(~variable, scales = "free") +   # labeller = label_parsed
      xlab("") + ylab("") +
      scale_fill_manual( values=cbPalette ) +
      theme_bw() +
      theme( legend.title = element_blank(),
             legend.background = element_blank(),
             legend.position="bottom",
             axis.text.x = element_text(colour="grey20",size=11),
             axis.text.y = element_text(colour="grey20",size=13),
             legend.text = element_text(size=20),
             strip.text.x = element_text(size = 20),
             strip.text.y = element_text(size = 20) )

  }

  print(p)

}

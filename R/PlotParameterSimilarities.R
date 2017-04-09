#' Plot prior and posterior parameter distributions (for comparison).
#'
#' @param RealisationsTable list of the filtered realisations
#' @param parameters This is a named data frame containing the parameter table, where each column corresponds to a parameter and each row to a realization.
#' @param selectedParams is a list of pid to filter
#' @param synparameters is the synthetic parameter set (if available)
#' @param labelPRE label for the prior
#' @param labelPOST label for the posterior
#'
#' @return A plot comparing prior and posterior parameter distributions.
#'
#' @examples
#' # PlotParameterSimilarities(RealisationsTable, parameters)
#'

PlotParameterSimilarities <- function(RealisationsTable,
                                      parameters,
                                      selectedParams=NULL,
                                      synparameters=NULL,
                                      labelPRE = "PRIOR",
                                      labelPOST = "POSTERIOR"){

  # Colorblind-friendly palette with grey:
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  ParameterNames <- names(parameters)[3:24]

  # Chi-square test
  #chi2IsUniform<-function(dataset,significance=0.05){
  #  chi2IsUniform=(chisq.test(dataset)$p.value>significance)
  #}

  if (is.null(selectedParams)) {

    ParamTable <- RealisationsTable[,ParameterNames]

  }else{

    ParamTable <- RealisationsTable[,selectedParams]

  }

  NewParamTable <- data.frame(matrix(NA,
                                     ncol=dim(ParamTable)[2],
                                     nrow=dim(ParamTable)[1]))

  i <- 0
  for (col in 1:dim(ParamTable)[2]){
    print(paste("PARAMETER: ",names(ParamTable)[col],sep=""))
    Pdistr <- as.numeric(as.character(ParamTable[!is.na(ParamTable[,col]),col]))
    if ( length(Pdistr)<6 ) {
      print("Too many NA! This parameter distribution will be discarded.")
      NewParamTable <- NewParamTable[,1:dim(NewParamTable)[2]]
    }else{
      print(paste("uniform?", ifelse(chisq.test(Pdistr)$p.value < 0.05, T, F)))
      i <- i + 1
      NewParamTable[,i] <- as.numeric(as.character(ParamTable[,col]))
      names(NewParamTable)[i] <- names(ParamTable)[col]
    }
  }

  col2remove <- c()
  for (newCol in 1:dim(NewParamTable)[2]){
    if (all(is.na(NewParamTable[,newCol]))) {
      col2remove <- append(col2remove,newCol)
    }
  }
  if ( length(col2remove)>0 ) NewParamTable <- NewParamTable[,-col2remove]

  newparams <- NewParamTable
  oldparams <- parameters[,names(NewParamTable)]

  newp <- cbind("Type"=labelPOST,newparams)
  oldp <- cbind("Type"=labelPRE,oldparams)

  # To compare parameter distributions together in a single plot:
  mergeparams <- melt( rbind(oldp,newp), id=c("Type") )

  Type <- NULL
  value <- NULL

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

  if (!is.null(synparameters)) {

    label <- NULL
    synparameters[synparameters==-999] <- NA
    myValue <- as.numeric(synparameters[names(NewParamTable)])
    trueValues <- data.frame("variable" = names(NewParamTable),
                             "value" = myValue,
                             "label" = "Synthetic Values")

    p <- p +
      geom_point(data = trueValues,
                 aes(x=value, y=0, colour= label), size=3)

  }

  print(p)

}

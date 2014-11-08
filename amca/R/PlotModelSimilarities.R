#' Plot model similarities (for comparison).
#'
#' @param ModelList This is a table which contains at least 1 colum named "mid". Other informations can be added as additional columns but will be ignored.
#' @param EnsembleTable table containing alle the combination of mid and pid selected so far
#' @param synMID if running synthetic experiments, this is the synthetic MID number (otherwise leave it empty)
#' @param plotType default is "frequency" (alternative: "persistence")
#' @param myTitle title of the plot
#' @param modellingOptions is a vector of labels to specify which modelling options should be used. If set to NULL, it uses all the modelling options.
#'
#' @return A plot comparing model similarities.
#'
#' @examples
#' # PlotModelSimilarities(ModelList,results$RETable)
#'

PlotModelSimilarities <- function(ModelList,EnsembleTable,synMID="",
                                  plotType="frequency",myTitle="",
                                  modellingOptions=NULL){

  if (is.null(modellingOptions)) {
    modellingOptions <- c("rferr","arch1","arch2",
                          "qsurf","qperc","esoil","qintf","q_tdh")
  }

  if (plotType=="frequency"){

    dfF <- RelativeFrequency(EnsembleTable,modellingOptions)

    p <- ggplot(dfF,aes(x=factor(Option),
                        y=RelativeFrequency)) +
      geom_bar(stat="identity",position="dodge",fill="grey", colour="grey") +
      facet_grid(~Decision, scales = "free") +
      theme(legend.position="none") +
      xlab("") + ylab("Relative Frequency") + ggtitle(myTitle) +
      theme_bw() +
      theme(plot.title = element_text(face="bold", size=20),
            axis.text.x = element_text(colour="grey20",size=12,
                                       angle = 90, hjust = 1),
            axis.text.y = element_text(colour="grey20",size=13),
            legend.text = element_text(size = 13),
            strip.text.x = element_text(size = 13),
            strip.text.y = element_text(size = 13) )

  }

  if (plotType=="persistence"){

    dfP <- PersistenceRate(EnsembleTable,ModelList,modellingOptions)

    p <- ggplot(dfP,aes(x=factor(Option),
                        y=PersistenceRate)) +
      geom_bar(stat="identity",position="dodge",fill="grey", colour="grey") +
      facet_grid(~Decision, scales = "free")+
      theme(legend.position="none") +
      xlab("") + ylab("Persistence Rate") + ggtitle(myTitle) +
      theme_bw() +
      theme(plot.title = element_text(face="bold", size=20),
            axis.text.x = element_text(colour="grey20",size=12,
                                       angle = 90, hjust = 1),
            axis.text.y = element_text(colour="grey20",size=13),
            legend.text = element_text(size = 13),
            strip.text.x = element_text(size = 13),
            strip.text.y = element_text(size = 13) )

  }

  print(p)

}

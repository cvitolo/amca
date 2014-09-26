#' Plot model similarities (for comparison).
#'
#' @param ModelList This is a table which contains at least 1 colum named "mid". Other informations can be added as additional columns but will be ignored.
#' @param EnsembleTable table containing alle the combination of mid and pid selected so far
#' @param synMID if running synthetic experiments, this is the synthetic MID number (otherwise leave it empty)
#' @param plotType default is "frequency", TODO: "persistency"
#' @param myTitle title of the plot
#'
#' @return A plot comparing model similarities.
#'
#' @examples
#' # PlotModelSimilarities(ModelList,EnsembleTable)
#'

PlotModelSimilarities <- function(ModelList,EnsembleTable,
                                  synMID="",plotType="frequency",myTitle=""){

  # require(fuse)

  variable <- c("arch1","arch1","arch1",
               "arch2","arch2","arch2","arch2",
               "qsurf","qsurf","qsurf",
               "qperc","qperc","qperc",
               "esoil","esoil",
               "qintf","qintf",
               "q_tdh","q_tdh")

  value <- c("onestate_1","tension1_1","tension2_1",
             "fixedsiz_2","tens2pll_2","unlimfrc_2","unlimpow_2",
             "arno_x_vic","prms_varnt","tmdl_param",
             "perc_f2sat","perc_lower","perc_w2sat",
             "rootweight","sequential",
             "intflwnone","intflwsome",
             "no_routing","rout_gamma")

  allOptions <- data.frame(variable, value)

  if (synMID != "") {

    variable0 = c("arch1","arch2","qsurf","qperc","esoil","qintf","q_tdh")
    value0 = readmd2var(synMID)[c("arch1","arch2","qsurf",
                                  "qperc","esoil","qintf","q_tdh")]

    trueValues <- data.frame(variable = variable0, value = value0)

  }else{

    trueValues <- NULL

  }

  List2Plot <- EnsembleTable[,c("mid","pid")]
  tableMS <- matrix(NA,nrow=dim(List2Plot)[1],ncol=9)

  MIDs <- as.numeric(as.character(unique(List2Plot$mid)))

  for (mid in MIDs) {
    #print(mid)
    j <- which(List2Plot$mid == mid)
    tableMS[j,1:2] <- as.matrix(List2Plot[j,],ncol=2)
    tableMS[j,3] <- readmd2var(mid,number=FALSE,text=TRUE)["arch1"]
    tableMS[j,4] <- readmd2var(mid,number=FALSE,text=TRUE)["arch2"]
    tableMS[j,5] <- readmd2var(mid,number=FALSE,text=TRUE)["qsurf"]
    tableMS[j,6] <- readmd2var(mid,number=FALSE,text=TRUE)["qperc"]
    tableMS[j,7] <- readmd2var(mid,number=FALSE,text=TRUE)["esoil"]
    tableMS[j,8] <- readmd2var(mid,number=FALSE,text=TRUE)["qintf"]
    tableMS[j,9] <- readmd2var(mid,number=FALSE,text=TRUE)["q_tdh"]
  }
  tableMS <- data.frame(tableMS)
  names(tableMS) <- c("mid","pid","arch1","arch2","qsurf","qperc","esoil","qintf","q_tdh")

  listoffactors <- c("onestate_1","tension1_1","tension2_1",
                     "fixedsiz_2","tens2pll_2","unlimfrc_2","unlimpow_2",
                     "arno_x_vic","prms_varnt","tmdl_param",
                     "perc_f2sat","perc_lower","perc_w2sat",
                     "rootweight","sequential",
                     "intflwnone","intflwsome",
                     "no_routing","rout_gamma")

  MeltedTable <- melt(tableMS,id=c("mid","pid"))

  if (plotType=="frequency"){

    p <- ggplot( MeltedTable, aes(value) ) +
      geom_bar(aes(value),alpha = 0.2) +
      facet_grid(~variable, scales = "free") +
      geom_point(data = trueValues, y = 0, color="red", size = 3) +
      geom_point(data = allOptions, y = 0, color=NA, size = 3) +
      theme(legend.position="none") +
      xlab("") + ylab("") + ggtitle(myTitle) +
      scale_x_discrete(limits=levels(listoffactors)) +
      theme_bw() +
      theme(plot.title = element_text(face="bold", size=20),
            #panel.margin = unit(0.1, "in"),
            axis.text.x = element_text(colour="grey20",size=12,
                                       angle = 90, hjust = 1),
            axis.text.y = element_text(colour="grey20",size=13),
            legend.text = element_text(size=13),
            strip.text.x = element_text(size = 13),
            strip.text.y = element_text(size = 13) )

  }

  if (plotType=="persistency"){

    newTable <- data.frame(matrix(NA,nrow=17,ncol=4))
    names(newTable) <- c("Switch","Option","Value","TrueOption")
    rowN <- 1
    # persistency rate
    for (switchOption in c("arch1","arch2","qsurf","qperc","esoil","qintf")){
      for (i in 1:length(names(table(tableMS[,switchOption])))){
        newTable[rowN,"Switch"] <- switchOption
        newTable[rowN,"Option"] <- names(table(tableMS[,switchOption]))[i]
        # Rate of Persistency
        x1 <- table(ModelList[,switchOption])[i]
        # x2 <- table(EnsembleTable[,switchOption])[i]
        x2 <- table(tableMS[,switchOption])[i]
        newTable[rowN,"Value"] <- x2/sum(table(tableMS[,switchOption])) - x1/sum(table(ModelList[,switchOption]))
        if (newTable[rowN,"Option"] %in% trueValues$value) {
          newTable[rowN,"TrueOption"] <- TRUE
        }else{
          newTable[rowN,"TrueOption"] <- FALSE
        }
        rowN <- rowN + 1
      }
    }

    Option <- NULL
    TrueOption <- NULL
    Value <- NULL
    p <- ggplot(newTable, aes(Option,fill=TrueOption)) +
      geom_bar(aes(Option,Value), stat="identity", position="dodge") +
      facet_grid(~Switch, scales = "free") +
      xlab("") + ylab("")+
      theme_bw() +
      theme(#panel.margin = unit(0.5, "in"),
        axis.text.x = element_text(colour="grey20",size=13, angle=0),
        axis.text.y = element_text(colour="grey20",size=13),
        legend.text = element_text(size=13),
        strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 13) ) +
      ggtitle(myTitle)

  }

  print(p)

}

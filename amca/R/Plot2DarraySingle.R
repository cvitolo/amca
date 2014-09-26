#' This function plots the 2Darray of a certain index.
#'
#' @param indices arrayP containing the performance measures for all the realisations
#' @param mList This is a table which contains at least 1 colum named "mid". Other informations can be added as additional columns but will be ignored.
#' @param idx is the index number
#'
#' @return A matrix plot with color coded cells.
#'
#' @examples
#' # Plot2DarraySingle(Indices,mList,idx)
#'

Plot2DarraySingle <- function(indices,mList,idx){

  # Colorblind-friendly palette with grey:
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  indicesNames <- c("LAGTIME", "MAE","NSHF","NSLF","RR")

  mset <- as.numeric(as.character(unique(mList$mid)))
  pset <- as.numeric(as.character(unique(mList$pid)))

  x <- data.frame(table("pid"=rep(pset,length(mset)),"mid"=rep(mset,length(pset))))[,1:2]

  p <- melt(indices[,idx,])
  x <- cbind(x,p[,3])

  names(x) <- c("PID","MID",indicesNames[idx])

  pm <- melt(x,id=c("PID","MID"))

  modelBreaks <- seq(round(length(mset)/5,0),length(mset),round(length(mset)/5,0))
  modelLabels <- mset[modelBreaks]

  paramBreaks <- seq(round(length(pset)/5,0),length(pset),round(length(pset)/5,0))
  paramLabels <- pset[paramBreaks]

  MID <- NULL
  PID <- NULL
  value <- NULL
  ggplot(pm, aes(x=MID, y=PID)) +
    geom_tile(aes(fill = value)) +
    scale_fill_gradient(name = "",low=3, high=2) +
    scale_x_discrete(breaks=paramBreaks, labels=paramLabels) +
    scale_y_discrete(breaks=modelBreaks, labels=modelLabels) +
    ggtitle(indicesNames[idx]) +
    xlab("model structure ID number (MID)") +
    ylab("parameter set ID number (PID)") +
    theme(plot.title = element_text(lineheight=2, face="bold"))

}

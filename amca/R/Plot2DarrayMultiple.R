#' This function plots the 2Darray of a certain index.
#'
#' @param indices arrayP containing the performance measures for all the realisations
#' @param mList This is a table which contains at least 1 colum named "mid". Other informations can be added as additional columns but will be ignored.
#'
#' @return A grid of matrix plots with color coded cells.
#'
#' @examples
#' # Plot2DarrayMultiple(Indices,mList)
#'

Plot2DarrayMultiple <- function(indices,mList){

  # require(reshape2)
  # require(ggplot2)

  # Colorblind-friendly palette with grey:
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  indicesNames <- c("LAGTIME", "MAE","NSHF","NSLF")

  mset <- mList[,"mid"]
  pset <- seq(1:dim(indices)[1])

  x <- data.frame(table("pid"=rep(pset,length(mset)),
                        "mid"=rep(mset,length(pset))))[,1:2]

  for (idx in 1:dim(indices)[2]) {

    p <- melt(indices[,idx,])

    x <- cbind(x,p[,3])

  }

  names(x) <- c("PID","MID",indicesNames)

  pm <- melt(x,id=c("PID","MID"))
  tempBreaks <- c(floor(max(mList$mid)/4*1),
                  floor(max(mList$mid)/4*2),
                  floor(max(mList$mid)/4*3),
                  floor(max(mList$mid)/4*4))

  MID <- NULL
  PID <- NULL
  value <- NULL
  ggplot(pm, aes(x=MID, y=PID)) +
    geom_tile(aes(fill = value)) +
    scale_fill_gradient(name = "",low=3, high=2) +
    scale_x_discrete(breaks=tempBreaks) +
    scale_y_discrete(breaks=c(round(0.25*dim(indices)[1],0),
                              round(0.50*dim(indices)[1],0),
                              round(0.75*dim(indices)[1],0),
                              round(1*dim(indices)[1],0))) +
    xlab("\n Model structure ID number (MID)") +
    ylab("Sampled parameter sets") + # Parameter set ID number (PID)
    facet_grid( ~ variable) # + theme(text = element_text(size=24))

}

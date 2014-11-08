#' Create bar plots
#'
#' @param mTable molt table
#' @param selectedOption a selection of options
#'
#' @return bar plot
#'
#' @examples
#' # BarPlots(mTable,selectedOption)
#'

BarPlots <- function(mTable,selectedOption){

  value <- NULL # to avoid Note in check

  breaksOption <- FindBreaksLabels(selectedOption)$breaks
  labelsOption <- FindBreaksLabels(selectedOption)$labels
  titleOption  <- FindBreaksLabels(selectedOption)$title

  m <- mTable[which(mTable$variable==selectedOption),]

  m$value <- factor(m$value, levels=FindBreaksLabels(selectedOption)$breaks)

  p <- ggplot(m, aes(value)) + #, fill=value
    geom_bar() +
    facet_grid(. ~ Simulation) +
    xlab("") + theme_bw() +
    ggtitle(paste(titleOption,"\n")) +
    theme(plot.title = element_text(lineheight=1.0, face="bold",size=20)) +
    scale_x_discrete(breaks=breaksOption,
                     labels=labelsOption,
                     drop=FALSE) +
    theme(strip.text.x = element_text(size = 14),
          axis.text.x  = element_text(angle=90, vjust=0.5, size=14),
          axis.text.y  = element_text(vjust=0.5, size=14),
          axis.title.y = element_text(size=14),
          legend.position = "none")
  #legend.text = element_text(size = 14),
  #legend.title = element_text(size=16, face="bold")) +
  #scale_fill_discrete(name=titleOption,
  #                    breaks=breaksOption,
  #                    labels=labelsOption)

  print(p)

}

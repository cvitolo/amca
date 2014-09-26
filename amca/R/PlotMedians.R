#' Plot the 1 panel for each MPI, showing the median for each MID.
#'
#' @param ModelList This is a table which contains at least 1 colum named "mid". Other informations can be added as additional columns but will be ignored.
#' @param Indices arrayP containing the performance measures for all the realisations
#'
#' @return A plot showing median values for each MPI and MID.
#'
#' @examples
#' # PlotMedians <- function(ModelList,Indices)
#'

PlotMedians <- function(ModelList,Indices){

  df <- data.frame("MID"=ModelList$mid,
                   "LAGTIME"=apply(Indices[,1,],2,median),
                   "MAE"=apply(Indices[,2,],2,median),
                   "NSHF"=apply(Indices[,3,],2,median),
                   "NSLF"=apply(Indices[,4,],2,median),
                   "RR"=apply(Indices[,5,],2,median))

  dfmelt <- melt(df,id="MID")

  MID <- NULL
  value <- NULL
  ggplot(dfmelt,aes(x=MID, y=value)) +
    geom_line(size=0.3) +
    facet_grid(variable ~ .) +
    scale_x_continuous(name="MID") +
    scale_y_continuous(name="Median")

}

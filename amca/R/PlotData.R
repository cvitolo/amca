#' Plot input data
#'
#' @param DATA This is a data.frame containing the observed time series (zoo objects).
#'             It is structured into three columns:
#'             "P" containing precipitation,
#'             "E" containing evapo-transpiration and
#'             "Q" containing streamflow discharge.
#'
#' @return A plot divided into 3 panels: P, E and Q
#'
#' @examples
#' # PlotData(DATA)
#'

PlotData <- function(DATA){

  # require(reshape2)
  # require(ggplot2)
  # require(zoo)

  facetNames <- list(
    'P'='Precipitation [mm/d]',
    'E'='Pot. Evapotr. [mm/d]',
    'Q'='Discharge [mm/d]'
  )

  facetLabeller <- function(variable,value){
    return(facetNames[value])
  }

  datetime <- NULL
  value <- NULL

  x <- data.frame("datetime"=index(DATA),
                  "P"=coredata(DATA$P),
                  "E"=coredata(DATA$E),
                  "Q"=coredata(DATA$Q))

  x2 <- melt(x,id="datetime")

  ggplot(x2, aes(x=datetime, y=value)) +
    geom_bar(data=x2[x2$variable=="P", ], stat="identity") +
    geom_line(data=x2[x2$variable=="E", ]) +
    geom_line(data=x2[x2$variable=="Q", ]) +
    facet_grid(variable ~ .,scales="free_y", , labeller=facetLabeller) +
    theme_bw() + xlab("") +  ylab("")

}

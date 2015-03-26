#' Split dataset into solar years (from 1st January to the following 31st December)
#'
#' @param myDATA it is a dataframe containing three zoo time series: P (precipitation), E(evapotranspiration) and Q(discharge)
#' @param myYear number identifying a particular year to output. If this is not null, the year is returned and not saved in a file.
#'
#' @return saves in the working directory an rda file for each year
#'

SplitSolarYears <- function(myDATA, myYear=NULL){

  # split data into years
  P <- tapply(myDATA$P, format(index(myDATA), "%Y"), c)
  E <- tapply(myDATA$E, format(index(myDATA), "%Y"), c)
  Q <- tapply(myDATA$Q, format(index(myDATA), "%Y"), c)
  datetime <- tapply(index(myDATA), format(index(myDATA), "%Y"), c)

  for (y in names(datetime)){
    DATA <- data.frame("datetime"=eval(parse(text=paste("datetime$'",y,"'",sep=""))),
                       "P"=eval(parse(text=paste("P$'",y,"'",sep=""))),
                       "E"=eval(parse(text=paste("E$'",y,"'",sep=""))),
                       "Q"=eval(parse(text=paste("Q$'",y,"'",sep=""))))

    if (is.null(myYear)){
      save(DATA,file=paste("datasetSY",y,".rda",sep=""))
    }else{
      if (y==myYear) {
        return(DATA)
      }
    }

  }

}

#' Split dataset into hydrological years (from 1st October to the following 30th September)
#'
#' @param myDATA it is a dataframe containing three zoo time series: P (precipitation), E(evapotranspiration) and Q(discharge)
#' @param myYear number identifying a particular year to output. If this is not null, the year is returned and not saved in a file.
#'
#' @return saves in the working directory an rda file for each year
#'

SplitHydroYears <- function(myDATA, myYear=NULL){

  # split data into hydrological years
  datetime <- tapply(index(myDATA), format(index(myDATA), "%Y"), c)

  P <- as.xts(myDATA$P)
  E <- as.xts(myDATA$E)
  Q <- as.xts(myDATA$Q)

  counter <- 0

  for ( counter in 1:(length(names(datetime))-2) ){

    Pyear <- rbind(split(P[.indexmon(P) %in% 9:11], f="years")[[counter]],
                   split(P[.indexmon(P) %in% 0:8], f="years")[[counter + 1]])

    Eyear <- rbind(split(E[.indexmon(E) %in% 9:11], f="years")[[counter]],
                   split(E[.indexmon(E) %in% 0:8], f="years")[[counter + 1]])

    Qyear <- rbind(split(Q[.indexmon(Q) %in% 9:11], f="years")[[counter]],
                   split(Q[.indexmon(Q) %in% 0:8], f="years")[[counter + 1]])

    DATA <- merge("P"=zoo(Pyear,order.by=index(Pyear)),
                  "E"=zoo(Eyear,order.by=index(Eyear)),
                  "Q"=zoo(Qyear,order.by=index(Qyear)))

    if (is.null(myYear)){
      save(DATA,file=paste("datasetHY",names(datetime)[counter],".rda",sep=""))
    }else{
      if (names(datetime)[counter]==myYear) {
        return(DATA)
      }
    }

  }

}

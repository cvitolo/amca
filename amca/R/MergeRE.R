#' Merge Reduced Ensembles.
#'
#' @param periodRuns vector containing the years to merge
#' @param path directory path containing AMCA results
#' @param parameters data frame containing all the sampled parameters
#'
#' @return A list of objects to infer.
#'
#' @examples
#' # MergeRE( periodRuns, path )
#'

MergeRE <- function(periodRuns, path, parameters){

  start1 <- periodRuns[1]
  end1   <- periodRuns[length(periodRuns)]

  for(i in start1:end1){

    print(i)
    results <- NULL

    if ( !exists("allRE") ) {
      load(paste(path,i,".rda",sep=""))
      allRE <- results$RETable
    }else{
      load(paste(path,i,".rda",sep=""))
      allRE <- rbind(allRE,results$RETable)
    }

  }

  # only model components and params
  tableX <- allRE[,c("rferr","arch1","arch2","qsurf","qperc","esoil","qintf",
                     "q_tdh","maxwatr_1","maxwatr_2","fracten","frchzne",
                     "fprimqb","rtfrac1","percrte","percexp","sacpmlt",
                     "sacpexp","percfrac","iflwrte","baserte","qb_powr",
                     "qb_prms","qbrate_2a","qbrate_2b","sareamax",
                     "axv_bexp","loglamb","tishape","timedelay")]

  # remove all NA
  if (any(sapply(tableX, function(x)all(is.na(x))))) {
    cols2remove <- which(sapply(tableX, function(x)all(is.na(x))))
    tableX <- tableX[,-cols2remove]
  }

  # Set the number of breaks for transforming ranges to categorical variables
  numberOfBreaks <- 5

  for ( parColumn in 1:dim(tableX)[2] ) {

    print(paste("Column",parColumn))

    if (parColumn == 1){

      newTable <- data.frame(as.factor(tableX[,parColumn]))

    }else{

      nameColumn <- names(tableX)[parColumn]

      if (nameColumn %in% c("rferr","arch1","arch2","qsurf","qperc",
                            "esoil","qintf","q_tdh")){

        newTable <- cbind(newTable,as.factor(tableX[,parColumn]))

      }else{

        myCol <- eval(parse(text=paste("parameters$",nameColumn,sep="")))
        
        if (all(myCol==-999)){
          
          newTable <- cbind(newTable,rep(NA,dim(newTable)[1]))
          
        }else{
          
          myBreaks <- seq(min(myCol), max(myCol),
                          by = (max(myCol)-min(myCol))/numberOfBreaks)
          
          newTable <- cbind(newTable,cut(as.numeric(tableX[,parColumn]),
                                         breaks=myBreaks))
          
        }

      }

    }

  }

  names(newTable) <- names(tableX)
  
  # remove all NA
  if (any(sapply(newTable, function(x)all(is.na(x))))) {
    cols2remove <- which(sapply(newTable, function(x)all(is.na(x))))
    newTable <- newTable[,-cols2remove]
  }


  return(newTable)

}

#' Calculate persistance rate
#'
#' @param REtable summary table of reduced ensemble
#' @param ModelList this is the list of model structures
#' @param modellingOptions vector containing the names of modelling options (default is c("rferr","arch1","arch2","qsurf","qperc","esoil","qintf","q_tdh"))
#'
#' @return data.frame containing the persistence rate in a new column
#'
#' @examples
#' # PersistenceRate(REtable,ModelList,modellingOptions)
#'

PersistenceRate <- function(REtable,ModelList,modellingOptions){

  counter <- 0
  Pr <- c()
  option <- c()
  decision <- c()

  for (decisionType in modellingOptions) {

    # Calculate frequency for original model list
    totalModelList <- sum(table(ModelList[,decisionType]))
    # Calculate frequency for the new set of models
    totalREtable <- sum(table(REtable[,decisionType]))

    for (optName in names(table(ModelList[,decisionType]))){

      counter <- counter + 1

      lowerFrac <- table(ModelList[,decisionType])[[optName]]/totalModelList

      if (optName %in% names(table(REtable[,decisionType]))) {
        upperFrac <- table(REtable[,decisionType])[[optName]]/totalREtable
      }else{
        upperFrac <- 0
      }

      Pr[counter] <- round(upperFrac/lowerFrac,2)

      #now normalise Pr
      #Pr[counter] <- Pr[counter]/(totalModelList/totalREtable)

      option[counter] <- LabelOptions(optName)
      decision[counter] <- decisionType

    }

  }

  df <- data.frame("Decision"=decision,
                   "Option"=option,
                   "Persistence"=Pr)

  df$Decision <- factor(df$Decision,
                        levels = modellingOptions)

  return(df)

}

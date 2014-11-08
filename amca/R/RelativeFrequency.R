#' Calculate relative frequency
#'
#' @param REtable summary table of reduced ensemble
#' @param modellingOptions vector containing the names of modelling options (default is c("rferr","arch1","arch2","qsurf","qperc","esoil","qintf","q_tdh"))
#'
#' @return data.frame containing the relative frequency in a new column
#'
#' @examples
#' # RelativeFrequency(REtable,modellingOptions)
#'

RelativeFrequency <- function(REtable,modellingOptions){

  ModelList <- PrepareTable()

  counter <- 0
  Rf <- c()
  option <- c()
  decision <- c()

  for (decisionType in modellingOptions) {

    totalREtable <- sum(table(REtable[,decisionType]))

    for (optName in names(table(ModelList[,decisionType]))){

      counter <- counter + 1

      if (optName %in% names(table(REtable[,decisionType]))) {
        upperFrac <- table(REtable[,decisionType])[[optName]]/totalREtable
      }else{
        upperFrac <- 0
      }

      Rf[counter] <- round(upperFrac,2)
      option[counter] <- LabelOptions(optName)
      decision[counter] <- decisionType

    }

  }

  df <- data.frame("Decision"=decision,
                   "Option"=option,
                   "RelativeFrequency"=Rf)

  df$Decision <- factor(df$Decision,
                        levels = modellingOptions)

  return(df)

}

PersistenceRate <- function(REtable,ModelList,modellingOptions){

  counter <- 0
  Pr <- c()
  option <- c()
  decision <- c()

  for (decisionType in modellingOptions) {

    totalREtable <- sum(table(REtable[,decisionType]))
    totalModelList <- sum(table(ModelList[,decisionType]))

    for (optName in names(table(ModelList[,decisionType]))){

      counter <- counter + 1

      lowerFrac <- table(ModelList[,decisionType])[[optName]]/totalModelList

      if (optName %in% names(table(REtable[,decisionType]))) {
        upperFrac <- table(REtable[,decisionType])[[optName]]/totalREtable
      }else{
        upperFrac <- 0
      }

      Pr[counter] <- round(upperFrac/lowerFrac,2)
      option[counter] <- LabelOptions(optName)
      decision[counter] <- decisionType

    }

  }

  df <- data.frame("Decision"=decision,
                   "Option"=option,
                   "PersistenceRate"=Pr)

  df$Decision <- factor(df$Decision,
                        levels = modellingOptions)

  return(df)

}

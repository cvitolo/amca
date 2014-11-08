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

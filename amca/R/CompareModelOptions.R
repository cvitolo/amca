CompareModelOptions <- function(listResults, experimentName="A"){

  # listResults <- list(resultsA1,resultsA2,resultsA3) or load("/mnt/homes_uca/cvitolo/x.rda")

  #library(ggplot2)
  #library(manipulate)

  counter <- 1

  temp <- listResults[[counter]]$RETable[,c("rferr","arch1","arch2","qsurf","qperc","esoil","qintf","q_tdh")]
  row.names(temp)<- NULL
  temp$Simulation <- paste(experimentName,counter,sep="")
  xMelted <- melt(temp, id.vars=c("Simulation"))

  for (counter in 2:length(listResults)){

    temp <- listResults[[counter]]$RETable[,c("rferr","arch1","arch2","qsurf","qperc","esoil","qintf","q_tdh")]
    row.names(temp)<- NULL
    temp$Simulation <- paste(experimentName,counter,sep="")
    xMelted <- rbind(xMelted, melt(temp, id.vars=c("Simulation")))

  }

  return(xMelted)

}

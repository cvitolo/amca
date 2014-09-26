#' Extract (from the object discharges) the time series for a certain realization
#'
#' @param mid model structure identification number
#' @param pid parameter set identification number
#' @param SimulationFolder path to the folder where the function saves the results
#'
#' @return subset of discharges table
#'
#' @examples
#' # ScenarioPlotDTW(mid,pid,SimulationFolder)
#'

ScenarioPlotDTW <- function(mid,pid,SimulationFolder){

  load(paste(SimulationFolder,"/MID_",mid,".Rdata",sep=""))

  discharges <- discharges

  ts <- t(discharges[pid,])

  return(ts)

}

#' This function calculates the catchment's time of concentration (Tc) using Kirpich's formula.
#'
#' @param length this is the length of the main river channel (in metres, usually obtained by GIS layers)
#' @param slope this is the average slope of the main river channel (dimensionless, usually obtained by GIS layers)
#'
#' @return time of concentration (in days)
#'
#' @examples
#' # For Severn catchment (mid-Wales, UK)
#' # Tc <- KirpichTc( length=4601, slope=0.065 )
#'

KirpichTc <- function(length,slope){

  Tc_minutes <- 0.0195*(length^0.77)/(slope^0.385)

  Tc_days <- round(Tc_minutes/60/24,3)

  return(Tc_days)

}

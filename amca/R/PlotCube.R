#' Plot cube to visualise 3D array
#'
#' @param myDimensions vector listing the number of elements in each dimension: number of parameter stes, number of indices and number of model structures. By default this is c(10000,5,1248).
#'
#' @return A plot showing the 3D array
#'
#' @examples
#' # PlotCube()
#'

PlotCube <- function(myDimensions=NULL){

  if (is.null(myDimensions)) {
    myDimensions <- c(10000,5,1248)
  }

  model_ID <- seq(1, myDimensions[3], length=10)
  Parameter_Sets <- seq(1, myDimensions[1], length=10)

  f <- function(x,y) { numeric(length=100) +3}
  #-100 to have the cube without the cutting surface

  Performance_Measures <- outer(model_ID, Parameter_Sets, f)

  #setEPS()
  #postscript(filepath)

  persp(model_ID, Parameter_Sets, Performance_Measures,
        theta=30, phi=30, zlim=c(1,myDimensions[2]),
        expand = 1, col = "lightgray", ticktype="detailed",
        main="",
        xlab="\n\n Model ID number (MID)",
        ylab="\n\n \n Parameter set ID number (PID)",
        zlab="\n\n MPI")

  #dev.off()

  # use option axes=F in persp
  # use locator() to get the coordinates where you want to put your new labels
  # use text(x,y,...) to write your new labels
  #text(-0.6071403 -0.6071403 -0.5968060, "Performance Measures",P)

  #text3d(0, 0, -10, "Hello world", P)

}

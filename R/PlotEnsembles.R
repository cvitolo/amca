#' Compare 2 distributions percentiles over time.
#'
#' @param bounds This is a data.frame structured into the following columns:
#'             "Dates" containing timestamps,
#'             "Qobs" containing observed streamflow discharge,
#'             "lQ" lower percentile of the set of realisations,
#'             "uQ" upper percentile of the set of realisations.
#' @param dischargeTable This is a numerical matrix structured into R number of rows and C number of columns. R corresponds to the number of realisations and C corresponds to the number of timesteps.
#' @param lowerP lower percentile to visualise in red (default = 5)
#' @param upperP upper percentile to visualise in red (default = 95)
#' @param label1 label for the x-axis.
#' @param label2 label for the y-axis.
#' @param type If "B" means the reference is min-max. If "P" it means the reference is lower-upper percentile.
#'
#' @return A plot comparing ensemble distributions.
#' 
#' @export
#'
#' @examples
#' # PlotEnsembles(bounds, dischargeTable)
#'

PlotEnsembles <- function(bounds, dischargeTable,lowerP=5,upperP=95,
                          label1="T limits",label2="T' percentiles",type="B"){

  # require(fanplot)
  # require(colorspace)
  # require(RColorBrewer)

  if (type=="P"){
    L <- bounds$LP
    U <- bounds$UP
    boundsMax <- max(unlist(U),unlist(dischargeTable))
   boundsMin <- min(unlist(L),unlist(dischargeTable))
  }else{
    L <- bounds$LB
    U <- bounds$UB
    boundsMax <- max(U)
    boundsMin <- min(L)
  }

  plot(NULL, xlim = c(-15, dim(dischargeTable)[2]+15),
       ylim =c(boundsMin,boundsMax),
       main="", xlab="",ylab="Discharge [mm/d]",xaxt="n")

  fan(data = dischargeTable, fan.col = sequential_hcl, ln=0, alpha=0.5)
  fan(dischargeTable, ln=c(lowerP, upperP), llab=TRUE, alpha=0, ln.col="red")

  lines(bounds$Qobs, col="black", lwd=3)
  lines(U, col="black", lwd=1, lty=2)
  lines(L, col="black", lwd=1, lty=2)

  mySeq <- c(length(bounds$Dates)%%5,
             round(length(bounds$Dates)/5,0),
             round(length(bounds$Dates)/5,0)*2,
             round(length(bounds$Dates)/5,0)*3,
             round(length(bounds$Dates)/5,0)*4,
             round(length(bounds$Dates)/5,0)*5)

  myLabels <- bounds$Dates[mySeq]
  axis(1, at=mySeq, labels=myLabels)

  legend("topright",
         c("Observations", label1, label2),
         horiz = FALSE,
         y.intersp=0.5,
         bty = "n",
         lwd =c(3, 1, 1),
         lty = c(1, 2, 1),
         col = c("black","black","red"),
         cex = 1)

}

#'Plot QQ and results of statistical tests.
#'
#' @param simulated table m(rows) x n(cols) containing m predictions for each n time steps
#' @param observed observed discharge time series
#' @param warmupDTW additional warmup period (optional)
#' @param verbose if set to TRUE it prints running information
#'
#' @return A plot with QQ plots and results of statistical reliability tests
#' 
#' @export
#'
#' @examples
#' # reliability(predDTW,observed,warmupDTW=0,verbose=FALSE)
#'

reliability <- function(simulated,observed,warmupDTW=0,verbose=FALSE){

  observed <- observed[(warmupDTW+1):length(observed)]
  simulated <- simulated[,(warmupDTW+1):dim(simulated)[2]]

  z <- rep(NA,length(observed))
  #calculate cumulative distribution function
  for (i in 1:length(observed)){

      P <- ecdf(simulated[,i])

    z[i] <- P(observed[i]) # Quantile of observed p-value
  }

  R <- rank(z)
  r <- R/length(observed) # Theoretical quantile

  #INDEPENDENCE TEST cor.test(z, observed, method="kendall")$estimate
  Ktau <- round(cor.test(z, observed, method="kendall")$estimate,3)
  if ( Ktau < 1.645 ) {
    if (verbose==TRUE) print(paste("Kendall independence-test: VERIFIED!, tau=",Ktau))
  }else{
    if (verbose==TRUE) print(paste("Kendall independence-test: NOT VERIFIED! tau=",Ktau))
  }

  #UNIFORMITY TEST
  KSD <- round(ks.test(z,observed, conf.level=0.95, exact=TRUE)$statistic,3)
  if (KSD < 1.358 ) {
    if (verbose==TRUE) print(paste("Kolmogorov-Smirnov uniformity-test: VERIFIED!, D=",KSD))
  }else{
    if (verbose==TRUE) print(paste("Kolmogorov-Smirnov uniformity-test: NOT VERIFIED! D=",KSD))
  }

  # RENARD et al. 2010 consider the QQ plot with inverted axis compared with LAIO and TAMEA
  if (verbose==TRUE) print("Calculate RELIABILITY and RESOLUTION (RENARD et al. 2010)")
  alfa1 <- sum(abs(z-r))/length(observed)
  reliability_alfa <- round(1 - alfa1,2)
  if (verbose==TRUE) print(paste("reliability_alfa = ",reliability_alfa))
  xi1 <- ifelse(z==0 | z==1, 1, 0)
  reliability_xi <- 1 - round(sum(xi1*z)/length(observed),2)
  if (verbose==TRUE) print(paste("reliability_xi = ",reliability_xi))
  resolution_abs <- round(sum(1/apply(simulated,2,sd))/length(observed),2)
  if (verbose==TRUE) print(paste("resolution_abs = ",resolution_abs))
  resolution_rel <- round(sum(apply(simulated,2,mean)/apply(simulated,2,sd))/length(observed),2)
  if (verbose==TRUE) print(paste("resolution_rel = ",resolution_rel))
  co.var <- round(mean(apply(simulated,2,sd)/apply(simulated,2,mean)),2)
  if (verbose==TRUE) print(paste("Average Coefficient of Variation = ",co.var))

  if (verbose==TRUE) {
    plot(r~z,main="", xlab="Theoretical Quantiles",ylab="Data Quantiles",
         type="p",col="grey", xlim=c(0,1),ylim=c(0,1), cex=0.5)
    lines(0:1,0:1,col="black")
    text(0.50,0.2, paste("Kendall's \U03C4","=",Ktau,sep=" "), cex = .8, adj = c(0,0))
    text(0.50,0.1, paste("Kolmogorov-Smirnov's D =",round(KSD,1),sep=" "), cex = .8, adj = c(0,0))
    text(0.50,0.0, paste("Renard's \U03B1","=",reliability_alfa,sep=" "), cex = .8, adj = c(0,0))
  }

  return(round(reliability_alfa*100, 0))

}

#'   "PPLOTpostvar2D"  Plot of posterior of Landmark variance parameter from MCMC sampling
#'
#'    2D landmark data in Pre-shape(Gaussian likelihood with Isotropic Error Variance)
#' Plot of posterior of Landmark variance parameter
#'
#' "Sigma" = Isotropic error variation parameter

#'@param mcmcmat  A matrix containing MCMC sample values from posterior of  sigma- parameters
#'@param burnin Value for MCMC burn-in
#'@param Nsample number of MCMC samples
#'@param colu  colour of plot
#' @keywords PPLOTpostvar2D
#' @return plot
#' @export
#' @examples
#' \dontrun{
#'require(shapes)
#'data(apes)
#'myData = apes$x
#'for(i in 1:dim(myData)[3])
#'{myData[,,i]=
#'  Morpho::rotonto(myData[,,1],myData[,,i])$yrot}
#'ape5000=MCMCpostPsample2D(1.5,rep(0.1,1),myData,5000)
#'PPLOTpostvar2D(ape5000, 1000)
#'
#'}




PPLOTpostvar2D=function(mcmcmat, burnin,colu)
{
  if(length(mcmcmat)<burnin)
  {print("Error! nrow(mcmcmat)should be > burnin")
  }
  Nsample=length(mcmcmat)
  TT=matrix(rep(1,1),1,1)
  colnames(TT)<-c("Sigma");

  plot(mcmcmat, type="s", xpd=NA, ylab=paste(colnames(TT)[1]), xlab="MCMC Sample", las=1,main=expression(paste(bold("(MCMC plot from Posterior of  parameters"))),col=colu)
  abline(v=burnin, col="black",lwd=2, lty=2)
}




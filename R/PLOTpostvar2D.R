#'   "PLOTpostvar2D"  Plot of posterior of Landmark variance parameter from MCMC sampling
#'
#'    2D landmark data(Gaussian likelihood with Isotropic Error Variance)
#' Plot of posterior of Landmark variance parameter
#'
#' "Sigma" = Isotropic error variation parameter

#'@param mcmcmat  A matrix containing MCMC sample values from posterior of  5*1 parameters
#'@param burnin Value for MCMC burn-in
#'@param Nsample number of MCMC samples
#'
#' @keywords PLOTpostvar2D
#' @return plot
#' @export
#' @examples
#' \dontrun{
#'require(shapes)
#'data(apes)
#'myData = apes$x
#'ape10000=MCMCpostsample2D(rnorm(5, mean=1, sd=1),rep(1,5),
#'apes$x,10,10000)
#'PLOTpostvar2D(ape10000, 1000)
#'
#'}




PLOTpostvar2D=function(mcmcmat, burnin)
{
  if(ncol(mcmcmat)!=5)
  {print("Error! This plot function is for 2D  Landmarks, hence ncol(mcmcmat) is 5")
    }
  else
  {print("We will proceed with mcmcmat[5] (the last column) as MCMC sample of Sigma ")
    }
  if(nrow(mcmcmat)<burnin)
  {print("Error! nrow(mcmcmat)should be > burnin")
    }
  Nsample=nrow(mcmcmat)
  TT=matrix(rep(1,5),1,5)
  colnames(TT)<-c("c1","c2", "b", "theta", "Sigma");

    plot(mcmcmat[(burnin+1):Nsample,5], type="s", xpd=NA, ylab=paste(colnames(TT)[5]), xlab="MCMC Sample", las=1,main=expression(paste(bold("(MCMC plot from Posterior of  parameters"))))

}




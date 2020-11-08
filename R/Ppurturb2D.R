#'   "Ppurturb2D" function for Pre-Shape space Data & MCMC posterior sampling
#'
#'    2D landmark -Pre-Shape space Data(Gaussian likelihood with Isotropic Error Variance)
#' generates pertrubed point from 1 parameter sigma.
#'
#' "Sigma" = Isotropic error variation parameter

#'@param t  An array containing 5*1 parameters
#' @param tune array containing 5*1 tuning value
#' @keywords Ppurturb2D
#' @return g, which is a (new) purturbed point from parameter space
#' @export
#' @examples
#' \dontrun{
#' NN=10; temp=rep(NA,NN); tempp=rep(1,1);
#' plot(x = 1,type = "n",xlim = c(0, NN+1), ylim = c(-10, 10),pch = 16,xlab = "steps", ylab = "mean(t)",main = "Purturbed point plot")
#'abline(h=mean(tempp))
#'for(k in 1:NN)
#'{   Sys.sleep(.1)
#' temp[k]=mean(Ppurturb2D(tempp, rep(1,1)))
#'  points( temp[k]~k,pch = 10,col =k)
#' Sys.sleep(0)
#'}
#'}



Ppurturb2D <- function(t, tune) {
  e = abs(rnorm(1,0,1))

    sig=t[1];
  #rnorm(1, x, 0.1)

  us = runif(1,0,1)
  if( us<0.5 )
  { new_sig = sig+0.5*tune*e
  }
  else
  {  new_sig = max(0.001,sig-(0.5*tune*e))
  }

  g=c(new_sig)
  return(g)

}

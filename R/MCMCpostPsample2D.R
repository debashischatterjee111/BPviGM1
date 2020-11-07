#'   MCMC posterior sampling for 2D landmark data (in Pre-shape space)
#'   (Gaussian likelihood with Isotropic Error Variance)
#'
#' Draws posterior from 5 parameter "Sigma" from Whole Data with pre-shape spaced landmarks
#'
#' "Sigma" = Isotropic error variation parameter
#' Note that, here we are assuming Isotropy of error variance
#'
#'@param tune Tuning value of MCMC sampler
#'@param myData  3D array containing 2 dimensional landmark
#'@param Nsample Number of MCMC sample desired
#'@param initial The start value of  parameter sigma for MCMC run
#'
#' @keywords MCMCpostPsample2D
#' @return matrix containing samples from posterior density of parameter
#' @export
#' @examples
#' \dontrun{
#'require(shapes)
#'data(apes)
#'myData = apes$x
#'for(i in 1:dim(myData)[3])
#'{myData[,,i]=
 #'Morpho::rotonto(myData[,,1],myData[,,i])$yrot}
#'ape5000=MCMCpostPsample2D(1.5,
#'rep(0.1,1),myData,5000)
#'head(ape5000)
#'}
#'

MCMCpostPsample2D=function(initial,tune,myData, Nsample)
{
  ress <- Prun2D(initial,tune, myData, Nsample)
  return(ress);
}


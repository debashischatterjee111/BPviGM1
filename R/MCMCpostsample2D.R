#'   MCMC posterior sampling for 2D landmark data
#'   (Gaussian likelihood with Isotropic Error Variance)
#'
#' Draws posterior from 5 parameters "c1","c2", "b", "theta", "Sigma"
#'
#' "c1","c2" = Location parameter
#'  "b" = dilation parameter
#' "theta" =Rotation angle
#' "Sigma" = Isotropic error variation parameter
#' Note that, here we are assuming Isotropy of error variance
#'
#'@param tune Tuning value of MCMC sampler
#'@param myData  3D array containing 2 dimensional landmark
#'@param choice Which to compare with 1st object or mean obj
#'@param Nsample Number of MCMC sample desired
#'@param initial The start value of 5*1 parameter vector for MCMC run
#'
#' @keywords MCMCpostsample2D
#' @return matrix containing samples from posterior density of parameter
#' @export
#' @examples
#' \dontrun{
#'require(shapes)
#'data(apes)
#'myData = apes$x
#'ape10000=MCMCpostsample2D(rnorm(5,1,1),
#'rep(1,5),apes$x,10,10000)
#'head(ape10000)
#'}
#'

MCMCpostsample2D=function(initial,tune,myData, choice, Nsample)
{
  ress <- run2D(initial,tune, myData, choice, Nsample)
  return(ress);
}


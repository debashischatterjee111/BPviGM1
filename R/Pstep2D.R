#'   "Pstep2D"  MCMC sampling function: Accept new point with probability alpha
#'
#'    2D landmark data in Pre Shape space(Gaussian likelihood with Isotropic Error Variance)
#' Accepts new parameter 5*1 point with probability alpha
#'
#' "Sigma" = Isotropic error variation parameter

#'@param t  An array containing 1 parameters
#' @param tune array containing 1 tuning value
#' @param myData  3D array containing 2 dimensional landmark

#' @keywords Pstep2D
#' @return Returns the 1 parameter point
#' @export
#' @examples
#' \dontrun{
#' require(shapes)
#' data(apes)
#'myData = apes$x
#'for(i in 1:dim(myData)[3])
#'{myData[,,i]=Morpho::rotonto(myData[,,1],myData[,,i])$yrot}
#'tt=rep(1,1)
#'for(i in 1:3){
#' s=Pstep2D(tt,rep(0.01,1),myData)
#'print(s)
#'tt=Ppurturb2D(tt, rep(0.01,1))
#'}
#'}


Pstep2D = function(t,tune,myData)
{

  ## Pick new point
  t_p = Ppurturb2D(t, tune)

  ## Acceptance probability:
  alpha <- min(1, Pfratio2D(t_p,t,myData))
  ## Accept new point with probability alpha:
  if (runif(1) < alpha)
    t <- t_p

  ## Returning the point:
  return(t)
}

#'   "step2D"  MCMC sampling function: Accept new point with probability alpha
#'
#'    2D landmark data(Gaussian likelihood with Isotropic Error Variance)
#' Accepts new parameter 5*1 point with probability alpha
#'
#' "c1","c2" = Location parameter
#'  "b" = dilation parameter
#' "theta" =Rotation angle
#' "Sigma" = Isotropic error variation parameter

#'@param t  An array containing 5*1 parameters
#' @param tune array containing 5*1 tuning value
#' @param myData  3D array containing 2 dimensional landmark
#'@param choice Which to compare with 1st object or mean obj
#'
#' @keywords step2D
#' @return Returns the 5*1 parameter point
#' @export
#' @examples
#' \dontrun{
#' require(shapes)
#' data(apes)
#'myData = apes$x
#'tt=rep(1,5)
#'for(i in 1:3){
#' s=step2D(tt,rep(0.1,5),myData,10)
#'print(s)
#'tt=purturb2D(tt, rep(0.1,5))
#'}
#'}


step2D = function(t,tune,myData, choice)
{

  ## Pick new point
  t_p = purturb2D(t, tune)

  ## Acceptance probability:
  alpha <- min(1, fratio2D(t_p,t,myData, choice))
  ## Accept new point with probability alpha:
  if (runif(1) < alpha)
    t <- t_p
  ## Returning the point:
  return(t)
}

#'   "run2D"  MCMC sampling function for running MCMC sampler
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
#'@param Nsample number of MCMC sample to  generated
#'
#' @keywords run2D
#' @return Returns a large  matrix containing the actual samples from posterior of 5*1 parameter point
#' @export
#' @examples
#' \dontrun{
#' require(shapes)
#' data(apes)
#'myData = apes$x
#' r=run2D(rnorm(5, mean=1, sd=1),rep(0.1,5),
#'  myData, 10, 5000)
#'head(r); dim(r)
#'}


run2D <- function(t, tune, myData, choice, Nsample)
  {
  nsteps=Nsample
  print(Nsample)
  res <- matrix(NA, nsteps, length(t))
  for (i in seq_len(nsteps)){
    res[i,] <- t <- step2D(t,tune,myData, choice)

    print(i)
    if (i == nsteps) cat(': Done')
    # else cat('\014')
  }
  drop(res)
}

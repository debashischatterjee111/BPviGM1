#'   "Prun2D"  MCMC sampling function for running MCMC sampler
#'
#'    2D landmark data in Pre Shape  Space (Gaussian likelihood with Isotropic Error Variance)
#' Accepts new parameter 5*1 point with probability alpha
#' "Sigma" = Isotropic error variation parameter

#'@param t  An array containing 1 parameters
#' @param tune array containing 1 tuning value
#' @param myData  3D array containing 2 dimensional landmark
#'@param Nsample number of MCMC sample to  generated
#'
#' @keywords Prun2D
#' @return Returns a large  matrix containing the actual samples from posterior of 1 parameter point
#' @export
#' @examples
#' \dontrun{
#' require(shapes)
#' data(apes)
#'myData = apes$x
#'for(i in 1:dim(myData)[3])
#'{myData[,,i]=
#'Morpho::rotonto(myData[,,1],myData[,,i])$yrot}
#' r=Prun2D(runif(1, 0.1,1),rep(0.01,1),
#'  myData, 5000)
#'head(r); dim(r)
#'}


Prun2D <- function(t, tune, myData, Nsample)
{
  nsteps=Nsample
   res <- matrix(NA, nsteps, length(t))
  for (i in seq_len(nsteps)){
    res[i,] <- t <- Pstep2D(t,tune,myData)

    print(i)
    if (i == nsteps) cat(': Done')
    # else cat('\014')
  }
  drop(res)
}

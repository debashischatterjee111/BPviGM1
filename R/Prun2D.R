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
  nsteps=Nsample;
  if(nsteps<100)
  {stop("Error! Nsample should be greater than 100");}
  pb <- txtProgressBar(min = 0, max = nsteps, style = 3, char="=~>", width=10)
   res <- matrix(NA, nsteps, length(t));
   ptm <- proc.time();
  for (i in seq_len(nsteps)){
    res[i,] <- t <- Pstep2D(t,tune,myData)

   # print(i)
    # if (i == nsteps) cat(': Done')
    # else cat('\014')
    #setTxtProgressBar(pb, i)
    setTxtProgressBar(pb, i);
    if(i==1|i==round(nsteps/nsteps)|i==round(nsteps/20)|i==round(nsteps/10)|i==round(nsteps/9)|i==round(nsteps/8)|i==round(nsteps/7)|i==round(nsteps/6)|i==round(nsteps/5)|i==round(nsteps/4)|i==round(nsteps/3)|i==round(nsteps/2)|i==round(nsteps/1.5)|i==round(nsteps/1.2)|i==round(nsteps/1))
      #{print(i)}
      # progress(i,progress.bar = T)

    {     cat(paste0('current sample:[', i,'] mcmc_run: ', round(i/ (nsteps-1) * 100), '% completed'))

    }
    if (i == nsteps)
    { print("***");
      cat(': Done :');
      print("***");
    }
  }
   print("Time taken:");
   print(proc.time()-ptm);
  drop(res);
}

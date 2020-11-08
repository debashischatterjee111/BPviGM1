#'   "Pfratio2D" function for MCMC posterior sampling
#'
#'    2D landmark data in Pre-shape space (Gaussian likelihood with Isotropic Error Variance)
#' as the name suggest, it evaluates fratio for two parameter vector t1, t2
#'
# "Sigma" = Isotropic error variation parameter

#'@param t1  An array containing 1 parameter sigma
#' @param t2 An array containing another 1 parameter sigma
#' @param myData  3D array containing 2 dimensional landmark
#' @keywords Pfratio2D
#' @return fratio value
#' @export
#' @examples
#' \dontrun{
#'require(shapes);
#'require(Morpho);
#'data(apes)
#'myData = apes$x
#'for(i in 1:dim(myData)[3])
#'{myData[,,i]=Morpho::rotonto(myData[,,1],myData[,,i])$yrot
#'}
#'for(k in 1:10)
#'{tempp2=runif(1, 0.1, 2);
#'tempp1=Ppurturb2D(tempp2,rep(0.01,1))
#'p=Pfratio2D (tempp1, tempp2,  myData)
#' print(p)
#' }
#'}


Pfratio2D <- function(t1, t2,  myData)
{Dim=dim(myData)
  M = myData[,,1]
  W=myData[,,2]
    if(nrow(M)!=nrow(W))
  {print("Error! Landmark Mismatch! Format the Data Properly into 3D array!")}
  p=nrow(M)

  sig=t1;
  ssig=t2;


   sum1=0
  for(k in 1:Dim[3])
  {
  for(i in 1:p)
  { sum1= sum1+ mvtnorm::dmvnorm(myData[i,,k], mean=myData[i,,1], sigma=(sig^2)*diag(2),log=T)
  }
    }
  f11=sum1;

  sum2=0;
  for(k in 1:Dim[3])
  {
    for(i in 1:p)
    { sum2= sum2+ mvtnorm::dmvnorm(myData[i,,k], mean=myData[i,,1], sigma=(ssig^2)*diag(2),log=T)

    }
  }
   f22=sum2;


  return(exp(f11-f22))
}



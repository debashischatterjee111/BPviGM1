#'   "fratio2D" function for MCMC posterior sampling
#'
#'    2D landmark data(Gaussian likelihood with Isotropic Error Variance)
#' as the name suggest, it evaluates fratio for two parameter vector t1, t2
#'
#' "c1","c2" = Location parameter
#'  "b" = dilation parameter
#' "theta" =Rotation angle
#' "Sigma" = Isotropic error variation parameter

#'@param t1  An array containing 5*1 parameters
#' @param t2 An array containing another 5*1 parameter
#' @param myData  3D array containing 2 dimensional landmark
#'@param choice Which to compare with 1st object or mean obj
#' @keywords fratio2D
#' @return fratio value
#' @export
#' @examples
#' \dontrun{
 #'require(shapes)
#'data(apes)
#'myData = apes$x
#'for(k in 1:10)
#'{tempp2=rnorm(5, mean=1, sd=1);
#'tempp1=purturb2D(tempp2,rep(0.001,5))
#'p=fratio2D (tempp1, tempp2,  myData, 10)
#' print(p)
#' }
#'}


fratio2D <- function(t1, t2,  myData, choice)
{
  M = myData[,,1]
  W=myData[,,choice]
  Z=M; Y=W;
  if(nrow(M)!=nrow(W))
  {print("Error! Landmark Mismatch! Format the Data Properly into 3D array!")}
  p=nrow(M)
    c=c(t1[1],t1[2]);
  b=t1[3];
  th=t1[4];
  sig=t1[5];

  cc=c(t2[1],t2[2]);
  bb=t2[3];
  tth=t2[4];
  ssig=t2[5];

  mu1=mu2=W;
  for(i in 1:p)
  {mu1[i,1]=c[1]+ (b*(cos(th)*Z[i,1]))+(b*(sin(th)*Z[i,2]))
  mu1[i,2]=c[2]+ (b*(-sin(th)*Z[i,1]))+(b*(cos(th)*Z[i,2]))
  }

  for(i in 1:p)
  {mu2[i,1]=cc[1]+ (bb*(cos(tth)*Z[i,1]))+(bb*(sin(tth)*Z[i,2]))
  mu2[i,2]=cc[2]+ (bb*(-sin(tth)*Z[i,1]))+(bb*(cos(tth)*Z[i,2]))
  }


  sum1=0
  for(i in 1:p)
  { sum1= sum1+ mvtnorm::dmvnorm(W[i,], mean=mu1[i,], sigma=(sig^2)*diag(2),log=T)
  }
  f11=sum1;

  sum2=0
  for(i in 1:p)
  { sum2= sum2+ mvtnorm::dmvnorm(W[i,], mean=mu2[i,], sigma=(ssig^2)*diag(2),log=T)
  }
  f22=sum2;


  return(exp(f11-f22))
}



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
#'@param myData  3D array containing 2 dimensional landmark
#'@param choice Which to compare with 1st object or mean obj
#'@param Nlandmark How many landmarks are there for each object? (usually nrow(myData[,,1]))
#'@param tune Tuning value of MCMC sampler
#'@param Nsample Number of MCMC sample desired
#' @keywords TMCMCpostsample2D
#' @return matrix containing samples from posterior density of parameter
#' @export
#' @examples
#' \dontrun{
#'require(shapes)
#'data(apes)
#'myData = apes$x
#'ape_ress_10000=MCMCpostsample2D(apes$x,10,nrow(myData[,,1]), tune=1,10)
#'}
#'

TMCMCpostsample2D=function(myData, choice, Nlandmark, tune,Nsample)
{
  p=Nlandmark;
  M = myData[,,1]
  W=myData[,,choice]
  Z=M; Y=W;
  th=1;c=1:2;b=1;sig=0.1;dimm=d=2


  fratio <- function(t1, t2)
  {
    c=c(t1[1],t1[2]);
    b=t1[3];
    th=t1[4];
    sig=t1[5];

    cc=c(t2[1],t2[2]);
    bb=t2[3];
    tth=t2[4];
    ssig=t2[5];

    #Building mu_1, mu_2

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
    { sum2= sum2+ dmvnorm(W[i,], mean=mu2[i,], sigma=(ssig^2)*diag(2),log=T)
    }
    f22=sum2;


    #print(f22)
    return(exp(f11-f22))
  }



  purturb11 <- function(t) {
    e = abs(rnorm(1,0,1))

    c=c(t[1],t[2]);
    b=t[3];
    th=t[4];
    sig=t[5];
    #rnorm(1, x, 0.1)
    u1 = runif(1,0,1)
    if( u1<0.5 )
    { new_c_1 = c[1]+0.5*tune*e
    }
    else
    { new_c_1 = c[1]-0.5*tune*e
    }

    u2 = runif(1,0,1)
    if( u2<0.5 )
    { new_c_2 = c[2]+0.5*tune*e
    }
    else
    { new_c_2 = c[2]-0.5*tune*e
    }

    new_c=c(new_c_1, new_c_2)

    ub = runif(1,0,1)
    if( ub<0.5 )
    { new_b = b+0.5*tune*e
    }
    else
    { new_b = b-0.5*tune*e
    }


    ut = runif(1,0,1)
    if( ut<0.5 )
    { new_th = th+0.5*tune*e
    }
    else
    { new_th = th-0.5*tune*e
    }

    us = runif(1,0,1)
    if( us<0.5 )
    { new_sig = sig+0.5*tune*e
    }
    else
    {  new_sig = min(0.01,sig-0.5*e)
    }

    g=c(new_c[1], new_c[2],new_b, new_th,new_sig)
    return(g)

  }

  step = function(t, purturb11)
  {

    ## Pick new point
    t_p = purturb(t)

    ## Acceptance probability:
    alpha <- min(1, fratio(t_p,t))
    ## Accept new point with probability alpha:
    if (runif(1) < alpha)
      t <- t_p
    ## Returning the point:
    return(t)
  }

  #step(rep(1,5),q)

  #q2 <- function(x) rnorm(1, x, 0.08)

  run <- function(t, purturb11, nsteps) {
    res <- matrix(NA, nsteps, length(t))
    for (i in seq_len(nsteps)){
      res[i,] <- t <- step(t, purturb)

      print(i)
      if (i == nsteps) cat(': Done')
      # else cat('\014')
    }
    drop(res)
  }


  #run
  ress <- run(rnorm(5, mean=1, sd=1),  purturb11, Nsample)


  naming=function()
  {
    TT=matrix(rep(1,5),1,5)
    colnames(TT)<-c("c1","c2", "b", "theta", "Sigma");
    # names <- colnames(TT)
  }
  plot_mcmc=function()
  {
    naming();
    for(i in 1:5)
    {
      plot(ress[1001:10000,i], type="s", xpd=NA, ylab=paste(colnames(TT)[i]), xlab="Sample", las=1,main=expression(paste(bold("(MCMC plot from Posterior of  parameters"))))
    }
  }
  plot_mcmc();

  return(ress);

}


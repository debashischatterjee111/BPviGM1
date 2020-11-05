#'   "purturb2D" function for MCMC posterior sampling
#'
#'    2D landmark data(Gaussian likelihood with Isotropic Error Variance)
#' generates pertrubed point from 5 parameter space.
#'
#' "c1","c2" = Location parameter
#'  "b" = dilation parameter
#' "theta" =Rotation angle
#' "Sigma" = Isotropic error variation parameter

#'@param t  An array containing 5*1 parameters
#' @param tune array containing 5*1 tuning value
#' @keywords purturb2D
#' @return g, which is a (new) purturbed point from parameter space
#' @export
#' @examples
#' \dontrun{
#' NN=10; temp=rep(NA,NN); tempp=rep(1,5);
#' plot(x = 1,type = "n",xlim = c(0, NN+1), ylim = c(-10, 10),pch = 16,xlab = "steps", ylab = "mean(t)",main = "Purturbed point plot")
#'abline(h=mean(tempp))
#'for(k in 1:NN)
#'{   Sys.sleep(.1)
#' temp[k]=mean(purturb(tempp, rep(11,5)))
#'  points( temp[k]~k,pch = 10,col =k)
#' Sys.sleep(0)
#'}
#'}



  purturb2D <- function(t, tune) {
    e = abs(rnorm(1,0,1))

    c=c(t[1],t[2]);
    b=t[3];
    th=t[4];
    sig=t[5];
    #rnorm(1, x, 0.1)
    u1 = runif(1,0,1)
    if( u1<0.5 )
    { new_c_1 = c[1]+0.5*tune[1]*e
    }
    else
    { new_c_1 = c[1]-0.5*tune[1]*e
    }

    u2 = runif(1,0,1)
    if( u2<0.5 )
    { new_c_2 = c[2]+0.5*tune[2]*e
    }
    else
    { new_c_2 = c[2]-0.5*tune[2]*e
    }

    new_c=c(new_c_1, new_c_2)

    ub = runif(1,0,1)
    if( ub<0.5 )
    { new_b = b+0.5*tune[3]*e
    }
    else
    { new_b = b-0.5*tune[3]*e
    }


    ut = runif(1,0,1)
    if( ut<0.5 )
    { new_th = th+0.5*tune[4]*e
    }
    else
    { new_th = th-0.5*tune[4]*e
    }

    us = runif(1,0,1)
    if( us<0.5 )
    { new_sig = sig+0.5*tune[5]*e
    }
    else
    {  new_sig = min(0.01,sig-0.5*tune[5]*e)
    }

    g=c(new_c[1], new_c[2],new_b, new_th,new_sig)
    return(g)

  }

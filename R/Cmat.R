#' This function changes a 3D array to a matrix using row-bind
#' @param darr  3D array of dimension =(p,d,n)
#' @param nobj Number of object (=n)
#' @keywords Cmat
#' @return matrix of dimension (pn*d)
#' @export
#' @examples
#' a =  sample(1:5,24,replace = TRUE)
#' dim(a) = c(3,2,4)
#' print("3-dimension array:")
#' print(a)
#' b=Cmat(a,nobj=4)
#'print(b)


Cmat=function(darr, nobj)
{temp=darr[,,1]
for(i in 2:nobj)
{
  temp=rbind(temp,darr[,,i])
}
return(temp)
}



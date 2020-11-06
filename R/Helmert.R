#'  "Helmert"  computes The Helmert sub-matrix
#'
#'@param dim  dimension of Helmert matrix

#' @keywords Helmert
#' @return Returns The Helmert sub-matrix of dimension (dim - 1, dim)
#' @export
#' @examples
#' \dontrun{
#' Helmert(2)
#' Helmert(3)
#'}



Helmert<- function(dim) {
  helm <- matrix(0, dim - 1, dim)
  k = 2:dim
  r =1 / sqrt( k * (k - 1) )
  for ( j in 1:(dim - 1 ) )
    {helm[j, 1: c(j + 1) ] = c( rep(r[j], j), - j * r[j]  )
  }
 return(helm)
}

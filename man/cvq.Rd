% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cvq.R
\docType{data}
\name{cvq}
\alias{cvq}
\title{1000 Convex Quadrilaterals  in 2D (raw)}
\format{
An object of class \code{array} of dimension 4 x 2 x 1000.
}
\source{
\href{https://github.com/debashischatterjee111/BPviGM1/data}
}
\usage{
data(cvq)
}
\description{
1000 Simulated coordinate for 1000 Convex Quadrilaterals  in 2D (raw), Data  built by : Debashis Chatterjee
}
\examples{
data(cvq)
plot(c(-10, 30), c(-12,10), type = "n", main=" Simulated RawData: 1000 Random convex & concave Quadrilaterals",cex.main=0.98, xlab="x",ylab="y")
polygon(cvq[,1,1],cvq[,2,1],density=0, col="pink", border="red", lwd=2)
for(k in 1:1000){
 polygon(cvq[,1,k],cvq[,2,k],density=0, col="pink", border=alpha(rgb(1,0,0), 0.018), lty=5, lwd=1)}
legend("bottomright", legend=c("1000 convex Quadrilaterals, true sigma=1.5", "1000 concave quadrilaterals, true sigma=0.8"),col=c("red", "blue"), lty=c(1,1), cex=0.65,title="Object shape types", text.font=4, bg='white')

}
\references{
chatterjee et al. (2020) (\href{https://github.com/debashischatterjee111/BPviGM1})
}
\keyword{datasets}

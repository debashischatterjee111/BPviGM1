% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MCMCpostsample2D.R
\name{MCMCpostsample2D}
\alias{MCMCpostsample2D}
\title{MCMC posterior sampling for 2D landmark data
(Gaussian likelihood with Isotropic Error Variance)}
\usage{
MCMCpostsample2D(initial, tune, myData, choice, Nsample)
}
\arguments{
\item{initial}{The start value of 5*1 parameter vector for MCMC run}

\item{tune}{Tuning value of MCMC sampler}

\item{myData}{3D array containing 2 dimensional landmark}

\item{choice}{Which to compare with 1st object or mean obj}

\item{Nsample}{Number of MCMC sample desired}
}
\value{
matrix containing samples from posterior density of parameter
}
\description{
Draws posterior from 5 parameters "c1","c2", "b", "theta", "Sigma"
}
\details{
"c1","c2" = Location parameter
"b" = dilation parameter
"theta" =Rotation angle
"Sigma" = Isotropic error variation parameter
Note that, here we are assuming Isotropy of error variance
}
\examples{
\dontrun{
require(shapes)
data(apes)
myData = apes$x
ape10000=MCMCpostsample2D(rnorm(5,1,1),
rep(1,5),apes$x,10,10000)
head(ape10000)
}

}
\keyword{MCMCpostsample2D}

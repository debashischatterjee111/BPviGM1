% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PPLOTpostvar2D.R
\name{PPLOTpostvar2D}
\alias{PPLOTpostvar2D}
\title{"PPLOTpostvar2D"  Plot of posterior of Landmark variance parameter from MCMC sampling}
\usage{
PPLOTpostvar2D(mcmcmat, burnin, colu)
}
\arguments{
\item{mcmcmat}{A matrix containing MCMC sample values from posterior of  sigma- parameters}

\item{burnin}{Value for MCMC burn-in}

\item{colu}{colour of plot}

\item{Nsample}{number of MCMC samples}
}
\value{
plot
}
\description{
2D landmark data in Pre-shape(Gaussian likelihood with Isotropic Error Variance)
Plot of posterior of Landmark variance parameter
}
\details{
"Sigma" = Isotropic error variation parameter
}
\examples{
\dontrun{
require(shapes)
data(apes)
myData = apes$x
for(i in 1:dim(myData)[3])
{myData[,,i]=
 Morpho::rotonto(myData[,,1],myData[,,i])$yrot}
ape5000=MCMCpostPsample2D(1.5,rep(0.1,1),myData,5000)
PPLOTpostvar2D(ape5000, 1000)

}
}
\keyword{PPLOTpostvar2D}

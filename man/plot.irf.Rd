% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.irf.R
\name{plot.irf}
\alias{plot.irf}
\title{Plot impulse response funtion}
\usage{
\method{plot}{irf}(x, impulse, response, alpha = 0.05, lty.cb = 2, lwd.cb = 1, ...)
}
\arguments{
\item{x}{A irf object.}

\item{impulse}{Impulse variable name or index.}

\item{response}{Response Variable name or index.}

\item{alpha}{Significance level for the confidence intervals. May be more than one value in a vector.}

\item{lty.cb}{Confidence bands graphic control.}

\item{lwd.cb}{Confidence bands graphic control.}

\item{...}{Other graphical parameters.}
}
\description{
Plot impulse response function for the chosen impulse variable and response variable.
}
\examples{
## == This example uses the Brazilian inflation data from
#Garcia, Medeiros and Vasconcelos (2017) == ##

# = This is an ilustrative example = #
# = The identification ignores which variables are more exogenous = #
data("BRinf")
Y=BRinf[,1:59]# remove expectation variables
modelB=lbvar(Y,p=3)
identB=identification(modelB)
irfB=irf(modelB,identB,h=12,M=100)
plot(irfB,1,2,alpha=0.1)


}
\references{
Garcia, Medeiros and Vasconcelos (2017).
}
\seealso{
\code{\link{predict}}, \code{\link{lbvar}}, \code{\link{identification}}, \code{\link{irf}}
}
\keyword{Bayesian}
\keyword{High-dimension,}
\keyword{VAR,}
\keyword{irf,}
\keyword{models}

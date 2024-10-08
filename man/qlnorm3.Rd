% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/8_dpq_distributions_adSim.R
\name{qlnorm3}
\alias{qlnorm3}
\title{qlnorm3: The Lognormal Distribution (3 Parameter)}
\usage{
qlnorm3(p, meanlog, sdlog, threshold, ...)
}
\arguments{
\item{p}{A numeric vector of probabilities.}

\item{meanlog, sdlog}{The mean and standard deviation of the distribution on the log scale with default values of \code{0} and \code{1} respectively.}

\item{threshold}{The threshold parameter, default is \code{0}.}

\item{...}{Additional arguments that can be passed to \code{uniroot}.}
}
\value{
\code{dlnorm3} gives the density, \code{plnorm3} gives the distribution function, and \code{qlnorm3} gives the quantile function.
}
\description{
Density function, distribution function, and quantile function for the Lognormal distribution.
}
\details{
The Lognormal distribution with \code{meanlog} parameter zeta, \code{sdlog} parameter sigma, and \code{threshold} parameter theta has a density given by:

\deqn{f(x) = \frac{1}{\sqrt{2\pi}\sigma(x-\theta)}\exp\left(-\frac{(\log(x-\theta)-\zeta)^2}{2\sigma^2}\right)}

The cumulative distribution function is given by:

\deqn{F(x) = \Phi\left(\frac{\log(x-\theta)-\zeta}{\sigma}\right)}

where \eqn{\Phi} is the cumulative distribution function of the standard normal distribution.
}
\examples{
dlnorm3(x = 2, meanlog = 0, sdlog = 1/8, threshold = 1)
temp <- plnorm3(q = 2, meanlog = 0, sdlog = 1/8, threshold = 1)
temp
qlnorm3(p = temp, meanlog = 0, sdlog = 1/8, threshold = 1)
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3.3_Factorial_designs_Functions.R
\name{fracDesign}
\alias{fracDesign}
\title{fracDesign}
\usage{
fracDesign(
  k = 3,
  p = 0,
  gen = NULL,
  replicates = 1,
  blocks = 1,
  centerCube = 0,
  random.seed = 1234
)
}
\arguments{
\item{k}{Numeric value giving the number of factors. By default \code{k} is set to `3`.}

\item{p}{Numeric integer between `0` and `7`. p is giving the number of additional factors in the response surface design by aliasing effects.
A 2^k-p factorial design will be generated and the generators of the standard designs available in fracChoose() will be used.
By default p is set to `0`. Any other value will cause the function to omit the argument gen given by the user and replace it by the one out of the table of standard designs (see: \code{\link{fracChoose}}).
Replicates and blocks can be set anyway!}

\item{gen}{One or more defining relations for a fractional factorial design, for example:  \code{`C=AB`}. By default gen is set to \code{NULL}.}

\item{replicates}{Numeric value giving the number of replicates per factor combination. By default \code{replicates} is set to `1`.}

\item{blocks}{Numeric value giving the number of blocks. By default blocks is set to `1`.}

\item{centerCube}{Numeric value giving the number of center points within the 2^k design. By default \code{centerCube} is set to `0`.}

\item{random.seed}{Seed for randomization of the design}
}
\value{
The function \code{fracDesign} returns an object of class \code{\link{facDesign.c}}.
}
\description{
Generates a 2^k-p fractional factorial design.
}
\examples{
#Example 1
#Returns a 2^4-1 fractional factorial design. Factor D will be aliased with
vp.frac = fracDesign(k = 4, gen = "D=ABC")
#the three-way-interaction ABC (i.e. I = ABCD)
vp.frac$.response(rnorm(2^(4-1)))
# summary of the fractional factorial design
vp.frac$summary()

#Example 2
#Returns a full factorial design with 3 replications per factor combination and 4 center points
vp.rep = fracDesign(k = 3, replicates = 3, centerCube = 4)
#Summary of the replicated fractional factorial design
vp.rep$summary()
}
\seealso{
\code{\link{facDesign}}, \code{\link{fracChoose}}, \code{\link{rsmDesign}}, \code{\link{pbDesign}}, \code{\link{taguchiDesign}}
}

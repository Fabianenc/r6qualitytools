% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3.3_Factorial_designs_Functions.R
\name{overall}
\alias{overall}
\title{overall: Overall Desirability.}
\usage{
overall(fdo, steps = 20, constraints, ...)
}
\arguments{
\item{fdo}{An object of class \code{\link{facDesign.c}} containing \code{fits} and \code{desires}.}

\item{steps}{A numeric value indicating the number of points per factor to be evaluated, which also specifies the grid size. Default is `20`.}

\item{constraints}{A list of constraints for the factors in coded values, such as \code{list(A > 0.5, B < 0.2)}.}

\item{...}{Further arguments passed to other methods.}
}
\value{
A \code{data.frame} with a column for each factor, the desirability for each response, and a column for the overall desirability.
}
\description{
This function calculates the desirability for each response as well as the overall desirability. The resulting \code{data.frame} can be used to plot the overall desirability as well as the desirabilities for each response. This function is designed to visualize the desirability approach for multiple response optimization.
}
\examples{
#Example 1: Arbitrary example with random data
rsdo = rsmDesign(k = 2, blocks = 2, alpha = "both")
rsdo$.response(data.frame(y = rnorm(rsdo$nrow()), y2 = rnorm(rsdo$nrow())))
rsdo$set.fits(rsdo$lm(y ~ A*B + I(A^2) + I(B^2)))
rsdo$set.fits(rsdo$lm(y2 ~ A*B + I(A^2) + I(B^2)))
rsdo$desires(desirability(y, -1, 2, scale = c(1, 1), target = "max"))
rsdo$desires(desirability(y2, -1, 0, scale = c(1, 1), target = "min"))
dVals = overall(rsdo, steps = 10, constraints = list(A = c(-0.5,1), B = c(0, 1)))
}
\seealso{
\code{\link{facDesign}}, \code{\link{rsmDesign}}, \code{\link{desirability}}.
}

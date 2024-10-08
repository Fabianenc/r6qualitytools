% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/2.3_gageRR_Functions.R
\name{gageRRDesign}
\alias{gageRRDesign}
\title{gageRRDesign: Gage R&R - Gage Repeatability and Reproducibility}
\usage{
gageRRDesign(
  Operators = 3,
  Parts = 10,
  Measurements = 3,
  method = "crossed",
  sigma = 6,
  randomize = TRUE
)
}
\arguments{
\item{Operators}{Numeric value giving a number or a character vector defining the Operators.
By default \code{Operators} is set to `3`.}

\item{Parts}{A number or character vector defining the Parts.
By default \code{parts} is set to `10`.}

\item{Measurements}{A number defining the measurements per part. By default \code{Measurements} is set to `3`.}

\item{method}{Character string specifying the Gage R&R method. \code{`crossed`} which is the typical design for performing a Measurement Systems Analysis using Gage Repeatability and Reproducibility or \code{`nested`} which is used for destructive testing (i.e. the same part cannot be measured twice). Operators measure each a different sample of parts under the premise that the parts of each batch are alike.
By default \code{method} is set to \code{`crossed`}.}

\item{sigma}{For \code{sigma=6} this relates to 99.73 percent representing the full spread of a normal distribution function (i.e. \code{pnorm(3) - pnorm(-3)}).
Another popular setting \code{sigma=5.15} relates to 99 percent (i.e. \code{pnorm(2.575) - pnorm(-2.575)}). By default \code{sigma} is set to `6`.}

\item{randomize}{Logical value. \code{TRUE} (default) randomizes the gageRR design.}
}
\value{
The function \code{gageRRDesign} returns an object of class \code{gageRR}.
}
\description{
Function to Creates a Gage R&R design.
}
\examples{
design <- gageRRDesign(Operators = 3, Parts = 10, Measurements = 3,
                       method = "crossed", sigma = 6, randomize = TRUE)
}
\seealso{
\code{\link{gageRR.c}}, \code{\link{gageRR}}.
}

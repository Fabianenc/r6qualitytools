% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/6.3_Plackett_Burman_designs_Functions.R
\name{pbDesign}
\alias{pbDesign}
\title{pbDesign: Plackett-Burman Designs}
\usage{
pbDesign(n, k, randomize = TRUE, replicates = 1)
}
\arguments{
\item{n}{Integer value giving the number of trials.}

\item{k}{Integer value giving the number of factors.}

\item{randomize}{A logical value (\code{TRUE}/\code{FALSE}) that specifies whether to randomize the RunOrder of the design.
By default, \code{randomize} is set to \code{TRUE}.}

\item{replicates}{An integer specifying the number of replicates for each run in the design.}
}
\value{
A \code{pbDesign} returns an object of class \code{pbDesign}.
}
\description{
Function to create a Plackett-Burman  design.
}
\note{
This function creates Placket-Burman Designs down to n=26. Bigger Designs are not implemented because of lack in practicability. For the creation either the number of factors or the number of trials can be denoted. Wrong combinations will lead to an error message. Originally Placket-Burman-Design are applicable for number of trials divisible by 4. If n is not divisble by 4 this function will take the next larger Placket-Burman Design and truncate the last rows and columns.
}
\examples{
pbdo<- pbDesign(n=5)
pbdo$summary()
}
\seealso{
\itemize{
\item{\code{\link{facDesign}}: for 2^k factorial designs.}
\item{\code{\link{rsmDesign}}: for response surface designs.}
\item{\code{\link{fracDesign}}: for fractional factorial design.}
\item{\code{\link{gageRRDesign}}: for gage designs.}
}
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/2.2_gageRR_Classes.R
\name{gageRR.c}
\alias{gageRR.c}
\title{gageRR-class: Class `gageRR`}
\description{
R6 Class for Gage R&R (Repeatability and Reproducibility) Analysis
}
\examples{
#create gageRR-object
gdo <- gageRRDesign(Operators = 3, Parts = 10, Measurements = 3, randomize = FALSE)
#vector of responses
y <- c(0.29,0.08, 0.04,-0.56,-0.47,-1.38,1.34,1.19,0.88,0.47,0.01,0.14,-0.80,
      -0.56,-1.46, 0.02,-0.20,-0.29,0.59,0.47,0.02,-0.31,-0.63,-0.46,2.26,
      1.80,1.77,-1.36,-1.68,-1.49,0.41,0.25,-0.11,-0.68,-1.22,-1.13,1.17,0.94,
      1.09,0.50,1.03,0.20,-0.92,-1.20,-1.07,-0.11, 0.22,-0.67,0.75,0.55,0.01,
      -0.20, 0.08,-0.56,1.99,2.12,1.45,-1.25,-1.62,-1.77,0.64,0.07,-0.15,-0.58,
      -0.68,-0.96,1.27,1.34,0.67,0.64,0.20,0.11,-0.84,-1.28,-1.45,-0.21,0.06,
      -0.49,0.66,0.83,0.21,-0.17,-0.34,-0.49,2.01,2.19,1.87,-1.31,-1.50,-2.16)

#appropriate responses
gdo$response(y)
# perform and gageRR
gdo <- gageRR(gdo)

# Using the plots
gdo$plot()

## ------------------------------------------------
## Method `gageRR.c$plot`
## ------------------------------------------------

# Create gageRR-object
gdo = gageRRDesign(Operators = 3, Parts = 10, Measurements = 3, randomize = FALSE)
# Vector of responses
y = c(0.29,0.08, 0.04,-0.56,-0.47,-1.38,1.34,1.19,0.88,0.47,0.01,0.14,-0.80,
      -0.56,-1.46, 0.02,-0.20,-0.29,0.59,0.47,0.02,-0.31,-0.63,-0.46,2.26,
      1.80,1.77,-1.36,-1.68,-1.49,0.41,0.25,-0.11,-0.68,-1.22,-1.13,1.17,0.94,
      1.09,0.50,1.03,0.20,-0.92,-1.20,-1.07,-0.11, 0.22,-0.67,0.75,0.55,0.01,
      -0.20, 0.08,-0.56,1.99,2.12,1.45,-1.25,-1.62,-1.77,0.64,0.07,-0.15,-0.58,
      -0.68,-0.96,1.27,1.34,0.67,0.64,0.20,0.11,-0.84,-1.28,-1.45,-0.21,0.06,
      -0.49,0.66,0.83,0.21,-0.17,-0.34,-0.49,2.01,2.19,1.87,-1.31,-1.50,-2.16)

# Appropriate responses
gdo$response(y)
# Perform and gageRR
gdo <- gageRR(gdo)

gdo$plot()

## ------------------------------------------------
## Method `gageRR.c$errorPlot`
## ------------------------------------------------

# Create gageRR-object
gdo = gageRRDesign(Operators = 3, Parts = 10, Measurements = 3, randomize = FALSE)
# Vector of responses
y = c(0.29,0.08, 0.04,-0.56,-0.47,-1.38,1.34,1.19,0.88,0.47,0.01,0.14,-0.80,
      -0.56,-1.46, 0.02,-0.20,-0.29,0.59,0.47,0.02,-0.31,-0.63,-0.46,2.26,
      1.80,1.77,-1.36,-1.68,-1.49,0.41,0.25,-0.11,-0.68,-1.22,-1.13,1.17,0.94,
      1.09,0.50,1.03,0.20,-0.92,-1.20,-1.07,-0.11, 0.22,-0.67,0.75,0.55,0.01,
      -0.20, 0.08,-0.56,1.99,2.12,1.45,-1.25,-1.62,-1.77,0.64,0.07,-0.15,-0.58,
      -0.68,-0.96,1.27,1.34,0.67,0.64,0.20,0.11,-0.84,-1.28,-1.45,-0.21,0.06,
      -0.49,0.66,0.83,0.21,-0.17,-0.34,-0.49,2.01,2.19,1.87,-1.31,-1.50,-2.16)

# Appropriate responses
gdo$response(y)
# Perform and gageRR
gdo <- gageRR(gdo)

gdo$errorPlot()

## ------------------------------------------------
## Method `gageRR.c$whiskersPlot`
## ------------------------------------------------

# Create gageRR-object
gdo = gageRRDesign(Operators = 3, Parts = 10, Measurements = 3, randomize = FALSE)
# Vector of responses
y = c(0.29,0.08, 0.04,-0.56,-0.47,-1.38,1.34,1.19,0.88,0.47,0.01,0.14,-0.80,
      -0.56,-1.46, 0.02,-0.20,-0.29,0.59,0.47,0.02,-0.31,-0.63,-0.46,2.26,
      1.80,1.77,-1.36,-1.68,-1.49,0.41,0.25,-0.11,-0.68,-1.22,-1.13,1.17,0.94,
      1.09,0.50,1.03,0.20,-0.92,-1.20,-1.07,-0.11, 0.22,-0.67,0.75,0.55,0.01,
      -0.20, 0.08,-0.56,1.99,2.12,1.45,-1.25,-1.62,-1.77,0.64,0.07,-0.15,-0.58,
      -0.68,-0.96,1.27,1.34,0.67,0.64,0.20,0.11,-0.84,-1.28,-1.45,-0.21,0.06,
      -0.49,0.66,0.83,0.21,-0.17,-0.34,-0.49,2.01,2.19,1.87,-1.31,-1.50,-2.16)

# Appropriate responses
gdo$response(y)
# Perform and gageRR
gdo <- gageRR(gdo)

gdo$whiskersPlot()

## ------------------------------------------------
## Method `gageRR.c$averagePlot`
## ------------------------------------------------

# Create gageRR-object
gdo = gageRRDesign(Operators = 3, Parts = 10, Measurements = 3, randomize = FALSE)
# Vector of responses
y = c(0.29,0.08, 0.04,-0.56,-0.47,-1.38,1.34,1.19,0.88,0.47,0.01,0.14,-0.80,
      -0.56,-1.46, 0.02,-0.20,-0.29,0.59,0.47,0.02,-0.31,-0.63,-0.46,2.26,
      1.80,1.77,-1.36,-1.68,-1.49,0.41,0.25,-0.11,-0.68,-1.22,-1.13,1.17,0.94,
      1.09,0.50,1.03,0.20,-0.92,-1.20,-1.07,-0.11, 0.22,-0.67,0.75,0.55,0.01,
      -0.20, 0.08,-0.56,1.99,2.12,1.45,-1.25,-1.62,-1.77,0.64,0.07,-0.15,-0.58,
      -0.68,-0.96,1.27,1.34,0.67,0.64,0.20,0.11,-0.84,-1.28,-1.45,-0.21,0.06,
      -0.49,0.66,0.83,0.21,-0.17,-0.34,-0.49,2.01,2.19,1.87,-1.31,-1.50,-2.16)

# Appropriate responses
gdo$response(y)
# Perform and gageRR
gdo <- gageRR(gdo)

gdo$averagePlot()

## ------------------------------------------------
## Method `gageRR.c$compPlot`
## ------------------------------------------------

# Create gageRR-object
gdo = gageRRDesign(Operators = 3, Parts = 10, Measurements = 3, randomize = FALSE)
# Vector of responses
y = c(0.29,0.08, 0.04,-0.56,-0.47,-1.38,1.34,1.19,0.88,0.47,0.01,0.14,-0.80,
      -0.56,-1.46, 0.02,-0.20,-0.29,0.59,0.47,0.02,-0.31,-0.63,-0.46,2.26,
      1.80,1.77,-1.36,-1.68,-1.49,0.41,0.25,-0.11,-0.68,-1.22,-1.13,1.17,0.94,
      1.09,0.50,1.03,0.20,-0.92,-1.20,-1.07,-0.11, 0.22,-0.67,0.75,0.55,0.01,
      -0.20, 0.08,-0.56,1.99,2.12,1.45,-1.25,-1.62,-1.77,0.64,0.07,-0.15,-0.58,
      -0.68,-0.96,1.27,1.34,0.67,0.64,0.20,0.11,-0.84,-1.28,-1.45,-0.21,0.06,
      -0.49,0.66,0.83,0.21,-0.17,-0.34,-0.49,2.01,2.19,1.87,-1.31,-1.50,-2.16)

# Appropriate responses
gdo$response(y)
# Perform and gageRR
gdo <- gageRR(gdo)

gdo$compPlot()
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{X}}{Data frame containing the measurement data.}

\item{\code{ANOVA}}{List containing the results of the Analysis of Variance (ANOVA) for the gage study.}

\item{\code{RedANOVA}}{List containing the results of the reduced ANOVA.}

\item{\code{method}}{Character string specifying the method used for the analysis (e.g., \code{`crossed`}, \code{`nested`}).}

\item{\code{Estimates}}{List of estimates including variance components, repeatability, and reproducibility.}

\item{\code{Varcomp}}{List of variance components.}

\item{\code{Sigma}}{Numeric value representing the standard deviation of the measurement system.}

\item{\code{GageName}}{Character string representing the name of the gage.}

\item{\code{GageTolerance}}{Numeric value indicating the tolerance of the gage.}

\item{\code{DateOfStudy}}{Character string representing the date of the gage R&R study.}

\item{\code{PersonResponsible}}{Character string indicating the person responsible for the study.}

\item{\code{Comments}}{Character string for additional comments or notes about the study.}

\item{\code{b}}{Factor levels for operator.}

\item{\code{a}}{Factor levels for part.}

\item{\code{y}}{Numeric vector or matrix containing the measurement responses.}

\item{\code{facNames}}{Character vector specifying the names of the factors (e.g., \code{`Operator`}, \code{`Part`}).}

\item{\code{numO}}{Integer representing the number of operators.}

\item{\code{numP}}{Integer representing the number of parts.}

\item{\code{numM}}{Integer representing the number of measurements per part-operator combination.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-gageRR.c-new}{\code{gageRR.c$new()}}
\item \href{#method-gageRR.c-print}{\code{gageRR.c$print()}}
\item \href{#method-gageRR.c-subset}{\code{gageRR.c$subset()}}
\item \href{#method-gageRR.c-summary}{\code{gageRR.c$summary()}}
\item \href{#method-gageRR.c-get.response}{\code{gageRR.c$get.response()}}
\item \href{#method-gageRR.c-response}{\code{gageRR.c$response()}}
\item \href{#method-gageRR.c-names}{\code{gageRR.c$names()}}
\item \href{#method-gageRR.c-as.data.frame}{\code{gageRR.c$as.data.frame()}}
\item \href{#method-gageRR.c-get.tolerance}{\code{gageRR.c$get.tolerance()}}
\item \href{#method-gageRR.c-set.tolerance}{\code{gageRR.c$set.tolerance()}}
\item \href{#method-gageRR.c-get.sigma}{\code{gageRR.c$get.sigma()}}
\item \href{#method-gageRR.c-set.sigma}{\code{gageRR.c$set.sigma()}}
\item \href{#method-gageRR.c-plot}{\code{gageRR.c$plot()}}
\item \href{#method-gageRR.c-errorPlot}{\code{gageRR.c$errorPlot()}}
\item \href{#method-gageRR.c-whiskersPlot}{\code{gageRR.c$whiskersPlot()}}
\item \href{#method-gageRR.c-averagePlot}{\code{gageRR.c$averagePlot()}}
\item \href{#method-gageRR.c-compPlot}{\code{gageRR.c$compPlot()}}
\item \href{#method-gageRR.c-clone}{\code{gageRR.c$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-gageRR.c-new"></a>}}
\if{latex}{\out{\hypertarget{method-gageRR.c-new}{}}}
\subsection{Method \code{new()}}{
Initialize the fiels of the \code{gageRR} object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{gageRR.c$new(
  X,
  ANOVA = NULL,
  RedANOVA = NULL,
  method = NULL,
  Estimates = NULL,
  Varcomp = NULL,
  Sigma = NULL,
  GageName = NULL,
  GageTolerance = NULL,
  DateOfStudy = NULL,
  PersonResponsible = NULL,
  Comments = NULL,
  b = NULL,
  a = NULL,
  y = NULL,
  facNames = NULL,
  numO = NULL,
  numP = NULL,
  numM = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{X}}{Data frame containing the measurement data.}

\item{\code{ANOVA}}{List containing the results of the Analysis of Variance (ANOVA) for the gage study.}

\item{\code{RedANOVA}}{List containing the results of the reduced ANOVA.}

\item{\code{method}}{Character string specifying the method used for the analysis (e.g., "crossed", "nested").}

\item{\code{Estimates}}{List of estimates including variance components, repeatability, and reproducibility.}

\item{\code{Varcomp}}{List of variance components.}

\item{\code{Sigma}}{Numeric value representing the standard deviation of the measurement system.}

\item{\code{GageName}}{Character string representing the name of the gage.}

\item{\code{GageTolerance}}{Numeric value indicating the tolerance of the gage.}

\item{\code{DateOfStudy}}{Character string representing the date of the gage R&R study.}

\item{\code{PersonResponsible}}{Character string indicating the person responsible for the study.}

\item{\code{Comments}}{Character string for additional comments or notes about the study.}

\item{\code{b}}{Factor levels for operator.}

\item{\code{a}}{Factor levels for part.}

\item{\code{y}}{Numeric vector or matrix containing the measurement responses.}

\item{\code{facNames}}{Character vector specifying the names of the factors (e.g., "Operator", "Part").}

\item{\code{numO}}{Integer representing the number of operators.}

\item{\code{numP}}{Integer representing the number of parts.}

\item{\code{numM}}{Integer representing the number of measurements per part-operator combination.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-gageRR.c-print"></a>}}
\if{latex}{\out{\hypertarget{method-gageRR.c-print}{}}}
\subsection{Method \code{print()}}{
Return the data frame containing the measurement data (\code{X})
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{gageRR.c$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-gageRR.c-subset"></a>}}
\if{latex}{\out{\hypertarget{method-gageRR.c-subset}{}}}
\subsection{Method \code{subset()}}{
Return a subset of the data frame that containing the measurement data (\code{X})
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{gageRR.c$subset(i, j)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{i}}{The i-position of the row of \code{X}.}

\item{\code{j}}{The j-position of the column of \code{X}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-gageRR.c-summary"></a>}}
\if{latex}{\out{\hypertarget{method-gageRR.c-summary}{}}}
\subsection{Method \code{summary()}}{
Summarize the information of the fields of the \code{gageRR} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{gageRR.c$summary()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-gageRR.c-get.response"></a>}}
\if{latex}{\out{\hypertarget{method-gageRR.c-get.response}{}}}
\subsection{Method \code{get.response()}}{
Get or get the response for a \code{gageRRDesign} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{gageRR.c$get.response()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-gageRR.c-response"></a>}}
\if{latex}{\out{\hypertarget{method-gageRR.c-response}{}}}
\subsection{Method \code{response()}}{
Set the response for a \code{gageRRDesign} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{gageRR.c$response(value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{New response vector.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-gageRR.c-names"></a>}}
\if{latex}{\out{\hypertarget{method-gageRR.c-names}{}}}
\subsection{Method \code{names()}}{
Methods for function \code{names} in Package \code{base}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{gageRR.c$names()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-gageRR.c-as.data.frame"></a>}}
\if{latex}{\out{\hypertarget{method-gageRR.c-as.data.frame}{}}}
\subsection{Method \code{as.data.frame()}}{
Methods for function \code{as.data.frame} in Package \code{base}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{gageRR.c$as.data.frame()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-gageRR.c-get.tolerance"></a>}}
\if{latex}{\out{\hypertarget{method-gageRR.c-get.tolerance}{}}}
\subsection{Method \code{get.tolerance()}}{
Get the \code{tolerance} for an object of class \code{gageRR}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{gageRR.c$get.tolerance()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-gageRR.c-set.tolerance"></a>}}
\if{latex}{\out{\hypertarget{method-gageRR.c-set.tolerance}{}}}
\subsection{Method \code{set.tolerance()}}{
Set the \code{tolerance} for an object of class \code{gageRR}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{gageRR.c$set.tolerance(value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{A data.frame or vector for the new value of tolerance.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-gageRR.c-get.sigma"></a>}}
\if{latex}{\out{\hypertarget{method-gageRR.c-get.sigma}{}}}
\subsection{Method \code{get.sigma()}}{
Get the \code{sigma} for an object of class \code{gageRR}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{gageRR.c$get.sigma()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-gageRR.c-set.sigma"></a>}}
\if{latex}{\out{\hypertarget{method-gageRR.c-set.sigma}{}}}
\subsection{Method \code{set.sigma()}}{
Set the \code{sigma} for an object of class \code{gageRR}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{gageRR.c$set.sigma(value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{Valor of \code{sigma}}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-gageRR.c-plot"></a>}}
\if{latex}{\out{\hypertarget{method-gageRR.c-plot}{}}}
\subsection{Method \code{plot()}}{
This function creates a customized plot using the data from the \code{gageRR.c} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{gageRR.c$plot(main = NULL, xlab = NULL, ylab = NULL, col, lwd, fun = mean)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{main}}{Character string specifying the title of the plot.}

\item{\code{xlab}}{A character string for the x-axis label.}

\item{\code{ylab}}{A character string for the y-axis label.}

\item{\code{col}}{A character string or vector specifying the color(s) to be used for the plot elements.}

\item{\code{lwd}}{A numeric value specifying the line width of plot elements}

\item{\code{fun}}{Function to use for the calculation of the interactions (e.g., \code{mean}, \code{median}). Default is \code{mean}.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{# Create gageRR-object
gdo = gageRRDesign(Operators = 3, Parts = 10, Measurements = 3, randomize = FALSE)
# Vector of responses
y = c(0.29,0.08, 0.04,-0.56,-0.47,-1.38,1.34,1.19,0.88,0.47,0.01,0.14,-0.80,
      -0.56,-1.46, 0.02,-0.20,-0.29,0.59,0.47,0.02,-0.31,-0.63,-0.46,2.26,
      1.80,1.77,-1.36,-1.68,-1.49,0.41,0.25,-0.11,-0.68,-1.22,-1.13,1.17,0.94,
      1.09,0.50,1.03,0.20,-0.92,-1.20,-1.07,-0.11, 0.22,-0.67,0.75,0.55,0.01,
      -0.20, 0.08,-0.56,1.99,2.12,1.45,-1.25,-1.62,-1.77,0.64,0.07,-0.15,-0.58,
      -0.68,-0.96,1.27,1.34,0.67,0.64,0.20,0.11,-0.84,-1.28,-1.45,-0.21,0.06,
      -0.49,0.66,0.83,0.21,-0.17,-0.34,-0.49,2.01,2.19,1.87,-1.31,-1.50,-2.16)

# Appropriate responses
gdo$response(y)
# Perform and gageRR
gdo <- gageRR(gdo)

gdo$plot()
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-gageRR.c-errorPlot"></a>}}
\if{latex}{\out{\hypertarget{method-gageRR.c-errorPlot}{}}}
\subsection{Method \code{errorPlot()}}{
The data from an object of class \code{gageRR} can be analyzed by running `Error Charts` of the individual deviations from the accepted rference values. These `Error Charts` are provided by the function \code{errorPlot}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{gageRR.c$errorPlot(main, xlab, ylab, col, pch, ylim, legend = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{main}}{a main title for the plot.}

\item{\code{xlab}}{A character string for the x-axis label.}

\item{\code{ylab}}{A character string for the y-axis label.}

\item{\code{col}}{Plotting color.}

\item{\code{pch}}{An integer specifying a symbol or a single character to be used as the default in plotting points.}

\item{\code{ylim}}{The y limits of the plot.}

\item{\code{legend}}{A logical value specifying whether a legend is plotted automatically. By default legend is set to `TRUE`.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{# Create gageRR-object
gdo = gageRRDesign(Operators = 3, Parts = 10, Measurements = 3, randomize = FALSE)
# Vector of responses
y = c(0.29,0.08, 0.04,-0.56,-0.47,-1.38,1.34,1.19,0.88,0.47,0.01,0.14,-0.80,
      -0.56,-1.46, 0.02,-0.20,-0.29,0.59,0.47,0.02,-0.31,-0.63,-0.46,2.26,
      1.80,1.77,-1.36,-1.68,-1.49,0.41,0.25,-0.11,-0.68,-1.22,-1.13,1.17,0.94,
      1.09,0.50,1.03,0.20,-0.92,-1.20,-1.07,-0.11, 0.22,-0.67,0.75,0.55,0.01,
      -0.20, 0.08,-0.56,1.99,2.12,1.45,-1.25,-1.62,-1.77,0.64,0.07,-0.15,-0.58,
      -0.68,-0.96,1.27,1.34,0.67,0.64,0.20,0.11,-0.84,-1.28,-1.45,-0.21,0.06,
      -0.49,0.66,0.83,0.21,-0.17,-0.34,-0.49,2.01,2.19,1.87,-1.31,-1.50,-2.16)

# Appropriate responses
gdo$response(y)
# Perform and gageRR
gdo <- gageRR(gdo)

gdo$errorPlot()
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-gageRR.c-whiskersPlot"></a>}}
\if{latex}{\out{\hypertarget{method-gageRR.c-whiskersPlot}{}}}
\subsection{Method \code{whiskersPlot()}}{
In a Whiskers Chart, the high and low data values and the average (median) by part-by-operator are plotted to provide insight into the consistency between operators, to indicate outliers and to discover part-operator interactions. The Whiskers Chart reminds of boxplots for every part and every operator.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{gageRR.c$whiskersPlot(main, xlab, ylab, col, ylim, legend = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{main}}{a main title for the plot.}

\item{\code{xlab}}{A character string for the x-axis label.}

\item{\code{ylab}}{A character string for the y-axis label.}

\item{\code{col}}{Plotting color.}

\item{\code{ylim}}{The y limits of the plot.}

\item{\code{legend}}{A logical value specifying whether a legend is plotted automatically. By default legend is set to `TRUE`.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{# Create gageRR-object
gdo = gageRRDesign(Operators = 3, Parts = 10, Measurements = 3, randomize = FALSE)
# Vector of responses
y = c(0.29,0.08, 0.04,-0.56,-0.47,-1.38,1.34,1.19,0.88,0.47,0.01,0.14,-0.80,
      -0.56,-1.46, 0.02,-0.20,-0.29,0.59,0.47,0.02,-0.31,-0.63,-0.46,2.26,
      1.80,1.77,-1.36,-1.68,-1.49,0.41,0.25,-0.11,-0.68,-1.22,-1.13,1.17,0.94,
      1.09,0.50,1.03,0.20,-0.92,-1.20,-1.07,-0.11, 0.22,-0.67,0.75,0.55,0.01,
      -0.20, 0.08,-0.56,1.99,2.12,1.45,-1.25,-1.62,-1.77,0.64,0.07,-0.15,-0.58,
      -0.68,-0.96,1.27,1.34,0.67,0.64,0.20,0.11,-0.84,-1.28,-1.45,-0.21,0.06,
      -0.49,0.66,0.83,0.21,-0.17,-0.34,-0.49,2.01,2.19,1.87,-1.31,-1.50,-2.16)

# Appropriate responses
gdo$response(y)
# Perform and gageRR
gdo <- gageRR(gdo)

gdo$whiskersPlot()
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-gageRR.c-averagePlot"></a>}}
\if{latex}{\out{\hypertarget{method-gageRR.c-averagePlot}{}}}
\subsection{Method \code{averagePlot()}}{
\code{averagePlot} creates all x-y plots of averages by size out of an object of class \code{gageRR}. Therfore the averages of the multiple readings by each operator on each part are plotted with the reference value or overall part averages as the index.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{gageRR.c$averagePlot(main, xlab, ylab, col, single = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{main}}{a main title for the plot.}

\item{\code{xlab}}{A character string for the x-axis label.}

\item{\code{ylab}}{A character string for the y-axis label.}

\item{\code{col}}{Plotting color.}

\item{\code{single}}{A logical value.If `TRUE` a new graphic device will be opened for each plot. By default \code{single} is set to `FALSE`.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{# Create gageRR-object
gdo = gageRRDesign(Operators = 3, Parts = 10, Measurements = 3, randomize = FALSE)
# Vector of responses
y = c(0.29,0.08, 0.04,-0.56,-0.47,-1.38,1.34,1.19,0.88,0.47,0.01,0.14,-0.80,
      -0.56,-1.46, 0.02,-0.20,-0.29,0.59,0.47,0.02,-0.31,-0.63,-0.46,2.26,
      1.80,1.77,-1.36,-1.68,-1.49,0.41,0.25,-0.11,-0.68,-1.22,-1.13,1.17,0.94,
      1.09,0.50,1.03,0.20,-0.92,-1.20,-1.07,-0.11, 0.22,-0.67,0.75,0.55,0.01,
      -0.20, 0.08,-0.56,1.99,2.12,1.45,-1.25,-1.62,-1.77,0.64,0.07,-0.15,-0.58,
      -0.68,-0.96,1.27,1.34,0.67,0.64,0.20,0.11,-0.84,-1.28,-1.45,-0.21,0.06,
      -0.49,0.66,0.83,0.21,-0.17,-0.34,-0.49,2.01,2.19,1.87,-1.31,-1.50,-2.16)

# Appropriate responses
gdo$response(y)
# Perform and gageRR
gdo <- gageRR(gdo)

gdo$averagePlot()
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-gageRR.c-compPlot"></a>}}
\if{latex}{\out{\hypertarget{method-gageRR.c-compPlot}{}}}
\subsection{Method \code{compPlot()}}{
\code{compPlot} creates comparison x-y plots of an object of class \code{gageRR}. The averages of the multiple readings by each operator on each part are plotted against each other with the operators as indices. This plot compares the values obtained by one operator to those of another.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{gageRR.c$compPlot(main, xlab, ylab, col, cex.lab, fun = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{main}}{a main title for the plot.}

\item{\code{xlab}}{A character string for the x-axis label.}

\item{\code{ylab}}{A character string for the y-axis label.}

\item{\code{col}}{Plotting color.}

\item{\code{cex.lab}}{The magnification to be used for x and y labels relative to the current setting of cex.}

\item{\code{fun}}{Optional function that will be applied to the multiple readings of each part. fun should be an object of class \code{function} like \code{mean},\code{median}, \code{sum}, etc. By default, \code{fun} is set to `NULL` and all readings will be plotted.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{# Create gageRR-object
gdo = gageRRDesign(Operators = 3, Parts = 10, Measurements = 3, randomize = FALSE)
# Vector of responses
y = c(0.29,0.08, 0.04,-0.56,-0.47,-1.38,1.34,1.19,0.88,0.47,0.01,0.14,-0.80,
      -0.56,-1.46, 0.02,-0.20,-0.29,0.59,0.47,0.02,-0.31,-0.63,-0.46,2.26,
      1.80,1.77,-1.36,-1.68,-1.49,0.41,0.25,-0.11,-0.68,-1.22,-1.13,1.17,0.94,
      1.09,0.50,1.03,0.20,-0.92,-1.20,-1.07,-0.11, 0.22,-0.67,0.75,0.55,0.01,
      -0.20, 0.08,-0.56,1.99,2.12,1.45,-1.25,-1.62,-1.77,0.64,0.07,-0.15,-0.58,
      -0.68,-0.96,1.27,1.34,0.67,0.64,0.20,0.11,-0.84,-1.28,-1.45,-0.21,0.06,
      -0.49,0.66,0.83,0.21,-0.17,-0.34,-0.49,2.01,2.19,1.87,-1.31,-1.50,-2.16)

# Appropriate responses
gdo$response(y)
# Perform and gageRR
gdo <- gageRR(gdo)

gdo$compPlot()
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-gageRR.c-clone"></a>}}
\if{latex}{\out{\hypertarget{method-gageRR.c-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{gageRR.c$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}

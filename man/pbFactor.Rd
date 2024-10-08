% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/6.2_Plackett_Burman_designs_Classes.R
\name{pbFactor}
\alias{pbFactor}
\title{pbFactor}
\description{
An R6 class representing a factor in a Plackett-Burman design.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{values}}{A vector containing the levels or values associated with the factor. Default is \code{NA}.}

\item{\code{name}}{A character string specifying the name of the factor. Default is an empty string \code{``}.}

\item{\code{unit}}{A character string specifying the unit of measurement for the factor. Default is an empty string \code{``}.}

\item{\code{type}}{A character string specifying the type of the factor, which can be either \code{`numeric`} or \code{`categorical`}. Default is \code{`numeric`}.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-pbFactor-attributes}{\code{pbFactor$attributes()}}
\item \href{#method-pbFactor-.values}{\code{pbFactor$.values()}}
\item \href{#method-pbFactor-.unit}{\code{pbFactor$.unit()}}
\item \href{#method-pbFactor-names}{\code{pbFactor$names()}}
\item \href{#method-pbFactor-clone}{\code{pbFactor$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-pbFactor-attributes"></a>}}
\if{latex}{\out{\hypertarget{method-pbFactor-attributes}{}}}
\subsection{Method \code{attributes()}}{
Get the attributes of the factor.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pbFactor$attributes()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-pbFactor-.values"></a>}}
\if{latex}{\out{\hypertarget{method-pbFactor-.values}{}}}
\subsection{Method \code{.values()}}{
Get and set the \code{values} for the factors in an object of class \code{pbFactor}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pbFactor$.values(value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{New values, If missing value get the \code{values}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-pbFactor-.unit"></a>}}
\if{latex}{\out{\hypertarget{method-pbFactor-.unit}{}}}
\subsection{Method \code{.unit()}}{
Get and set the \code{units} for the factors in an object of class \code{pbFactor}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pbFactor$.unit(value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{New unit, If missing value get the \code{units}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-pbFactor-names"></a>}}
\if{latex}{\out{\hypertarget{method-pbFactor-names}{}}}
\subsection{Method \code{names()}}{
Get and set the \code{names} in an object of class \code{pbFactor}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pbFactor$names(value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{New names, If missing value get the \code{names}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-pbFactor-clone"></a>}}
\if{latex}{\out{\hypertarget{method-pbFactor-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pbFactor$clone(deep = FALSE)}\if{html}{\out{</div>}}
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

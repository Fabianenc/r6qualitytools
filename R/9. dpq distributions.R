#### WEIBULL3 ####
#' @title dweibull3: The Weibull Distribution (3 Parameter)
#' @description Density function, distribution function, and quantile function for the Weibull distribution with a threshold parameter.
#'
#' @usage
#' dweibull3(x, shape, scale, threshold)
#' pweibull3(q, shape, scale, threshold)
#' qweibull3(p, shape, scale, threshold, ...)
#'
#' @param x,q A numeric vector of quantiles.
#' @param p A numeric vector of probabilities.
#' @param shape The shape parameter of the Weibull distribution. Default is 1.
#' @param scale The scale parameter of the Weibull distribution. Default is 1.
#' @param threshold The threshold (or location) parameter of the Weibull distribution. Default is 0.
#' @param ... Additional arguments passed to \code{uniroot} for \code{qweibull3}.
#'
#' @details The Weibull distribution with the \code{scale} parameter alpha, \code{shape} parameter c, and \code{threshold} parameter zeta has a density function given by:
#' \deqn{f(x) = \frac{c}{\alpha} \left(\frac{x - \zeta}{\alpha}\right)^{c-1} \exp\left(-\left(\frac{x - \zeta}{\alpha}\right)^c\right)}
#'
#' The cumulative distribution function is given by:
#' \deqn{F(x) = 1 - \exp\left(-\left(\frac{x - \zeta}{\alpha}\right)^c\right)}
#'
#' @return \code{dweibull3} returns the density, \code{pweibull3} returns the distribution function, and \code{qweibull3} returns the quantile function for the Weibull distribution with a threshold.
#'
#' @examples
#' dweibull3(x = 1, scale = 1, shape = 5, threshold = 0)
#' temp <- pweibull3(q = 1, scale = 1, shape = 5, threshold = 0)
#' temp
#' qweibull3(p = temp, scale = 1, shape = 5, threshold = 0)
dweibull3 <- function(x,shape,scale,threshold)
{
  if(missing(x))
    stop("x must be a vector")
  if(missing(threshold))
    threshold=0
  if(missing(shape))
    shape=1
  if(missing(scale))
    scale=1
  temp=function(x)
  {
    if(x>=threshold)
      return((shape/scale)*(((x-threshold)/scale)^(shape-1))*exp(-((x-threshold)/scale)^shape))
    else
      return(0)
  }
  return(unlist(lapply(x,temp)))
}

#' @title pweibull3: The Weibull Distribution (3 Parameter)
#' @description Density function, distribution function, and quantile function for the Weibull distribution with a threshold parameter.
#'
#' @usage
#' dweibull3(x, shape, scale, threshold)
#' pweibull3(q, shape, scale, threshold)
#' qweibull3(p, shape, scale, threshold, ...)
#'
#' @param x,q A numeric vector of quantiles.
#' @param p A numeric vector of probabilities.
#' @param shape The shape parameter of the Weibull distribution. Default is 1.
#' @param scale The scale parameter of the Weibull distribution. Default is 1.
#' @param threshold The threshold (or location) parameter of the Weibull distribution. Default is 0.
#' @param ... Additional arguments passed to \code{uniroot} for \code{qweibull3}.
#'
#' @details The Weibull distribution with the \code{scale} parameter alpha, \code{shape} parameter c, and \code{threshold} parameter zeta has a density function given by:
#' \deqn{f(x) = \frac{c}{\alpha} \left(\frac{x - \zeta}{\alpha}\right)^{c-1} \exp\left(-\left(\frac{x - \zeta}{\alpha}\right)^c\right)}
#'
#' The cumulative distribution function is given by:
#' \deqn{F(x) = 1 - \exp\left(-\left(\frac{x - \zeta}{\alpha}\right)^c\right)}
#'
#' @return \code{dweibull3} returns the density, \code{pweibull3} returns the distribution function, and \code{qweibull3} returns the quantile function for the Weibull distribution with a threshold.
#'
#' @examples
#' dweibull3(x = 1, scale = 1, shape = 5, threshold = 0)
#' temp <- pweibull3(q = 1, scale = 1, shape = 5, threshold = 0)
#' temp
#' qweibull3(p = temp, scale = 1, shape = 5, threshold = 0)
pweibull3 <- function(q,shape,scale,threshold)
{
  if(missing(q))
    stop("q must be a vector")
  if(missing(threshold))
    threshold=0
  if(missing(shape))
    shape=1
  if(missing(scale))
    scale=1
  temp=function(q)
  {
    if(q>=threshold)
      return(1-exp(-((q-threshold)/scale)^shape))
    else
      return(0)
  }
  return(unlist(lapply(q,temp)))
}

#' @title qweibull3: The Weibull Distribution (3 Parameter)
#' @description Density function, distribution function, and quantile function for the Weibull distribution with a threshold parameter.
#'
#' @usage
#' dweibull3(x, shape, scale, threshold)
#' pweibull3(q, shape, scale, threshold)
#' qweibull3(p, shape, scale, threshold, ...)
#'
#' @param x,q A numeric vector of quantiles.
#' @param p A numeric vector of probabilities.
#' @param shape The shape parameter of the Weibull distribution. Default is 1.
#' @param scale The scale parameter of the Weibull distribution. Default is 1.
#' @param threshold The threshold (or location) parameter of the Weibull distribution. Default is 0.
#' @param ... Additional arguments passed to \code{uniroot} for \code{qweibull3}.
#'
#' @details The Weibull distribution with the \code{scale} parameter alpha, \code{shape} parameter c, and \code{threshold} parameter zeta has a density function given by:
#' \deqn{f(x) = \frac{c}{\alpha} \left(\frac{x - \zeta}{\alpha}\right)^{c-1} \exp\left(-\left(\frac{x - \zeta}{\alpha}\right)^c\right)}
#'
#' The cumulative distribution function is given by:
#' \deqn{F(x) = 1 - \exp\left(-\left(\frac{x - \zeta}{\alpha}\right)^c\right)}
#'
#' @return \code{dweibull3} returns the density, \code{pweibull3} returns the distribution function, and \code{qweibull3} returns the quantile function for the Weibull distribution with a threshold.
#'
#' @examples
#' dweibull3(x = 1, scale = 1, shape = 5, threshold = 0)
#' temp <- pweibull3(q = 1, scale = 1, shape = 5, threshold = 0)
#' temp
#' qweibull3(p = temp, scale = 1, shape = 5, threshold = 0)
qweibull3 <- function(p,shape,scale,threshold,...)
{
  if(missing(p))
    stop("p must be a vector")
  if(missing(threshold))
    threshold=0
  if(missing(shape))
    shape=1
  if(missing(scale))
    scale=1
  myfun = function(x,p) pweibull3(q = x,
                                  threshold = threshold, scale = scale, shape = shape) - p
  temp=function(p)
  {
    return(uniroot(f=myfun,lower=threshold,upper=threshold+10000000,p=p,...)$root)       #solve myfun=0
  }
  return(unlist(lapply(p,temp)))
}

#### LOG-NORM3 ####
#' @title dlnorm3: The Lognormal Distribution (3 Parameter)
#' @description
#' Density function, distribution function, and quantile function for the Lognormal distribution.
#'
#' @usage
#' dlnorm3(x, meanlog, sdlog, threshold)
#' plnorm3(q, meanlog, sdlog, threshold)
#' qlnorm3(p, meanlog, sdlog, threshold, ...)
#'
#' @param x,q A numeric vector of quantiles.
#' @param p A numeric vector of probabilities.
#' @param meanlog,sdlog The mean and standard deviation of the distribution on the log scale with default values of \code{0} and \code{1} respectively.
#' @param threshold The threshold parameter, default is \code{0}.
#' @param ... Additional arguments that can be passed to \code{uniroot}.
#'
#' @details
#' The Lognormal distribution with \code{meanlog} parameter zeta, \code{sdlog} parameter sigma, and \code{threshold} parameter theta has a density given by:
#'
#' \deqn{f(x) = \frac{1}{\sqrt{2\pi}\sigma(x-\theta)}\exp\left(-\frac{(\log(x-\theta)-\zeta)^2}{2\sigma^2}\right)}
#'
#' The cumulative distribution function is given by:
#'
#' \deqn{F(x) = \Phi\left(\frac{\log(x-\theta)-\zeta}{\sigma}\right)}
#'
#' where \eqn{\Phi} is the cumulative distribution function of the standard normal distribution.
#'
#' @return
#' \code{dlnorm3} gives the density, \code{plnorm3} gives the distribution function, and \code{qlnorm3} gives the quantile function.
#'
#' @examples
#' dlnorm3(x = 2, meanlog = 0, sdlog = 1/8, threshold = 1)
#' temp <- plnorm3(q = 2, meanlog = 0, sdlog = 1/8, threshold = 1)
#' temp
#' qlnorm3(p = temp, meanlog = 0, sdlog = 1/8, threshold = 1)
dlnorm3 <- function(x,meanlog,sdlog,threshold)
{
  if(missing(x))
    stop("x must be a vector")
  if(missing(threshold))
    threshold=0
  if(missing(meanlog))
    meanlog=0
  if(missing(sdlog))
    sdlog=1
  temp=function(x)
  {
    if(x>threshold)
      return((1/(sqrt(2*pi)*sdlog*(x-threshold)))*exp(-(((log((x-threshold))-meanlog)^2)/(2*(sdlog)^2))))
    else
      return(0)
  }
  return(unlist(lapply(x,temp)))
}

#' @title plnorm3: The Lognormal Distribution (3 Parameter)
#' @description
#' Density function, distribution function, and quantile function for the Lognormal distribution.
#'
#' @usage
#' dlnorm3(x, meanlog, sdlog, threshold)
#' plnorm3(q, meanlog, sdlog, threshold)
#' qlnorm3(p, meanlog, sdlog, threshold, ...)
#'
#' @param x,q A numeric vector of quantiles.
#' @param p A numeric vector of probabilities.
#' @param meanlog,sdlog The mean and standard deviation of the distribution on the log scale with default values of \code{0} and \code{1} respectively.
#' @param threshold The threshold parameter, default is \code{0}.
#' @param ... Additional arguments that can be passed to \code{uniroot}.
#'
#' @details
#' The Lognormal distribution with \code{meanlog} parameter zeta, \code{sdlog} parameter sigma, and \code{threshold} parameter theta has a density given by:
#'
#' \deqn{f(x) = \frac{1}{\sqrt{2\pi}\sigma(x-\theta)}\exp\left(-\frac{(\log(x-\theta)-\zeta)^2}{2\sigma^2}\right)}
#'
#' The cumulative distribution function is given by:
#'
#' \deqn{F(x) = \Phi\left(\frac{\log(x-\theta)-\zeta}{\sigma}\right)}
#'
#' where \eqn{\Phi} is the cumulative distribution function of the standard normal distribution.
#'
#' @return
#' \code{dlnorm3} gives the density, \code{plnorm3} gives the distribution function, and \code{qlnorm3} gives the quantile function.
#'
#' @examples
#' dlnorm3(x = 2, meanlog = 0, sdlog = 1/8, threshold = 1)
#' temp <- plnorm3(q = 2, meanlog = 0, sdlog = 1/8, threshold = 1)
#' temp
#' qlnorm3(p = temp, meanlog = 0, sdlog = 1/8, threshold = 1)
plnorm3 <- function(q,meanlog,sdlog,threshold)
{
  if(missing(q))
    stop("q must be a vector")
  if(missing(threshold))
    threshold=0
  if(missing(meanlog))
    meanlog=0
  if(missing(sdlog))
    sdlog=1
  temp=function(q)
  {
    if(q>threshold)
      return(pnorm((log((q-threshold))-meanlog)/sdlog))
    else
      return(0)
  }
  return(unlist(lapply(q,temp)))
}

#' @title qlnorm3: The Lognormal Distribution (3 Parameter)
#' @description
#' Density function, distribution function, and quantile function for the Lognormal distribution.
#'
#' @usage
#' dlnorm3(x, meanlog, sdlog, threshold)
#' plnorm3(q, meanlog, sdlog, threshold)
#' qlnorm3(p, meanlog, sdlog, threshold, ...)
#'
#' @param x,q A numeric vector of quantiles.
#' @param p A numeric vector of probabilities.
#' @param meanlog,sdlog The mean and standard deviation of the distribution on the log scale with default values of \code{0} and \code{1} respectively.
#' @param threshold The threshold parameter, default is \code{0}.
#' @param ... Additional arguments that can be passed to \code{uniroot}.
#'
#' @details
#' The Lognormal distribution with \code{meanlog} parameter zeta, \code{sdlog} parameter sigma, and \code{threshold} parameter theta has a density given by:
#'
#' \deqn{f(x) = \frac{1}{\sqrt{2\pi}\sigma(x-\theta)}\exp\left(-\frac{(\log(x-\theta)-\zeta)^2}{2\sigma^2}\right)}
#'
#' The cumulative distribution function is given by:
#'
#' \deqn{F(x) = \Phi\left(\frac{\log(x-\theta)-\zeta}{\sigma}\right)}
#'
#' where \eqn{\Phi} is the cumulative distribution function of the standard normal distribution.
#'
#' @return
#' \code{dlnorm3} gives the density, \code{plnorm3} gives the distribution function, and \code{qlnorm3} gives the quantile function.
#'
#' @examples
#' dlnorm3(x = 2, meanlog = 0, sdlog = 1/8, threshold = 1)
#' temp <- plnorm3(q = 2, meanlog = 0, sdlog = 1/8, threshold = 1)
#' temp
#' qlnorm3(p = temp, meanlog = 0, sdlog = 1/8, threshold = 1)
qlnorm3 <- function(p,meanlog,sdlog,threshold,...)
{
  if(missing(p))
    stop("p must be a vector")
  if(missing(meanlog))
    meanlog=0
  if(missing(threshold))
    threshold=0
  if(missing(sdlog))
    sdlog=1
  myfun = function(x, p) plnorm3(q = x,
                                 meanlog = meanlog, sdlog = sdlog, threshold = threshold) - p
  temp=function(p)
  {
    return(uniroot(f=myfun,lower=threshold,upper=threshold+10000000,p=p,...)$root)       #solve myfun=0
  }
  return(unlist(lapply(p,temp)))
}

#### GAMMA3 ####
#' @title dgamma3: The gamma Distribution (3 Parameter)
#' @description
#' Density function, distribution function, and quantile function for the Gamma distribution.
#'
#' @usage
#' dgamma3(x, shape, scale, threshold)
#' pgamma3(q, shape, scale, threshold)
#' qgamma3(p, shape, scale, threshold, ...)
#'
#' @param x,q A numeric vector of quantiles.
#' @param p A numeric vector of probabilities.
#' @param shape The shape parameter, default is \code{1}.
#' @param scale The scale parameter, default is \code{1}.
#' @param threshold The threshold parameter, default is \code{0}.
#' @param ... Additional arguments that can be passed to \code{uniroot}.
#'
#' @details
#' The Gamma distribution with \code{scale} parameter alpha, \code{shape} parameter c, and \code{threshold} parameter zeta has a density given by:
#'
#' \deqn{f(x) = \frac{c}{\alpha}\left(\frac{x-\zeta}{\alpha}\right)^{c-1}\exp\left(-\left(\frac{x-\zeta}{\alpha}\right)^c\right)}
#'
#' The cumulative distribution function is given by:
#'
#' \deqn{F(x) = 1 - \exp\left(-\left(\frac{x-\zeta}{\alpha}\right)^c\right)}
#'
#' @return
#' \code{dgamma3} gives the density, \code{pgamma3} gives the distribution function, and \code{qgamma3} gives the quantile function.
#'
#' @examples
#' dgamma3(x = 1, scale = 1, shape = 5, threshold = 0)
#' temp <- pgamma3(q = 1, scale = 1, shape = 5, threshold = 0)
#' temp
#' qgamma3(p = temp, scale = 1, shape = 5, threshold = 0)
dgamma3 <- function(x,shape,scale,threshold)
{
  if(missing(x))
    stop("x must be a vector")
  if(missing(threshold))
    threshold=0
  if(missing(shape))
    shape=1
  if(missing(scale))
    scale=1
  temp=function(x)
  {
    if(x>=threshold)
      return( dgamma(x-threshold,shape,scale) )
    return(0)
  }
  return(unlist(lapply(x,temp)))
}

#' @title pgamma3: The gamma Distribution (3 Parameter)
#' @description
#' Density function, distribution function, and quantile function for the Gamma distribution.
#'
#' @usage
#' dgamma3(x, shape, scale, threshold)
#' pgamma3(q, shape, scale, threshold)
#' qgamma3(p, shape, scale, threshold, ...)
#'
#' @param x,q A numeric vector of quantiles.
#' @param p A numeric vector of probabilities.
#' @param shape The shape parameter, default is \code{1}.
#' @param scale The scale parameter, default is \code{1}.
#' @param threshold The threshold parameter, default is \code{0}.
#' @param ... Additional arguments that can be passed to \code{uniroot}.
#'
#' @details
#' The Gamma distribution with \code{scale} parameter alpha, \code{shape} parameter c, and \code{threshold} parameter zeta has a density given by:
#'
#' \deqn{f(x) = \frac{c}{\alpha}\left(\frac{x-\zeta}{\alpha}\right)^{c-1}\exp\left(-\left(\frac{x-\zeta}{\alpha}\right)^c\right)}
#'
#' The cumulative distribution function is given by:
#'
#' \deqn{F(x) = 1 - \exp\left(-\left(\frac{x-\zeta}{\alpha}\right)^c\right)}
#'
#' @return
#' \code{dgamma3} gives the density, \code{pgamma3} gives the distribution function, and \code{qgamma3} gives the quantile function.
#'
#' @examples
#' dgamma3(x = 1, scale = 1, shape = 5, threshold = 0)
#' temp <- pgamma3(q = 1, scale = 1, shape = 5, threshold = 0)
#' temp
#' qgamma3(p = temp, scale = 1, shape = 5, threshold = 0)
pgamma3 <- function(q,shape,scale,threshold)
{
  if(missing(q))
    stop("q must be a vector")
  if(missing(threshold))
    threshold=-2
  if(missing(shape))
    shape=1
  if(missing(scale))
    scale=1
  temp=function(q)
  {
    if(q>=threshold)
      return(pgamma(q-threshold,shape,scale))
    else
      return(0)
  }
  return(unlist(lapply(q,temp)))
}

#' @title qgamma3: The gamma Distribution (3 Parameter)
#' @description
#' Density function, distribution function, and quantile function for the Gamma distribution.
#'
#' @usage
#' dgamma3(x, shape, scale, threshold)
#' pgamma3(q, shape, scale, threshold)
#' qgamma3(p, shape, scale, threshold, ...)
#'
#' @param x,q A numeric vector of quantiles.
#' @param p A numeric vector of probabilities.
#' @param shape The shape parameter, default is \code{1}.
#' @param scale The scale parameter, default is \code{1}.
#' @param threshold The threshold parameter, default is \code{0}.
#' @param ... Additional arguments that can be passed to \code{uniroot}.
#'
#' @details
#' The Gamma distribution with \code{scale} parameter alpha, \code{shape} parameter c, and \code{threshold} parameter zeta has a density given by:
#'
#' \deqn{f(x) = \frac{c}{\alpha}\left(\frac{x-\zeta}{\alpha}\right)^{c-1}\exp\left(-\left(\frac{x-\zeta}{\alpha}\right)^c\right)}
#'
#' The cumulative distribution function is given by:
#'
#' \deqn{F(x) = 1 - \exp\left(-\left(\frac{x-\zeta}{\alpha}\right)^c\right)}
#'
#' @return
#' \code{dgamma3} gives the density, \code{pgamma3} gives the distribution function, and \code{qgamma3} gives the quantile function.
#'
#' @examples
#' dgamma3(x = 1, scale = 1, shape = 5, threshold = 0)
#' temp <- pgamma3(q = 1, scale = 1, shape = 5, threshold = 0)
#' temp
#' qgamma3(p = temp, scale = 1, shape = 5, threshold = 0)
qgamma3 <- function(p,shape,scale,threshold,...)
{
  if(missing(p))
    stop("p must be a vector")
  if(missing(threshold))
    threshold=0
  if(missing(shape))
    shape=1
  if(missing(scale))
    scale=1
  myfun = function(x,p) pgamma3(q = x,
                                threshold = threshold, scale = scale, shape = shape) - p
  temp=function(p)
  {
    return(uniroot(f=myfun,lower=threshold,upper=threshold+10000000,p=p,...)$root)       #solve myfun=0
  }
  return(unlist(lapply(p,temp)))
}




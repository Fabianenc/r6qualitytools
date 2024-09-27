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




#### adSim ----
adSim <- function(x, distribution = "normal", b = 10000){
  #' @title adSim: Bootstrap-based Anderson-Darling Test for Univariate
  #' @description Performs the Anderson-Darling test for univariate distributions with an option for bootstrap-based p-value determination. It also allows p-value determination using tabled critical values.
  #'
  #' @usage
  #' adSim(x, distribution = "normal", b = 10000)
  #'
  #' @param x A numeric vector.
  #' @param distribution A character string specifying the distribution to test. Recognized distributions include \code{"cauchy"}, \code{"exponential"}, \code{"gumbel"}, \code{"gamma"}, \code{"log-normal"}, \code{"lognormal"}, \code{"logistic"}, \code{"normal"}, and \code{"weibull"}.
  #' @param b A numeric value indicating the number of bootstraps to perform. Allowed values range from 1000 to 1,000,000. If \code{b} is set to \code{NA}, the Anderson-Darling test will be applied without simulation. Note that higher values of \code{b} can significantly increase computation time, potentially taking hours depending on the distribution, sample size, and computer system.
  #'
  #' @details The function first estimates the parameters for the tested distribution, typically using Maximum-Likelihood Estimation (MLE) via the \code{\link{FitDistr}} function. For normal and log-normal distributions, parameters are estimated by the mean and standard deviation. Cauchy distribution parameters are fitted by the sums of the weighted order statistic when using tabled critical values. The Anderson-Darling statistic is then calculated based on these estimated parameters.
  #'
  #' Parametric bootstrapping generates the distribution of the Anderson-Darling test statistic, which is used to determine the p-value. This simulation-based Anderson-Darling distribution and its corresponding critical values for selected quantiles can be printed. If simulation is not performed, a p-value is obtained from tabled critical values, although no exact expressions exist except for the log-normal, normal, and exponential distributions.
  #'
  #' @return A list containing the following components:
  #' \describe{
  #'   \item{\code{distribution}}{The distribution for which the Anderson-Darling test was applied.}
  #'   \item{\code{parameter_estimation}}{The estimated parameters for the distribution.}
  #'   \item{\code{Anderson_Darling}}{The value of the Anderson-Darling test statistic.}
  #'   \item{\code{p_value}}{The corresponding p-value, either simulated or from tabled values.}
  #'   \item{\code{critical_values}}{The critical values corresponding to the 0.75, 0.90, 0.95, 0.975, and 0.99 quantiles, either simulated or from tables.}
  #'   \item{\code{simAD}}{The bootstrap-based Anderson-Darling distribution.}
  #' }
  #'
  #' @examples
  #' x <- rnorm(25, 32, 2)
  #' adSim(x)
  #' adSim(x, "logistic", 2000)
  #' adSim(x, "cauchy", NA)
  #' adSim(x, "exponential", 2000)
  #' adSim(x, "gumbel", 2000)
  #' x <- rgamma(n = 25, shape = 1)
  #' adSim(x, "gamma", 2000)
  #' adSim(x, "log-normal", 2000)
  #' adSim(x, "lognormal", 2000)
  #' adSim(x, "weibull", 2000)


  if(mode(x) != "numeric")
    stop(paste("\n","          adSim() requires numeric x data"))

  if(any(is.na(x)))
    stop(paste("\n","          x data has missing values (NA)"))

  distr <- c("exponential","cauchy","gumbel","gamma","log-normal","lognormal","logistic","normal","weibull")

  if(any(distr == distribution)  == FALSE)
    stop(paste("\n","          adSim() can not apply for",distribution,"distribution.				Please choose one of the following distributions for testing goodness-of-fit:				exponential, cauchy, gumbel, gamma, log-normal, lognormal, logistic, normal, weibull"))


  if(any(x<=0)){
    if(distribution == "exponential" || distribution == "lognormal" || distribution == "log-normal" || distribution == "gamma" || distribution == "weibull"){
      stop(paste("\n","          adSim() can not apply for", distribution ,"distribution while x contains negative values or x has values equal zero."))}}

  if(distribution == "lognormal" || distribution == "log-normal"){
    x <- log(x)
    testDistr <- "normal"    				}


  if(distribution == "weibull"){
    x <- -log(x)
    testDistr <- "gumbel"}

  if(distribution != "lognormal" & distribution != "log-normal" & distribution != "weibull"){
    testDistr <- distribution}

  n = length(x)

  if(n<3)
    stop(paste("\n","          adSim() can not apply for sample sizes n < 3."))


  x = sort(x, decreasing = FALSE)

  if(testDistr != "normal" & testDistr != "gumbel" & testDistr != "cauchy"){
    parafit <- FitDistr(x,testDistr)[c('estimate','sd')]
  }


  if(testDistr == "normal"){
    parafit 	<- numeric(2)
    parafit[1] 	= mean(x)
    parafit[2] 	= sd(x)


    if(distribution == "lognormal" || distribution == "log-normal"){
      names(parafit) = c(  "meanlog",  "sdlog")   # 2.41
    }else{names(parafit) = c(  "mean",  "sd")}		# 2.42
  }


  if(testDistr == "cauchy"){


    if(is.na(b) == FALSE){
      parafit <- FitDistr(x,testDistr)

    }else{

      parafit 	<- numeric(2)

      uWeight = numeric(n)
      uWeight[1:n] <- sin( 4*pi* ( 1:n/(n+1) - 0.5)  ) / (n*tan( pi* ( 1:n/(n+1) - 0.5 )  ) )


      if(ifelse(n %% 2, TRUE, FALSE)){
        if(length(na.omit(uWeight)) + 1 == length(uWeight)){

          uWeight[which(is.na(uWeight))] = (n+1)/n - sum(na.omit(uWeight))
        }}
      parafit[1] 	<- uWeight %*% x


      vWeight = numeric(n)
      vWeight[1:n] <- 8*tan( pi* ( 1:n/(n+1) - 0.5)  )*(cos( pi*(1:n/(n+1) - 0.5 )))^4 / n

      parafit[2]	<- vWeight %*% x


      names(parafit) = c(  "location",  "scale")
    }
  }



  if(testDistr == "gumbel"){

    pgumbel = function (q, location = 0, scale = 1){
      answer = exp(-exp(-(q - location)/scale))
      answer[scale <= 0] = NaN
      answer}


    is.Numeric = function (x, allowable.length = Inf, integer.valued = FALSE, positive = FALSE){
      if (all(is.numeric(x)) && all(is.finite(x)) && (if (is.finite(allowable.length)) length(x) ==
                                                      allowable.length else TRUE) && (if (integer.valued) all(x == round(x)) else TRUE) &&
          (if (positive) all(x > 0) else TRUE)) TRUE else FALSE}

    rgumbel = function (n, location = 0, scale = 1){
      use.n = if ((length.n <- length(n)) > 1)
        length.n
      else if (!is.Numeric(n, integ = TRUE, allow = 1, posit = TRUE))
        stop("bad input for argument 'n'")
      else n
      answer = location - scale * log(-log(runif(use.n)))
      answer[scale <= 0] = NaN
      answer}



    f <- function(p) 1/n*sum(x) - (sum(x*exp(-x/p)))/(sum(exp(-x/p)))-p



    itSol <- suppressWarnings(uniroot(f,c(-100,100),tol = 0.0000001, maxiter = 100000))
    beta <- as.numeric(itSol$root)
    alpha = - beta * log(sum(exp(-x/beta)/n))

    parafit 	<- numeric(2)
    parafit[1] 	= alpha
    parafit[2] 	= beta

    names(parafit) = c(  "location",  "scale")

  }


  pit = numeric(n)


  if(testDistr == "normal")
    pit[1:n] = pnorm(x[1:n],parafit[1],parafit[2])

  if(testDistr == "exponential")
    pit[1:n] = pexp(x[1:n],parafit$estimate["rate"])

  if(testDistr == "cauchy")
    if(is.na(b) == FALSE){
      pit[1:n] = pcauchy(x[1:n],parafit$estimate["location"],parafit$estimate["scale"])
    }else{  pit[1:n] = pcauchy(x[1:n],parafit[1],parafit[2])}

  if(testDistr == "gamma")
    pit[1:n] = pgamma(x[1:n],parafit$estimate["shape"],parafit$estimate["rate"])

  if(testDistr == "gumbel")
    pit[1:n] = pgumbel(x[1:n],parafit[1],parafit[2])

  if(testDistr == "logistic")
    pit[1:n] = plogis(x[1:n],parafit$estimate["location"],parafit$estimate["scale"])


  h = matrix(ncol=n)

  h[1,] = (2*col(h)-1)*log(pit) + (2*n + 1 - 2*col(h))* log(1 - pit)

  AD = -n-(1/n) * rowSums(h)


  if(is.na(AD) == TRUE || AD<0)
    stop(paste("\n","        The calculation of the Anderson Darling statistic fails."))


  if(is.na(b) == FALSE){


    if(b<1000)
      stop("b is chosen too small for generate an accurate p-Value.")
    if(b>1000000)
      stop("b is chosen too big for generate an p-value within a reasonable time.")


    cat("\n","   ... simulating the Anderson-Darling distribution by",b,"bootstraps for",distribution,"distribution...","\n","\n")


    if(testDistr == "normal"){
      Y = t(replicate(b, sort(rnorm(n,parafit[1],parafit[2]))))

      paraMean = rowMeans(Y)
      paraSd = sqrt(rowSums((Y-rowMeans(Y))^2) /(ncol(Y)-1))
    }


    if(testDistr == "exponential"){
      Y = t(replicate(b, sort(rexp(n, parafit$estimate["rate"]))))

      unParaList <- unlist(apply(Y,1,function(x) FitDistr(x,testDistr,rate = parafit$estimate["rate"])))
      paraRate = unParaList[names(unParaList) == "estimate.rate"]
    }

    if(testDistr == "cauchy"){
      Y = t(replicate(b, sort(rcauchy(n, parafit$estimate["location"],parafit$estimate["scale"]))))

      unParaList <- unlist(apply(Y,1,function(x) FitDistr(x,testDistr,list(location = parafit$estimate["location"], scale = parafit$estimate["scale"]))))
      paraLocation = unParaList[names(unParaList) == "estimate.location"]
      paraScale = unParaList[names(unParaList) == "estimate.scale"]
    }

    if(testDistr == "gamma"){
      Y = t(replicate(b, sort(rgamma(n, parafit$estimate["shape"],parafit$estimate["rate"]))))

      unParaList <- unlist(apply(Y,1,function(x) FitDistr(x,testDistr,list(shape = parafit$estimate["shape"], rate = parafit$estimate["rate"]))))
      paraShape = unParaList[names(unParaList) == "estimate.shape"]
      paraRate = unParaList[names(unParaList) == "estimate.rate"]
    }

    if(testDistr == "gumbel"){
      Y = t(replicate(b, sort(rgumbel(n, parafit[1], parafit[2]))))

      paraBeta	= numeric(b)
      paraAlpha	= numeric(b)

      for(j in 1:b){


        itSol <- suppressWarnings(uniroot(function(p) 1/n*sum(Y[j,])-
                                            (sum(Y[j,]*exp(-Y[j,]/p)))/(sum(exp(-Y[j,]/p)))-p,c(-100,100),tol = 0.0000000001, maxiter = 100000))
        paraBeta[j] <- as.numeric(itSol$root)
        paraAlpha[j] = - paraBeta[j] * log(sum(exp(-Y[j,]/paraBeta[j])/n))
      }
    }

    if(testDistr == "logistic"){
      Y = t(replicate(b, sort(rlogis(n, parafit$estimate["location"],parafit$estimate["scale"]))))

      unParaList <- unlist(apply(Y,1,function(x) FitDistr(x,testDistr,list(location = parafit$estimate["location"], scale = parafit$estimate["scale"]))))
      paraLocation = unParaList[names(unParaList) == "estimate.location"]
      paraScale = unParaList[names(unParaList) == "estimate.scale"]
    }


    if(testDistr == "normal")
      Y[,1:n] <- pnorm(Y[,1:n],paraMean,paraSd)


    if(testDistr == "exponential")
      Y[,1:n] <- pexp(Y[,1:n],paraRate)


    if(testDistr == "cauchy")
      Y[,1:n] <- pcauchy(Y[,1:n],paraLocation,paraScale)

    if(testDistr == "gamma")
      Y[,1:n] <- pgamma(Y[,1:n],paraShape,paraRate)


    if(testDistr == "gumbel")
      Y[,1:n] <- pgumbel(Y[,1:n],paraAlpha,paraBeta)

    if(testDistr == "logistic")
      Y[,1:n] <- plogis(Y[,1:n],paraLocation,paraScale)



    Y[1:b,] <- (2*col(Y)-1)*log(Y[1:b,]) +  (2*n + 1 - 2*col(Y)) * log(1 - Y[1:b,])


    d = rowSums(Y)

    simAD = numeric(b)

    simAD[1:b] = -n-(1/n)*d[1:b]

    if(any(is.na(simAD))){
      cat("    The simulated Anderson-Darling distribution contains NAs or NaNs!","\n","\n")}


    critValues = round (matrix(	c(  0.75, 0.90, 0.95, 0.975, 0.990,
                                    quantile(simAD, 0.75,na.rm = TRUE), quantile(simAD, 0.90,na.rm = TRUE), quantile(simAD, 0.95,na.rm = TRUE),
                                    quantile(simAD, 0.975,na.rm = TRUE), quantile(simAD, 0.99,na.rm = TRUE) ), nrow= 2, byrow = TRUE ), digits = 5)


    pValue = sum(na.omit(simAD) > AD)/length(na.omit(simAD))



  }else{


    simAD = NA


    critValues = matrix(	c(  0.75, 0.90, 0.95, 0.975, 0.990, NA, NA ,NA, NA, NA), nrow = 2, byrow = TRUE)


    if(testDistr == "normal" || testDistr == "exponential" ||testDistr == "gumbel" || testDistr == "logistic"){

      if(testDistr == "normal"){



        normalMtx = matrix(c(	0.50 , 0.75, 0.85 , 0.90 , 0.95 , 0.975 , 0.99 , 0.995 ,
                              0.341 , 0.470 , 0.561 , 0.631 , 0.752 , 0.873 , 1.035 , 1.159), nrow=2, byrow = TRUE )


        normalMtx[2,1:ncol(normalMtx)] = normalMtx[2,1:ncol(normalMtx)]/(1+0.75/n+2.25/n^2)
        refMtx = normalMtx


        cAD = AD*(1+0.75/n+2.25/n^2)


        if(0.600<cAD)
        {pValue = exp(1.2937-5.709*cAD+0.0186*cAD^2)}
        if(0.340<cAD & cAD<0.600)
        {pValue = exp(0.9177-4.279*cAD-1.38*cAD^2)}
        if(0.200<cAD & cAD<0.340)
        {pValue = 1-exp(-8.318+42.796*cAD-59.938*cAD^2)}
        if(cAD<0.200)
        {pValue = 1-exp(-13.436+101.14*cAD-223.73*cAD^2)}
      }


      if(testDistr == "exponential"){


        expMtx = matrix( c( 0.75, 0.80, 0.85 , 0.90 , 0.95 , 0.975 , 0.99 , 0.995 , 0.9975,
                            0.736 , 0.816 , 0.916 , 1.062 , 1.321 , 1.591 , 1.959 , 2.244, 2.534), nrow = 2, byrow = TRUE )


        expMtx[2,1:ncol(expMtx)] = expMtx[2,1:ncol(expMtx)]/(1+0.6/n)

        refMtx = expMtx


        cAD = AD*(1+0.6/n)


        if(0.950<cAD)
        {pValue = exp(0.731 - 3.009*cAD + 0.15*cAD^2)}
        if(0.510<cAD & cAD<0.950)
        {pValue = exp(0.9209 - 3.353*cAD + 0.300*cAD^2)}
        if(0.260<cAD & cAD<0.510)
        {pValue = 1 - exp(-6.1327 + 20.218*cAD - 18.663*cAD^2)}
        if(cAD<0.260)
        {pValue = 1 - exp(-12.2204 + 67.459*cAD - 110.3*cAD^2)}
      }


      if(testDistr == "gumbel" || testDistr == "logistic"){

        if(testDistr == "gumbel"){


          gumbelMtx = matrix(c( 0.75, 0.90 , 0.95 , 0.975 , 0.99 , 0.474, 0.637, 0.757, 0.877, 1.038), nrow = 2, byrow = TRUE )



          gumbelMtx[2,1:ncol(gumbelMtx)] = gumbelMtx[2,1:ncol(gumbelMtx)]/(1 + 0.2/sqrt(n))
          refMtx = gumbelMtx
        }


        if(testDistr == "logistic"){


          logisMtx = matrix(c( 0.75, 0.90 , 0.95 , 0.975 , 0.99 , 0.995 , 0.426, 0.563, 0.660, 0.769, 0.906, 1.010 ), nrow = 2, byrow = TRUE )


          logisMtx[2,1:ncol(logisMtx)] = logisMtx[2,1:ncol(logisMtx)]/(1 + 0.25/n)
          refMtx = logisMtx
        }



        critCheck <- refMtx[2,1:ncol(refMtx)] > AD


        if(any(critCheck)){


          firPos <- min(which(critCheck)) - 1

        }else{

          firPos <- ncol(refMtx)}


        if(firPos == 0){
          pValue <- 1 - refMtx[1,1]
          pValue <- paste(">",pValue)
        }else{
          pValue <- 1 - refMtx[1,firPos]
          pValue <- paste("<=",pValue)}}


      for(i in 1:ncol(critValues)){


        if(any(refMtx[1,1:ncol(refMtx)] == critValues[1,i])){


          position <- (1:length(refMtx[1,1:ncol(refMtx)] == critValues[1,i]))[(refMtx[1,1:ncol(refMtx)] == critValues[1,i])]


          critValues[2,i] <- refMtx[2,position]
        }else{

          critValues[2,i] <- NA}}
    }

    if(testDistr == "gamma"){


      gammaDF = data.frame(	c( 1,     2,     3,     4,     5,     6,     8,    10,    12,    15,    20,     Inf  ),
                            c( 0.486, 0.477, 0.475, 0.473, 0.472, 0.472, 0.471, 0.471, 0.471, 0.47,  0.47,  0.47 ),
                            c( 0.657, 0.643, 0.639, 0.637, 0.635, 0.635, 0.634, 0.633, 0.633, 0.632, 0.632, 0.631),
                            c( 0.786, 0.768, 0.762, 0.759, 0.758, 0.757, 0.755, 0.754, 0.754, 0.754, 0.753, 0.752),
                            c( 0.917, 0.894, 0.886, 0.883, 0.881, 0.88,  0.878, 0.877, 0.876, 0.876, 0.875, 0.873),
                            c( 1.092, 1.062, 1.052, 1.048, 1.045, 1.043, 1.041, 1.04,  1.039, 1.038, 1.037, 1.035),
                            c( 1.227, 1.19,  1.178, 1.173, 1.17,  1.168, 1.165, 1.164, 1.163, 1.162, 1.161, 1.159))

      names(gammaDF) = c(  "m",  0.75,  0.90,  0.95,  0.975, 0.99,  0.995)




      critCheck <- gammaDF[min(which(gammaDF$m >= parafit$estimate["shape"])),2:ncol(gammaDF)] > AD


      if(any(critCheck)){

        firPos <- min(which(critCheck))
      }else{


        firPos <- ncol(gammaDF) }


      if(firPos == 1){
        pValue <- 1 - as.numeric(names(gammaDF)[2])
        pValue <- paste(">",pValue)
      }else{
        pValue <- 1 - as.numeric(names(gammaDF)[firPos])
        pValue <- paste("<=",pValue)}


      for(i in 1:ncol(critValues)){


        if(any(names(gammaDF) == critValues[1,i] )){



          critValues[2,i] <- gammaDF[min(which(gammaDF$m >= parafit$estimate["shape"] )),which(names(gammaDF) == critValues[1,i])]
        }else{

          critValues[2,i] <- NA}}
    }


    if(testDistr == "cauchy"){

      cauchyDF = data.frame(
        c( 5,     8,     10,   12,   15,   20,    25,    30,    40,    50,    60,    100,   Inf),
        c( 0.835, 0.992, 1.04, 1.04, 1.02, 0.975, 0.914, 0.875, 0.812, 0.774, 0.743, 0.689, 0.615),
        c( 1.14,  1.52,  1.63, 1.65, 1.61, 1.51,  1.4,   1.3,   1.16,  1.08,  1.02,  0.927, 0.78),
        c( 1.4,   2.06,  2.27, 2.33, 2.28, 2.13,  1.94,  1.76,  1.53,  1.41,  1.3,   1.14,  0.949),
        c( 1.77,  3.2,   3.77, 4.14, 4.25, 4.05,  3.57,  3.09,  2.48,  2.14,  1.92,  1.52,  1.225),
        c( 2,     4.27,  5.58, 6.43, 7.2,  7.58,  6.91,  5.86,  4.23,  3.37,  2.76,  2.05,  1.52),
        c( 2.16,  5.24,  7.5,  9.51, 11.5, 14.57, 14.96, 13.8,  10.2,  7.49,  5.32,  3.3,   1.9))

      names(cauchyDF) = c(  "n",  0.75,  0.85, 0.90,  0.95,  0.975, 0.99)



      if(any(cauchyDF[1:13,1] == n)){


        critCheck <- cauchyDF[which(cauchyDF[1:13,1] == n),2:ncol(cauchyDF)] > AD


        if(any(critCheck)){


          firPos <- min(which(critCheck))

        }else{

          firPos <- ncol(cauchyDF) }


        if(firPos == 1){
          pValue <- 1 - as.numeric(names(cauchyDF)[2])
          pValue <- paste(">",pValue)
        }else{
          pValue <- 1 - as.numeric(names(cauchyDF)[firPos])
          pValue <- paste("<=",pValue)}



        for(i in 1:ncol(critValues)){


          if(any(names(cauchyDF) == critValues[1,i] )){

            critValues[2,i] <- cauchyDF[which(cauchyDF[1:13,1] == n),which(names(cauchyDF) == critValues[1,i])]
          }else{

            critValues[2,i] <- NA}
        }


      }else{

        pValue <- NA

        critValues[2,1:ncol(critValues)] <- NA
        cat("\n","Critical values / p-Values for the Cauchy Distribution are only tabled for sample sizes: n = 5, 8, 10, 12, 15, 20, 25, 30, 40, 50, 60, 100","\n")}

    }
  }


  if(distribution == "weibull"){


    names(parafit) = c(  "shape",  "scale")


    parafitCopy = parafit


    parafit[1] 	= (1/parafitCopy[2])
    parafit[2] 	= exp(- parafitCopy[1])}


  print(list(distribution = distribution, parameter_estimation = parafit,Anderson_Darling = AD, p_value = pValue))
  invisible(list(distribution = distribution, parameter_estimation = parafit,Anderson_Darling = AD,p_value = pValue,crititical_values = critValues,simAD = simAD))

}

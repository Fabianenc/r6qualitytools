######################################################################
###################### DISTRIBUTION - FUNCIONES ######################
######################################################################

# ParetoChart ----
paretoChart <- function (x, weight, main, col, border, xlab, ylab = "Frequency",
                         percentVec, showTable = TRUE, showPlot = TRUE){
  #' @title paretoChart: Pareto Chart
  #' @description Function to create a Pareto chart, displaying the relative frequency of categories.
  #' @param x A vector of qualitative values.
  #' @param weight A numeric vector of weights corresponding to each category in \code{x}.
  #' @param main A character string for the main title of the plot.
  #' @param col A numerical value or character string defining the fill-color of the bars.
  #' @param border A numerical value or character string defining the border-color of the bars.
  #' @param xlab A character string for the x-axis label.
  #' @param ylab A character string for the y-axis label. By default, \code{ylab} is set to \code{`Frequency`}.
  #' @param percentVec A numerical vector giving the position and values of tick marks for percentage axis.
  #' @param showTable Logical value indicating whether to display a table of frequencies. By default, \code{showTable} is set to \code{TRUE}.
  #' @param showPlot Logical value indicating whether to display the Pareto chart. By default, \code{showPlot} is set to \code{TRUE}.
  #' @return \code{paretoChart} returns a Pareto chart along with a frequency table if \code{showTable} is \code{TRUE}.
  #' Additionally, the function returns an invisible list containing:
  #' \item{plot}{The generated Pareto chart.}
  #' \item{table}{A data.frame with the frequencies and percentages of the categories.}
  #' @examples
  #' # Example 1: Creating a Pareto chart for defect types
  #' defects1 <- c(rep("E", 62), rep("B", 15), rep("F", 3), rep("A", 10),
  #'               rep("C", 20), rep("D", 10))
  #' paretoChart(defects1)
  #'
  #' # Example 2: Creating a Pareto chart with weighted frequencies
  #' defects2 <- c("E", "B", "F", "A", "C", "D")
  #' frequencies <- c(62, 15, 3, 10, 20, 10)
  #' weights <- c(1.5, 2, 0.5, 1, 1.2, 1.8)
  #' names(weights) <- defects2  # Assign names to the weights vector
  #'
  #' paretoChart(defects2, weight = frequencies * weights)

  varName = deparse(substitute(x))[1]
  corp.col = "#C4B9FF"
  corp.border = "#9E0138"
  if (!is.vector(x) & !is.data.frame(x) & !is.table(x))
    stop("x should be a vector, dataframe or a table")
  if (is.table(x)) {
    xtable = x
  }
  if (is.vector(x)) {
    if (!is.null(names(x)))
      xtable = as.table(x)
    else xtable = table(x)
  }
  if (!missing(weight)) {
    if (!is.numeric(weight))
      stop("weight must be numeric!")
    if (is.null(names(weight)))
      stop("weight is missing names for matching!")
    else {
      if (FALSE %in% (sort(names(weight)) == sort(names(xtable))))
        stop("names of weight and table do not match!")
      else {
        for (i in 1:length(xtable)) {
          xtable[i] = weight[names(weight) == names(xtable)[i]] *
            xtable[i]
        }
      }
    }
  }
  else {
    weight = FALSE
  }
  if (missing(showTable))
    showTable = TRUE
  if (missing(xlab))
    xlab = ""
  if (missing(main))
    main = paste("Pareto Chart for", varName)
  if (missing(col))
    col = corp.col
  if (missing(border))
    border = corp.border
  if (missing(percentVec))
    percentVec = seq(0, 1, by = 0.25)
  call <- match.call(expand.dots = TRUE)
  # Plot
  if (length(xtable) > 1) {
    ylim = c(min(xtable), max(xtable) * 1.025)
    xtable = c(sort(xtable, decreasing = TRUE, na.last = TRUE))
    cumFreq = cumsum(xtable)
    sumFreq = sum(xtable)
    percentage = xtable/sum(xtable) * 100
    cumPerc = cumFreq/sumFreq * 100

    data <- data.frame(Frequency = xtable,
                       Cum.Frequency = cumFreq,
                       Percentage = round(percentage, digits = 2),
                       Cum.Percentage = round(cumPerc, digits = 2))
    tabla <- t(data)

    p <- ggplot(data, aes(x = reorder(names(xtable), -xtable), y = Frequency)) +
      geom_col(aes(fill = "Frequency"), width = 0.7) +
      geom_point(aes(y = Cum.Frequency, color = "Cumulative Percentage")) +
      geom_line(aes(y = Cum.Frequency, group = 1, color = "Cumulative Percentage")) +
      scale_y_continuous(name = ylab,
                         sec.axis = sec_axis(~ . / sum(xtable),
                                             name = "Cumulative Percentage",
                                             labels = percentVec)) +
      scale_x_discrete(name = xlab) +
      scale_color_manual(values = c(border, border)) +
      scale_fill_manual(values = col) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = main)+theme(plot.title = element_text(hjust = 0.5,face = "bold"))

  }
  else {
    warning("data should have at least two categories!")
  }
  if(showPlot == TRUE){
    if(showTable == TRUE){
      show(p/tableGrob(tabla))
    }
    else {
      show(p)
    }
  }
  else{
    show(tabla)
  }

  invisible(list(plot = p, table = tabla))
}
# distribution ----
distribution <- function(x = NULL, distrib = "weibull", ...) {
  #' @title distribution: Distribution
  #' @description Calculates the most likely parameters for a given distribution.
  #' @param x Vector of distributed values from which the parameter should be determined.
  #' @param distrib Character string specifying the distribution of x. The function \code{distribution} will accept the following character strings for \code{distribution}:
  #' \itemize{
  #'   \item{\code{`normal`}}
  #'   \item{\code{`chi-squared`}}
  #'   \item{\code{`exponential`}}
  #'   \item{\code{`logistic`}}
  #'   \item{\code{`gamma`}}
  #'   \item{\code{`weibull`}}
  #'   \item{\code{`cauchy`}}
  #'   \item{\code{`beta`}}
  #'   \item{\code{`f`}}
  #'   \item{\code{`t`}}
  #'   \item{\code{`geometric`}}
  #'   \item{\code{`poisson`}}
  #'   \item{\code{`negative binomial`}}
  #'   \item{\code{`log-normal`}}
  #' }
  #' By default, \code{distribution} is set to \code{`weibull`}.
  #' @param ... Additional arguments to be passed to the fitting function.
  #' @return \code{distribution()} returns an object of class \code{DistrCollection}.
  #' @examples
  #' data1 <- rnorm(100, mean = 5, sd = 2)
  #' distribution(data1, distrib = "normal")
  #' @seealso \code{\link{Distr}}, \code{\link{DistrCollection}}

  distr_coll <- DistrCollection$new()
  if (is.character(distrib))
    distrib = tolower(distrib)
  allDistr = c("beta", "cauchy", "chi-squared", "exponential", "f", "gamma", "geometric", "log-normal", "logistic", "negative binomial", "normal", "poisson",
               "t", "weibull")
  if (distrib %in% allDistr){
    distrVec = distrib
  }
  else{distrVec = c("normal")}
  if (identical(distrib, "all"))
    distrVec = allDistr
  if (identical(distrib, "quality"))
    distrVec = c("normal", "log-normal", "exponential", "weibull")
  for (i in seq(along = distrVec)) {
    temp <- suppressWarnings(FitDistr(x, densfun = distrVec[i]))
    fit <- Distr$new(x = x,
                     name = distrVec[i],
                     parameters = temp$estimate,
                     sd = temp$sd,
                     loglik = temp$loglik,
                     n = length(x))
    distr_coll$add(fit)
  }
  return(distr_coll)
}

# FitDistr ----
FitDistr <- function (x, densfun, start, ...){
  #' @title FitDistr: Maximum-likelihood Fitting of Univariate Distributions
  #' @description Maximum-likelihood fitting of univariate distributions, allowing parameters to be held fixed if desired.
  #' @param x A numeric vector of length at least one containing only finite values.
  #' Either a character string or a function returning a density evaluated at its first argument.
  #' @param densfun character string specifying the density function to be used for fitting the distribution. Distributions `"beta"`, `"cauchy"`, `"chi-squared"`, `"exponential"`, `"gamma"`, `"geometric"`, `"log-normal"`, `"lognormal"`, `"logistic"`, `"negative binomial"`, `"normal"`, `"Poisson"`, `"t"` and "weibull" are recognised, case being ignored.
  #' @param start A named list giving the parameters to be optimized with initial values. This can be omitted for some of the named distributions and must be for others (see Details).
  #' @param ... Additional parameters, either for `densfun` or for `optim`. In particular, it can be used to specify bounds via `lower` or `upper` or both. If arguments of `densfun` (or the density function corresponding to a character-string specification) are included they will be held fixed.
  #' @details For the Normal, log-Normal, geometric, exponential and Poisson distributions the closed-form MLEs (and exact standard errors) are used, and `start` should not be supplied.
  #'
  #' For all other distributions, direct optimization of the log-likelihood is performed using `optim`. The estimated standard errors are taken from the observed information matrix, calculated by a numerical approximation. For one-dimensional problems the Nelder-Mead method is used and for multi-dimensional problems the BFGS method, unless arguments named `lower` or `upper` are supplied (when `L-BFGS-B` is used) or `method` is supplied explicitly.
  #'
  #' For the `"t"` named distribution the density is taken to be the location-scale family with location `m` and scale `s`.
  #'
  #' For the following named distributions, reasonable starting values will be computed if `start` is omitted or only partially specified: `"cauchy"`, `"gamma"`, `"logistic"`, `"negative binomial"` (parametrized by mu and size), `"t"` and `"weibull"`. Note that these starting values may not be good enough if the fit is poor: in particular they are not resistant to outliers unless the fitted distribution is long-tailed.
  #'
  #' There are `print`, `coef`, `vcov` and `logLik` methods for class `"FitDistr"`.
  #'
  #' @return The function `FitDistr` returns an object of class `fitdistr`, which is a list containing:
  #' \item{estimate}{a named vector of parameter estimates.}
  #' \item{sd}{a named vector of the estimated standard errors for the parameters.}
  #' \item{vcov}{the estimated variance-covariance matrix of the parameter estimates.}
  #' \item{loglik}{the log-likelihood of the fitted model.}
  #' \item{n}{length vector.}
  #'
  #' @seealso \code{\link{distribution}}, \code{\link{Distr}}, \code{\link{DistrCollection}}.
  #' @examples
  #' set.seed(123)
  #' x = rgamma(100, shape = 5, rate = 0.1)
  #' FitDistr(x, "gamma")
  #'
  #' # Now do this directly with more control.
  #' FitDistr(x, dgamma, list(shape = 1, rate = 0.1), lower = 0.001)
  #'
  #' set.seed(123)
  #' x2 = rt(250, df = 9)
  #' FitDistr(x2, "t", df = 9)
  #'
  #' # Allow df to vary: not a very good idea!
  #' FitDistr(x2, "t")
  #'
  #' # Now do fixed-df fit directly with more control.
  #' mydt = function(x, m, s, df) dt((x-m)/s, df)/s
  #' FitDistr(x2, mydt, list(m = 0, s = 1), df = 9, lower = c(-Inf, 0))
  #'
  #' set.seed(123)
  #' x3 = rweibull(100, shape = 4, scale = 100)
  #' FitDistr(x3, "weibull")

  myfn = function(parm, ...) -sum(log(dens(parm, ...)))
  mylogfn = function(parm, ...) -sum(dens(parm, ..., log = TRUE))
  mydt = function(x, m, s, df, log) dt((x - m)/s, df, log = TRUE) -
    log(s)
  Call = match.call()
  if (missing(start))
    start = NULL
  dots = names(list(...))
  dots = dots[!is.element(dots, c("upper", "lower"))]
  if (missing(x) || length(x) == 0L || mode(x) != "numeric")
    stop("'x' must be a non-empty numeric vector")
  if (any(!is.finite(x)))
    stop("'x' contains missing or infinite values")
  if (missing(densfun) || !(is.function(densfun) || is.character(densfun)))
    stop("'densfun' must be supplied as a function or name")
  control = list()
  n = length(x)
  if (is.character(densfun)) {
    distname = tolower(densfun)
    densfun = switch(distname, beta = dbeta, cauchy = dcauchy,
                     `chi-squared` = dchisq, exponential = dexp, f = df,
                     gamma = dgamma, geometric = dgeom, `log-normal` = dlnorm,
                     lognormal = dlnorm, logistic = dlogis, `negative binomial` = dnbinom,
                     normal = dnorm, poisson = dpois, t = mydt, weibull = dweibull,
                     NULL)
    if (is.null(densfun))
      stop("unsupported distribution")
    if (distname %in% c("lognormal", "log-normal")) {
      if (!is.null(start))
        stop(gettextf("supplying pars for the %s distribution is not supported",
                      "log-Normal"), domain = NA)
      if (any(x <= 0))
        stop("need positive values to fit a log-Normal")
      lx = log(x)
      sd0 = sqrt((n - 1)/n) * sd(lx)
      mx = mean(lx)
      estimate = c(mx, sd0)
      sds = c(sd0/sqrt(n), sd0/sqrt(2 * n))
      names(estimate) = names(sds) = c("meanlog", "sdlog")
      vc = matrix(c(sds[1]^2, 0, 0, sds[2]^2), ncol = 2,
                  dimnames = list(names(sds), names(sds)))
      names(estimate) = names(sds) = c("meanlog", "sdlog")
      return(structure(list(estimate = estimate, sd = sds,
                            vcov = vc, n = n, loglik = sum(dlnorm(x, mx, sd0, log = TRUE))), class = "FitDistr"))
    }
    if (distname == "normal") {
      if (!is.null(start))
        stop(gettextf("supplying pars for the %s distribution is not supported",
                      "Normal"), domain = NA)
      sd0 = sqrt((n - 1)/n) * sd(x)
      mx = mean(x)
      estimate = c(mx, sd0)
      sds = c(sd0/sqrt(n), sd0/sqrt(2 * n))
      names(estimate) = names(sds) = c("mean", "sd")
      vc = matrix(c(sds[1]^2, 0, 0, sds[2]^2), ncol = 2,
                  dimnames = list(names(sds), names(sds)))
      return(structure(list(estimate = estimate, sd = sds,
                            vcov = vc, n = n, loglik = sum(dnorm(x, mx,
                                                                 sd0, log = TRUE))), class = "FitDistr"))
    }
    if (distname == "poisson") {
      if (!is.null(start))
        stop(gettextf("supplying pars for the %s distribution is not supported",
                      "Poisson"), domain = NA)
      estimate = mean(x)
      sds = sqrt(estimate/n)
      names(estimate) = names(sds) = "lambda"
      vc = matrix(sds^2, ncol = 1, nrow = 1, dimnames = list("lambda",
                                                             "lambda"))
      return(structure(list(estimate = estimate, sd = sds,
                            vcov = vc, n = n, loglik = sum(dpois(x, estimate,
                                                                 log = TRUE))), class = "FitDistr"))
    }
    if (distname == "exponential") {
      if (any(x < 0))
        stop("Exponential values must be >= 0")
      if (!is.null(start))
        stop(gettextf("supplying pars for the %s distribution is not supported",
                      "exponential"), domain = NA)
      estimate = 1/mean(x)
      sds = estimate/sqrt(n)
      vc = matrix(sds^2, ncol = 1, nrow = 1, dimnames = list("rate",
                                                             "rate"))
      names(estimate) = names(sds) = "rate"
      return(structure(list(estimate = estimate, sd = sds,
                            vcov = vc, n = n, loglik = sum(dexp(x, estimate,
                                                                log = TRUE))), class = "FitDistr"))
    }
    if (distname == "geometric") {
      if (!is.null(start))
        stop(gettextf("supplying pars for the %s distribution is not supported",
                      "geometric"), domain = NA)
      estimate = 1/(1 + mean(x))
      sds = estimate * sqrt((1 - estimate)/n)
      vc = matrix(sds^2, ncol = 1, nrow = 1, dimnames = list("prob",
                                                             "prob"))
      names(estimate) = names(sds) = "prob"
      return(structure(list(estimate = estimate, sd = sds,
                            vcov = vc, n = n, loglik = sum(dgeom(x, estimate,
                                                                 log = TRUE))), class = "FitDistr"))
    }
    if (distname == "weibull" && is.null(start)) {
      if (any(x <= 0))
        stop("Weibull values must be > 0")
      lx = log(x)
      m = mean(lx)
      v = var(lx)
      shape = 1.2/sqrt(v)
      scale = exp(m + 0.572/shape)
      start = list(shape = shape, scale = scale)
      start = start[!is.element(names(start), dots)]
    }
    if (distname == "gamma" && is.null(start)) {
      if (any(x < 0))
        stop("gamma values must be >= 0")
      m = mean(x)
      v = var(x)
      start = list(shape = m^2/v, rate = m/v)
      start = start[!is.element(names(start), dots)]
      control = list(parscale = c(1, start$rate))
    }
    if (distname == "negative binomial" && is.null(start)) {
      m = mean(x)
      v = var(x)
      size = if (v > m)
        m^2/(v - m)
      else 100
      start = list(size = size, mu = m)
      start = start[!is.element(names(start), dots)]
    }
    if (is.element(distname, c("cauchy", "logistic")) && is.null(start)) {
      start = list(location = median(x), scale = IQR(x)/2)
      start = start[!is.element(names(start), dots)]
    }
    if (distname == "t" && is.null(start)) {
      start = list(m = median(x), s = IQR(x)/2, df = 10)
      start = start[!is.element(names(start), dots)]
    }
    if (distname == "beta" && is.null(start)){
      distr_list = EnvStats::ebeta(x)
      start = list(shape1 = distr_list$parameters[[1]], shape2 = distr_list$parameters[[2]])
    }
    if (distname == "chi-squared" && is.null(start)){
      start = list(df = 1)
    }
    if (distname == "f" && is.null(start)){
      start = list(df1 = 1, df2 = 1)
    }
  }
  if (is.null(start) || !is.list(start)){
    structure(list(estimate = NA, sd = NA, vcov = NA,
                   loglik = NA, n = NA), class = "FitDistr")
  }
  else{
    nm = names(start)
    f = formals(densfun)
    args = names(f)
    m = match(nm, args)
    if (any(is.na(m)))
      stop("'start' specifies names which are not arguments to 'densfun'")
    formals(densfun) = c(f[c(1, m)], f[-c(1, m)])
    dens = function(parm, x, ...) densfun(x, parm, ...)
    if ((l = length(nm)) > 1L)
      body(dens) = parse(text = paste("densfun(x,", paste("parm[",
                                                          1L:l, "]", collapse = ", "), ", ...)"))
    Call[[1L]] = quote(stats::optim)
    Call$densfun = Call$start = NULL
    Call$x = x
    Call$par = start
    Call$fn = if ("log" %in% args)
      mylogfn
    else myfn
    Call$hessian = TRUE
    if (length(control))
      Call$control = control
    if (is.null(Call$method)) {
      if (any(c("lower", "upper") %in% names(Call)))
        Call$method = "L-BFGS-B"
      else if (length(start) > 1L)
        Call$method = "BFGS"
      else Call$method = "Nelder-Mead"
    }
    res = suppressWarnings(eval.parent(Call))
    if (res$convergence > 0L)
      stop("optimization failed")
    vc = solve(res$hessian)
    sds = sqrt(diag(vc))
    structure(list(estimate = res$par, sd = sds, vcov = vc,
                   loglik = -res$value, n = n), class = "FitDistr")
  }
}

# qqPlot -----
qqPlot <- function(x, y, confbounds = TRUE, alpha, main, xlab, ylab, xlim, ylim, border = "red",
                   bounds.col = "black", bounds.lty = 1, start, showPlot = TRUE,
                   axis.y.right = FALSE, bw.theme = FALSE){
  #' @title qqPlot: Quantile-Quantile Plots for various distributions
  #' @description Function \code{qqPlot} creates a QQ plot of the values in x including a line which passes through the first and third quartiles.
  #' @param x The sample for qqPlot.
  #' @param y Character string specifying the distribution of \code{x}. The function \code{qqPlot} supports the following character strings for \code{y}:
  #' \itemize{
  #'   \item{\code{`beta`}}
  #'   \item{\code{`cauchy`}}
  #'   \item{\code{`chi-squared`}}
  #'   \item{\code{`exponential`}}
  #'   \item{\code{`f`}}
  #'   \item{\code{`gamma`}}
  #'   \item{\code{`geometric`}}
  #'   \item{\code{`log-normal`}}
  #'   \item{\code{`lognormal`}}
  #'   \item{\code{`logistic`}}
  #'   \item{\code{`negative binomial`}}
  #'   \item{\code{`normal`}}
  #'   \item{\code{`Poisson`}}
  #'   \item{\code{`weibull`}}
  #' }
  #' By default \code{distribution} is set to \code{`normal`}.
  #' @param confbounds Logical value indicating whether to display confidence bounds. By default, \code{confbounds} is set to \code{TRUE}.
  #' @param alpha Numeric value specifying the significance level for the confidence bounds, set to `0.05` by default.
  #' @param main A character string for the main title of the plot.
  #' @param xlab A character string for the x-axis label.
  #' @param ylab A character string for the y-axis label.
  #' @param xlim A numeric vector of length 2 to specify the limits of the x-axis.
  #' @param ylim A numeric vector of length 2 to specify the limits of the y-axis.
  #' @param border A numerical value or single character string giving the color of the interpolation line. By default, \code{border} is set to \code{`red`}.
  #' @param bounds.col a character string specifying the color of the confidence bounds. By default, \code{bounds.col} is set to \code{`black`}.
  #' @param bounds.col A numerical value or single character string giving the color of the confidence bounds lines. By default, \code{bounds.col} is set to \code{`black`}.
  #' @param bounds.lty A numeric or character: line type for the confidence bounds lines. This can be specified with either an integer (0-6) or a name:
  #' \itemize{
  #'   \item{0: blank}
  #'   \item{1: solid}
  #'   \item{2: dashed}
  #'   \item{3: dotted}
  #'   \item{4: dotdash}
  #'   \item{5: longdash}
  #'   \item{6: twodash}
  #' }
  #' Default is `1` (solid line).
  #' @param start A named list giving the parameters to be fitted with initial values. Must be supplied for some distributions (see Details).
  #' @param showPlot Logical value indicating whether to display the plot. By default, \code{showPlot} is set to \code{TRUE}.
  #' @param axis.y.right Logical value indicating whether to display the y-axis on the right side. By default, \code{axis.y.right} is set to \code{FALSE}.
  #' @param bw.theme Logical value indicating whether to use a black-and-white theme from the \code{ggplot2} package for the plot. By default, \code{bw.theme} is set to \code{FALSE}.
  #' @details Distribution fitting is performed using the \code{FitDistr} function from this package.
  #' For the computation of the confidence bounds, the variance of the quantiles is estimated using the delta method,
  #' which involves the estimation of the observed Fisher Information matrix as well as the gradient of the CDF of the fitted distribution.
  #' Where possible, those values are replaced by their normal approximation.
  #' @return The function \code{qqPlot} returns an invisible list containing:
  #' \item{x}{Sample quantiles.}
  #' \item{y}{Theoretical quantiles.}
  #' \item{int}{Intercept of the fitted line.}
  #' \item{slope}{Slope of the fitted line.}
  #' \item{plot}{The generated QQ plot.}
  #' @seealso \code{\link{ppPlot}}, \code{\link{FitDistr}}.
  #' @examples
  #' set.seed(123)
  #' qqPlot(rnorm(20, mean=90, sd=5), "normal",alpha=0.30)
  #' qqPlot(rcauchy(100), "cauchy")
  #' qqPlot(rweibull(50, shape = 1, scale = 1), "weibull")
  #' qqPlot(rlogis(50), "logistic")
  #' qqPlot(rlnorm(50) , "log-normal")
  #' qqPlot(rbeta(10, 0.7, 1.5),"beta")
  #' qqPlot(rpois(20,3), "poisson")
  #' qqPlot(rchisq(20, 10),"chi-squared")
  #' qqPlot(rgeom(20, prob = 1/4), "geometric")
  #' qqPlot(rnbinom(n = 20, size = 3, prob = 0.2), "negative binomial")
  #' qqPlot(rf(20, df1 = 10, df2 = 20), "f")


  parList = list()
  if (is.numeric(x)) {
    x1 <- sort(na.omit(x))
    if (missing(xlim))
      xlim = c(min(x1) - 0.1 * diff(range(x1)), max(x1) +
                 0.1 * diff(range(x1)))
  }
  if(inherits(x, "DistrCollection")){
    distList <- x$distr
    grap <- qqPlot(distList[[1]]$x, showPlot = FALSE, ylab = "", xlab = "", main = paste(distList[[1]]$name,"distribution"))
    for (i in 2:length(distList)){
      aux <- qqPlot(distList[[i]]$x, showPlot = FALSE, ylab = "", xlab = "", main = paste(distList[[i]]$name,"distribution"))
      grap$plot <-  grap$plot + aux$plot
    }
    show(grap$plot + plot_annotation(title = "QQ Plot for a Collection Distribution"))
    invisible()
  }
  else{
    if (missing(y)){
      y = "normal"
    }
    if(missing(alpha)){
      alpha = 0.05
    }
    if (alpha <=0 || alpha >=1){
      stop(paste("alpha should be between 0 and 1!"))
    }
    if (missing(main)){
      main = paste("QQ Plot for", deparse(substitute(y)), "distribution")
    }
    if (missing(xlab)){
      xlab = paste("Quantiles for", deparse(substitute(x)))
    }
    if (missing(ylab)){
      ylab = paste("Quantiles from", deparse(substitute(y)), "distribution")
    }
    if (is.numeric(y)) {
      message("\ncalling (original) qqplot from namespace stats!\n")
      return(stats::qqplot(x, y))
    }
    qFun = NULL
    theoretical.quantiles = NULL
    xs = sort(x)
    distribution = tolower(y)
    distWhichNeedParameters = c("weibull", "logistic", "gamma","exponential", "f",
                                "geometric", "chi-squared", "negative binomial",
                                "poisson")
    threeParameterDistr = c("weibull3", "lognormal3", "gamma3")
    threeParameter = distribution %in% threeParameterDistr
    if(threeParameter) distribution = substr(distribution, 1, nchar(distribution)-1)
    if(is.character(distribution)){
      qFun = .charToDistFunc(distribution, type = "q")
      if (is.null(qFun))
        stop(paste(deparse(substitute(y)), "distribution could not be found!"))
    }
    theoretical.probs = ppoints(xs)
    xq = NULL
    yq = quantile(xs, prob = c(0.25, 0.75))

    if(TRUE){
      fitList = .lfkp(parList, formals(qFun))
      fitList$x = xs
      fitList$densfun = distribution
      if(!missing(start))
        fitList$start = start
      if(!threeParameter){
        fittedDistr = do.call(FitDistr, fitList)
        parameter = fittedDistr$estimate

        #save the distribution parameter#
        thethas = fittedDistr$estimate
        # save the cariance-covariance matrix
        varmatrix = fittedDistr$vcov

      }
      else{
        parameter = do.call(paste(".",distribution, "3", sep = ""), list(xs) )
        threshold = parameter$threshold
      }

      parameter = .lfkp(as.list(parameter), formals(qFun))
      params = .lfkp(parList, formals(qFun))
      parameter = .lfrm(as.list(parameter), params)
      parameter = c(parameter, params)
      theoretical.quantiles = do.call(qFun, c(list(c(theoretical.probs)), parameter))

      if(!threeParameter){
        confIntCapable = c("exponential", "log-normal", "logistic", "normal", "weibull", "gamma", "beta", "cauchy")
        if(confbounds == TRUE){
          if(distribution %in% confIntCapable){
            getConfIntFun = .charToDistFunc(distribution, type = ".confint")
            confInt = getConfIntFun(xs, thethas, varmatrix, alpha)
          }
        }
      }

      xq <- do.call(qFun, c(list(c(0.25, 0.75)), parameter))
    }
    else {
      params =.lfkp(parList, formals(qFun))
      params$p = theoretical.probs
      theoretical.quantiles = do.call(qFun, params)
      params$p = c(0.25, 0.75)
      xq = do.call(qFun, params)
    }

    params =.lfkp(parList, c(formals(plot.default), par()))

    if(!threeParameter){
      params$y = theoretical.quantiles
    }
    else{
      params$y = theoretical.quantiles+threshold
    }
    params$x = xs
    params$xlab = xlab
    params$ylab = ylab
    params$main = main
    params$lwd = 1

    ############ PLOT ############
    p <- ggplot(data = data.frame(x=params$x, y=params$y), mapping=aes(x=x, y=y)) +
      geom_point() + labs(x = xlab, y = ylab, title = main) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))

    params =.lfkp(parList, c(formals(abline), par()))
    params$a = 0
    params$b = 1
    params$col = border
    p <- p + geom_abline(intercept = params$a, slope = params$b, col = params$col, lwd = params$lwd)

    if(!threeParameter){
      if(confbounds == TRUE){
        if(distribution %in% confIntCapable){
          params =.lfkp(parList, c(formals(lines), par()))
          params$col = bounds.col
          params$lty = bounds.lty
          # under bound
          p <- p + geom_line(data = subset(data.frame(x=confInt[[3]], y=confInt[[1]]), x >= xlim[1] & x <= xlim[2]),
                             aes(x = x, y = y),
                             col = params$col, lty = params$lty, lwd = params$lwd)
          params$col = bounds.col
          params$lty = bounds.lty
          # upper bound
          p <- p + geom_line(data = subset(data.frame(x=confInt[[3]], y=confInt[[2]]), x >= xlim[1] & x <= xlim[2]),
                             aes(x = x, y = y),
                             col = params$col, lty = params$lty, lwd = params$lwd)
        }
      }
    }
    if(axis.y.right){
      p <- p + scale_y_continuous(position = "right")
    }
    if(bw.theme){
      p <- p + theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
    }
    if(main == ""){
      p <- p + labs(title = NULL)
    }
    if(showPlot){
      show(p)
      invisible(list(x = theoretical.quantiles, y = xs, int = params$a, slope = params$b, plot = p))
    }
    else{
      invisible(list(x = theoretical.quantiles, y = xs, int = params$a, slope = params$b, plot = p))
    }
  }
}
# ppPlot ---------------------
ppPlot <- function (x, distribution, confbounds = TRUE, alpha, probs, main, xlab, ylab, xlim, ylim,
                    border = "red", bounds.col = "black", bounds.lty = 1,
                    start, showPlot = TRUE, axis.y.right = FALSE, bw.theme = FALSE){
  #' @title ppPlot: Probability Plots for various distributions
  #' @description Function \code{ppPlot} creates a Probability plot of the values in x including a line.
  #' @param x Numeric vector containing the sample data for the \code{ppPlot}.
  #' @param distribution Character string specifying the distribution of x. The function \code{ppPlot} will support the following character strings for \code{distribution}:
  #' \itemize{
  #'   \item{\code{`beta`}}
  #'   \item{\code{`cauchy`}}
  #'   \item{\code{`chi-squared`}}
  #'   \item{\code{`exponential`}}
  #'   \item{\code{`f`}}
  #'   \item{\code{`gamma`}}
  #'   \item{\code{`geometric`}}
  #'   \item{\code{`log-normal`}}
  #'   \item{\code{`lognormal`}}
  #'   \item{\code{`logistic`}}
  #'   \item{\code{`negative binomial`}}
  #'   \item{\code{`normal`}}
  #'   \item{\code{`Poisson`}}
  #'   \item{\code{`weibull`}}
  #' }
  #' By default \code{distribution} is set to \code{`normal`}.
  #' @param confbounds Logical value: whether to display confidence bounds. Default is \code{TRUE}.
  #' @param alpha Numeric value: significance level for confidence bounds, default is `0.05`.
  #' @param probs Vector containing the percentages for the y axis. All the values need to be between `0` and `1`.
  #' If `probs` is missing it will be calculated internally.
  #' @param main Character string: title of the plot.
  #' @param xlab Character string: label for the x-axis.
  #' @param ylab Character string: label for the y-axis.
  #' @param xlim Numeric vector of length 2: limits for the x-axis.
  #' @param ylim Numeric vector of length 2: limits for the y-axis.
  #' @param border Character or numeric: color for the border of the line through the quantiles. Default is \code{`red`}.
  #' @param bounds.col Character or numeric: color for the confidence bounds lines. Default is \code{`black`}.
  #' @param bounds.lty Numeric or character: line type for the confidence bounds lines. This can be specified with either an integer (0-6) or a name:
  #' \itemize{
  #'   \item{0: blank}
  #'   \item{1: solid}
  #'   \item{2: dashed}
  #'   \item{3: dotted}
  #'   \item{4: dotdash}
  #'   \item{5: longdash}
  #'   \item{6: twodash}
  #' }
  #' Default is `1` (solid line).
  #' @param start A named list giving the parameters to be fitted with initial values. Must be supplied for some distributions (see Details).
  #' @param showPlot Logical value indicating whether to display the plot. By default, \code{showPlot} is set to \code{TRUE}.
  #' @param axis.y.right Logical value indicating whether to display the y-axis on the right side. By default, \code{axis.y.right} is set to \code{FALSE}.
  #' @param bw.theme Logical value indicating whether to use a black-and-white theme from the \code{ggplot2} package for the plot. By default, \code{bw.theme} is set to \code{FALSE}.
  #' @details Distribution fitting is performed using the \code{FitDistr} function from this package.
  #' For the computation of the confidence bounds, the variance of the quantiles is estimated using the delta method,
  #' which involves the estimation of the observed Fisher Information matrix as well as the gradient of the CDF of the fitted distribution.
  #' Where possible, those values are replaced by their normal approximation.
  #' @return The function \code{ppPlot} returns an invisible list containing:
  #' \item{x}{x coordinates.}
  #' \item{y}{y coordinates.}
  #' \item{int}{Intercept.}
  #' \item{slope}{Slope.}
  #' \item{plot}{The generated PP plot.}
  #' @seealso \code{\link{qqPlot}}, \code{\link{FitDistr}}.
  #' @examples
  #' set.seed(123)
  #' ppPlot(rnorm(20, mean=90, sd=5), "normal",alpha=0.30)
  #' ppPlot(rcauchy(100), "cauchy")
  #' ppPlot(rweibull(50, shape = 1, scale = 1), "weibull")
  #' ppPlot(rlogis(50), "logistic")
  #' ppPlot(rlnorm(50) , "log-normal")
  #' ppPlot(rbeta(10, 0.7, 1.5),"beta")
  #' ppPlot(rpois(20,3), "poisson")
  #' ppPlot(rchisq(20, 10),"chi-squared")
  #' ppPlot(rgeom(20, prob = 1/4), "geometric")
  #' ppPlot(rnbinom(n = 20, size = 3, prob = 0.2), "negative binomial")
  #' ppPlot(rf(20, df1 = 10, df2 = 20), "f")

  conf.level = 0.95
  conf.lines = TRUE
  if (!(is.numeric(x) | inherits(x, "DistrCollection")))
    stop(paste(deparse(substitute(x)), " needs to be numeric or an object of class distrCollection"))
  parList = list()
  if (is.null(parList[["col"]]))
    parList$col = c("black", "red", "gray")
  if (is.null(parList[["pch"]]))
    parList$pch = 19
  if(is.null(parList[["lwd"]]))
    parList$lwd = 1
  if (is.null(parList[["cex"]]))
    parList$cex = 1
  qFun = NULL
  xq = NULL
  yq = NULL
  x1 = NULL
  if(missing(alpha))
    alpha = 0.05
  if (alpha <=0 || alpha >=1)
    stop(paste("alpha should be between 0 and 1!"))
  if (missing(probs))
    probs = ppoints(11)
  else if (min(probs) <= 0 || max(probs) >= 1)
    stop("probs should be values within (0,1)!")
  probs = round(probs, 2)
  if (is.numeric(x)) {
    x1 <- sort(na.omit(x))
    if (missing(xlim))
      xlim = c(min(x1) - 0.1 * diff(range(x1)), max(x1) +0.1 * diff(range(x1)))
  }
  if (missing(distribution))
    distribution = "normal"
  if (missing(ylim))
    ylim = NULL
  if (missing(main))
    main = paste("Probability Plot for", deparse(substitute(distribution)),
                 "distribution")
  if (missing(xlab))
    xlab = deparse(substitute(x))
  if (missing(ylab))
    ylab = "Probability"
  if(inherits(x, "DistrCollection")){
    distList <- x$distr
    grap <- ppPlot(distList[[1]]$x, showPlot = FALSE, ylab = "", xlab = "", main = paste(distList[[1]]$name,"distribution"))
    for (i in 2:length(distList)){
      aux <- ppPlot(distList[[i]]$x, showPlot = FALSE, ylab = "", xlab = "", main = paste(distList[[i]]$name,"distribution"))
      grap$plot <-  grap$plot + aux$plot
    }
    show(grap$plot + plot_annotation(title = "QQ Plot for a Collection Distribution"))
    invisible()
  }
  distWhichNeedParameters = c("weibull", "gamma", "logistic","exponential","f",
                              "geometric", "chi-squared", "negative binomial",
                              "poisson")
  threeParameterDistr = c("weibull3", "lognormal3", "gamma3")
  threeParameter = distribution %in% threeParameterDistr
  if(threeParameter) distribution = substr(distribution, 1, nchar(distribution)-1)
  if (is.character(distribution)) {
    qFun = .charToDistFunc(distribution, type = "q")
    pFun = .charToDistFunc(distribution, type = "p")
    dFun = .charToDistFunc(distribution, type = "d")
    if (is.null(qFun))
      stop(paste(deparse(substitute(y)), "distribution could not be found!"))
  }
  if (TRUE) {
    fitList = .lfkp(parList, formals(qFun))
    fitList$x = x1
    fitList$densfun = distribution
    if (!missing(start))
      fitList$start = start
    if(!threeParameter){
      fittedDistr = do.call(FitDistr, fitList)
      parameter = fittedDistr$estimate
      thethas = fittedDistr$estimate
      varmatrix = fittedDistr$vcov
    }
    else{
      parameter = do.call(paste(".",distribution, "3", sep = ""), list(x1) )
      threshold = parameter$threshold
    }
    parameter = .lfkp(as.list(parameter), formals(qFun))
    params = .lfkp(parList, formals(qFun))
    parameter = .lfrm(as.list(parameter), params)
    parameter = c(parameter, params)
    if(!threeParameter){
      confIntCapable = c("exponential", "log-normal", "logistic", "normal", "weibull", "gamma", "beta", "cauchy")
      if(confbounds == TRUE){
        if(distribution %in% confIntCapable){
          getConfIntFun = .charToDistFunc(distribution, type = ".confint")
          confInt = getConfIntFun(x1, thethas, varmatrix, alpha)
        }
      }
    }
    y = do.call(qFun, c(list(ppoints(x1)), as.list(parameter)))
    yc = do.call(qFun, c(list(ppoints(x1)), as.list(parameter)))
    cv = do.call(dFun, c(list(yc), as.list(parameter)))
    axisAtY = do.call(qFun, c(list(probs), as.list(parameter)))
    yq = do.call(qFun, c(list(c(0.25, 0.75)), as.list(parameter)))
    xq = quantile(x1, probs = c(0.25, 0.75))
  }
  else {
    params = .lfkp(parList, formals(qFun))
    params$p = ppoints(x1)
    y = do.call(qFun, params)
    params$p = probs
    axisAtY = do.call(qFun, params)
    params$p = c(0.25, 0.75)
    yq = do.call(qFun, params)
    xq = quantile(x1, probs = c(0.25, 0.75))
  }
  params = .lfkp(parList, c(formals(plot.default), par()))
  params$x = x1
  params$y = y
  params$xlab = xlab
  params$ylab = ylab
  params$main = main
  params$xlim = xlim
  params$axes = FALSE
  params$lwd = 1
  if (!(is.null(params$col[1]) || is.na(params$col[1])))
    params$col = params$col[1]
  # PLOT
  p <- ggplot(data.frame(x = x1, y = y), aes(x = x, y = y)) +
    geom_point(size = 1, color = "black") +
    labs(x = xlab, y = ylab, title = main) +
    scale_x_continuous(limits = xlim, expand = c(0, 0)) +
    scale_y_continuous(labels = scales::percent_format(scale = 1/max(x1), suffix = "", accuracy = 0.01))+
    theme_minimal() +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold"),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  # Regression line
  params = .lfkp(parList, c(formals(abline), par()))
  if(!threeParameter){
    params$a = 0
  }
  else{
    params$a = -threshold
  }
  params$b = 1
  params$col = border
  p <- p + geom_abline(intercept = params$a, slope = params$b, color = border)
  if(!threeParameter){
    if(confbounds == TRUE){
      if(distribution %in% confIntCapable){
        params =.lfkp(parList, c(formals(lines), par()))
        params$col = bounds.col
        params$lty = bounds.lty
        # lower bound
        p <- p + geom_line(data = subset(data.frame(x=confInt[[3]], y=confInt[[1]]), x >= xlim[1] & x <= xlim[2]),
                           aes(x = x, y = y),
                           col = params$col, lty = params$lty, lwd = params$lwd)

        params$col = bounds.col
        params$lty = bounds.lty
        # upper bound
        p <- p + geom_line(data = subset(data.frame(x=confInt[[3]], y=confInt[[2]]), x >= xlim[1] & x <= xlim[2]),
                           aes(x = x, y = y),
                           col = params$col, lty = params$lty, lwd = params$lwd)
      }
    }
  }
  if(axis.y.right){
    p <- p + scale_y_continuous(position = "right")
  }
  if(bw.theme){
    p <- p + theme_bw()
  }
  if(main == ""){
    p <- p + labs(title = NULL)
  }
  if(showPlot){
    show(p)
    invisible(list(x = x, y = y, int = params$a, slope = params$b, plot = p))
  }
  else{
    invisible(list(x = x, y = y, int = params$a, slope = params$b, plot = p))
  }
}

# cg_RunChart ----
cg_RunChart <- function (x, target, tolerance, ref.interval, facCg, facCgk,
                         n = 0.2, col = "black", pch = 19,
                         xlim = NULL, ylim = NULL, main = "Run Chart",
                         conf.level = 0.95, cgOut = TRUE){
  #' @title cg_RunChart
  #' @description Function visualize the given values of measurement in a Run Chart
  #' @param x A vector containing the measured values.
  #' @param target A numeric value giving the expected target value for the x-values.
  #' @param tolerance Vector of length 2 giving the lower and upper specification limits.
  #' @param ref.interval Numeric value giving the confidence interval on which the calculation is based. By default it is based on 6 sigma methodology.
  #' Regarding the normal distribution this relates to \code{pnorm(3) - pnorm(-3)} which is exactly 99.73002 percent. If the calculation is based on another sigma value \code{ref.interval} needs to be adjusted.
  #' To give an example: If the sigma-level is given by 5.15 the \code{ref.interval} relates to \code{pnorm(5.15/2)-pnorm(-5.15/2)} which is exactly 0.989976 percent.
  #' @param facCg Numeric value as a factor for the calculation of the gage potential index. The default Value for \code{facCg} is \code{0.2}.
  #' @param facCgk Numeric value as a factor for the calculation of the gage capability index. The default value for \code{facCgk} is \code{0.1}.
  #' @param n Numeric value between \code{0} and \code{1} giving the percentage of the tolerance field (values between the upper and lower specification limits given by \code{tolerance}) where the values of \code{x} should be positioned. Limit lines will be drawn. Default value is \code{0.2}.
  #' @param col Character or numeric value specifying the color of the curve in the run chart. Default is \code{`black`}.
  #' @param pch Numeric or character specifying the plotting symbol. Default is \code{19} (filled circle).
  #' @param xlim Numeric vector of length 2 specifying the limits for the x-axis. Default is \code{NULL} which means the limits are set automatically.
  #' @param ylim Numeric vector of length 2 specifying the limits for the y-axis. Default is \code{NULL} which means the limits are set automatically.
  #' @param main Character string specifying the title of the plot. Default is \code{`Run Chart`}.
  #' @param conf.level Confidence level for internal \code{t.test} checking the significance of the bias between \code{target} and mean of \code{x}. The default value is \code{0.95}. The result of the \code{t.test} is shown in the histogram on the left side.
  #' @param cgOut Logical value deciding whether the \code{Cg} and \code{Cgk} values should be plotted in a legend. Default is \code{TRUE}.
  #' @details The calculation of the potential and actual gage capability are based on the following formulae:
  #' \itemize{
  #' \item{\code{Cg = (facCg * tolerance[2]-tolerance[1])/ref.interval}}
  #' \item{\code{Cgk = (facCgk * abs(target-mean(x))/(ref.interval/2)}}
  #' }
  #' If the usage of the historical process variation is preferred the values for the tolerance \code{tolerance} must be adjusted manually. That means in case of the 6 sigma methodology for example, that \code{tolerance = 6 * sigma[process]}.
  #' @return The function \code{cg_RunChart} returns a list of numeric values. The first element contains the calculated centralized gage potential index \code{Cg} and the second contains the non-centralized gage capability index \code{Cgk}.
  #' @seealso \code{\link{cg_HistChart}}, \code{\link{cg_ToleranceChart}},  \code{\link{cg}}
  #' @examples
  #'
  #' x <- c(9.991, 10.013, 10.001, 10.007, 10.010, 10.013, 10.008,9.992,
  #'        10.017, 10.005, 10.005, 10.002, 10.017, 10.005, 10.002, 9.996,
  #'        10.011, 10.009, 10.006, 10.008, 10.003, 10.002, 10.006, 10.010, 10.013)
  #'
  #' cg_RunChart(x = x, target = 10.003, tolerance = c(9.903, 10.103))

  if (missing(x)) {
    stop("x must be given as a vector")
  }

  if (missing(target)) {
    target <- mean(x)
    targetmissing <- FALSE
  } else {
    targetmissing <- TRUE
  }

  if (missing(ref.interval)) {
    ref.interval <- pnorm(3) - pnorm(-3)
  }

  sd <- sd(x)
  mean <- mean(x)
  ref.ar <- qnorm(ref.interval, mean, sd) - qnorm(1 - ref.interval, mean, sd)

  if (missing(facCg)) {
    facCg <- 0.2
  }

  if (missing(facCgk)) {
    facCgk <- 0.1
  }

  if (missing(tolerance)) {
    width <- ref.ar/facCg
    tolerance <- numeric(2)
    tolerance[1] <- mean - width/2
    tolerance[2] <- mean + width/2
  }

  quant1 <- qnorm((1 - ref.interval)/2, mean, sd)
  quant2 <- qnorm(ref.interval + (1 - ref.interval)/2, mean, sd)

  if (length(tolerance) != 2) {
    stop("tolerance has wrong length")
  }

  if (missing(xlim)) {
    xlim <- c(0, length(x))
  }

  if (missing(ylim)) {
    ylim <- c(min(x, target - n/2 * (abs(diff(tolerance))), quant1, quant2),
              max(x, target + n/2 * (abs(diff(tolerance))), quant1, quant2))
  }

  if (missing(main)) {
    main <- "Run Chart"
  }

  Cg <- (facCg * tolerance[2]-tolerance[1])/ref.interval
  Cgk <- (facCgk * abs(target-mean(x))/(ref.interval/2))

  # Create a data frame for plotting
  df <- data.frame(x = x, y = x)

  # Add target line
  df$y_target <- target

  # Calculate the upper and lower control limits
  df$y_lower <- quant1
  df$y_upper <- quant2
  # Calculate the tolerance limits
  df$y_tolerance_lower <- tolerance[1]
  df$y_tolerance_upper <- tolerance[2]

  # Calculate the mean
  df$y_mean <- mean

  # Calculate the Cg and Cgk values
  df$Cg <- Cg
  df$Cgk <- Cgk

  # Create the ggplot
  # 1. Principal plot and target line
  p <- ggplot(df, aes(x = seq_along(x), y = x)) +
    geom_point(color = col, shape = pch) +
    geom_line(color = col, linetype = "solid") +
    scale_x_continuous(limits = c(xlim[1] - 0.05 * xlim[2], xlim[2]), expand = c(0, 0)) +
    labs(title = main, x = "Index", y = "x") +
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
    geom_hline(aes(yintercept = target)) # Linea target

  # 2. Red Plot (Lowess)
  p <- p + geom_smooth(method = "loess", color = "red", se = FALSE, span = 1.25, linewidth = 0.25)

  # 3. Green lines
  p <- p + geom_hline(aes(yintercept = mean), linetype = "dashed", color = "seagreen")+  #center line
    geom_hline(aes(yintercept = quant1), linetype = "dashed", color = "seagreen") + # Bottom line
    geom_hline(aes(yintercept = quant2), linetype = "dashed", color = "seagreen")   # Top line
  # 4. Xtar +- 0.1
  p <- p + geom_hline(yintercept = c(target + n/2 * (abs(diff(tolerance))), target - n/2 * (abs(diff(tolerance)))), color = "#012B78", linetype = "solid") # Agregar líneas
  #Label
  p <- p + scale_y_continuous(limits = ylim, expand = c(0, 0),sec.axis =
                                sec_axis(~ .,breaks = c(target,mean,quant1,quant2,target + n/2 * (abs(diff(tolerance))),
                                                        target - n/2 * (abs(diff(tolerance)))),
                                         labels=c("target",
                                                  expression(bar(x)),
                                                  substitute(x[a * b], list(a = round(((1 - ref.interval)/2) * 100, 3), b = "%")),
                                                  substitute(x[a * b], list(a = round(((ref.interval + (1 - ref.interval)/2)) * 100,3), b = "%")),
                                                  substitute(x[tar] + a, list(a = round(n/2, 4))),
                                                  substitute(x[tar] - a, list(a = round(n/2, 4)))))) +
    theme(axis.text.y.right = element_text(size = 15))

  # Label  Cg and Cgk
  if (cgOut == TRUE) {
    caja <- ggplot(data = data.frame(x = 0, y = 0), aes(x, y)) +
      theme_bw() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_blank()
      ) +
      xlim(c(0.25,0.26)) + ylim(c(0.24, 0.31))

    caja <- caja +
      annotate('text', x = 0.25, y = 0.30,
               label = paste("Cg: ", round(Cg,digits = 6)),
               parse = TRUE, size = 3, hjust = 0) +
      annotate('text', x = 0.25, y = 0.25,
               label = paste("Cgk:", round(Cgk,digits = 6)),
               parse = TRUE, size = 3, hjust = 0)

    p <- p + inset_element(caja, left = 0.7, right = 1, top = 1, bottom = 0.75)
    suppressMessages(show(p))
  }
  else{
    suppressMessages(show(p))
  }
  invisible(list(Cg, Cgk))
}

# cg_HistChart ----
cg_HistChart <- function (x, target, tolerance, ref.interval, facCg, facCgk,
                          n = 0.2, col, xlim, ylim, main, conf.level = 0.95, cgOut = TRUE){
  #' @title cg_HistChart
  #' @description Function visualize the given values of measurement in a histogram
  #' @param x A vector containing the measured values.
  #' @param target A numeric value giving the expected target value for the x-values.
  #' @param tolerance Vector of length 2 giving the lower and upper specification limits.
  #' @param ref.interval Numeric value giving the confidence interval on which the calculation is based. By default it is based on 6 sigma methodology.
  #' Regarding the normal distribution this relates to \code{pnorm(3) - pnorm(-3)} which is exactly 99.73002 percent. If the calculation is based on another sigma value \code{ref.interval} needs to be adjusted.
  #' To give an example: If the sigma-level is given by 5.15 the \code{ref.interval} relates to \code{pnorm(5.15/2)-pnorm(-5.15/2)} which is exactly 0.989976 percent.
  #' @param facCg Numeric value as a factor for the calculation of the gage potential index. The default Value for \code{facCg} is \code{0.2}.
  #' @param facCgk Numeric value as a factor for the calculation of the gage capability index. The default value for \code{facCgk} is \code{0.1}.
  #' @param n Numeric value between \code{0} and \code{1} giving the percentage of the tolerance field (values between the upper and lower specification limits given by \code{tolerance}) where the values of \code{x} should be positioned. Limit lines will be drawn. Default value is \code{0.2}.
  #' @param col Character or numeric value specifying the color of the histogram. Default is \code{`black`}.
  #' @param xlim Numeric vector of length 2 specifying the limits for the x-axis. Default is \code{NULL} which means the limits are set automatically.
  #' @param ylim Numeric vector of length 2 specifying the limits for the y-axis. Default is \code{NULL} which means the limits are set automatically.
  #' @param main Character string specifying the title of the plot. Default is \code{`Histogram of x - target`}.
  #' @param conf.level Confidence level for internal \code{t.test} checking the significance of the bias between \code{target} and mean of \code{x}. The default value is \code{0.95}.
  #' @param cgOut Logical value deciding whether the \code{Cg} and \code{Cgk} values should be plotted in a legend. Default is \code{TRUE}.
  #' @details The calculation of the potential and actual gage capability are based on the following formulae:
  #' \itemize{
  #' \item{\code{Cg = (facCg * tolerance[2]-tolerance[1])/ref.interval}}
  #' \item{\code{Cgk = (facCgk * abs(target-mean(x))/(ref.interval/2)}}
  #' }
  #' If the usage of the historical process variation is preferred the values for the tolerance \code{tolerance} must be adjusted manually. That means in case of the 6 sigma methodology for example, that \code{tolerance = 6 * sigma[process]}.
  #' @return The function \code{cg_HistChart} returns a list of numeric values. The first element contains the calculated centralized gage potential index \code{Cg} and the second contains the non-centralized gage capability index \code{Cgk}.
  #' @seealso \code{\link{cg_RunChart}}, \code{\link{cg_ToleranceChart}}, \code{\link{cg}}
  #' @examples
  #'
  #' x <- c(9.991, 10.013, 10.001, 10.007, 10.010, 10.013, 10.008,9.992,
  #'        10.017, 10.005, 10.005, 10.002, 10.017, 10.005, 10.002, 9.996,
  #'        10.011, 10.009, 10.006, 10.008, 10.003, 10.002, 10.006, 10.010, 10.013)
  #'
  #' cg_HistChart(x = x, target = 10.003, tolerance = c(9.903, 10.103))

  if (missing(x))
    stop("x must be given as a vector")
  if (missing(target)) {
    target = mean(x)
    targetmissing = FALSE
  }
  else targetmissing = TRUE
  if (missing(ref.interval))
    ref.interval = pnorm(3) - pnorm(-3)
  sd = sd(x)
  mean = mean(x)
  ref.ar = qnorm(ref.interval, mean, sd) - qnorm(1 - ref.interval,
                                                 mean, sd)
  if (missing(facCg))
    facCg = 0.2
  if (missing(facCgk))
    facCgk = 0.1
  if (missing(tolerance))
    warning("Missing tolerance! The specification limits are choosen to get Cg = 1")
  if (missing(tolerance)) {
    width = ref.ar/facCg
    tolerance = numeric(2)
    tolerance[1] = mean(x) - width/2
    tolerance[2] = mean(x) + width/2
  }
  quant1 = qnorm((1 - ref.interval)/2, mean, sd)
  quant2 = qnorm(ref.interval + (1 - ref.interval)/2, mean,
                 sd)
  if (length(tolerance) != 2)
    stop("tolerance has wrong length")
  if (missing(col))
    col = "lightblue"
  if (missing(xlim))
    xlim = c(0, length(x))
  if (missing(ylim))
    ylim = c(min(x, target - n/2 * (abs(diff(tolerance))),
                 quant1, quant2), max(x, target + n/2 * (abs(diff(tolerance))),
                                      quant1, quant2))
  if (missing(main))
    main = paste("Histogram of", deparse(substitute(x)),
                 "- target")

  Cg <- (facCg * tolerance[2]-tolerance[1])/ref.interval
  Cgk <- (facCgk * abs(target-mean(x))/(ref.interval/2))

  # Calculos previos
  x.c <- x - target
  temp <- hist(x.c, plot = FALSE)
  # Obtenemos la información para el histograma
  df <- data.frame(
    mid = temp$mids,
    density = temp$density
  )
  width <- diff(df$mid)[1] # Ancho de cada barra
  # Histograma
  p <- ggplot(df, aes(x = mid, y = density)) +
    geom_bar(stat = "identity", width = width, fill = "lightblue", color = "black", alpha = 0.5) +
    labs(y = "Density", x = "x.c", title = main) +
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
    guides(color = guide_legend(title.position = "top", title.hjust = 0.5))
  # Linea x=0
  p <- p + geom_vline(xintercept = 0, color = "red")
  # Intervalos de Confianza - Azules
  test = t.test(x.c, mu = 0, conf.level = conf.level)
  p <- p + geom_vline(aes(xintercept = test$conf.int[1], color = "Confidence interval"), linetype = "dashed", col = "blue") +
    geom_vline(aes(xintercept = test$conf.int[2], color = "Confidence interval"), linetype = "dashed", col = "blue") +
    theme(legend.position = "none")
  # Curva de densidad
  p <- p + geom_line(data = data.frame(x = density(x.c)$x, y = density(x.c)$y), aes(x = x, y = y), color = "black", linewidth = 0.5)

  # Hipotesis, P_val y t_val
  caja <- ggplot(data = data.frame(x = 0, y = 0), aes(x, y)) +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.border = element_blank()
    ) +
    xlim(c(0.25,0.26)) + ylim(c(0.24, 0.36))

  caja <- caja +
    annotate('text', x = 0.25, y = 0.35,
             label = paste("H[0]==", "0"),
             parse = TRUE, size = 3, hjust = 0) +
    annotate('text', x = 0.25, y = 0.30,
             label = paste("t-value: ", round(test$statistic, digits = 3)),
             parse = TRUE, size = 3, hjust = 0) +
    annotate('text', x = 0.25, y = 0.25,
             label = paste("p-value: ", round(test$p.value, 3)),
             parse = TRUE, size = 3, hjust = 0)

  p <- p + inset_element(caja, left = 0, right = 0.35, top = 1, bottom = 0.65)

  # Añadir label del Cg y Cgk
  if (cgOut == TRUE) {
    caja <- ggplot(data = data.frame(x = 0, y = 0), aes(x, y)) +
      theme_bw() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_blank()
      ) +
      xlim(c(0.25,0.26)) + ylim(c(0.24, 0.31))

    caja <- caja +
      annotate('text', x = 0.25, y = 0.30,
               label = paste("Cg: ", round(Cg,digits = 6)),
               parse = TRUE, size = 3, hjust = 0) +
      annotate('text', x = 0.25, y = 0.25,
               label = paste("Cgk:", round(Cgk,digits = 6)),
               parse = TRUE, size = 3, hjust = 0)

    p <- p + inset_element(caja, left = 0.7, right = 1, top = 1, bottom = 0.75)
    show(p)
  }
  else{
    caja <- ggplot(data = data.frame(x = 0, y = 0), aes(x, y)) +
      theme_bw() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_blank()
      ) +
      xlim(c(0.25,0.26)) + ylim(c(0.24, 0.31))

    caja <- caja +
      annotate('text', x = 0.25, y = 0.30,
               label = "----- conf. int", size = 3, hjust = 0, colour = "blue")

    p <- p + inset_element(caja, left = 0.75, right = 1, top = 1, bottom = 0.75)
    show(p)
  }
  invisible(list(Cg, Cgk))
}

# cg_ToleranceChart ----
cg_ToleranceChart <- function (x, target, tolerance, ref.interval, facCg, facCgk,
                               n = 0.2, col, pch, xlim, ylim, main, conf.level = 0.95,
                               cgOut = TRUE){
  #' @title cg_ToleranceChart
  #' @description Function visualize the given values of measurement in a Tolerance View.
  #' @param x A vector containing the measured values.
  #' @param target A numeric value giving the expected target value for the x-values.
  #' @param tolerance Vector of length 2 giving the lower and upper specification limits.
  #' @param ref.interval Numeric value giving the confidence interval on which the calculation is based. By default it is based on 6 sigma methodology.
  #' Regarding the normal distribution this relates to \code{pnorm(3) - pnorm(-3)} which is exactly 99.73002 percent. If the calculation is based on another sigma value \code{ref.interval} needs to be adjusted.
  #' To give an example: If the sigma-level is given by 5.15 the \code{ref.interval} relates to \code{pnorm(5.15/2)-pnorm(-5.15/2)} which is exactly 0.989976 percent.
  #' @param facCg Numeric value as a factor for the calculation of the gage potential index. The default value for \code{facCg} is \code{0.2}.
  #' @param facCgk Numeric value as a factor for the calculation of the gage capability index. The default value for \code{facCgk} is \code{0.1}.
  #' @param n Numeric value between \code{0} and \code{1} giving the percentage of the tolerance field (values between the upper and lower specification limits given by \code{tolerance}) where the values of \code{x} should be positioned. Limit lines will be drawn. Default value is \code{0.2}.
  #' @param col Character or numeric value specifying the color of the line and points in the tolerance view. Default is \code{`black`}.
  #' @param pch Numeric or character specifying the plotting symbol. Default is \code{19} (filled circle).
  #' @param xlim Numeric vector of length 2 specifying the limits for the x-axis. Default is \code{NULL} which means the limits are set automatically.
  #' @param ylim Numeric vector of length 2 specifying the limits for the y-axis. Default is \code{NULL} which means the limits are set automatically.
  #' @param main Character string specifying the title of the plot. Default is \code{`Tolerance View`}.
  #' @param conf.level Confidence level for internal \code{t.test} checking the significance of the bias between \code{target} and mean of \code{x}. The default value is \code{0.95}.
  #' @param cgOut Logical value deciding whether the \code{Cg} and \code{Cgk} values should be plotted in a legend. Default is \code{TRUE}.
  #' @details The calculation of the potential and actual gage capability are based on the following formulae:
  #' \itemize{
  #' \item{\code{Cg = (facCg * tolerance[2]-tolerance[1])/ref.interval}}
  #' \item{\code{Cgk = (facCgk * abs(target-mean(x))/(ref.interval/2)}}
  #' }
  #' If the usage of the historical process variation is preferred the values for the tolerance \code{tolerance} must be adjusted manually. That means in case of the 6 sigma methodology for example, that \code{tolerance = 6 * sigma[process]}.
  #' @return The function \code{cg_ToleranceChart} returns a list of numeric values. The first element contains the calculated centralized gage potential index \code{Cg} and the second contains the non-centralized gage capability index \code{Cgk}.
  #' @seealso \code{\link{cg_RunChart}}, \code{\link{cg_HistChart}},  \code{\link{cg}}
  #' @examples
  #'
  #' x <- c(9.991, 10.013, 10.001, 10.007, 10.010, 10.013, 10.008,9.992,
  #'        10.017, 10.005, 10.005, 10.002, 10.017, 10.005, 10.002, 9.996,
  #'        10.011, 10.009, 10.006, 10.008, 10.003, 10.002, 10.006, 10.010, 10.013)
  #'
  #' cg_ToleranceChart(x = x, target = 10.003, tolerance = c(9.903, 10.103))

  if (missing(x))
    stop("x must be given as a vector")
  if (missing(target)) {
    target = mean(x)
    targetmissing = FALSE
  }
  else targetmissing = TRUE
  if (missing(ref.interval))
    ref.interval = pnorm(3) - pnorm(-3)
  sd = sd(x)
  mean = mean(x)
  ref.ar = qnorm(ref.interval, mean, sd) - qnorm(1 - ref.interval,
                                                 mean, sd)
  if (missing(facCg))
    facCg = 0.2
  if (missing(facCgk))
    facCgk = 0.1
  if (missing(tolerance))
    warning("Missing tolerance! The specification limits are choosen to get Cg = 1")
  if (missing(tolerance)) {
    width = ref.ar/facCg
    tolerance = numeric(2)
    tolerance[1] = mean(x) - width/2
    tolerance[2] = mean(x) + width/2
  }
  quant1 = qnorm((1 - ref.interval)/2, mean, sd)
  quant2 = qnorm(ref.interval + (1 - ref.interval)/2, mean, sd)
  if (length(tolerance) != 2)
    stop("tolerance has wrong length")
  if (missing(col))
    col = 1
  if (missing(pch))
    pch = 19
  if (missing(xlim))
    xlim = c(0, length(x))
  if (missing(ylim))
    ylim = c(min(x, target - n/2 * (abs(diff(tolerance))),
                 quant1, quant2), max(x, target + n/2 * (abs(diff(tolerance))),
                                      quant1, quant2))
  if (missing(main))
    main = "Tolerance View"
  Cg <- (facCg * tolerance[2]-tolerance[1])/ref.interval
  Cgk <- (facCgk * abs(target-mean(x))/(ref.interval/2))

  # Gráfica
  p <- ggplot(data.frame(x = 1:length(x), y = x), aes(x = x, y = y)) +
    geom_point(color = col, shape = pch) +
    geom_line(color = col, linetype = "solid")+
    geom_hline(aes(yintercept = target), linetype = "dashed", color = "red") +
    geom_hline(aes(yintercept = tolerance[1]), linetype = "dashed", color = "blue") +
    geom_hline(aes(yintercept = tolerance[2]), linetype = "dashed", color = "blue") +
    geom_hline(aes(yintercept = (target + n/2 * (tolerance[2] - tolerance[1]))), color = "black") +
    geom_hline(aes(yintercept = (target - n/2 * (tolerance[2] - tolerance[1]))), color = "black") +
    scale_color_manual(values = c("Data" = col)) +
    labs(x = "", y = "x", color = "Variable", title = main)+
    theme_bw()+theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
    theme(legend.position = "none")

  if (cgOut == TRUE) {
    caja <- ggplot(data = data.frame(x = 0, y = 0), aes(x, y)) +
      theme_bw() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_blank()
      ) +
      xlim(c(0.25,0.26)) + ylim(c(0.24, 0.31))

    caja <- caja +
      annotate('text', x = 0.25, y = 0.30,
               label = paste("Cg: ", round(Cg,digits = 6)),
               parse = TRUE, size = 3, hjust = 0) +
      annotate('text', x = 0.25, y = 0.25,
               label = paste("Cgk:", round(Cgk,digits = 6)),
               parse = TRUE, size = 3, hjust = 0)

    p <- p + inset_element(caja, left = 0.7, right = 1, top = 1, bottom = 0.75)
    show(p)
  }
  else{
    show(p)
  }
  invisible(list(Cg, Cgk))
}

# cg ----
cg <- function (x, target, tolerance, ref.interval, facCg, facCgk, n = 0.2,
                col, pch, xlim, ylim, conf.level = 0.95){
  #' @title cg: Function to calculate and visualize the gage capability.
  #' @description Function visualize the given values of measurement in a run chart and in a histogram. Furthermore the \code{centralized Gage potential index} \code{Cg} and the \code{non-centralized Gage Capability index} \code{Cgk} are calculated and displayed.
  #' @param x A vector containing the measured values.
  #' @param target A numeric value giving the expected target value for the x-values.
  #' @param tolerance Vector of length 2 giving the lower and upper specification limits.
  #' @param ref.interval Numeric value giving the confidence interval on which the calculation is based. By default it is based on 6 sigma methodology.
  #' Regarding the normal distribution this relates to \code{pnorm(3) - pnorm(-3)} which is exactly 99.73002 percent. If the calculation is based on another sigma value \code{ref.interval} needs to be adjusted.
  #' To give an example: If the sigma-level is given by 5.15 the \code{ref.interval} relates to \code{pnorm(5.15/2)-pnorm(-5.15/2)} which is exactly 0.989976 percent.
  #' @param facCg Numeric value as a factor for the calculation of the gage potential index. The default value for \code{facCg} is \code{0.2}.
  #' @param facCgk Numeric value as a factor for the calculation of the gage capability index. The default value for \code{facCgk} is \code{0.1}.
  #' @param n Numeric value between \code{0} and \code{1} giving the percentage of the tolerance field (values between the upper and lower specification limits given by \code{tolerance}) where the values of \code{x} should be positioned. Limit lines will be drawn. Default value is \code{0.2}.
  #' @param col Character or numeric value specifying the color of the curve in the run chart. Default is \code{`black`}.
  #' @param pch Numeric or character specifying the plotting symbol. Default is \code{19} (filled circle).
  #' @param xlim Numeric vector of length 2 specifying the limits for the x-axis. Default is \code{NULL} which means the limits are set automatically.
  #' @param ylim Numeric vector of length 2 specifying the limits for the y-axis. Default is \code{NULL} which means the limits are set automatically.
  #' @param conf.level Confidence level for internal \code{t.test} checking the significance of the bias between \code{target} and mean of \code{x}. The default value is \code{0.95}. The result of the \code{t.test} is shown in the histogram on the left side.
  #' @details The calculation of the potential and actual gage capability are based on the following formula:
  #' \itemize{
  #' \item{\code{Cg = (facCg * tolerance[2]-tolerance[1])/ref.interval}}
  #' \item{\code{Cgk = (facCgk * abs(target-mean(x))/(ref.interval/2)}}
  #' }
  #' If the usage of the historical process variation is preferred the values for the tolerance \code{tolerance} must be adjusted manually. That means in case of the 6 sigma methodology for example, that \code{tolerance = 6 * sigma[process]}.
  #' @return The function \code{cg} returns a list of numeric values. The first element contains the calculated centralized gage potential index \code{Cg} and the second contains the non-centralized gage capability index \code{Cgk}.
  #' @seealso \code{\link{cg_RunChart}}, \code{\link{cg_HistChart}}, \code{\link{cg_ToleranceChart}}.
  #' @examples
  #'
  #' x <- c(9.991, 10.013, 10.001, 10.007, 10.010, 10.013, 10.008,9.992,
  #'        10.017, 10.005, 10.005, 10.002, 10.017, 10.005, 10.002, 9.996,
  #'        10.011, 10.009, 10.006, 10.008, 10.003, 10.002, 10.006, 10.010, 10.013)
  #'
  #' cg(x = x, target = 10.003, tolerance = c(9.903, 10.103))

  old.par <- par(no.readonly = TRUE)
  if (missing(x))
    stop("x must be given as a vector")
  if (missing(target)) {
    target = mean(x)
    targetmissing = FALSE
  }
  else
    targetmissing = TRUE
  if (missing(ref.interval))
    ref.interval = pnorm(3) - pnorm(-3)
  sd = sd(x)
  mean = mean(x)
  ref.ar = qnorm(ref.interval, mean, sd) - qnorm(1 - ref.interval,
                                                 mean, sd)
  if (missing(facCg))
    facCg = 0.2
  if (missing(facCgk))
    facCgk = 0.1
  if (missing(tolerance))
    warning("Missing tolerance! The specification limits are choosen to get Cg = 1")
  if (missing(tolerance)) {
    width = ref.ar / facCg
    tolerance = numeric(2)
    tolerance[1] = mean(x) - width / 2
    tolerance[2] = mean(x) + width / 2
  }
  quant1 = qnorm((1 - ref.interval) / 2, mean, sd)
  quant2 = qnorm(ref.interval + (1 - ref.interval) / 2, mean,
                 sd)
  if (length(tolerance) != 2)
    stop("tolerance has wrong length")
  if (missing(col))
    col = 1
  if (missing(pch))
    pch = 19
  if (missing(xlim))
    xlim = c(0, length(x))
  if (missing(ylim))
    ylim = c(min(x, target - n / 2 * (abs(diff(
      tolerance
    ))),
    quant1, quant2),
    max(x, target + n / 2 * (abs(diff(
      tolerance
    ))),
    quant1, quant2))
  Cg = .cg(x, target, tolerance, ref.interval, facCg, facCgk)[[1]]
  Cgk = .cg(x, target, tolerance, ref.interval, facCg, facCgk)[[2]]

  # Plots

  # RunChart
  df1 <- data.frame(x = x, y = x)
  df1$y_target <- target
  df1$y_lower <- quant1
  df1$y_upper <- quant2
  df1$y_tolerance_lower <- tolerance[1]
  df1$y_tolerance_upper <- tolerance[2]
  df1$y_mean <- mean
  df1$Cg <- Cg
  df1$Cgk <- Cgk
  p1 <- ggplot(df1, aes(x = seq_along(x), y = x)) +
    geom_point(color = col, shape = pch) +
    geom_line(color = col, linetype = "solid") +
    scale_x_continuous(limits = c(xlim[1] - 0.05 * xlim[2], xlim[2]),
                       expand = c(0, 0)) +
    labs(title = "Run Chart", x = "Index", y = "x") +
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    geom_hline(aes(yintercept = target)) +
    geom_smooth(
      method = "loess",
      color = "red",
      se = FALSE,
      span = 1.25,
      size = 0.25
    ) +
    geom_hline(aes(yintercept = mean),
               linetype = "dashed",
               color = "seagreen") +  #center line
    geom_hline(aes(yintercept = quant1),
               linetype = "dashed",
               color = "seagreen") + # Bottom line
    geom_hline(aes(yintercept = quant2),
               linetype = "dashed",
               color = "seagreen") +
    geom_hline(
      yintercept = c(target + n / 2 * (abs(diff(
        tolerance
      ))), target - n / 2 * (abs(diff(
        tolerance
      )))),
      color = "#012B78",
      linetype = "solid"
    ) +
    scale_y_continuous(
      limits = ylim,
      expand = c(0, 0),
      sec.axis =
        sec_axis(
          ~ .,
          breaks = c(
            target,
            mean,
            quant1,
            quant2,
            target + n / 2 * (abs(diff(tolerance))),
            target - n / 2 * (abs(diff(tolerance)))
          ),
          labels = c(
            "target",
            expression(bar(x)),
            substitute(x[a * b], list(a = round(((1 - ref.interval) / 2
            ) * 100, 3), b = "%")),
            substitute(x[a * b], list(a = round(((ref.interval + (1 - ref.interval) /
                                                    2)
            ) * 100,
            3), b = "%")),
            substitute(x[tar] + a, list(a = round(n / 2, 4))),
            substitute(x[tar] - a, list(a = round(n / 2, 4)))
          )
        )
    ) + theme(axis.text.y.right = element_text(size = 15))

  # HistChart
  x.c <- x - target
  temp <- hist(x.c, plot = FALSE)
  df3 <- data.frame(mid = temp$mids,
                    density = temp$density)
  width <- diff(df3$mid)[1] # Ancho de cada barra
  p3 <- ggplot(df3, aes(x = mid, y = density)) +
    geom_bar(
      stat = "identity",
      width = width,
      fill = "lightblue",
      color = "black",
      alpha = 0.5
    ) +
    labs(y = "Density", x = "x.c", title = paste("Histogram of",deparse(substitute(x)),"- target")) +
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
    geom_vline(xintercept = 0, color = "red")

  test = t.test(x.c, mu = 0, conf.level = conf.level)
  p3 <-
    p3 + geom_vline(
      aes(xintercept = test$conf.int[1], color = "Confidence interval"),
      linetype = "dashed",
      col = "blue"
    ) +
    geom_vline(
      aes(xintercept = test$conf.int[2], color = "Confidence interval"),
      linetype = "dashed",
      col = "blue"
    ) +
    theme(legend.position = "none") +
    geom_line(
      data = data.frame(x = density(x.c)$x, y = density(x.c)$y),
      aes(x = x, y = y),
      color = "black",
      linewidth = 0.5
    )

  p3 <-
    p3 + annotation_custom(grob = grid::textGrob(
      label = c(expression(paste(H[0], " : Bias = 0"))),
      x = unit(0.05, "npc") + unit(0.05, "cm"),
      y = unit(1, "npc") - unit(0.05, "cm"),
      just = c("left", "top"),
      gp = grid::gpar(fontsize = 6, fontface = "bold")
    )) +
    annotation_custom(grob = grid::textGrob(
      label = c(paste("t-value: ", round(test$statistic, 3))),
      x = unit(0.05, "npc") + unit(0.05, "cm"),
      y = unit(1, "npc") - unit(0.4, "cm"),
      just = c("left", "top"),
      gp = grid::gpar(fontsize = 6)
    )) +
    annotation_custom(grob = grid::textGrob(
      label = c(paste("p-value: ", round(test$p.value, 3))),
      x = unit(0.05, "npc") + unit(0.05, "cm"),
      y = unit(1, "npc") - unit(0.65, "cm"),
      just = c("left", "top"),
      gp = grid::gpar(fontsize = 6)
    ))

  # Tolerance View
  p4 <-
    ggplot(data.frame(x = 1:length(x), y = x), aes(x = x, y = y)) +
    geom_point(color = col, shape = pch) +
    geom_line(color = col, linetype = "solid") +
    geom_hline(aes(yintercept = target),
               linetype = "dashed",
               color = "red") +
    geom_hline(aes(yintercept = tolerance[1]),
               linetype = "dashed",
               color = "blue") +
    geom_hline(aes(yintercept = tolerance[2]),
               linetype = "dashed",
               color = "blue") +
    geom_hline(aes(yintercept = (target + n / 2 * (
      tolerance[2] - tolerance[1]
    ))), color = "black") +
    geom_hline(aes(yintercept = (target - n / 2 * (
      tolerance[2] - tolerance[1]
    ))), color = "black") +
    scale_color_manual(values = c("Data" = col)) +
    labs(
      x = "",
      y = "x",
      color = "Variable",
      title = "Tolerance View"
    ) +
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(legend.position = "none")

  # Data Box
  p2 <- ggplot(data = data.frame(x = 0, y = 0), aes(x, y)) +
    theme_bw() +
    annotate(
      'text',
      x = 0.25,
      y = 0.40,
      label = paste("bar(x)==", round(mean, 2)),
      parse = TRUE,
      size = 3,
      hjust = 0
    ) +
    annotate(
      'text',
      x = 0.25,
      y = 0.35,
      label = paste("s==", round(sd, 2)),
      parse = TRUE,
      size = 3,
      hjust = 0
    ) +
    annotate(
      'text',
      x = 0.25,
      y = 0.3,
      label = paste("target==", round(target, 5)),
      parse = TRUE,
      size = 3,
      hjust = 0
    ) +
    annotate(
      'text',
      x = 0.25,
      y = 0.25,
      label = paste("C[g]==", round(Cg, 2)),
      parse = TRUE,
      size = 3,
      hjust = 0
    ) +
    annotate(
      'text',
      x = 0.25,
      y = 0.20,
      label = paste("C[gk]==", round(Cgk, 2)),
      parse = TRUE,
      size = 3,
      hjust = 0
    ) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    xlim(c(0.15, 0.5)) + ylim(c(0.1, 0.5))

  design <- "
  112
  113
  114
  "
  p <- p1 + p2 + p3 + p4 + plot_layout(design = design)

  suppressMessages(show(p))

  invisible(list(Cg, Cgk))
}

# print_adtest ----
print_adtest <- function(x, digits = 4, quote = TRUE, prefix = "", ...) {
  #' @title print_adtest: Test Statistics
  #' @description Function to show \code{adtest}.
  #' @usage print_adtest(x, digits = 4, quote = TRUE, prefix = "", ...)
  #' @param x Needs to be an object of class \code{adtest}.
  #' @param digits Minimal number of significant digits.
  #' @param quote Logical, indicating whether or not strings should be printed with surrounding quotes.
  #' By default \code{quote} is set to \code{TRUE}.
  #' @param prefix Single character or character string that will be printed in front of \code{x}.
  #' @param ... Further arguments passed to or from other methods.
  #' @return The function returns a summary of Anderson Darling Test
  #' @examples
  #' data <- rnorm(20, mean = 20)
  #' pcr1<-pcr(data, "normal", lsl = 17, usl = 23, plot = FALSE)
  #' print_adtest(pcr1$adTest)


  cat(strwrap(x$method, prefix = "\t"), sep = "\n")
  cat("\n")
  cat("data: ", x$data.name, "\n")
  out <- character()
  if (!is.null(x$statistic))
    out <- c(out, paste(names(x$statistic), "=", format(round(x$statistic[[1]], 4))))
  if (!is.null(x$parameter))
    out <- c(out, paste(names(x$parameter), "=", format(round(x$parameter, 3))))
  if (!is.null(x$p.value)) {
    fp <- format.pval(x$p.value[[1]], digits = digits)
    if (x$tableValue) {
      if (x$smaller)
        out <- c(out, paste("p-value", if (substr(fp, 1L, 1L) == "<") fp else paste("<=", fp)))
      else out <- c(out, paste("p-value", if (substr(fp, 1L, 1L) == "=") fp else paste(">", fp)))
    }
    else {
      out <- c(out, paste("p-value", if (substr(fp, 1L, 1L) == "<") fp else paste("=", fp)))
    }
  }
  cat(strwrap(paste(out, collapse = ", ")), sep = "\n")
  cat("alternative hypothesis: ")
  if (!is.null(x$null.value)) {
    if (length(x$null.value) == 1) {
      cat("true", names(x$null.value), "is not equal to", x$null.value, "\n")
    }
    else {
      message(x$alternative, "\nnull values:\n")
      message(x$null.value, ...)
    }
  }
  if (!is.null(x$conf.int)) {
    cat(format(100 * attr(x$conf.int, "conf.level")), "percent confidence interval:\n", format(c(x$conf.int[1L], x$conf.int[2L])), "\n")
  }
  if (!is.null(x$estimate)) {
    cat("sample estimates:\n")
    print(x$estimate, ...)
  }
  invisible(x)
}

# pcr ----
pcr <- function (x, distribution = "normal", lsl, usl, target, boxcox = FALSE,
                 lambda = c(-5, 5), main, xlim, grouping = NULL, std.dev = NULL,
                 conf.level = 0.9973002, bounds.lty = 3, bounds.col = "red",
                 col.fill = "lightblue", col.border = "black",
                 col.curve = "red", plot = TRUE, ADtest = TRUE){
  #' @title pcr: Process Capability Indices
  #' @description Calculates the process capability \code{cp}, \code{cpk}, \code{cpkL} (onesided) and \code{cpkU} (onesided) for a given dataset and distribution.
  #' A histogram with a density curve is displayed along with the specification limits and a Quantile-Quantile Plot for the specified distribution.
  #' Lower-, upper and total fraction of nonconforming entities are calculated. Box-Cox Transformations are supported as well as the calculation of Anderson Darling Test Statistics.
  #' @param x Numeric vector containing the values for which the process capability should be calculated.
  #' @param distribution Character string specifying the distribution of \code{x}. The function \code{cp} will accept the following character strings for \code{distribution}:
  #'   \itemize{
  #'     \item \code{`normal`}
  #'     \item \code{`log-normal`}
  #'     \item \code{`exponential`}
  #'     \item \code{`logistic`}
  #'     \item \code{`gamma`}
  #'     \item \code{`weibull`}
  #'     \item \code{`cauchy`}
  #'     \item \code{`gamma3`}
  #'     \item \code{`weibull3`}
  #'     \item \code{`lognormal3`}
  #'     \item \code{`beta`}
  #'     \item \code{`f`}
  #'     \item \code{`geometric`}
  #'     \item \code{`poisson`}
  #'     \item \code{`negative-binomial`}
  #'   }
  #' By default \code{distribution} is set to \code{`normal`}.
  #' @param lsl A numeric value specifying the lower specification limit.
  #' @param usl A numeric value specifying the upper specification limit.
  #' @param target (Optional) numeric value giving the target value.
  #' @param boxcox Logical value specifying whether a Box-Cox transformation should be performed or not. By default \code{boxcox} is set to \code{FALSE}.
  #' @param lambda (Optional) lambda for the transformation, default is to have the function estimate lambda.
  #' @param main A character string specifying the main title of the plot.
  #' @param xlim A numeric vector of length 2 specifying the x-axis limits for the plot.
  #' @param grouping (Optional) If grouping is given the standard deviation is calculated as mean standard deviation of the specified subgroups corrected by the factor \code{c4} and expected fraction of nonconforming is calculated using this standard deviation.
  #' @param std.dev An optional numeric value specifying the historical standard deviation (only provided for normal distribution). If \code{NULL}, the standard deviation is calculated from the data.
  #' @param conf.level Numeric value between \code{0} and \code{1} giving the confidence interval.
  #' By default \code{conf.level} is \code{0.9973} (99.73\%) which is the reference interval bounded by the 99.865\% and 0.135\% quantile.
  #' @param bounds.lty graphical parameter. For further details see \code{ppPlot} or \code{qqPlot}.
  #' @param bounds.col A character string specifying the color of the capability bounds. Default is "red".
  #' @param col.fill A character string specifying the fill color for the histogram plot. Default is "lightblue".
  #' @param col.border A character string specifying the border color for the histogram plot. Default is "black".
  #' @param col.curve A character string specifying the color of the fitted distribution curve. Default is "red".
  #' @param plot A logical value indicating whether to generate a plot. Default is \code{TRUE}.
  #' @param ADtest A logical value indicating whether to print the Anderson-Darling. Default is \code{TRUE}.
  #' @details
  #' Distribution fitting is delegated to the function \code{FitDistr} from this package, as well as the calculation of lambda for the Box-Cox Transformation. p-values for the Anderson-Darling Test are reported for the most important distributions.
  #'
  #' The process capability indices are calculated as follows:
  #' \itemize{
  #'   \item \strong{cpk}: minimum of \code{cpK} and \code{cpL}.
  #'   \item \strong{pt}: total fraction nonconforming.
  #'   \item \strong{pu}: upper fraction nonconforming.
  #'   \item \strong{pl}: lower fraction nonconforming.
  #'   \item \strong{cp}: process capability index.
  #'   \item \strong{cpkL}: lower process capability index.
  #'   \item \strong{cpkU}: upper process capability index.
  #'   \item \strong{cpk}: minimum process capability index.
  #' }
  #'
  #' For a Box-Cox transformation, a data vector with positive values is needed to estimate an optimal value of lambda for the Box-Cox power transformation of the values. The Box-Cox power transformation is used to bring the distribution of the data vector closer to normality. Estimation of the optimal lambda is delegated to the function \code{boxcox} from the \code{MASS} package. The Box-Cox transformation has the form \eqn{y(\lambda) = \frac{y^\lambda - 1}{\lambda}} for \eqn{\lambda \neq 0}, and \eqn{y(\lambda) = \log(y)} for \eqn{\lambda = 0}. The function \code{boxcox} computes the profile log-likelihoods for a range of values of the parameter lambda. The function \code{boxcox.lambda} returns the value of lambda with the maximum profile log-likelihood.
  #'
  #' In case no specification limits are given, \code{lsl} and \code{usl} are calculated to support a process capability index of 1.
  #' @return The function returns a list with the following components:
  #'
  #' The function \code{pcr} returns a list with \code{lambda}, \code{cp}, \code{cpl}, \code{cpu}, \code{ppt}, \code{ppl}, \code{ppu}, \code{A}, \code{usl}, \code{lsl}, \code{target}, \code{asTest}, \code{plot}.
  #' @examples
  #' set.seed(1234)
  #' data <- rnorm(20, mean = 20)
  #' pcr(data, "normal", lsl = 17, usl = 23)
  #'
  #' set.seed(1234)
  #' weib <- rweibull(20, shape = 2, scale = 8)
  #' pcr(weib, "weibull", usl = 20)


  data.name = deparse(substitute(x))[1]

  parList = list()
  if (is.null(parList[["col"]]))
    parList$col = "lightblue"
  if (is.null(parList[["border"]]))
    parList$border = 1
  if (is.null(parList[["lwd"]]))
    parList$lwd = 1
  if (is.null(parList[["cex.axis"]]))
    parList$cex.axis = 1.5
  if (missing(lsl))
    lsl = NULL
  if (missing(usl))
    usl = NULL
  if (missing(target))
    target = NULL
  if (missing(lambda))
    lambda = c(-5, 5)
  if (!is.numeric(lambda))
    stop("lambda needs to be numeric")
  if (any(x < 0) && any(distribution == c("beta", "chi-squared",
                                          "exponential", "f", "geometric", "lognormal", "log-normal",
                                          "negative binomial", "poisson", "weibull", "gamma")))
    stop("choosen distribution needs all values in x to be > 0!")
  if (any(x > 1) && distribution == "beta")
    stop("choosen distribution needs all values in x to be between 0 and 1!")

  paramsList = vector(mode = "list", length = 0)
  estimates = vector(mode = "list", length = 0)
  varName = deparse(substitute(x))
  dFun = NULL
  pFun = NULL
  qFun = NULL
  cp = NULL
  cpu = NULL
  cpl = NULL
  cpk = NULL
  ppt = NULL
  ppl = NULL
  ppu = NULL
  xVec = numeric(0)
  yVec = numeric(0)

  if (is.vector(x))
    x = as.data.frame(x)
  any3distr = FALSE
  not3distr = FALSE

  if (distribution == "weibull3" || distribution == "lognormal3" ||
      distribution == "gamma3")
    any3distr = TRUE
  if (distribution != "weibull3" && distribution != "lognormal3" &&
      distribution != "gamma3")
    not3distr = TRUE
  if (boxcox) {
    distribution = "normal"
    if (length(lambda) >= 2) {
      temp = boxcox(x[, 1] ~ 1, lambda = seq(min(lambda),max(lambda), 1/10), plotit = FALSE)
      i = order(temp$y, decreasing = TRUE)[1]
      lambda = temp$x[i]
    }
    x = as.data.frame(x[, 1]^lambda)
  }
  numObs = nrow(x)
  if (!is.null(grouping))
    if (is.vector(grouping))
      grouping = as.data.frame(grouping)
  center = colMeans(x)
  if (!is.null(x) & !is.null(grouping)) {
    if (nrow(x) != nrow(grouping))
      stop(paste("length of ", deparse(substitute(grouping)),
                 " differs from length of ", varName))
  }
  if (missing(main))
    if (boxcox)
      main = paste("Process Capability using box cox transformation for",varName)
  else main = paste("Process Capability using", as.character(distribution),
                    "distribution for", varName)
  if (is.null(std.dev)) {
    if (is.null(grouping))
      std.dev = .sdSg(x)
    else std.dev = .sdSg(x, grouping)
  }
  if (conf.level < 0 | conf.level > 1)
    stop("conf.level must be a value between 0 and 1")
  confHigh = conf.level + (1 - conf.level)/2
  confLow = 1 - conf.level - (1 - conf.level)/2
  distWhichNeedParameters = c("weibull", "logistic", "gamma",
                              "exponential", "f", "geometric", "chi-squared", "negative binomial",
                              "poisson")
  if (is.character(distribution)) {
    dis = distribution
    if (identical(distribution, "weibull3"))
      dis = "weibull3"
    if (identical(distribution, "gamma3"))
      dis = "gamma3"
    if (identical(distribution, "lognormal3"))
      dis = "lognormal3"
    qFun = .charToDistFunc(dis, type = "q")
    pFun = .charToDistFunc(dis, type = "p")
    dFun = .charToDistFunc(dis, type = "d")
    if (is.null(qFun) & is.null(pFun) & is.null(dFun))
      stop(paste(deparse(substitute(y)), "distribution could not be found!"))
  }
  if (TRUE) {
    fitList = vector(mode = "list", length = 0)
    fitList$x = x[, 1]
    fitList$densfun = dis
    if (not3distr) {
      fittedDistr = do.call(FitDistr, fitList)
      estimates = as.list(fittedDistr$estimate)
      paramsList = estimates
    }
    if (distribution == "weibull3") {
      paramsList = .weibull3(x[, 1])
      estimates = paramsList
    }
    if (distribution == "lognormal3") {
      paramsList = .lognormal3(x[, 1])
      estimates = paramsList
    }
    if (distribution == "gamma3") {
      paramsList = .gamma3(x[, 1])
      estimates = paramsList
    }

  }
  paramsList = c(paramsList, .lfkp(parList, formals(qFun)))
  if (distribution == "normal") {
    paramsList$mean = center
    paramsList$sd = std.dev
    estimates = paramsList
  }
  if (boxcox) {
    if (!is.null(lsl))
      lsl = lsl^lambda
    if (!is.null(usl))
      usl = usl^lambda
    if (!is.null(target))
      target = target^lambda
  }
  if (is.null(lsl) && is.null(usl)) {
    paramsList$p = confLow
    lsl = do.call(qFun, paramsList)
    paramsList$p = confHigh
    usl = do.call(qFun, paramsList)
  }

  if (identical(lsl, usl))
    stop("lsl == usl")
  if (!is.null(lsl) && !is.null(target) && target < lsl)
    stop("target is less than lower specification limit")
  if (!is.null(usl) && !is.null(target) && target > usl)
    stop("target is greater than upper specification limit")
  if (!is.null(lsl) && !is.null(usl))
    if (lsl > usl) {
      temp = lsl
      lsl = usl
      usl = temp
    }
  paramsList$p = c(confLow, 0.5, confHigh)
  paramsListTemp = .lfkp(paramsList, formals(qFun))
  qs = do.call(qFun, paramsListTemp)
  paramsListTemp = .lfkp(paramsList, formals(pFun))
  if (!is.null(lsl) && !is.null(usl))
    cp = (usl - lsl)/(qs[3] - qs[1])
  if (!is.null(usl)) {
    cpu = (usl - qs[2])/(qs[3] - qs[2])
    paramsListTemp$q = usl
    ppu = 1 - do.call(pFun, paramsListTemp)
  }
  if (!is.null(lsl)) {
    cpl = (qs[2] - lsl)/(qs[2] - qs[1])
    paramsListTemp$q = lsl
    ppl = do.call(pFun, paramsListTemp)
  }
  cpk = min(cpu, cpl)
  ppt = sum(ppl, ppu)

  # PLOT ------------------
  {
    # ----------------------------- IF PLOT == TRUE -----------------------------------------------------------
    if (missing(xlim)) {
      xlim <- range(x[, 1], usl, lsl)
      xlim <- xlim + diff(xlim) * c(-0.2, 0.2)
    }

    if (boxcox){
      binwidth.b = NULL
    }
    else{
      binwidth.b = 1
    }

    # 1. Histograma --------------------------------------------------------------------------

    # Histograma
    p1 <- ggplot(data.frame(x = x[, 1]), aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)),
                     binwidth = binwidth.b,  # Ajusta el ancho del bin
                     colour = col.border, fill = col.fill) +
      geom_density(colour = col.curve, lwd = 0.5 ) +
      labs(y = "", x = "", title = "") + xlim(xlim) +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
      guides(color = guide_legend(title.position = "top", title.hjust = 0.5))

    #  etiquetas de los límites
    if (!is.null(lsl) & !is.null(usl)){
      p1 <- p1 +
        geom_vline(aes(xintercept = usl, color = "Confidence interval"), linetype = "dashed", col = bounds.col) + # USL
        geom_vline(aes(xintercept = lsl, color = "Confidence interval"), linetype = "dashed", col = bounds.col) + # LSL
        scale_x_continuous(limits = xlim, expand = c(0, 0),
                           sec.axis = sec_axis(~ ., breaks = c(lsl, usl),
                                               labels = c(paste("LSL =",format(lsl, digits = 3)), paste("USL =",format(usl, digits = 3)))
                           )) +
        theme(axis.text.y.right = element_text(size = 15))
    }else{
      if(!is.null(lsl)){
        p1 <- p1 +
          geom_vline(aes(xintercept = lsl, color = "Confidence interval"), linetype = "dashed", col = bounds.col) + # LSL
          scale_x_continuous(limits = xlim, expand = c(0, 0),
                             sec.axis = sec_axis(~ ., breaks = lsl, labels = paste("LSL =",format(lsl, digits = 3)) )) +
          theme(axis.text.y.right = element_text(size = 15))
      }
      if(!is.null(usl)){
        p1 <- p1 +
          geom_vline(aes(xintercept = usl, color = "Confidence interval"), linetype = "dashed", col = bounds.col) + # USL
          scale_x_continuous(limits = xlim, expand = c(0, 0),
                             sec.axis = sec_axis(~ ., breaks = usl,labels = paste("USL =",format(usl, digits = 3)))) +
          theme(axis.text.y.right = element_text(size = 15))
      }
    }

    # 2. Cajita de info Cp's --------------------------------------------------------------------------
    p2 <- ggplot(data = data.frame(x = 0, y = 0), aes(x, y)) +
      theme_bw() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) + xlim(c(0.24, 0.26)) + ylim(c(0.21, 0.43))
    {
      if(is.null(cpu))
        p2 <- p2 + annotate('text', x = 0.25, y = 0.40,
                            label = paste("C[pkL] == ", "NA"),
                            parse = TRUE, size = 3.5, hjust = 0.5)
      else p2 <- p2 + annotate('text', x = 0.25, y = 0.40,
                               label = paste("C[pkU]==", round(cpu, 2)),
                               parse = TRUE, size = 3.5, hjust = 0.5)
      if(is.null(cpl))
        p2 <- p2 + annotate('text', x = 0.25, y = 0.35,
                            label = paste("C[pkL] == ", "NA"),
                            parse = TRUE, size = 3.5, hjust = 0.5)
      else p2 <- p2 + annotate('text', x = 0.25, y = 0.35,
                               label = paste("C[pkL]==", round(cpl, 2)),
                               parse = TRUE, size = 3.5, hjust = 0.5)
      if(is.null(cpk))
        p2 <- p2 + annotate('text', x = 0.25, y = 0.30,
                            label = paste("C[pkL] == ", "NA"),
                            parse = TRUE, size = 3.5, hjust = 0.5)
      else p2 <- p2 + annotate('text', x = 0.25, y = 0.30,
                               label = paste("C[pk]==", round(cpk, 2)),
                               parse = TRUE, size = 3.5, hjust = 0.5)
      if(is.null(cp))
        p2 <- p2 + annotate('text', x = 0.25, y = 0.25,
                            label = paste("C[pkL] == ", "NA"),
                            parse = TRUE, size = 3.5, hjust = 0.5)
      else p2 <- p2 + annotate('text',x = 0.25,y = 0.25,
                               label = paste("C[p]==", round(cp, 2)),
                               parse = TRUE,size = 3.5,hjust = 0.5)
      }
    # 3. Cajita de info n, means, sd --------------------------------------------------------------------------
    index = 1:(length(estimates) + 3)
    names(x) = data.name
    if(not3distr){
      names(x) = data.name
      adTestStats = .myADTest(x, distribution)
      A = numeric()
      p = numeric()
      if (adTestStats$class == "adtest"){
        A = adTestStats$statistic$A
        p = adTestStats$p.value$p
      }

      # Caja de Info ----
      p3 <- ggplot(data = data.frame(x = 0, y = 0), aes(x, y)) +
        theme_bw() +
        theme(
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        ) +
        xlim(c(0.24, 0.26)) + ylim(c(0.19, 0.43))
      {
        # n y A
        p3 <- p3 + annotate('text', x = 0.25, y = 0.40,
                            label = paste("n==", numObs),
                            parse = TRUE, size = 3, hjust = 0.5) +
          annotate('text', x = 0.25, y = 0.35,
                   label = paste("A==", format(as.numeric(A), digits = 3)),
                   parse = TRUE, size = 3, hjust = 0.5)

        # p
        if (!is.null(adTestStats$smaller) && adTestStats$smaller){
          p3 <- p3 + annotate(
            'text',
            x = 0.25,
            y = 0.30,
            label = paste("p<", format(as.numeric(p), digits =3)),
            parse = TRUE,
            size = 3,
            hjust = 0.5
          )
        }
        if (!is.null(adTestStats$smaller) && !adTestStats$smaller){
          p3 <- p3 + annotate(
            'text',
            x = 0.25,
            y = 0.30,
            label = paste("p>=", format(as.numeric(p),digits = 3)),
            parse = TRUE,
            size = 3,
            hjust = 0.5
          )
        }
        if (is.null(adTestStats$smaller)){
          p3 <- p3 + annotate(
            'text',
            x = 0.25,
            y = 0.30,
            label = paste("p==", format(as.numeric(p), digits = 3)),
            parse = TRUE,
            size = 3,
            hjust = 0.5
          )
        }

        # mean y sd
        p3 <- p3 + annotate('text', x = 0.25, y = 0.25,
                            label = paste(names(estimates)[1], "==", format(estimates[[names(estimates)[1]]], digits = 3)),
                            parse = TRUE, size = 3, hjust = 0.5) +
          annotate('text', x = 0.25, y = 0.20,
                   label = paste(names(estimates)[2], "==", format(estimates[[names(estimates)[2]]], digits = 3)),
                   parse = TRUE, size = 3, hjust = 0.5)
        }
    }
    if(any3distr){
      p3 <- ggplot(data = data.frame(x = 0, y = 0), aes(x, y)) +
        theme_bw() +
        theme(
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        ) +
        xlim(c(0.24, 0.26)) + ylim(c(0.21, 0.43))
      {
        # n y A
        p3 <- p3 + annotate('text', x = 0.25, y = 0.40,
                            label = paste("n==", numObs),
                            parse = TRUE, size = 3, hjust = 0.5) +
          annotate('text', x = 0.25, y = 0.35,
                   label = paste("A = ", "*"),
                   parse = FALSE, size = 3, hjust = 0.5) +
          annotate('text', x = 0.25, y = 0.30,
                   label = paste("p = ", "*"),
                   parse = FALSE, size = 3, hjust = 0.5) +
          annotate('text', x = 0.25, y = 0.25,
                   label = paste("mean==", format(estimates[[1]], digits = 3)),
                   parse = TRUE, size = 3, hjust = 0.5) +
          annotate('text', x = 0.25, y = 0.20,
                   label = paste("sd==", format(estimates[[2]], digits = 3)),
                   parse = TRUE, size = 3, hjust = 0.5)
        }
    }

    # 4. qqPlot --------------------------------------------------------------------------
    p4 <- qqPlot(x[, 1], y = distribution, xlab = "", ylab = "", main = "",
                 bounds.lty = bounds.lty, bounds.col = bounds.col, showPlot = FALSE, axis.y.right = TRUE, bw.theme = TRUE)

    # Unimos las 4 primeras gráficas
    main_plot <- p1 + (p2 / p3 / p4$plot) + plot_layout(widths = c(5, 1))

    # 5. Cajita de info 4 (Expected Fraction Nonconforming) --------------------------------------------------------------------------
    p5 <- ggplot(data.frame(x = c(-1, 1),y = c(0.5, 5)), aes(x = x, y = y)) +
      theme_bw() +
      ggtitle("Expected Fraction Nonconforming") +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(hjust = 0.5, vjust = -0.5,margin = margin(b = -12),size = 10)
      )

    p5_left <- ggplot(data = data.frame(x = 0, y = 0), aes(x, y)) +
      theme_bw() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_blank()
      ) +
      xlim(c(0.25,0.26)) + ylim(c(0.24, 0.36))
    {
      p5_left <- p5_left +
        annotate('text', x = 0.25, y = 0.35, label = "-----", size = 3, hjust = 0, colour = "white")
      # Pt
      p5_left <- p5_left +
        annotate("text", x = 0.25, y = 0.33, label = paste("p[t]==", format(ppt, digits = 6)),
                 parse = TRUE, size = 3.5, hjust = 0)
      # PL
      if(is.null(ppl)){
        p5_left <- p5_left +
          annotate("text", x = 0.25, y = 0.3, label = paste("p[L]==", "0"),
                   parse = TRUE, size = 3.5, hjust = 0)
      }else{
        p5_left <- p5_left +
          annotate("text", x = 0.25, y = 0.3, label = paste("p[L]==", format(ppl, digits = 6)),
                   parse = TRUE, size = 3.5, hjust = 0)
      }
      # PU
      if(is.null(ppu)){
        p5_left <- p5_left +
          annotate("text", x = 0.25, y = 0.27, label = paste("p[U]==", "0"),
                   parse = TRUE, size = 3.5, hjust = 0)
      }else{
        p5_left <- p5_left +
          annotate("text", x = 0.25, y = 0.27, label = paste("p[U]==", format(ppu, digits = 6)),
                   parse = TRUE, size = 3.5, hjust = 0)
      }
      }

    p5_right <- ggplot(data = data.frame(x = 0, y = 0), aes(x, y)) +
      theme_bw() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_blank()
      ) +
      xlim(c(0.25,0.26)) + ylim(c(0.24, 0.36))
    {
      p5_right <- p5_right +
        annotate('text', x = 0.25, y = 0.35, label = "-----", size = 3, hjust = 0, colour = "white")

      # ppm
      p5_right <- p5_right +
        annotate("text", x = 0.25, y = 0.33, label = paste("ppm==", format(ppt * 1e+06, digits = 6)),
                 parse = TRUE, size = 3.5, hjust = 0)
      if(is.null(ppl)){
        p5_right <- p5_right +
          annotate("text", x = 0.25, y = 0.3, label = paste("ppm==", "0"),
                   parse = TRUE, size = 3.5, hjust = 0)
      }else{
        p5_right <- p5_right +
          annotate("text", x = 0.25, y = 0.3, label = paste("ppm==", format(ppl * 1e+06, digits = 6)),
                   parse = TRUE, size = 3.5, hjust = 0)
      }
      if(is.null(ppu)){
        p5_right <- p5_right +
          annotate("text", x = 0.25, y = 0.27, label = paste("ppm==", "0"),
                   parse = TRUE, size = 3.5, hjust = 0)
      }else{
        p5_right <- p5_right +
          annotate("text", x = 0.25, y = 0.27, label = paste("ppm==", format(ppu * 1e+06, digits = 6)),
                   parse = TRUE, size = 3.5, hjust = 0)
      }
      }

    p5 <- p5 + inset_element(p5_left, left = 0, right = 0.5, top = 0,  bottom = 0.80)+
      inset_element(p5_right, left = 0.5, right = 1, top = 0,  bottom = 0.80)

    # Caja info 6. Observed --------------------------------------------------------------------------
    obsL = 0
    obsU = 0
    p6 <- ggplot(data.frame(x = 0,y = 0), aes(x = x, y = y)) +
      theme_bw() +
      ggtitle("Observed") +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = -0.5,margin = margin(b = -12), size = 10)
      ) +
      xlim(c(0.24, 0.26)) + ylim(c(0.27, 0.36))
    if (!is.null(lsl)){
      obsL = (sum(x < lsl)/length(x)) * 1e+06
      p6 <- p6 + annotate("text", x = 0.25, y = 0.31, label = paste("ppm==", format(obsL, digits = 6)),
                          parse = TRUE, size = 3.5, hjust = 0.5)
    } else{
      p6 <- p6 + annotate("text", x = 0.25, y = 0.31, label = paste("ppm==", 0),
                          parse = TRUE, size = 3.5, hjust = 0.5)
    }
    if (!is.null(usl)){
      obsU = (sum(x > usl)/length(x)) * 1e+06
      p6 <- p6 + annotate("text", x = 0.25, y = 0.28, label = paste("ppm==", format(obsU, digits = 6)),
                          parse = TRUE, size = 3.5, hjust = 0.5)
    } else{
      p6 <- p6 + annotate("text", x = 0.25, y = 0.28, label = paste("ppm==", 0),
                          parse = TRUE, size = 3.5, hjust = 0.5)
    }
    p6 <- p6 + annotate("text", x = 0.25, y = 0.34, label = paste("ppm==", format(obsL + obsU, digits = 6)),
                        parse = TRUE, size = 3.5, hjust = 0.5)

    # UNION --------------------------------------------------------------------------
    box_bottom <- p5 + p6 + plot_layout(widths = c(2, 1))
    main_plot <- p1 / box_bottom + plot_layout(heights = c(1, 0.5))
    box_right <- p2 / p3 / p4$plot
    box_right <- box_right/plot_spacer()
    main_plot <- (main_plot | box_right) + plot_layout(ncol = 2, widths = c(5, 1))
    main_plot <- main_plot + plot_annotation(
      title = main,
      theme = theme(plot.title = element_text(hjust = 0.5))
    )
  }

  if(ADtest){
    if(not3distr){
      print_adtest(adTestStats)
    }
  }

  if(plot==TRUE){
    suppressWarnings(print(main_plot))
    invisible(list(lambda = lambda, cp = cp, cpk = cpk,
                   cpl = cpl, cpu = cpu, ppt = ppt, ppl = ppl, ppu = ppu,
                   A = A, usl = usl, lsl = lsl, target = target,
                   adTest = adTestStats, plot = main_plot))
  }
  else{
    invisible(list(lambda = lambda, cp = cp, cpk = cpk,
                   cpl = cpl, cpu = cpu, ppt = ppt, ppl = ppl, ppu = ppu,
                   A = A, usl = usl, lsl = lsl, target = target,
                   adTest = adTestStats, plot = main_plot))
  }

}

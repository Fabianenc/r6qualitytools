#############################################################################
######################### DISTRIBUTION - AUXILIARES #########################
#############################################################################

# .cg ----
.cg=function(x, target, tolerance=c(-1,1), ref.interval, facCg, facCgk)
{
  if (missing(x))
    stop("x must be given as a vector")
  if (missing(target))
    target = mean(x)
  if (missing(ref.interval))
    ref.interval = pnorm(3) - pnorm(-3)
  sd = sd(x)
  mean = mean(x)
  ref.ar = qnorm(ref.interval, mean, sd) - qnorm(1 - ref.interval, mean, sd)
  if (missing(facCg))
    facCg = 0.2
  if (missing(facCgk))
    facCgk = 0.1
  if (missing(tolerance))
    stop("tolerance is missing!")
  if (length(tolerance) != 2)
    stop("tolerance has wrong length")
  Cg = (facCg * (abs(diff(tolerance))))/ref.ar
  Cgk = (facCgk * (abs(diff(tolerance))) - abs(target - mean))/(ref.ar/2)
  return(list(Cg, Cgk))
}
# internals.r ----
.confintbeta= function(xs, thethas, varmatrix, alpha) {

  th1= thethas[[1]]
  th2 =  thethas[[2]]

  prozent=ppoints(xs)

  perzentile=qbeta(prozent, th1, th2)

  h=1e-6
  dFdth1=(qbeta(prozent, th1, th2)-qbeta(prozent, th1+h, th2))/h
  dFdth2=(qbeta(prozent, th1, th2)-qbeta(prozent, th1, th2+h))/h

  Var = varmatrix[[1, 1]]*dFdth1^2 + 2*varmatrix[[1, 2]]*dFdth1*dFdth2 + varmatrix[[2, 2]]*dFdth2^2
  zalpha=qnorm(1-alpha/2)
  halfwidth = zalpha*sqrt(Var)

  lci=perzentile-halfwidth
  uci=perzentile+halfwidth

  bounds = list(lci, uci, perzentile)


  return (bounds)
}
.confintcauchy = function(xs, thethas, varmatrix, alpha) {

  th1= thethas[[1]]
  th2 =  thethas[[2]]

  prozent=ppoints(xs)

  perzentile=qcauchy(prozent, th1, th2)

  h=1e-6
  dFdth1=(qcauchy(prozent, th1, th2)-qcauchy(prozent, th1+h, th2))/h
  dFdth2=(qcauchy(prozent, th1, th2)-qcauchy(prozent, th1, th2+h))/h

  Var = varmatrix[[1, 1]]*dFdth1^2 + 2*varmatrix[[1, 2]]*dFdth1*dFdth2 + varmatrix[[2, 2]]*dFdth2^2
  zalpha=qnorm(1-alpha/2)
  halfwidth = zalpha*sqrt(Var)

  lci=perzentile-halfwidth
  uci=perzentile+halfwidth

  bounds = list(lci, uci, perzentile)

  return (bounds)
}
.confintexp=function(xs, thethas, varmatrix, alpha) {
  lambda=thethas[[1]]
  prozent=ppoints(xs)
  perzentile=qexp(prozent, lambda)
  logPerzentile = log(perzentile)
  zalpha=qnorm(1-alpha/2)
  halfwidth = zalpha*sqrt(varmatrix[[1, 1]]/lambda^2)
  lci = exp(logPerzentile - halfwidth);
  uci = exp(logPerzentile + halfwidth);

  bounds = list(lci, uci, perzentile)

  return (bounds)

}
.confintgamma= function(xs, thethas, varmatrix, alpha) {
  th1= thethas[[1]]
  th2 =  thethas[[2]]
  prozent=ppoints(xs)
  perzentile=qgamma(prozent, th1, th2)

  h=1e-6
  dFdth1=(qgamma(prozent, th1, th2)-qgamma(prozent, th1+h, th2))/h
  dFdth2=(qgamma(prozent, th1, th2)-qgamma(prozent, th1, th2+h))/h

  Var = varmatrix[[1, 1]]*dFdth1^2 + 2*varmatrix[[1, 2]]*dFdth1*dFdth2 + varmatrix[[2, 2]]*dFdth2^2
  zalpha=qnorm(1-alpha/2)
  halfwidth = zalpha*sqrt(Var)

  lci=perzentile-halfwidth
  uci=perzentile+halfwidth

  bounds = list(lci, uci, perzentile)

  return (bounds)
}
.confintlnorm=function(xs, thethas, varmatrix, alpha){
  th1= thethas[[1]]
  th2 =  thethas[[2]]

  prozent=ppoints(xs)

  perzentile=qlnorm(prozent, th1, th2)

  zp=qnorm(prozent)

  varPerzentile = varmatrix[[1, 1]]+2*varmatrix[[1, 2]]*zp+varmatrix[[2, 2]]*zp*zp

  zalpha=qnorm(1-alpha/2)
  lci=log(perzentile)-zalpha*sqrt(varPerzentile)
  uci=log(perzentile)+zalpha*sqrt(varPerzentile)

  bounds = list(exp(lci), exp(uci), perzentile)

  return (bounds)

}
.confintlogis= function(xs, thethas, varmatrix, alpha) {

  th1= thethas[[1]]
  th2 =  thethas[[2]]

  prozent=ppoints(xs)


  perzentile=qlogis(prozent, th1, th2)

  h=1e-6
  dFdth1=(qlogis(prozent, th1, th2)-qlogis(prozent, th1+h, th2))/h
  dFdth2=(qlogis(prozent, th1, th2)-qlogis(prozent, th1, th2+h))/h

  Var = varmatrix[[1, 1]]*dFdth1^2 + 2*varmatrix[[1, 2]]*dFdth1*dFdth2 + varmatrix[[2, 2]]*dFdth2^2
  zalpha=qnorm(1-alpha/2)
  halfwidth = zalpha*sqrt(Var)

  lci=perzentile-halfwidth
  uci=perzentile+halfwidth

  bounds = list(lci, uci, perzentile)

  return (bounds)
}
.confintnorm=function(xs, thethas, varmatrix, alpha){

  prozent=ppoints(xs)

  zp=qnorm(prozent)
  perzentile=qnorm(prozent, thethas[[1]], thethas[[2]])

  varPerzentile = varmatrix[[1, 1]]+2*varmatrix[[1, 2]]*zp+varmatrix[[2, 2]]*zp*zp

  zalpha=qnorm(1-alpha/2)
  lci=perzentile-zalpha*sqrt(varPerzentile)
  uci=perzentile+zalpha*sqrt(varPerzentile)

  bounds = list(lci, uci, perzentile)

  return (bounds)
}
.confintweibull= function(xs, thethas, varmatrix, alpha) {
  th1= thethas[[1]]
  th2 =  thethas[[2]]

  prozent=ppoints(xs)

  perzentile=qweibull(prozent, th1, th2)
  q=-log(1-prozent)
  logPerzentile=log(perzentile)
  logq=log(q)
  dB=1/th2
  dA=-1/(th1^2)

  Var = varmatrix[[1, 1]]*(dA*logq)^2 + 2*varmatrix[[1, 2]]*dB*dA*logq + varmatrix[[2, 2]]*dB^2
  zalpha=qnorm(1-alpha/2)
  halfwidth = zalpha*sqrt(Var)


  lci=exp(logPerzentile-halfwidth)
  uci=exp(logPerzentile+halfwidth)

  bounds = list(lci, uci, perzentile)

  # print(data.frame(prozent, uci, perzentile, lci))

  return (bounds)
}
.gamma3 = function(data) {
  n=length(data)
  data=sort(data)

  pEmp= (seq(1:n)-0.5)/n

  weight = 1 / sqrt(pEmp*(1-pEmp))

  thld = .99*min(data)
  shape=1
  scale=1

  gammaEst = function(param) {
    return( sum(weight*(pgamma(data-param[3], shape = exp(param[1]), scale = exp(param[2]))-pEmp)^2) )
  }

  paramEst = optim(c(shape, scale, thld), gammaEst, method = "Nelder-Mead")
  paramEst = paramEst$par
  return(list(shape = exp(paramEst[1]), scale = exp(paramEst[2]), threshold = paramEst[3]))
}
.lognormal3 = function(data) {

  n=length(data)
  data=sort(data)
  #compute the empirical cumulative distribution function of the data
  pEmp= (seq(1:n)-0.5)/n
  # will minimize the weighted sum of squared distances
  # so compute weights
  weight = 1 / sqrt(pEmp*(1-pEmp))

  # initial values for optimization
  thld = .99*min(data)
  mu0 = mean(log(data-thld))
  sigma0 = sd(log(data-thld))


  lnEst = function(param) {
    return( sum(weight*(plnorm(data-param[3], meanlog = param[1], sdlog = exp(param[2]))-pEmp)^2) )
  }

  logSigma0=log(sigma0)
  # optimize gammaEst using optim function
  paramEst = optim(c(mu0,logSigma0, thld), lnEst, method = "Nelder-Mead")
  param = paramEst$par

  return(list(meanlog = param[1], sdlog = exp(param[2]), threshold = param[3]))
}
.weibull3 = function(x){
  if(any(x < 0))
    stop("x must be positive")

  n = length(x)
  x = sort(x)
  p = ((1:n)-0.5)/n
  interval = c(0.75*min(x), 0.9999*min(x))

  wb3RSquared = function(th)
  {
    return(summary(lm(log(x-th) ~ log(-log(1-p))))$r.squared)
  }

  th = (optimize(wb3RSquared, interval = interval, maximum = TRUE))$maximum

  lm.1 = lm(log(x-th) ~ log(-log(1-p)))
  estimates = list(shape = 1/coef(lm.1)[[2]], scale = exp(coef(lm.1)[[1]]), threshold = th)
  return(estimates)
}
# .charToDistFunc ----
.charToDistFunc = function(distribution, type = "q") {
  fun = NULL
  if (identical("beta", distribution))
    fun = eval(parse(text = paste(type, "beta", sep = "")))
  if (identical("cauchy", distribution))
    fun = eval(parse(text = paste(type, "cauchy", sep = "")))
  if (identical("chi-squared", distribution))
    fun = eval(parse(text = paste(type, "chisq", sep = "")))
  if (identical("exponential", distribution))
    fun = eval(parse(text = paste(type, "exp", sep = "")))
  if (identical("f", distribution))
    fun = eval(parse(text = paste(type, "f", sep = "")))
  if (identical("geometric", distribution))
    fun = eval(parse(text = paste(type, "geom", sep = "")))
  if (identical("log-normal", distribution) || identical("lognormal", distribution))         ####
    fun = eval(parse(text = paste(type, "lnorm", sep = "")))
  if (identical("log-normal3", distribution) || identical("lognormal3", distribution))       ####
    fun = eval(parse(text = paste(type, "lnorm3", sep = "")))                              ####
  if (identical("logistic", distribution))
    fun = eval(parse(text = paste(type, "logis", sep = "")))
  if (identical("negative binomial", distribution))
    fun = eval(parse(text = paste(type, "nbinom", sep = "")))
  if (identical("normal", distribution))
    fun = eval(parse(text = paste(type, "norm", sep = "")))
  if (identical("poisson", distribution))
    fun = eval(parse(text = paste(type, "pois", sep = "")))
  if (identical("t", distribution))
    fun = eval(parse(text = paste(type, "t", sep = "")))
  if (identical("weibull", distribution))
    fun = eval(parse(text = paste(type, "weibull", sep = "")))
  if (identical("weibull3", distribution))                                                   ####
    fun = eval(parse(text = paste(type, "weibull3", sep = "")))                            ####
  if (identical("gamma", distribution))
    fun = eval(parse(text = paste(type, "gamma", sep = "")))
  if (identical("gamma3", distribution))
    fun = eval(parse(text = paste(type, "gamma3", sep = "")))
  return(fun)
}
# .myADtest ----
.myADTest = function(x, distribution, ...) {                                                                     ####   .MYADTESTS-FUNCTION
  #require(MASS, quietly = TRUE)
  if (missing(distribution))
    distribution = "normal"
  data.name = names(x)
  if (is.data.frame(x))
    x = x[, 1]
  dots = list()
  parameter = NULL
  smaller = NULL
  pFun = NULL
  tableValue = FALSE
  A = 0
  x <- sort(x[complete.cases(x)])
  n = length(x)
  if (n < 8)
    stop("sample size must be greater than 7")
  if (is.character(distribution)) {
    pFun = .charToDistFunc(distribution, type = "p")
    distribution = tolower(distribution)
    if (is.null(pFun))
      stop(paste(deparse(substitute(distribution)), " is not supported!"))
  }
  else {
    pFun = match.fun(distribution)
  }                                                                      ####
  if (length(dots) == 0) {
    fittedDistr = MASS::fitdistr(x, distribution)
    parameter = fittedDistr$estimate
    if (distribution == "normal") {
      parameter["mean"] = mean(x)
      parameter["sd"] = sd(x)
    }
    p = do.call(pFun, c(list(x), as.list(parameter)))
  }
  else {
    p = pFun(x, ...)
  }
  h = (2 * seq(1:n) - 1) * (log(p) + log(1 - rev(p)))
  A = -n - mean(h)
  AA = (1 + 0.75/n + 2.25/n^2) * A
  if (AA < 0.2) {
    pval <- 1 - exp(-13.436 + 101.14 * AA - 223.73 * AA^2)
  }
  else if (AA < 0.34) {
    pval <- 1 - exp(-8.318 + 42.796 * AA - 59.938 * AA^2)
  }
  else if (AA < 0.6) {
    pval <- exp(0.9177 - 4.279 * AA - 1.38 * AA^2)
  }
  else {
    pval <- exp(1.2937 - 5.709 * AA + 0.0186 * AA^2)
  }
  if (identical(distribution, "cauchy")) {
    pval = NA
  }
  if (identical(distribution, "beta")) {
    pval = NA
  }
  if (identical(distribution, "chi-squared")) {
    pval = NA
  }
  if (identical(distribution, "f")) {
    pval = NA
  }
  if (identical(distribution, "t")) {
    pval = NA
  }
  if (identical(distribution, "geometric")) {
    pval = NA
  }
  if (identical(distribution, "poisson")) {
    pval = NA
  }
  if (identical(distribution, "negative-binomial")) {
    pval = NA
  }
  if (identical(distribution, "weibull")) {
    AWei = A * (1 + 1/sqrt(n))
    tableValue = TRUE
    smaller = TRUE
    if (AWei < 0.474) {
      pval = 0.25
      smaller = FALSE
    }
    if (AWei >= 0.474)
      pval = 0.25
    if (AWei >= 0.637)
      pval = 0.1
    if (AWei >= 0.757)
      pval = 0.05
    if (AWei >= 0.877)
      pval = 0.025
    if (AWei >= 1.038)
      pval = 0.01
  }
  if (identical(distribution, "exponential")) {
    AExp = A * (1 + 0.6/n)
    pval = NA
    if (0.95 < AExp) {
      pval = exp(0.731 - 3.009 * AExp + 0.15 * AExp^2)
    }
    if (0.51 < AExp & AExp < 0.95) {
      pval = exp(0.9209 - 3.353 * AExp + 0.3 * AExp^2)
    }
    if (0.26 < AExp & AExp < 0.51) {
      pval = 1 - exp(-6.1327 + 20.218 * AExp - 18.663 * AExp^2)
    }
    if (AExp < 0.26) {
      pval = 1 - exp(-12.2204 + 67.459 * AExp - 110.3 * AExp^2)
    }
  }
  if (identical(distribution, "logistic")) {
    ALogist = A * (1 + 0.25/n)
    tableValue = TRUE
    smaller = TRUE
    if (ALogist < 0.426) {
      pval = 0.25
      smaller = FALSE
    }
    if (ALogist >= 0.426) {
      pval = 0.25
    }
    if (ALogist >= 0.563) {
      pval = 0.1
    }
    if (ALogist >= 0.66) {
      pval = 0.05
    }
    if (ALogist >= 0.769) {
      pval = 0.025
    }
    if (ALogist >= 0.906) {
      pval = 0.01
    }
    if (ALogist >= 1.1) {
      pval = 0.005
    }
  }
  if (identical(distribution, "gamma")) {
    tableValue = TRUE
    gammaDF = data.frame(c(1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20, Inf), c(0.486, 0.477, 0.475,
                                                                        0.473, 0.472, 0.472, 0.471, 0.471, 0.471, 0.47, 0.47, 0.47), c(0.657, 0.643, 0.639, 0.637,
                                                                                                                                       0.635, 0.635, 0.634, 0.633, 0.633, 0.632, 0.632, 0.631), c(0.786, 0.768, 0.762, 0.759,
                                                                                                                                                                                                  0.758, 0.757, 0.755, 0.754, 0.754, 0.754, 0.753, 0.752), c(0.917, 0.894, 0.886, 0.883,
                                                                                                                                                                                                                                                             0.881, 0.88, 0.878, 0.877, 0.876, 0.876, 0.875, 0.873), c(1.092, 1.062, 1.052, 1.048,
                                                                                                                                                                                                                                                                                                                       1.045, 1.043, 1.041, 1.04, 1.039, 1.038, 1.037, 1.035), c(1.227, 1.19, 1.178, 1.173,
                                                                                                                                                                                                                                                                                                                                                                                 1.17, 1.168, 1.165, 1.164, 1.163, 1.162, 1.161, 1.159))
    names(gammaDF) = c("m", 0.75, 0.9, 0.95, 0.975, 0.99, 0.995)
    critCheck <- gammaDF[min(which(gammaDF$m >= parameter["shape"])), 2:length(gammaDF)] > A
    if (any(critCheck)) {
      firPos <- min(which(critCheck))
    }
    else {
      firPos <- length(gammaDF)
    }
    if (firPos == 1) {
      pValue <- 1 - as.numeric(names(gammaDF)[2])
      pval = pValue
      pValue <- paste(">", pValue)
      smaller = FALSE
    }
    else {
      pValue <- 1 - as.numeric(names(gammaDF)[firPos])
      pval = pValue
      pValue <- paste("<=", pValue)
      smaller = TRUE
    }
  }
  out = list()
  out$data.name = data.name
  out$statistic = as.vector(data.frame(A = A))
  out$parameter = parameter
  out$p.value = as.vector(data.frame(p = pval))
  out$smaller = smaller
  out$tableValue = tableValue
  out$conf.int = NULL
  out$estimate = NULL
  temp = NULL
  if (is.character(distribution))
    temp = as.vector(distribution)
  else temp = deparse(substitute(distribution))
  names(temp) = "distribution"
  out$null.value = temp
  out$method = paste("Anderson Darling Test for", temp, "distribution")
  out$class = "adtest"
  invisible(out)
}
# .xyLimits ----
.xyLimits = function(distrCollection, lowerquantile = 0.001, upperquantile = 0.999) {
  x <- NULL
  y <- NULL
  for (i in seq_along(distrCollection$distr)) {
    object <- distrCollection$distr[[i]]
    xValues <- object$x
    parameters <- object$parameters
    distr <- object$name
    qFun <- .charToDistFunc(distr, type = "q")
    dFun <- .charToDistFunc(distr, type = "d")
    lq <- do.call(qFun, c(list(lowerquantile), as.list(parameters)))
    uq <- do.call(qFun, c(list(upperquantile), as.list(parameters)))
    x <- range(x, xValues, lq, uq)
    histObj <- hist(xValues, plot = FALSE)
    xPoints <- seq(x[1], x[2], length = 200)
    yPoints <- do.call(dFun, c(list(xPoints), as.list(parameters)))
    y <- range(y, 0, histObj$density, yPoints)
  }
  invisible(list(xlim = x, ylim = y))
}
# .sdSg, lfkp, lfrm  -----
.sdSg = function(x, grouping = NULL, method = c("NOWEIGHT", "MVLUE", "RMSDF"), na.rm = TRUE) {
  if (!is.data.frame(x) && !is.vector(x) && is.numeric(x))
    stop("x needs to be either a data.frame or a vector and numeric")
  if (is.null(grouping)) {
    if (is.data.frame(x))
      return(sd(x[, 1]))
    else return(sd(x))
  }
  else grouping = as.data.frame(grouping)
  group = unique(grouping)
  sdVec = numeric(length = length(group))
  for (i in 1:nrow(group)) {
    if (is.data.frame(x))
      temp = x[group[i, 1] == grouping[, 1], 1]
    if (is.vector(x))
      temp = x[group[i, 1] == grouping[, 1]]
    sdVec[i] = sd(temp, na.rm = T)/.c4(length(temp[!is.na(temp)]))
  }
  return((mean(sdVec)))
}
.lfkp = function(wholeList, filterList) {
  if (!is.list(wholeList))
    stop(paste(deparse(substitute(wholeList)), "is not a list!"))
  if (length(wholeList) == 0)
    return(wholeList)
  if (!is.list(filterList))
    stop(paste(deparse(substitute(filterList)), "is not a list!"))
  if (length(filterList) == 0)
    return(filterList)
  logVec = lapply(names(wholeList), "%in%", names(filterList))
  filteredList = wholeList[unlist(logVec)]
  return(filteredList)
}
.lfrm = function(wholeList, filterList) {
  if (!is.list(wholeList))
    stop(paste(deparse(substitute(wholeList)), "is not a list!"))
  if (length(wholeList) == 0)
    return(wholeList)
  if (!is.list(filterList))
    stop(paste(deparse(substitute(filterList)), "is not a list!"))
  if (length(filterList) == 0)
    return(wholeList)
  logVec = lapply(names(wholeList), "%in%", names(filterList))
  filteredList = wholeList[!unlist(logVec)]
  return(filteredList)
}
# .pcr -----
.pcr = function(x, distribution = "normal", lsl, usl, target, boxcox = FALSE, lambda = c(-5,5), main, xlim, ylim, grouping = NULL, std.dev = NULL, conf.level = 0.9973002, start, lineWidth = 1,
                lineCol = "red", lineType = "solid", specCol = "red3", specWidth = 1, cex.text = 2, cex.val = 1.5, cex.col = "darkgray", plot = TRUE, ...) {
  data.name = deparse(substitute(x))[1]                                 ####
  parList = list(...)
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
    x = as.data.frame(x)                                                                          ####
  any3distr=FALSE;not3distr=FALSE                                            ####
  if(distribution=="weibull3" || distribution=="lognormal3" || distribution=="gamma3")####
    any3distr=TRUE                                                             ####
  if (distribution!="weibull3" && distribution!="lognormal3" && distribution!="gamma3")####
    not3distr=TRUE                                                                  ####
  if (boxcox) {
    distribution = "normal"
    if (length(lambda) >= 2) {
      temp = boxcox(x[, 1] ~ 1, lambda = seq(min(lambda), max(lambda), 1/10), plotit = FALSE)
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
      stop(paste("length of ", deparse(substitute(grouping)), " differs from length of ", varName))
  }
  if (missing(main))
    if (boxcox)
      main = paste("Process Capability using box cox transformation for", varName)
  else main = paste("Process Capability using", as.character(distribution), "distribution for",
                    varName)
  if (is.null(std.dev)) {
    if (is.null(grouping))
      std.dev = .sdSg(x)
    else std.dev = .sdSg(x, grouping)
  }
  if (conf.level < 0 | conf.level > 1)
    stop("conf.level must be a value between 0 and 1")
  confHigh = conf.level + (1 - conf.level)/2
  confLow = 1 - conf.level - (1 - conf.level)/2
  distWhichNeedParameters = c("weibull", "logistic", "gamma", "exponential", "f", "geometric",
                              "chi-squared", "negative binomial", "poisson")
  if (is.character(distribution)) {
    dis=distribution                                                           ####
    if (identical(distribution,"weibull3"))                                     ####
      dis="weibull3"                                                             ####
    if (identical(distribution,"gamma3"))                                       ####
      dis="gamma3"                                                               ####
    if (identical(distribution,"lognormal3"))                                   ####
      dis="lognormal3"                                                           ####
    qFun = .charToDistFunc(dis, type = "q")                                 ####
    pFun = .charToDistFunc(dis, type = "p")                                 ####
    dFun = .charToDistFunc(dis, type = "d")                                 ####
    if (is.null(qFun) & is.null(pFun) & is.null(dFun))
      stop(paste(deparse(substitute(y)), "distribution could not be found!"))
  }
  if (TRUE) {                                                                 #### distribution!="weibull3" && distribution!="lognormal3" && distribution!="gamma3"
    fitList = vector(mode = "list", length = 0)
    fitList$x = x[, 1]
    fitList$densfun = dis                                                   ####
    if (!missing(start))
      fitList$start = start
    if (not3distr)                                                          ####
    {                                                                       ####
      fittedDistr = do.call(MASS::fitdistr, fitList)
      estimates = as.list(fittedDistr$estimate)
      paramsList = estimates
    }                                                                       ####
    if (distribution=="weibull3")                                           ####
    {                                                                       ####
      paramsList= .weibull3(x[,1])                                           ####
      estimates = paramsList                                                 ####
    }                                                                       ####
    if (distribution=="lognormal3")                                         ####
    {                                                                       ####
      paramsList= .lognormal3(x[,1])                                         ####
      estimates = paramsList                                                 ####
    }                                                                       ####
    if (distribution=="gamma3")                                             ####
    {                                                                       ####
      paramsList= .gamma3(x[,1])                                             ####
      estimates = paramsList                                                 ####
    }                                                                       ####
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

  if(plot==TRUE){
    if (missing(xlim)) {
      xlim <- range(x[, 1], usl, lsl)
      xlim <- xlim + diff(xlim) * c(-0.2, 0.2)
    }
    xVec <- seq(min(xlim), max(xlim), length = 200)
    dParamsList = .lfkp(paramsList, formals(dFun))
    dParamsList$x = xVec

    yVec = do.call(dFun, dParamsList)
    histObj <- hist(x[, 1], plot = FALSE)
    if (missing(ylim)) {
      ylim <- range(histObj$density, yVec)
      ylim <- ylim + diff(ylim) * c(0, 0.05)
    }

    # 1. Histograma --------------------------------------------------------------------------
    x.c <- x[, 1]
    temp <- hist(x.c, plot = FALSE)
    df <- data.frame(
      mid = temp$mids,
      density = temp$density
    )
    width <- diff(df$mid)[1] # Ancho de cada barra
    # Histograma
    p1 <- ggplot(df, aes(x = mid, y = density)) +
      geom_bar(stat = "identity", width = width, fill = "lightblue", color = "black", alpha = 0.5) +
      labs(y = "", x = "", title = main) +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
      guides(color = guide_legend(title.position = "top", title.hjust = 0.5))+
      geom_line(data = data.frame(x = xVec, y = yVec), aes(x = x, y = y), color = "red", linewidth = 0.5) + # densidad
      theme(legend.position = "none")

    #  etiquetas de los límites
    if (!is.null(lsl) & !is.null(usl)){
      p1 <- p1 +
        geom_vline(aes(xintercept = usl, color = "Confidence interval"), linetype = "dashed", col = "red") + # USL
        geom_vline(aes(xintercept = lsl, color = "Confidence interval"), linetype = "dashed", col = "red") + # LSL
        scale_x_continuous(limits = xlim, expand = c(0, 0),
                           sec.axis = sec_axis(~ ., breaks = c(lsl, usl),
                                               labels = c(paste("LSL =",format(lsl, digits = 3)), paste("USL =",format(usl, digits = 3)))
                           )) +
        theme(axis.text.y.right = element_text(size = 15))
    }else{
      if(!is.null(lsl)){
        p1 <- p1 +
          geom_vline(aes(xintercept = lsl, color = "Confidence interval"), linetype = "dashed", col = "red") + # LSL
          scale_x_continuous(limits = xlim, expand = c(0, 0),
                             sec.axis = sec_axis(~ ., breaks = lsl, labels = paste("LSL =",format(lsl, digits = 3)) )) +
          theme(axis.text.y.right = element_text(size = 15))
      }
      if(!is.null(usl)){
        p1 <- p1 +
          geom_vline(aes(xintercept = usl, color = "Confidence interval"), linetype = "dashed", col = "red") + # USL
          scale_x_continuous(limits = xlim, expand = c(0, 0),
                             sec.axis = sec_axis(~ ., breaks = usl,labels = paste("USL =",format(usl, digits = 3)))) +
          theme(axis.text.y.right = element_text(size = 15))
      }
    }
    return(list(lambda = lambda, cp = cp, cpk = cpk, cpl = cpl, cpu = cpu,        ####
                ppt = ppt, ppl = ppl, ppu = ppu, usl = usl,                  ####
                lsl = lsl, target = target, plot = p1))
  }

  return(list(lambda = lambda, cp = cp, cpk = cpk, cpl = cpl, cpu = cpu,        ####
              ppt = ppt, ppl = ppl, ppu = ppu, usl = usl,                  ####
              lsl = lsl, target = target))                                 ####
}

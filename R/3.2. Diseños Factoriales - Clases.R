##########################################################################
###################### DISEÑOS FACTORIALES - CLASES ######################
##########################################################################

# Clase facDesign.c ----
#' @title facDesign-class: Class "facDesign"
#' @description The \code{facDesign.c} class is used to represent factorial designs, including their factors, responses, blocks, and design matrices. This class supports various experimental designs and allows for the storage and manipulation of data related to the design and analysis of factorial experiments.
#' @field name Character string representing the name of the factorial design.
#' @field factors List of factors involved in the factorial design, including their levels and settings.
#' @field cube Data frame containing the design matrix for the cube portion of the factorial design.
#' @field star Data frame containing the design matrix for the star portion of the factorial design.
#' @field centerCube Data frame containing the center points within the cube portion of the factorial design.
#' @field centerStar Data frame containing the center points within the star portion of the factorial design.
#' @field generator List of generators used to create the fractional factorial design.
#' @field response Data frame containing the responses or outcomes measured in the design.
#' @field block Data frame specifying the block structures if the design is blocked.
#' @field blockGen Data frame specifying the block generators for the design.
#' @field runOrder Data frame specifying the order in which runs are performed.
#' @field standardOrder Data frame specifying the standard order of the runs.
#' @field desireVal List of desired values or targets for the response variables.
#' @field desirability List of desirability scores or metrics based on the desired values.
#' @field fits Data frame containing the fitted model parameters and diagnostics for the responses in the design.
#' @seealso \code{\link{mixDesign.c}}, \code{\link{taguchiDesign.c}}.

facDesign.c <- R6Class("facDesign", public = list(name = NULL,
                                                  factors = NULL,
                                                  cube = data.frame(),
                                                  star = data.frame(),
                                                  centerCube = data.frame(),
                                                  centerStar = data.frame(),
                                                  generator = NULL,
                                                  response = data.frame(),
                                                  block = data.frame(),
                                                  blockGen = data.frame(),
                                                  runOrder = data.frame(),
                                                  standardOrder = data.frame(),
                                                  desireVal = NULL,
                                                  desirability = list(),
                                                  fits = NULL,

                                                  #' @description Get the number of rows Design.
                                                  nrow = function(){
                                                    nrow(self$as.data.frame())
                                                  },
                                                  #' @description Get the number of columns Design.
                                                  ncol = function(){
                                                    ncol(self$as.data.frame())
                                                  },

                                                  #' @description Prints a formatted representation of the factorial design object, including design matrices and responses.
                                                  print = function(){
                                                    runIndex = order(self$runOrder[,1])
                                                    print(format(self$as.data.frame(), digits = 4))
                                                    invisible(self$as.data.frame())
                                                  },

                                                  #' @description Clears the factorial design object.
                                                  .clear = function(){
                                                    self$standardOrder = data.frame()
                                                    self$runOrder = data.frame()
                                                    self$cube = data.frame()
                                                    self$centerStar = data.frame()
                                                    self$centerCube = data.frame()
                                                    self$star = data.frame()
                                                    self$block = data.frame()
                                                    self$blockGen = data.frame()
                                                    self$response = data.frame()
                                                    invisible(self)
                                                  },

                                                  #' @description Get or set the names of the factors in the factorial design.
                                                  #' @param value Character vector with new names for the factors. If missing, retrieves the current names.
                                                  names = function(value){
                                                    if(missing(value)){
                                                      n <- c()
                                                      for (i in 1:length(self$factors)) {
                                                        n[i] <- self$factors[[i]]$name
                                                      }
                                                      return(n)
                                                    }
                                                    else {
                                                      for (i in 1:length(self$factors)){
                                                        self$factors[[i]]$name = as.character(value[i])

                                                      }
                                                      invisible(self)
                                                    }

                                                  },

                                                  #' @description Converts the factorial design object to a data frame.
                                                  as.data.frame = function() {
                                                    if (nrow(self$cube)>0) {
                                                      frameOut = self$cube
                                                      names(frameOut) = self$names()
                                                    }
                                                    else return(NULL)
                                                    if (nrow(self$centerCube)>0){
                                                      faux <- self$centerCube
                                                      names(faux) <- self$names()
                                                      frameOut = rbind(frameOut, faux)
                                                    }
                                                    if (nrow(self$star)>0){
                                                      faux <- self$star
                                                      names(faux) <- self$names()
                                                      frameOut = rbind(frameOut, faux)
                                                    }
                                                    if (nrow(self$centerStar)>0){
                                                      faux <- self$centerStar
                                                      names(faux) <- self$names()
                                                      frameOut = rbind(frameOut, faux)
                                                    }
                                                    aux <- list()
                                                    for (i in 1:length(self$names())) {
                                                      aux[[self$names()[i]]] <-.NAMES[i]
                                                    }
                                                    if (!is.null(self$factors) && length(self$factors) == dim(frameOut)[2]) {
                                                      names(frameOut) = as.character(aux)
                                                    }
                                                    # if (!is.null(self$blockGen) && nrow(self$blockGen) > 0) {
                                                    #   frameOut = cbind(self$blockGen, frameOut)
                                                    # }
                                                    if (!is.null(self$block) && nrow(self$block) > 0) {
                                                      frameOut = cbind(self$block, frameOut)
                                                    }
                                                    if (!is.null(self$runOrder) && nrow(self$runOrder) > 0) {
                                                      frameOut = cbind(self$runOrder, frameOut)
                                                    }
                                                    if (!is.null(self$standardOrder) && nrow(self$standardOrder) > 0) {
                                                      frameOut = cbind(self$standardOrder, frameOut)
                                                    }
                                                    if (!is.null(self$response) && nrow(frameOut) == nrow(self$response))
                                                      frameOut = cbind(frameOut, self$response)
                                                    else {
                                                      temp = as.data.frame(matrix(NA, nrow = nrow(frameOut), ncol = ncol(self$response)))
                                                      names(temp) = names(self$response)
                                                      frameOut = cbind(frameOut, temp)
                                                    }
                                                    runIndex = order(self$runOrder[,1])
                                                    out = frameOut[runIndex, ]
                                                    return(out)
                                                  },

                                                  #' @description Retrieves elements from the factorial design object.
                                                  #' @param i Row index.
                                                  #' @param j Column index.
                                                  get = function(i,j){
                                                    return(self$as.data.frame()[i, j])
                                                  },

                                                  #' @description Get or set the lower bounds of the factors in the factorial design.
                                                  #' @param value Numeric vector with new lower bounds. If missing, retrieves the current lower bounds.
                                                  lows = function(value){
                                                    if (missing(value)) {
                                                      listOut = vector(mode = "list")
                                                      for (i in seq(along = self$factors)) {
                                                        listOut[self$factors[[i]]$name] = self$factors[[i]]$.low()
                                                      }
                                                      return(listOut)
                                                    }
                                                    else {
                                                      for (i in seq(along = self$factors)) {
                                                        self$factors[[i]]$.low(value[i])
                                                      }
                                                      invisible(self)
                                                    }
                                                  },

                                                  #' @description Get or set the upper bounds of the factors in the factorial design.
                                                  #' @param value Numeric vector with new upper bounds. If missing, retrieves the current upper bounds.
                                                  highs = function(value){
                                                    if (missing(value)) {
                                                      listOut = vector(mode = "list")
                                                      for (i in seq(along = self$factors)) {
                                                        listOut[self$factors[[i]]$name] = self$factors[[i]]$.high()
                                                      }
                                                      return(listOut)
                                                    }
                                                    else {
                                                      for (i in seq(along = self$factors)) {
                                                        self$factors[[i]]$.high(value[i])
                                                      }
                                                      invisible(self)
                                                    }
                                                  },

                                                  #' @description Prints a summary of the factors attributes including their low, high, name, unit, and type.
                                                  .nfp = function(){
                                                    x = self$factors
                                                    atr <- c('low','high','name','unit','type')
                                                    if (is.list(x) && length(x[[1]]) > 0) {
                                                      numAttr = length(x[[1]]$attributes())
                                                      .numFac = length(x)
                                                      frameOut = data.frame(matrix(ncol = .numFac, nrow = numAttr ))
                                                      for (i in 1:numAttr ) {
                                                        charVec = character(0)
                                                        for (j in 1:.numFac) {
                                                          charVec = c(charVec, atr[i], "\t\t")
                                                          frameOut[i, j] = x[[j]]$attributes()[i]
                                                        }
                                                      }
                                                      names(frameOut) = self$names()
                                                      rownames(frameOut) = atr[1:numAttr ]
                                                    }
                                                    else {
                                                      stop("no list given or length of list < 1")
                                                    }
                                                    print(frameOut)
                                                  },

                                                  #' @description Returns the factorial design object itself, used to verify or return the object.
                                                  identity = function(){
                                                    identity = character(0)
                                                    identityList = vector(mode = "list", length = 0)
                                                    resolution = numeric(0)
                                                    temp = NULL
                                                    A = aliasTable(self, print = FALSE)
                                                    if (any(dim(A) == 0))
                                                      return(identityList)
                                                    temp = as.matrix(A["Identity", ])
                                                    boolTemp = apply(temp, 2, as.logical)
                                                    identity = row.names(temp)[boolTemp[, 1]]
                                                    if (length(identity) > 0) {
                                                      charList = strsplit(toupper(identity), split = "")
                                                      identityList = lapply(charList, match, .NAMES[1:25])
                                                      names(identityList) = identity
                                                    }
                                                    cat("Defining relations:\n")
                                                    if (length(identityList) > 0) {
                                                      for (i in 1:length(identityList)) {
                                                        identLen = length((strsplit(names(identityList)[i], split = character(0))[[1]]))
                                                        if (length(resolution) == 0 || identLen > resolution)
                                                          resolution = c(resolution, identLen)
                                                        cat("I = ", names(identityList)[i], "\t\tColumns:", identityList[[i]], "\n")
                                                      }
                                                      cat("\nResolution: ", as.character(as.roman(min(resolution))), "\n")
                                                    }
                                                    invisible(identityList)
                                                  },

                                                  #' @description Summarizes the factorial design object.
                                                  summary = function(){
                                                    doeFactors = self$factors
                                                    cat("Information about the factors:\n\n")
                                                    self$.nfp()
                                                    cat("-----------\n")
                                                    print(self$as.data.frame())
                                                    temp = aliasTable(self, print = FALSE)
                                                    if (ncol(temp) > 0) {
                                                      cat("\n---------\n\n")
                                                      self$identity()
                                                      cat("\n")
                                                    }
                                                    invisible(self$as.data.frame())
                                                  },

                                                  #' @description Get or set the response data in the factorial design object.
                                                  #' @param value Data frame or numeric vector with new responses. If missing, retrieves the current responses.
                                                  .response = function(value){
                                                    if(missing(value)){
                                                      iIntern <- order(self$runOrder[,1])
                                                      out <- data.frame(self$response[iIntern,])
                                                      names(out) <- names(self$response)
                                                      return(out)
                                                    }
                                                    else{
                                                      index = order(self$runOrder[,1])
                                                      if (!is.vector(value) && !is.data.frame(value))
                                                        stop("vector or data.frame expected!")
                                                      if (is.vector(value) && (is.numeric(value) || is.na(value))) {
                                                        if (nrow(self$response) != length(value))
                                                          stop(paste("Number of rows for Design does not equal length of vector ", nrow(object),
                                                                     " != ", length(value), " "))
                                                        self$response <- data.frame(value)
                                                        self$response[index, ] <- value
                                                        names(self$response) <- make.names(deparse(substitute(value)))
                                                        invisible(self)
                                                      }
                                                      if (is.data.frame(value)) {
                                                        self$response <- value
                                                        self$response[index, ] <- value
                                                        invisible(self)
                                                      }

                                                    }

                                                  },

                                                  #' @description Plots the effects of factors on the response variables.
                                                  #' @param factors Factors to be plotted.
                                                  #' @param fun Function applied to the response variables (e.g., mean).
                                                  #' @param response Optional; specifies which response variables to plot.
                                                  #' @param single Logical; if TRUE, plots effects for single factor; otherwise, for combinations of factors.
                                                  #' @param points Logical; if TRUE, plots data points.
                                                  #' @param classic Logical; if TRUE, uses classic plotting style.
                                                  #' @param axes Logical; if TRUE, includes axes in the plot.
                                                  #' @param lty Line type for plotting.
                                                  #' @param xlab Label for the x-axis.
                                                  #' @param ylab Label for the y-axis.
                                                  #' @param main Main title for the plot.
                                                  #' @param ylim Limits for the y-axis.
                                                  #' @param ... Additional plotting parameters.
                                                  effectPlot = function(factors, fun = mean, response = NULL, single = FALSE, points = FALSE, classic = FALSE, axes = TRUE, ###
                                                                        lty, xlab, ylab, main, ylim, ...) {

                                                    if(is.null(response)==FALSE)
                                                    {                                                                           ###
                                                      temp=self$.response()[response]                                            ###
                                                      self$.response(temp)                                                      ###
                                                    }
                                                    ylabmiss = FALSE
                                                    xlabmiss = FALSE
                                                    mainmiss = FALSE
                                                    ylimmiss = FALSE
                                                    if (missing(ylim))
                                                      ylimmiss = TRUE
                                                    if (missing(lty))
                                                      lty = 1
                                                    X = self$cube
                                                    Y = as.data.frame(self$response[1:nrow(X), ])
                                                    names(Y) = names(self$.response())
                                                    if (!missing(factors))
                                                      k = length(factors)
                                                    else #(missing(factors))                                                    ###
                                                    {
                                                      k = ncol(X)
                                                      factors = names(X)
                                                    }
                                                    numCol = 1
                                                    numRow = 1
                                                    if (!single && missing(factors)) {                                          ###
                                                      if (ncol(X) == 2) {
                                                        numCol = 2
                                                        numRow = 1
                                                      }
                                                      if (ncol(X) > 2) {
                                                        numCol = 2
                                                        numRow = 2
                                                      }
                                                    }
                                                    if (!single && !missing(factors)) {                                         ###
                                                      if (length(factors) == 2) {                                             ###
                                                        numCol = 2                                                          ###
                                                        numRow = 1                                                          ###
                                                      }                                                                       ###
                                                      if (length(factors) == 3) {                                             ###
                                                        numCol = 3                                                          ###
                                                        numRow = 1                                                          ###
                                                      }                                                                       ###
                                                      if (length(factors) == 4) {                                             ###
                                                        numCol = 2                                                          ###
                                                        numRow = 2                                                          ###
                                                      }                                                                       ###
                                                      if (length(factors) == 5) {                                             ###
                                                        numCol = 3                                                          ###
                                                        numRow = 2                                                          ###
                                                      }                                                                       ###
                                                      if (length(factors) == 6) {                                             ###
                                                        numCol = 3                                                          ###
                                                        numRow = 2                                                          ###
                                                      }                                                                       ###
                                                      if (length(factors) > 6) {                                              ###
                                                        numRow = ceiling(sqrt(length(factors)))                             ###
                                                        numCol = ceiling(sqrt(length(factors)))                             ###
                                                      }                                                                       ###
                                                    }                                                                           ###
                                                    if (classic) {
                                                      numCol = ncol(X)
                                                      numRow = 1
                                                    }
                                                    if (!single)
                                                      par(mfrow = c(numRow, numCol))
                                                    nextResponse = FALSE
                                                    if (missing(main)) {
                                                      main = paste("Effect Plot for", names(Y)[1])
                                                    }

                                                    list_plot <- list()
                                                    #############################################################################
                                                    for (j in 1:ncol(Y)){
                                                      counter = 0
                                                      cells = numeric(0)
                                                      for (i in 1:length(factors)){
                                                        cells = c(cells, as.vector(tapply(Y[, j], list(X[, factors[i]], rep(0, nrow(X))), fun)))
                                                        if (points)
                                                          cells = range(Y)
                                                      }
                                                      if (nextResponse & !single) {
                                                        dev.new()
                                                        par(mfrow = c(numRow, numCol))
                                                      }

                                                      # hacemos la primera afuera para ajustar los ejes
                                                      # 1. ------------------------------------
                                                      i <- 1
                                                      if ((counter != 0 & counter%%(numCol * numRow) == 0) & !single) {
                                                        dev.new()
                                                        par(mfrow = c(numRow, numCol))
                                                      }
                                                      if (missing(main)) {
                                                        main = paste("Effect Plot for", names(Y)[j])
                                                        mainmiss = TRUE
                                                      }
                                                      if (missing(xlab)) {
                                                        xlab = factors[i]
                                                        xlabmiss = TRUE
                                                      }
                                                      if (xlabmiss) {
                                                        if (identical(" ", self$names()[[i]]))
                                                          xlab = factors[i]
                                                        else xlab = paste(factors[i], ": ", self$names()[[i]], sep = "")
                                                      }
                                                      if (missing(ylab)) {
                                                        ylab = paste(deparse(substitute(fun)), "of ", names(Y)[j])
                                                        ylabmiss = TRUE
                                                      }
                                                      if (ylabmiss)
                                                        ylab = paste(deparse(substitute(fun)), "of ", names(Y)[j])
                                                      if (ylimmiss)
                                                        ylim = range(cells, na.rm = TRUE)
                                                      if (classic) {
                                                        p <- .m.interaction.plot(x.factor = X[, factors[i]], trace.factor = rep(0, nrow(X)), response = Y[, j], lty = lty, ylim = ylim, xlab = xlab, fun = fun,
                                                                                 ylab = ylab, main = " ", ...)
                                                        list_plot[[paste0("p",j,i)]] <- p$plot
                                                      }
                                                      else {
                                                        p <- .m.interaction.plot(x.factor = X[, factors[i]], trace.factor = rep(0, nrow(X)), response = Y[, j], lty = lty, ylim = ylim, xlab = xlab, fun = fun,
                                                                                 ylab = ylab, main = main, ...)
                                                        list_plot[[paste0("p",j,i)]] <- p$plot
                                                      }
                                                      counter = counter + 1
                                                      # 2. ------------------------------------
                                                      if(length(factors) >= 2){
                                                        for (i in 2:length(factors)) {
                                                          if ((counter != 0 & counter%%(numCol * numRow) == 0) & !single) {
                                                            dev.new()
                                                            par(mfrow = c(numRow, numCol))
                                                          }
                                                          if (missing(main)) {
                                                            main = paste("Effect Plot for", names(Y)[j])
                                                            mainmiss = TRUE
                                                          }
                                                          if (missing(xlab)) {
                                                            xlab = factors[i]
                                                            xlabmiss = TRUE
                                                          }
                                                          if (xlabmiss) {
                                                            if (identical(" ", self$names()[[i]]))
                                                              xlab = factors[i]
                                                            else xlab = paste(factors[i], ": ", self$names()[[i]], sep = "")
                                                          }
                                                          if (missing(ylab)) {
                                                            ylab = paste(deparse(substitute(fun)), "of ", names(Y)[j])
                                                            ylabmiss = TRUE
                                                          }
                                                          if (ylabmiss)
                                                            ylab = paste(deparse(substitute(fun)), "of ", names(Y)[j])
                                                          if (ylimmiss)
                                                            ylim = range(cells, na.rm = TRUE)
                                                          if (classic) {
                                                            aux <- .m.interaction.plot(x.factor = X[, factors[i]], trace.factor = rep(0, nrow(X)), response = Y[, j], lty = lty, ylim = ylim, xlab = xlab, fun = fun,
                                                                                       ylab = ylab, axes.y = FALSE, ytitle = FALSE , main = " ", ...)
                                                            list_plot[[paste0("p",j,i)]] <- aux$plot
                                                          }
                                                          else {
                                                            aux <- .m.interaction.plot(x.factor = X[, factors[i]], trace.factor = rep(0, nrow(X)), response = Y[, j], lty = lty, ylim = ylim, xlab = xlab, fun = fun,
                                                                                       ylab = ylab, main = main, ytitle = TRUE, ...)
                                                            list_plot[[paste0("p",j,i)]] <- aux$plot
                                                          }
                                                          counter = counter + 1
                                                        }
                                                      }
                                                      nextResponse = TRUE
                                                    }
                                                    # Obtener los nombres de todas las graficas que se crearon
                                                    grap <- c()
                                                    for(j in 1:ncol(Y)){
                                                      for(i in 1:length(factors)){
                                                        x <- paste0("p",j,i)
                                                        if(!x %in% grap){
                                                          grap <- c(grap, x)
                                                        }
                                                      }
                                                    }

                                                    p <- list_plot$p11
                                                    for(i in 2:length(grap)){
                                                      aux <- grap[i]
                                                      p <- p + list_plot[[aux]]
                                                    }

                                                    if(classic){
                                                      p <- p + plot_layout(ncol = numCol, nrow = numRow) +
                                                        plot_annotation(title = main,
                                                                        theme = theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold")))
                                                      print(p)
                                                    }
                                                    else{print(p)}
                                                    par(mfcol=c(1,1))
                                                  },

                                                  #' @description Fits a linear model to the response data in the factorial design object.
                                                  #' @param formula Formula specifying the model to be fitted.
                                                  lm = function(formula){
                                                    invisible(lm(formula, data = self$as.data.frame()))

                                                  },

                                                  #' @description Get or set the desirability values for the response variables.
                                                  #' @param value List of new desirability values. If missing, retrieves the current desirability values.
                                                  desires = function(value){
                                                    if (missing(value)) {
                                                      return(self$desirability)
                                                    }
                                                    else{
                                                      if (!any(value$response == names(self$.response())))
                                                        stop(paste(value$response, "is not a response!"))
                                                      listPos = length(self$desirability) + 1
                                                      yName = value$response
                                                      isIn = (yName == names(self$desirability))
                                                      if (any(isIn))
                                                        listPos = (1:length(names(self$desirability)))[isIn]
                                                      self$desirability[[listPos]] = value
                                                      names(self$desirability)[listPos] = yName
                                                      invisible(self)
                                                    }

                                                  },

                                                  #' @description Set the fits for the response variables in the factorial design object.
                                                  #' @param value New fits.
                                                  set.fits = function(value){
                                                    if (!identical(class(value), "lm"))
                                                      stop(paste(deparse(substitute(value)), "needs to an object of class lm"))
                                                    if (!any(names(value$model)[1] == names(self$.response())))
                                                      stop(paste("fitted response", names(value$model)[1], "could not be found in", deparse(substitute(x))))
                                                    listPos = length(self$fits) + 1
                                                    yName = names(value$model)[1]
                                                    isIn = (yName == names(self$fits))
                                                    if (any(isIn))
                                                      listPos = (1:length(names(self$fits)))[isIn]
                                                    self$fits[[listPos]] = value
                                                    names(self$fits)[listPos] = yName
                                                    invisible(self)

                                                  },

                                                  #' @description Get or set the types of designs used in the factorial design object.
                                                  #' @param value New design types. If missing, retrieves the current types.
                                                  types = function(value){
                                                    if (missing(value)) {
                                                      v <- list()
                                                      for (i in 1:length(self$factors)) {
                                                        v[[self$names()[i]]] <- self$factors[[i]]$.type()
                                                      }
                                                      return(v)

                                                    }
                                                    else{
                                                      for (i in 1:length(self$factors)) {
                                                        if (!identical(value[i], "numeric") & !identical(value[i], "factor"))
                                                          stop(paste(value[i], "\ttype of factor needs to be 'numeric' or 'factor'"))
                                                        self$factors[[i]]$.type(as.character(value[i]))
                                                      }
                                                      invisible(self)
                                                    }
                                                  },

                                                  #' @description Get or set the units for the factors in the factorial design object.
                                                  #' @param value New units. If missing, retrieves the current units.
                                                  unit = function(value){
                                                    if (missing(value)) {
                                                      v <- list()
                                                      for (i in 1:length(self$factors)) {
                                                        v[[self$names()[i]]] <- self$factors[[i]]$.unit()
                                                      }
                                                      return(v)
                                                    }
                                                    else{
                                                      for (i in 1:length(self$factors)) if (length(value) > 1)
                                                        self$factors[[i]]$.unit(as.character(value[i]))
                                                      else self$factors[[i]]$.unit(as.character(value[1]))
                                                      invisible(self)
                                                    }

                                                  },

                                                  #' @description Get or set the star points in the factorial design object.
                                                  #' @param value New star points. If missing, retrieves the current star points.
                                                  .star = function(value){
                                                    if (missing(value)) {
                                                      return(self$star)
                                                    }
                                                    else{
                                                      if (!is.data.frame(value))
                                                        stop("data.frame must be provided!")
                                                      if (.numFac(self) != ncol(value))
                                                        stop("number of columns not matching!")
                                                      if (nrow(value) == 0) {
                                                        return("TODO: remove star und Rest anpassen")
                                                      }
                                                      oldResponse = self$.response()
                                                      newDf = value
                                                      oldDf = self$star
                                                      numNewRow = nrow(newDf) - nrow(oldDf)
                                                      oldOrd = self$standardOrder
                                                      oldRunOrd = self$runOrder
                                                      len = nrow(oldOrd)
                                                      lenFirst = nrow(self$cube) + nrow(self$centerCube)
                                                      self$standardOrder = data.frame(StandOrd = 1:(len + numNewRow))
                                                      newRunOrd = data.frame()
                                                      if (numNewRow > 0) {
                                                        newNums = data.frame(newNums = seq(max(oldRunOrd) + 1, max(oldRunOrd) + numNewRow, by = 1))

                                                        names(newNums) = names(oldRunOrd)
                                                        newRunOrd = data.frame(oldRunOrd[1:lenFirst, ])

                                                        names(newRunOrd) = names(oldRunOrd)
                                                        restFrame = data.frame(oldRunOrd[-c(1:lenFirst), ])
                                                        names(restFrame) = names(oldRunOrd)
                                                        newRunOrd = rbind(newRunOrd, newNums, restFrame)

                                                      }
                                                      else {
                                                        newRunOrd = data.frame(oldRunOrd[1:(lenFirst + nrow(newDf) + nrow(self$centerStar)), ])
                                                        names(newRunOrd) = names(oldRunOrd)
                                                      }
                                                      self$runOrder = newRunOrd
                                                      naFrame = as.data.frame(matrix(rep(NA, times = ncol(oldResponse) * nrow(newDf)), ncol = ncol(oldResponse)))
                                                      names(naFrame) = names(oldResponse)
                                                      newResponse = data.frame(oldResponse[1:lenFirst, ])
                                                      names(newResponse) = names(oldResponse)
                                                      restFrame = data.frame(oldResponse[-c(1:(lenFirst + nrow(oldDf))), ])
                                                      names(restFrame) = names(oldResponse)
                                                      newResponse = rbind(newResponse, naFrame, restFrame)
                                                      self$.response(newResponse)

                                                      oldBlockGen = self$blockGen
                                                      if (ncol(oldBlockGen) > 0) {

                                                        newBlockGen = data.frame(oldBlockGen[1:lenFirst, ])
                                                        names(newBlockGen) = names(self$blockGen)
                                                        naFrameGen = as.data.frame(matrix(rep(NA, times = ncol(self$blockGen) * nrow(newDf)), ncol = ncol(self$blockGen)))
                                                        names(naFrameGen) = names(oldBlockGen)
                                                        restBlockGen = data.frame(oldBlockGen[-c(1:(lenFirst + nrow(oldDf))), ])
                                                        names(restBlockGen) = names(oldBlockGen)
                                                        newBlockGen = rbind(newBlockGen, naFrameGen, restBlockGen)

                                                        self$.blockGen(newBlockGen)
                                                      }
                                                      oldBlock = self$block
                                                      newBlock = data.frame(oldBlock[1:lenFirst, ])
                                                      names(newBlock) = names(oldBlock)
                                                      naFrame = as.data.frame(matrix(rep(max(newBlock) + 1, times = ncol(oldBlock) * nrow(newDf)),
                                                                                     ncol = ncol(oldBlock)))
                                                      names(naFrame) = names(oldBlock)
                                                      restBlock = data.frame(oldBlock[-c(1:(lenFirst + nrow(oldDf))), ])
                                                      names(restBlock) = names(oldBlock)
                                                      newBlock = rbind(newBlock, naFrame, restBlock)
                                                      self$.block(newBlock)
                                                      self$star <- newDf
                                                      invisible(self)
                                                    }
                                                  },

                                                  #' @description Get or set the block generators in the factorial design object.
                                                  #' @param value New block generators. If missing, retrieves the current block generators.
                                                  .blockGen = function(value){
                                                    if (missing(value)) {
                                                      return(self$blockGen)
                                                    }
                                                    else{
                                                      if (!is.vector(value) && !is.data.frame(value))
                                                        stop("vector or data.frame expected!")
                                                      if (is.vector(value) && (is.numeric(value) || is.na(value))) {
                                                        if (self$nrow() != length(value))
                                                          stop(paste("Number of rows for Design does not equal length of vector ", self$nrow(),
                                                                     " != ", length(value), " "))
                                                        self$blockGen <- as.data.frame(value)
                                                        names(self$blockGen) = deparse(substitute(value))

                                                      }
                                                      if (is.data.frame(value)) {
                                                        self$blockGen <- value

                                                      }
                                                      invisible(self)

                                                    }

                                                  },

                                                  #' @description Get or set the blocks in the factorial design object.
                                                  #' @param value New blocks. If missing, retrieves the current blocks.
                                                  .block = function(value){
                                                    if (missing(value)) {
                                                      return(self$block)
                                                    }
                                                    else{
                                                      if (!is.vector(value) && !is.data.frame(value))
                                                        stop("vector or data.frame expected!")
                                                      if (is.vector(value) && (is.numeric(value) || is.na(value))) {
                                                        if (self$nrow() != length(value))
                                                          stop(paste("Number of rows for Design does not equal length of vector ", nrow(object),
                                                                     " != ", length(value), " "))
                                                        self$block <- as.data.frame(value)
                                                        names(self$block) = deparse(substitute(value))

                                                      }
                                                      if (is.data.frame(value)) {
                                                        self$block <- value

                                                      }
                                                      invisible(self)

                                                    }
                                                  },

                                                  #' @description Get or set the center points in the cube portion of the factorial design.
                                                  #' @param value New center points for the cube. If missing, retrieves the current center points.
                                                  .centerCube = function(value){
                                                    if (missing(value)) {
                                                      return(self$centerCube)
                                                    }
                                                    else{

                                                      if (!is.data.frame(value))
                                                        stop("data.frame must be provided!")
                                                      if (.numFac(self) != ncol(value))
                                                        stop("number of columns not matching!")
                                                      if (nrow(value) == 0) {
                                                        return("TODO: remove CenterCube und Rest anpassen")
                                                      }
                                                      newDf = value
                                                      lenCube = nrow(self$cube)
                                                      oldDf = self$centerCube
                                                      oldRunOrd = self$runOrder
                                                      oldResponse = self$.response()
                                                      blockValues = unique(self$block[1:nrow(self$cube), ])
                                                      numBlocks = length(blockValues)
                                                      if (numBlocks > 1)
                                                        for (i in 1:(numBlocks - 1)) {
                                                          newDf = rbind(newDf, value)
                                                        }

                                                      numNewRow = nrow(newDf) - nrow(oldDf)
                                                      oldOrd = self$standardOrder
                                                      len = nrow(oldOrd)
                                                      self$standardOrder = data.frame(StandOrd = 1:(len + numNewRow))
                                                      newRunOrd = data.frame()
                                                      if (numNewRow > 0) {
                                                        newNums = data.frame(newNums = seq(max(oldRunOrd) + 1, max(oldRunOrd) + numNewRow, by = 1))
                                                        names(newNums) = names(oldRunOrd)

                                                        newRunOrd = data.frame(oldRunOrd[1:lenCube, ])
                                                        names(newRunOrd) = names(oldRunOrd)
                                                        restRunOrd = data.frame(oldRunOrd[-c(1:lenCube), ])
                                                        names(restRunOrd) = names(oldRunOrd)
                                                        newRunOrd = rbind(newRunOrd, newNums, restRunOrd)

                                                        self$runOrder = newRunOrd
                                                      }
                                                      else {
                                                        newRunOrd = data.frame(oldRunOrd[1:(lenCube + nrow(newDf)), ])
                                                        names(newRunOrd) = names(oldRunOrd)
                                                        restRunOrd = data.frame(oldRunOrd[-c(1:(lenCube + nrow(oldDf))), ])
                                                        names(restRunOrd) = names(oldRunOrd)
                                                        newRunOrd = rbind(newRunOrd, restRunOrd)

                                                        self$runOrder = newRunOrd
                                                      }
                                                      naFrame = as.data.frame(matrix(rep(NA, times = ncol(oldResponse) * nrow(newDf)), ncol = ncol(oldResponse)))
                                                      names(naFrame) = names(oldResponse)
                                                      newResponse = data.frame(oldResponse[1:lenCube, ])
                                                      names(newResponse) = names(oldResponse)
                                                      restResponse = data.frame(oldResponse[-c(1:(lenCube + nrow(oldDf))), ])
                                                      names(restResponse) = names(oldResponse)
                                                      newResponse = rbind(newResponse, naFrame, restResponse)

                                                      self$.response(newResponse)
                                                      oldBlockGen = self$blockGen
                                                      if (ncol(oldBlockGen) > 0) {

                                                        newBlockGen = data.frame(oldBlockGen[1:lenCube, ])
                                                        names(newBlockGen) = names(self$blockGen)
                                                        naFrameGen = as.data.frame(matrix(rep(NA, times = ncol(self$blockGen) * nrow(newDf)), ncol = ncol(self$blockGen)))
                                                        names(naFrameGen) = names(oldBlockGen)
                                                        restFrame = data.frame(oldBlockGen[-c(1:(lenCube + nrow(oldDf))), ])
                                                        names(restFrame) = names(self$blockGen)
                                                        newBlockGen = rbind(newBlockGen, naFrameGen, restFrame)
                                                        self$.blockGen(newBlockGen)

                                                      }
                                                      oldBlock = self$block
                                                      newBlock = data.frame(oldBlock[1:lenCube, ])
                                                      names(newBlock) = names(self$block)
                                                      naFrame = as.data.frame(matrix(rep(blockValues, times = nrow(newDf)/numBlocks), ncol = 1))
                                                      restFrame = as.data.frame(oldBlock[-c(1:(lenCube + nrow(oldDf))), ])
                                                      names(restFrame) = names(self$block)

                                                      names(naFrame) = names(oldBlock)
                                                      newBlock = rbind(newBlock, naFrame, restFrame)
                                                      self$.block(newBlock)
                                                      self$centerCube <- newDf
                                                      invisible(self)
                                                    }

                                                  },

                                                  #' @description Get or set the center points in the star portion of the factorial design.
                                                  #' @param value New center points for the star. If missing, retrieves the current center points.
                                                  .centerStar = function(value){
                                                    if (missing(value)) {
                                                      return(self$centerStar)
                                                    }
                                                    else{

                                                      if (!is.data.frame(value))
                                                        stop("data.frame must be provided!")
                                                      if (.numFac(self) != ncol(value))
                                                        stop("number of columns not matching!")
                                                      if (nrow(value) == 0) {
                                                        return("TODO: remove CenterCube und Rest anpassen")
                                                      }
                                                      newDf = value
                                                      oldDf = self$centerStar
                                                      numNewRow = nrow(newDf) - nrow(oldDf)
                                                      oldResponse = self$.response()
                                                      lenRest = nrow(self$cube) + nrow(self$centerCube) + nrow(self$star)
                                                      oldRunOrd = self$runOrder
                                                      oldOrd = self$standardOrder
                                                      len = nrow(oldOrd)
                                                      self$standardOrder = data.frame(StandOrd = 1:(len + numNewRow))
                                                      newRunOrd = data.frame(oldRunOrd[1:lenRest, ])
                                                      names(newRunOrd) = names(oldRunOrd)
                                                      if (numNewRow > 0) {
                                                        newNums = data.frame(newNums = seq(max(oldRunOrd) + 1, max(oldRunOrd) + numNewRow, by = 1))
                                                        names(newNums) = names(oldRunOrd)
                                                        restFrame = data.frame(oldRunOrd[-c(1:lenRest), ])
                                                        names(restFrame) = names(oldRunOrd)
                                                        newRunOrd = rbind(newRunOrd, newNums, restFrame)

                                                        self$runOrder = newRunOrd
                                                      }
                                                      else {
                                                        newRunOrd = data.frame(oldRunOrd[1:(lenRest + nrow(newDf)), ])
                                                        names(newRunOrd) = names(oldRunOrd)
                                                        self$runOrder = newRunOrd
                                                      }
                                                      naFrame = as.data.frame(matrix(rep(NA, times = ncol(oldResponse) * nrow(newDf)), ncol = ncol(oldResponse)))
                                                      names(naFrame) = names(oldResponse)
                                                      newResponse = data.frame(oldResponse[1:lenRest, ])
                                                      names(newResponse) = names(self$.response())
                                                      newResponse = rbind(newResponse, naFrame)

                                                      self$.response(newResponse)

                                                      oldBlockGen = self$blockGen
                                                      if (ncol(oldBlockGen) > 0) {
                                                        print("TODO: BlockGen Spalte(n) anpassen")
                                                        newBlockGen = data.frame(oldBlockGen[1:lenRest, ])
                                                        names(newBlockGen) = names(self$blockGen)
                                                        naFrameGen = as.data.frame(matrix(rep(NA, times = ncol(self$blockGen) * nrow(newDf)), ncol = ncol(self$block)))
                                                        names(naFrameGen) = names(oldBlockGen)
                                                        restBlockGen = data.frame(oldBlockGen[-c(1:(lenRest + nrow(oldDf))), ])
                                                        names(restBlockGen) = names(oldBlockGen)
                                                        newBlockGen = rbind(newBlockGen, naFrameGen, restBlockGen)

                                                        self$.blockGen(newBlockGen)
                                                      }
                                                      oldBlock = self$block
                                                      newBlock = data.frame(oldBlock[1:lenRest, ])
                                                      names(newBlock) = names(self$block)
                                                      naFrame = as.data.frame(matrix(rep(max(self$block[1:nrow(self$cube), ]) + 1, times = ncol(self$block) *
                                                                                           nrow(newDf)), ncol = ncol(self$block)))
                                                      names(naFrame) = names(oldBlock)
                                                      restBlock = data.frame(oldBlock[-c(1:(lenRest + nrow(oldDf))), ])
                                                      names(restBlock) = names(oldBlock)
                                                      newBlock = rbind(newBlock, naFrame, restBlock)
                                                      self$.block(newBlock)
                                                      self$centerStar <- newDf
                                                      invisible(self)

                                                    }

                                                  },

                                                  #' @description Get or set the generators for the factorial design.
                                                  #' @param value New generators. If missing, retrieves the current generators.
                                                  .generators = function(value){
                                                    if (missing(value)) {
                                                      return(self$generator)

                                                    }
                                                    else{
                                                      self$generator <- value
                                                      invisible(self)
                                                    }
                                                  }




)
)

# Clase doeFactor ----
#' @title doeFactor-class: Class "doeFactor"
#' @description An R6 class representing a factor in a design of experiments (DOE).
#' @field low Numeric value specifying the lower bound of the factor. Default is `-1`.
#' @field high Numeric value specifying the upper bound of the factor. Default is `1`.
#' @field name Character string specifying the name of the factor. Default is an empty string \code{""}.
#' @field unit Character string specifying the unit of measurement for the factor. Default is an empty string \code{""}.
#' @field type Character string specifying the type of the factor. Can be either \code{"numeric"} or \code{"categorical"}. Default is \code{"numeric"}.
#' @seealso \code{\link{taguchiFactor}}
doeFactor <- R6Class('doeFactor', public = list(low = -1,
                                                high = 1,
                                                name = "",
                                                unit = "",
                                                type = "numeric",

                                                #' @description Get the attributes of the factor.
                                                attributes = function(){
                                                  v <- c(self$low, self$high, self$name, self$unit, self$type)
                                                },

                                                #' @description Get and set the lower bound for the factor.
                                                #' @param value Numeric value to set as the lower bound. If missing, the current lower bound is returned.
                                                .low = function(value){
                                                  if (missing(value)) {
                                                    return(unlist(self$low))
                                                  }
                                                  else{
                                                    boolOld = is.numeric(unlist(self$low))
                                                    self$low <- value
                                                    boolNew = is.numeric(self$low)
                                                    if (boolNew)
                                                      self$type = "numeric"
                                                    else self$type = "factor"
                                                    if (boolOld != boolNew)
                                                      print("Note: The types of the factors were changed!")
                                                    invisible(self)
                                                  }
                                                },

                                                #' @description Get and set the upper bound for the factor.
                                                #' @param value Numeric value to set as the upper bound. If missing, the current upper bound is returned.
                                                .high = function(value){
                                                  if (missing(value)) {
                                                    return(unlist(self$high))
                                                  }
                                                  else{
                                                    boolOld = is.numeric(unlist(self$high))
                                                    self$high <- value
                                                    boolNew = is.numeric(self$high)
                                                    if (boolNew)
                                                      self$type = "numeric"
                                                    else self$type = "factor"
                                                    if (boolOld != boolNew)
                                                      print("Note: The types of the factors were changed!")
                                                    invisible(self)
                                                  }
                                                },

                                                #' @description Get and set the type of the factor.
                                                #' @param value Character string specifying the type of the factor. Can be `"numeric"` or `"categorical"`. If missing, the current type is returned.
                                                .type = function(value){
                                                  if (missing(value)) {
                                                    return(self$type)
                                                  }
                                                  else{
                                                    self$type <- value
                                                    invisible(self)
                                                  }
                                                },

                                                #' @description Get and set the unit of measurement for the factor.
                                                #' @param value Character string specifying the unit of measurement. If missing, the current unit is returned.
                                                .unit = function(value){
                                                  if (missing(value)){
                                                    return(self$unit)
                                                  }
                                                  else{
                                                    self$unit <- value
                                                    invisible(self)
                                                  }
                                                },

                                                #' @description Get and set the name of the factor.
                                                #' @param value Character string specifying the name of the factor. If missing, the current name is returned.
                                                names = function(value){
                                                  if (missing(value)) {
                                                    return(self$name)
                                                  }
                                                  else {
                                                    self$name <- value
                                                    invisible(self)
                                                  }
                                                },

                                                #' @description Print the characteristics of the factors.
                                                print = function(){
                                                  cat("Name: ", self$names(), "\n")
                                                  cat("low Setting: ", self$.low(), "\n")
                                                  cat("high setting: ", self$.high(), "\n")
                                                  cat("Unit: ", self$.unit(), "\n")
                                                  cat("type: ", self$.type(), "\n")
                                                  cat("\n")
                                                }

)
)




# Clase desirability.c ----
#' @title desirability-class: Class "desirability"
#' @description A class representing the desirability metrics for responses in a design.
#' @field response A numeric vector specifying the responses for which desirability is calculated.
#' @field low A numeric vector representing the lower bounds of the desirable range for each response.
#' @field high A numeric vector representing the upper bounds of the desirable range for each response.
#' @field target A numeric vector or character string indicating the target values or goals for each response.
#' @field scale A numeric vector specifying the scaling factors used in the desirability calculation.
#' @field importance A numeric vector indicating the importance of each response in the desirability calculation.
#' @seealso \code{\link{desirability}}, \code{\link{overall}}, \code{\link{optimum}}
desirability.c <- R6Class("desirability", public = list(response = NULL,
                                                        low = NULL,
                                                        high = NULL,
                                                        target = NULL,
                                                        scale = NULL,
                                                        importance = NULL,

                                                        #' @description Initializes a new \code{desirability.c} object with specified parameters.
                                                        #' @param response A numeric or character vector specifying the responses for which desirability is calculated.
                                                        #' @param low A numeric vector representing the lower bounds of the desirable range for each response.
                                                        #' @param high A numeric vector representing the upper bounds of the desirable range for each response.
                                                        #' @param target A numeric vector or character string indicating the target values or goals for each response.
                                                        #' @param scale A numeric vector specifying the scaling factors used in the desirability calculation.
                                                        #' @param importance A numeric vector indicating the importance of each response in the desirability calculation.
                                                        initialize = function(response=NULL, low=NULL, high=NULL, target=NULL, scale=NULL, importance=NULL) {
                                                          self$response <- response
                                                          self$low <- low
                                                          self$high <- high
                                                          self$target <- target
                                                          self$scale <- scale
                                                          self$importance <- importance
                                                        },

                                                        #' @description Prints the details of a \code{desirability.c} object.
                                                        print = function(){
                                                          if (!is.numeric(self$target))
                                                            cat("Target is to", paste(self$target, "imize", sep = ""), self$response, "\n")
                                                          else cat("Target is ", self$target, " for", self$response, "\n")
                                                          cat("lower Bound: ", self$low, "\n")
                                                          cat("higher Bound: ", self$high, "\n")
                                                          if (is.numeric(self$target))
                                                            cat("Scale factor is: low =", self$scale[1], "and high =", self$scale[2], "\n")
                                                          else if (identical("min", self$target) | identical("max", self$target))
                                                            cat("Scale factor is: ", self$scale, "\n")
                                                          cat("importance: ", self$importance, "\n")
                                                          cat("\n")
                                                        },

                                                        #' @description Plots the desirability functions based on the specified parameters.
                                                        #' @param y A numeric vector or data frame representing the responses to plot.
                                                        #' @param scale A numeric vector specifying the scaling factors used in the plot.
                                                        #' @param main A character string specifying the main title of the plot.
                                                        #' @param xlab A character string specifying the label for the x-axis.
                                                        #' @param ylab A character string specifying the label for the y-axis.
                                                        #' @param line.width A numeric value specifying the width of the plot lines.
                                                        #' @param col A vector of colors for the plot lines.
                                                        #' @param numPoints An integer specifying the number of points to plot (default is 500).
                                                        plot = function(y, scale, main, xlab, ylab, line.width, col, numPoints = 500){
                                                          xm1 = NULL
                                                          xm2 = NULL
                                                          ym = NULL
                                                          y = NULL
                                                          if (missing(main))
                                                            main = paste("Desirability function for", self$response)
                                                          if (missing(xlab))
                                                            xlab = self$response
                                                          if (missing(ylab))
                                                            ylab = "Desirability"
                                                          if (missing(line.width))
                                                            line.width = 0.75
                                                          if (missing(scale))
                                                            scale = self$scale
                                                          if (missing(col))
                                                            col = "red"
                                                          dFun = .desireFun(self$low, self$high, self$target, self$scale, self$importance)
                                                          xVals = seq(self$low - 0.04 * diff(range(self$low, self$high)), self$high + 0.04 * diff(range(self$low, self$high)), length = numPoints)
                                                          yVals = dFun(xVals)

                                                          df <- data.frame(X = xVals, Y = yVals)

                                                          p <- ggplot(df, aes(x = X, y = Y)) +
                                                            geom_line(color = col, size = line.width) +
                                                            labs(title = main, x = xlab, y = ylab) +
                                                            theme_minimal() +
                                                            theme(plot.title = element_text(hjust = 0.5))
                                                          # annotate("text", x = (mean(range(df$X)) * 1.05), y = mean(range(df$Y)), label = "scale = 1", size = 5) # Añadir la etiqueta en el centro

                                                          if (is.numeric(self$target)) {
                                                            xm1 <- mean(c(min(xVals), self$target))
                                                            xm2 <- mean(c(max(xVals), self$target))
                                                            ym1 <- df$Y[which.min(abs(df$X - xm1))]
                                                            ym2 <- df$Y[which.min(abs(df$X - xm2))]
                                                            p <- p +
                                                              annotate("text", x = xm1*1.05, y = ym1, label = paste("scale =", scale[1])) +
                                                              annotate("text", x = xm2*0.95, y = ym2, label = paste("scale =", scale[2]))
                                                          } else {
                                                            xm1 <- mean(range(xVals))
                                                            ym1 <- df$Y[which.min(abs(df$X - xm1))]
                                                            if (identical(self$target, "max")) {
                                                              p <- p +
                                                                annotate("text", x = xm1*1.05, y = ym1, label = paste("scale =", scale[1]))
                                                            } else {
                                                              p <- p +
                                                                annotate("text", x = xm1*1.05, y = ym1, label = paste("scale =", scale[1]))
                                                            }
                                                          }
                                                          print(p)
                                                          out = list(x = xVals, y = yVals)
                                                          names(out) = c(self$response, "desirability")
                                                          invisible(out)
                                                        }
)
)

# Clase steepAscent.c ----
#' @title steepAscent-class: Class "steepAscent"
#' @description The \code{steepAscent.c} class represents a steepest ascent algorithm in a factorial design context. This class is used for optimizing designs based on iterative improvements.
#' @field name A character string representing the name of the steep ascent design.
#' @field X A data frame containing the design matrix for the steepest ascent procedure. This matrix represents the factors and their levels at each iteration.
#' @field response A data frame containing the response values associated with the design matrix.
#' @seealso \code{\link{steepAscent}}, \code{\link{desirability.c}}, \code{\link{optimum}}
steepAscent.c <- R6Class("facDesign", public = list(name = NULL,
                                                    X = data.frame(),
                                                    response = data.frame(),

                                                    #' @description Get and set the `response` values in an object of class `steepAscent.c`.
                                                    #' @param value A data frame or numeric vector to set as the new `response`. If missing, returns the current `response`.
                                                    .response = function(value){
                                                      if (missing(value)) {
                                                        return(self$response)
                                                      }
                                                      else{
                                                        if (is.vector(value)) {
                                                          temp = data.frame(value)
                                                          names(temp) = deparse(substitute(value))
                                                          if (nrow(self$X) == nrow(temp)) {
                                                            self$response = temp
                                                            invisible(self)
                                                          }
                                                          else{
                                                            stop("number of rows differ!")
                                                          }
                                                        }
                                                        else if (is.data.frame(value)) {
                                                          if (nrow(self$X) == nrow(value)) {
                                                            self$response = value
                                                            invisible(self)
                                                          }
                                                          else{
                                                            stop("number of rows differ!")
                                                          }
                                                        }
                                                        else{
                                                          stop(paste(deparse(substitute(value)), " needs to be a vector or data.frame"))
                                                        }
                                                      }
                                                    },

                                                    #' @description Access specific elements in the design matrix or response data of the object.
                                                    #' @param i An integer specifying the row index to retrieve.
                                                    #' @param j An integer specifying the column index to retrieve.
                                                    get = function(i, j){
                                                      bound = ncol(self$X)
                                                      if (j <= bound)
                                                        self$X[i, j]
                                                      else self$response[i, j - bound]
                                                    },

                                                    #' @description Convert the object to a data frame.
                                                    as.data.frame = function(){
                                                      return(cbind(self$X, self$response))
                                                    },

                                                    #' @description Print the details of the object.
                                                    print = function(){
                                                      print(self$as.data.frame())
                                                    },

                                                    #' @description Plot the results of the steepest ascent procedure for an object of class `steepAscent.c`.
                                                    #' @param y The response variable to be plotted.
                                                    #' @param main The main title of the plot.
                                                    #' @param xlab The label for the x-axis.
                                                    #' @param ylab The label for the y-axis.
                                                    #' @param l.col Color for the line in the plot.
                                                    #' @param p.col Color for the points in the plot.
                                                    #' @param line.type Type of the line used in the plot.
                                                    #' @param point.shape Shape of the points used in the plot.
                                                    plot = function(y, main, xlab, ylab, l.col, p.col,
                                                                    line.type, point.shape){
                                                      Delta = (self$X)$Delta
                                                      frame = cbind(Delta, self$.response())
                                                      names(frame) = c("Delta", names(self$.response()))
                                                      if (missing(main))
                                                        main = ""
                                                      if (missing(xlab))
                                                        xlab = "Delta"
                                                      if (missing(ylab))
                                                        ylab = "predicted"
                                                      if (missing(l.col))
                                                        l.col = "red"
                                                      if (missing(p.col))
                                                        p.col = "red"
                                                      if (missing(line.type))
                                                        line.type = "dashed"
                                                      if (missing(line.type))
                                                        line.type = "dashed"
                                                      if (missing(point.shape))
                                                        point.shape = 16

                                                      p <- ggplot(frame, aes(x = Delta, y = predicted)) +
                                                        geom_line(color = l.col, linetype = line.type) +
                                                        geom_point(color = p.col, shape = point.shape) +
                                                        labs(title = main, x = xlab, y = ylab) +
                                                        theme_minimal()
                                                      print(p)
                                                    }


)
)

# Clase desOpt ----
#' @title desOpt-class: Class "desOpt"
#' @description The \code{desOpt} class represents an object that stores optimization results for factorial design experiments. It includes coded and real factors, responses, desirabilities, overall desirability, and the design object.
#' @field facCoded A list containing the coded values for the factors in the design.
#' @field facReal A list containing the real (actual) values for the factors in the design.
#' @field responses A list of response variables obtained from the design.
#' @field desirabilities A list of desirability scores for each response variable.
#' @field overall Numeric value representing the overall desirability score.
#' @field all A data frame containing all the relevant data from the design and optimization process.
#' @field fdo The factorial design object used in the optimization process.
#' @seealso \code{\link{optimum}}, \code{\link{facDesign}}, \code{\link{desirability}}
desOpt <- R6Class("desOpt", public = list(facCoded = list(),
                                          facReal = list(),
                                          responses = list(),
                                          desirabilities = list(),
                                          overall = NULL,
                                          all = data.frame(),
                                          fdo = NULL,

                                          #' @description Convert the object to a data frame.
                                          as.data.frame = function() {
                                            return(self$all)
                                          },

                                          #' @description Print a summary of the object.
                                          print = function(){
                                            cat(paste("\ncomposite (overall) desirability:", format(self$overall, digits = 3)))
                                            cat("\n")
                                            cat("\n")
                                            temp1 = do.call(data.frame, self$facCoded)
                                            temp2 = do.call(data.frame, self$facReal)
                                            facFrame = rbind(temp1, temp2)
                                            row.names(facFrame) = c("coded", "real")
                                            show(format(facFrame, digits = 3))
                                            temp1 = do.call(data.frame, self$responses)
                                            temp2 = do.call(data.frame, self$desirabilities)
                                            respDesFrame = rbind(temp1, temp2)
                                            row.names(respDesFrame) = c("Responses", "Desirabilities")
                                            cat("\n")
                                            show(format(respDesFrame, digits = 3))
                                          }
)
)


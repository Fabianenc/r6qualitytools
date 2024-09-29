######################################################################
################ DISEÑOS DE MEZCLAS - CLASES #########################
######################################################################

# mixDesign.c ----
#' @title mixDesign-class: Class `mixDesign`
#' @description mixDesign class for simplex lattice and simplex centroid mixture designs with optional center points and augmented points.
#' @field name Character string representing the name of the design.
#' @field factors List of factors involved in the mixture design, including their levels and settings.
#' @field total Numeric value representing the total number of runs in the design.
#' @field lower Numeric vector representing the lower bounds of the factors in the design.
#' @field design Data frame containing the design matrix for the mixture design.
#' @field designType Character string specifying the type of design (e.g., "simplex-lattice", "simplex-centroid").
#' @field pseudo Data frame containing pseudo-experimental runs if applicable.
#' @field response Data frame containing the responses or outcomes measured in the design.
#' @field Type Data frame specifying the type of design used (e.g., "factorial", "response surface").
#' @field block Data frame specifying block structures if the design is blocked.
#' @field runOrder Data frame specifying the order in which runs are performed.
#' @field standardOrder Data frame specifying the standard order of the runs.
#' @field desireVal List of desired values or targets for the response variables.
#' @field desirability List of desirability scores or metrics based on the desired values.
#' @field fits Data frame containing the fitted model parameters and diagnostics.
#' @seealso \code{\link{mixDesign}}, \code{\link{contourPlot3}}, \code{\link{wirePlot3}}
mixDesign.c <- R6Class("mixDesign.c", public = list(name = NULL,
                                                  factors =list(),
                                                  total = NULL,
                                                  lower = NULL,
                                                  design = data.frame(),
                                                  designType = NULL,
                                                  pseudo = data.frame(),
                                                  response = data.frame(),
                                                  Type = data.frame(),
                                                  block = data.frame(),
                                                  runOrder = data.frame(),
                                                  standardOrder = data.frame(),
                                                  desireVal = list(),
                                                  desirability = list(),
                                                  fits = data.frame(),

                                                  #' @description Get and set the \code{factors} in an object of class \code{mixDesign}
                                                  #' @param value New factors, If missing value get the \code{factors}.
                                                  .factors = function(value){
                                                    if (missing(value)) {
                                                      return(self$factors)
                                                    }
                                                    else{
                                                      if (length(value) != ncol(self$pseudo))
                                                        stop("\nNumber of factors doesn't match with number of columns for factorial Design\n")
                                                      self$factors <- value
                                                      invisible(self)
                                                    }
                                                  },

                                                  #' @description Get and set the \code{names} in an object of class \code{mixDesign}.
                                                  #' @param value New names, If missing value get the \code{names}.
                                                  names = function(value){
                                                    if(missing(value)){
                                                      aux <- list()
                                                      for (i in 1:length(self$factors)) {
                                                        aux[[.NAMES[i]]] <-self$factors[[i]]$name
                                                      }
                                                      return(aux)
                                                    }
                                                    else {
                                                      for (i in 1:length(self$factors)){
                                                        self$factors[[i]]$name = as.character(value[i])

                                                      }
                                                      invisible(self)
                                                    }

                                                  },

                                                  #' @description Methods for function \code{as.data.frame} in Package \code{base}.
                                                  as.data.frame = function(){
                                                    frameOut = cbind(self$standardOrder, self$runOrder, self$Type, self$pseudo, self$response)
                                                    return(frameOut)
                                                  },

                                                  #' @description Methods for function \code{print} in Package \code{base}.
                                                  print = function(){
                                                    print(format(self$as.data.frame(), digits = 4))
                                                    invisible(self$as.data.frame())
                                                  },

                                                  #' @description Get and set the the \code{response} in an object of class \code{mixDesign}.
                                                  #' @param value New response, If missing value get the \code{response}.
                                                  .response = function(value){
                                                    if (missing(value)) {
                                                      return(self$response)
                                                    }
                                                    else{
                                                      #print(deparse(substitute(value)))
                                                      if (!is.numeric(value) & !is.data.frame(value))
                                                        stop("vector or data.frame must be given")
                                                      if (is.numeric(value)) {
                                                        if (length(value) != nrow(self$pseudo))
                                                          stop("differing lengths")
                                                        temp = data.frame(value)
                                                        names(temp) = deparse(substitute(value))[1]
                                                        value = temp
                                                      }
                                                      if (is.data.frame(value)) {
                                                        if (nrow(value) != nrow(self$pseudo))
                                                          stop("differing number of rows")
                                                      }
                                                      self$response = value
                                                      invisible(self)
                                                    }

                                                  },

                                                  #' @description Prints a summary of the factors attributes including their low, high, name, unit, and type.
                                                  .nfp = function(){
                                                    x = self$.factors()
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

                                                  #' @description Methods for function \code{summary} in Package \code{base}.
                                                  summary = function(){
                                                    cat(paste("Simplex", toupper(self$designType), "Design"))
                                                    cat("\n")
                                                    cat("Information about the factors:\n\n")
                                                    self$.nfp()
                                                    cat("\n-----------\n")
                                                    cat("\n")
                                                    .npp(self)
                                                    cat("\n-----------\n")
                                                    cat("\n")
                                                    cat("Information about the constraints:\n\n")
                                                    lower = self$lower
                                                    temp = character(0)
                                                    for (i in seq(along = lower)) temp = c(temp, paste(LETTERS[i], ">=", lower[i]))
                                                    cat(temp)
                                                    cat("\n")
                                                    cat("\n-----------\n")
                                                    cat("\n")
                                                    times = nrow(self$pseudo)
                                                    pseudo = format(self$pseudo, digits = 2)
                                                    design = format(self$design, digits = 2)
                                                    amount = design
                                                    if (self$total[2] != 1)
                                                      amount = format(self$design * self$total[2], digits = 2)
                                                    temp = c("                             ", "PseudoComponent", "_|_", "Proportion", "_|_", "Amount")
                                                    cat(temp)
                                                    cat("\n")
                                                    cat("\n")
                                                    temp = cbind(pseudo, `_` = rep(" ", times = times), `|` = rep("|", times = times), `_` = rep(" ", times = times), design)
                                                    temp = cbind(temp, `_` = rep(" ", times = times), `|` = rep("|", times = times), `_` = rep(" ", times = times), amount)
                                                    temp = cbind(self$standardOrder, self$runOrder, self$Type, `|` = rep("|", times = times), temp, `|` = rep("|", times = times), self$response)
                                                    show(temp)
                                                    cat("\n-----------\n")
                                                    cat("\n")
                                                    cat(paste("Mixture Total:", self$total[1], "equals", self$total[2]))
                                                    cat("\n")
                                                    cat("\n")
                                                    invisible(self$as.data.frame())
                                                  },

                                                  #' @description Get and set the \code{units} for the factors in an object of class \code{mixDesign}.
                                                  #' @param value New units, If missing value get the \code{units}.
                                                  units = function(value){
                                                    if (missing(value)) {
                                                      v <- list()
                                                      for (i in 1:length(self$factors)) {
                                                        v[[unlist(self$names()[i])]] <- self$factors[[i]]$.unit()
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

                                                  #' @description Get and set the \code{lows} for the factors in an object of class \code{mixDesign}.
                                                  #' @param value New lows, If missing value get the \code{lows}.
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
                                                        if (length(value) > 1){
                                                          self$factors[[i]]$.low(value[i])
                                                        }
                                                        else{
                                                          self$factors[[i]]$.low(value)
                                                        }

                                                      }
                                                      invisible(self)
                                                    }
                                                  },

                                                  #' @description Get and set the \code{highs} for the factors in an object of class \code{mixDesign}.
                                                  #' @param value New highs, If missing value get the \code{highs}.
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
                                                        if (length(value) > 1){
                                                          self$factors[[i]]$.high(value[i])
                                                        }
                                                        else{
                                                          self$factors[[i]]$.high(value)
                                                        }

                                                      }
                                                      invisible(self)
                                                    }
                                                  }
)
)

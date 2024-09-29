############################################################################
####################### DISEÑO TAGUCHI - CLASES ############################
############################################################################

# Clase taguchiFactor ----
#' @title taguchiFactor
#' @description An R6 class representing a factor in a Taguchi design.
#' @field values A vector containing the levels or values associated with the factor. Default is \code{NA}.
#' @field name A character string specifying the name of the factor. Default is an empty string \code{``}.
#' @field unit A character string specifying the unit of measurement for the factor. Default is an empty string \code{``}.
#' @field type A character string specifying the type of the factor, which can be either \code{`numeric`} or \code{`categorical`}. Default is \code{`numeric`}.
taguchiFactor <- R6Class("taguchiFactor", public = list(values = NA,
                                                        name = "",
                                                        unit = "",
                                                        type = "numeric",

                                                        #' @description Get the attributes of the factor.
                                                        attributes = function(){
                                                          v <- c(self$values, self$name, self$unit, self$type)
                                                        },

                                                        #' @description Get and set the \code{values} for the factors in an object of class \code{taguchiFactor}.
                                                        #' @param value New values, If missing value get the \code{values}.
                                                        .values = function(value){
                                                          if (missing(value)) {
                                                            return(self$values)
                                                          }
                                                          else{
                                                            self$values <- value
                                                            invisible(self)
                                                          }
                                                        },

                                                        #' @description Get and set the \code{units} for the factors in an object of class \code{taguchiFactor}.
                                                        #' @param value New unit, If missing value get the \code{units}.
                                                        .unit = function(value){
                                                          if(missing(value)){
                                                            return(self$unit)
                                                          }
                                                          else{
                                                            self$unit <- value
                                                            invisible(self)
                                                          }
                                                        },

                                                        #' @description Get and set the \code{names} in an object of class \code{taguchiFactor}.
                                                        #' @param value New names, If missing value get the \code{names}.
                                                        names = function(value){
                                                          if(missing(value)){
                                                            return(self$name)
                                                          }
                                                          else{
                                                            self$name <- value
                                                            invisible(self)
                                                          }
                                                        }
)
)
# Clase taguchiDesign.c ----
#' @title taguchiDesign
#' @description An R6 class representing a Taguchi experimental design.
#' @field name A character string specifying the name of the design. Default is \code{NULL}.
#' @field factors A list of factors included in the Taguchi design. Each factor is typically an instance of the \code{taguchiFactor} class.
#' @field design A `data.frame` representing the design matrix of the experiment. This includes the levels of each factor for every run of the experiment. Default is an empty \code{data.frame}.
#' @field designType A character string specifying the type of Taguchi design used. Default is \code{NULL}.
#' @field replic A `data.frame` containing the replication information for the design. Default is an empty \code{data.frame}.
#' @field response A `data.frame` storing the response values collected from the experiment. Default is an empty \code{data.frame}.
#' @field Type A `data.frame` specifying the type of responses or factors involved in the design. Default is an empty \code{data.frame}.
#' @field block A `data.frame` indicating any blocking factors used in the design. Default is an empty \code{data.frame}.
#' @field runOrder A `data.frame` detailing the order in which the experimental runs were conducted. Default is an empty \code{data.frame}.
#' @field standardOrder A `data.frame` detailing the standard order of the experimental runs. Default is an empty \code{data.frame}.
#' @field desireVal A list storing desired values for responses in the experiment. Default is an empty list.
#' @field desirability A list storing desirability functions used to evaluate the outcomes of the experiment. Default is an empty list.
#' @field fits A `data.frame` containing model fits or other statistical summaries from the analysis of the experimental data. Default is an empty \code{data.frame}.
taguchiDesign.c <- R6Class("taguchiDesign.c", public = list(name = NULL,
                                                          factors = list(),
                                                          design = data.frame(),
                                                          designType = NULL,
                                                          replic = data.frame(),
                                                          response = data.frame(),
                                                          Type = data.frame(),
                                                          block = data.frame(),
                                                          runOrder = data.frame(),
                                                          standardOrder = data.frame(),
                                                          desireVal = list(),
                                                          desirability = list(),
                                                          fits = data.frame(),

                                                          #' @description Get and set the \code{values} for an object of class \code{taguchiDesign}.
                                                          #' @param value New value, If missing value get the \code{values}.
                                                          values = function(value){
                                                            if(missing(value)){
                                                              listOut = vector(mode = "list")
                                                              for (i in names(self$design)) {
                                                                listOut[[i]] = self$factors[[i]]$.values()
                                                              }
                                                              return(listOut)
                                                            }
                                                            else{
                                                              for (i in names(value)) {
                                                                if (i %in% names(self$design))
                                                                  if (length(value[[i]]) == length(unique(self$design[, i])))
                                                                    self$factors[[i]]$.values(value[[i]])
                                                                else stop("Number of values greater or less than number of factor settings!")
                                                              }
                                                              invisible(self)
                                                            }

                                                          },

                                                          #' @description Get and set the \code{units} for an object of class \code{taguchiDesign}.
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

                                                          #' @description Get and set the \code{factors} in an object of class \code{taguchiDesign}.
                                                          #' @param value New factors, If missing value get the \code{factors}.
                                                          .factors = function(value){
                                                            if (missing(value)) {
                                                              return(self$factors)
                                                            }
                                                            else{
                                                              if (length(value) != ncol(self$design))
                                                                stop("\nNumber of factors doesn't match with number of columns for factorial Design\n")
                                                              self$factors <- value
                                                              invisible(self)
                                                            }
                                                          },

                                                          #' @description Get and set the \code{names} in an object of class \code{taguchiDesign}.
                                                          #' @param value New names, If missing value get the \code{names}.
                                                          names = function(value){
                                                            if(missing(value)){
                                                              aux <- list()
                                                              for (i in 1:length(self$factors)) {
                                                                if(length(self$factors)>25){
                                                                  aux[[c(.NAMES,.generate_double_letters(length(self$factors)-25))[i]]] = self$factors[[i]]$name
                                                                } else{
                                                                  aux[[.NAMES[i]]] <- self$factors[[i]]$name
                                                                }
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

                                                          #' @description Return a data frame with the information of the object \code{taguchiDesign.c}.
                                                          as.data.frame = function(){
                                                            frameOut = cbind(self$standardOrder, self$runOrder, self$replic, self$design, self$response)
                                                            return(frameOut)
                                                          },

                                                          #' @description Methods for function \code{print} in Package \code{base}.
                                                          print = function(){
                                                            print(format(self$as.data.frame(), digits = 4))
                                                          },

                                                          #' @description Get and set the the \code{response} in an object of class \code{taguchiDesign}.
                                                          #' @param value New response, If missing value get the \code{response}.
                                                          .response = function(value){
                                                            if(missing(value)){
                                                              return(self$response)
                                                            }
                                                            else{
                                                              if (!is.numeric(value) & !is.data.frame(value))
                                                                stop("vector or data.frame must be given")
                                                              if (is.numeric(value)) {
                                                                if (length(value) != nrow(self$design))
                                                                  stop("differing lengths")
                                                                temp = data.frame(value)
                                                                names(temp) = deparse(substitute(value))[1]
                                                                value = temp
                                                              }
                                                              if (is.data.frame(value)) {
                                                                if (nrow(value) != nrow(self$design))
                                                                  stop("differing number of rows")
                                                              }
                                                              self$response = value
                                                              invisible(self)
                                                            }
                                                          },

                                                          #' @description Prints a summary of the factors attributes including their low, high, name, unit, and type.
                                                          .nfp = function(){
                                                            x = self$.factors()
                                                            DB = FALSE
                                                            if (is.list(x) && length(x[[1]]) > 0) {
                                                              numAttr = length(x[[1]]$attributes())
                                                              .numFac = length(x)
                                                              len = 0
                                                              for (i in names(x)) if (length(x[[i]]$values) > len)
                                                                len = length(x[[i]]$values)
                                                              #numAttr = numAttr + len
                                                              numrows = numAttr #- 1
                                                              frameOut = data.frame(matrix(NA, ncol = .numFac, nrow = numrows))
                                                              names(frameOut) = names(x)
                                                              rownames(frameOut) = c(paste("value", 1:len), "name", "unit", "type")
                                                              for (i in names(x)) {
                                                                vin = 1:length(x[[i]]$values)
                                                                frameOut[vin, i] = x[[i]]$values
                                                                frameOut[numrows - 2, i] = x[[i]]$name
                                                                frameOut[numrows - 1, i] = x[[i]]$unit
                                                                frameOut[numrows, i] = x[[i]]$type
                                                              }
                                                              print(frameOut)
                                                            }

                                                          },

                                                          #' @description Methods for function \code{summary} in Package \code{base}.
                                                          summary = function(){
                                                            cat(paste("Taguchi", toupper(self$designType), "Design"))
                                                            cat("\n")
                                                            cat("Information about the factors:\n\n")
                                                            self$.nfp()
                                                            cat("\n")
                                                            cat("-----------\n")
                                                            cat("\n")
                                                            print(self$as.data.frame())
                                                            cat("\n")
                                                            cat("-----------\n")
                                                            cat("\n")
                                                          },

                                                          #' @description Plots the effects of factors on the response variables.
                                                          #' @param factors Factors to be plotted.
                                                          #' @param fun Function applied to the response variables (e.g., mean).
                                                          #' @param response Optional; specifies which response variables to plot.
                                                          #' @param points Logical; if TRUE, plots data points.
                                                          #' @param lty Line type for plotting.
                                                          #' @param pch The symbol for plotting points.
                                                          #' @param xlab Label for the x-axis.
                                                          #' @param ylab Label for the y-axis.
                                                          #' @param main Main title for the plot.
                                                          #' @param ylim Limits for the y-axis.
                                                          #' @param l.col A color for the lines.
                                                          #' @param p.col A color for the points.
                                                          #' @param ld.col A color for the dashed line.
                                                          #' @examples
                                                          #' tdo = taguchiDesign("L9_3")
                                                          #' tdo$.response(rnorm(9))
                                                          #' tdo$effectPlot(points = TRUE, pch = 16, lty = 3)
                                                          effectPlot = function(factors, fun = mean, response = NULL, points = FALSE,
                                                                                l.col, p.col, ld.col,lty, xlab, ylab, main, ylim, pch){

                                                            response.original=self$.response()
                                                            if(missing(factors)){
                                                              factors = self$factors
                                                            }
                                                            else{
                                                              names(factors)=factors
                                                            }



                                                            if(is.null(response)==FALSE)
                                                            {
                                                              temp=self$.response()[response]
                                                              self$.response(temp)
                                                            }
                                                            ylabmiss = FALSE
                                                            xlabmiss = FALSE
                                                            mainmiss = FALSE
                                                            ylimmiss = FALSE
                                                            if (missing(ylim))
                                                              ylimmiss = TRUE
                                                            if (missing(lty))
                                                              lty = 1
                                                            if (missing(pch))
                                                              pch = 16
                                                            X = self$design
                                                            Y = self$.response()
                                                            if (!missing(factors))
                                                              k = length(factors)
                                                            else #(missing(factors))
                                                            {
                                                              k = ncol(X)
                                                              factors = names(X)
                                                            }
                                                            numCol = 1
                                                            numRow = 1
                                                            if (missing(factors)) {
                                                              if (ncol(X) == 2) {
                                                                numCol = 2
                                                                numRow = 1
                                                              }
                                                              if (ncol(X) > 2) {
                                                                numCol = 2
                                                                numRow = 2
                                                              }
                                                            }
                                                            if (!missing(factors)) {
                                                              if (length(factors) == 2) {
                                                                numCol = 2
                                                                numRow = 1
                                                              }
                                                              if (length(factors) == 3) {
                                                                numCol = 3
                                                                numRow = 1
                                                              }
                                                              if (length(factors) == 4) {
                                                                numCol = 2
                                                                numRow = 2
                                                              }
                                                              if (length(factors) == 5) {
                                                                numCol = 3
                                                                numRow = 2
                                                              }
                                                              if (length(factors) == 6) {
                                                                numCol = 3
                                                                numRow = 2
                                                              }
                                                              if (length(factors) > 6) {
                                                                numRow = ceiling(sqrt(length(factors)))
                                                                numCol = ceiling(sqrt(length(factors)))
                                                              }
                                                            }

                                                            list_plot <- list()

                                                            for (j in 1:ncol(Y)) {
                                                              for (i in 1:length(factors)) {
                                                                cells = as.vector(tapply(Y[, j], list(X[, names(factors[i])], rep(0, nrow(X))), fun))
                                                                if (points) {
                                                                  cells = range(Y)
                                                                }
                                                                if(missing(xlab)){
                                                                  xlab = names(factors[i])
                                                                }
                                                                if(missing(ylab)){
                                                                  ylab = paste(deparse(substitute(fun)), "of", names(Y)[j])

                                                                }
                                                                if(missing(main)){
                                                                  main = paste("Effect Plot for", names(Y)[j])
                                                                }
                                                                if(missing(ylim)){
                                                                  ylim = c(min(Y[, j]), max(Y[, j]))
                                                                }
                                                                grap <- .m.interaction.plot.taguchi(X[, names(factors[i])],rep(0, nrow(X)),Y[, j], fun, xlab = xlab,
                                                                                                    ylab = ylab, ylim = ylim, lty = lty, col = 1,
                                                                                                    main = main, xPoints = X[, names(factors[i])], yPoints = Y[, j], l.col, p.col, ld.col,pch,points=points)

                                                                p <- grap$plot

                                                                list_plot[[paste0("p",j,i)]] <- p
                                                              }
                                                            }

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

                                                            print(p)
                                                            self$.response(response.original)
                                                          },



                                                          #' @description Calculates the alias table for a fractional factorial design and prints an easy to read summary of the defining relations such as 'I = ABCD' for a standard 2^(4-1) factorial design.
                                                          identity = function(){
                                                            identity = character(0)
                                                            identityList = vector(mode = "list", length = 0)
                                                            resolution = numeric(0)
                                                            temp = NULL
                                                            A = aliasTable(self)
                                                            if (any(dim(A) == 0))
                                                              return(identityList)
                                                            temp = as.matrix(A["Identity", ])
                                                            boolTemp = apply(temp, 2, as.logical)
                                                            identity = row.names(temp)[boolTemp[, 1]]
                                                            if (length(identity) > 0) {
                                                              charList = strsplit(toupper(identity), split = "")
                                                              identityList = lapply(charList, match, LETTERS[1:26])
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
                                                          }

)
)



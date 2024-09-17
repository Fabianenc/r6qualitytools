####################################################################################
####################### DISEÑO PLACKETT BURMAN - CLASES ############################
####################################################################################

# Clase pbFactor ----
#' @title pbFactor
#' @description An R6 class representing a factor in a Plackett-Burman design.
#' @field values A vector containing the levels or values associated with the factor. Default is \code{NA}.
#' @field name A character string specifying the name of the factor. Default is an empty string \code{""}.
#' @field unit A character string specifying the unit of measurement for the factor. Default is an empty string \code{""}.
#' @field type A character string specifying the type of the factor, which can be either \code{"numeric"} or \code{"categorical"}. Default is \code{"numeric"}.
pbFactor <- R6Class("pbFactor", public = list(values = NA,
                                              name = "",
                                              unit = "",
                                              type = "numeric",

                                              #' @description Get the attributes of the factor.
                                              attributes = function(){
                                                v <- c(self$values,self$name, self$unit, self$type)
                                              },

                                              #' @description Get and set the \code{values} for the factors in an object of class \code{pbFactor}.
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

                                              #' @description Get and set the \code{units} for the factors in an object of class \code{pbFactor}.
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

                                              #' @description Get and set the \code{names} in an object of class \code{pbFactor}.
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

# Clase pbDesign ----
#' @title pbDesign
#' @description An R6 class representing a Plackett-Burman design.
#' @field name A character string specifying the name of the design. Default is \code{NULL}.
#' @field factors A list of factors included in the Taguchi design. Each factor is typically an instance of the \code{pbFactor} class.
#' @field design A \code{data.frame} representing the design matrix of the experiment. This includes the levels of each factor for every run of the experiment. Default is an empty \code{data.frame}.
#' @field designType A character string specifying the type of Taguchi design used. Default is \code{NULL}.
#' @field replic A \code{data.frame} containing the replication information for the design. Default is an empty \code{data.frame}.
#' @field response A \code{data.frame} storing the response values collected from the experiment. Default is an empty \code{data.frame}.
#' @field Type A \code{data.frame} specifying the type of responses or factors involved in the design. Default is an empty \code{data.frame}.
#' @field block A \code{data.frame} indicating any blocking factors used in the design. Default is an empty \code{data.frame}.
#' @field runOrder A \code{data.frame} detailing the order in which the experimental runs were conducted. Default is an empty \code{data.frame}.
#' @field standardOrder A \code{data.frame} detailing the standard order of the experimental runs. Default is an empty \code{data.frame}.
#' @field desireVal A list storing desired values for responses in the experiment. Default is an empty list.
#' @field desirability A list storing desirability functions used to evaluate the outcomes of the experiment. Default is an empty list.
#' @field fits A \code{data.frame} containing model fits or other statistical summaries from the analysis of the experimental data. Default is an empty \code{data.frame}.
pbDesign.c <- R6Class("pbDesign", public = list(name = NULL,
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

                                                #' @description Get and set the \code{values} for an object of class \code{pbDesign}.
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

                                                #' @description Get and set the \code{units} for an object of class \code{pbDesign}.
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

                                                #' @description Get and set the \code{factors} in an object of class \code{pbDesign}.
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

                                                #' @description Get and set the \code{names} in an object of class \code{pbDesign}.
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

                                                #' @description Return a data frame with the information of the object \code{pbDesign}.
                                                as.data.frame = function(){
                                                  frameOut = cbind(self$standardOrder, self$runOrder, self$replic, self$design, self$response)
                                                  return(frameOut)
                                                },

                                                #' @description Methods for function \code{print} in Package \code{base}.
                                                print = function(){
                                                  print(format(self$as.data.frame(), digits = 4))
                                                },

                                                #' @description Get and set the the \code{response} in an object of class \code{pbDesign}.
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
                                                  cat(paste("Plackett-Burman", toupper(self$designType), "Design"))
                                                  cat("\n")
                                                  cat("Information about the factors:\n\n")
                                                  self$.nfp
                                                  cat("\n")
                                                  cat("-----------\n")
                                                  cat("\n")
                                                  print(self$as.data.frame())
                                                  cat("\n")
                                                  cat("-----------\n")
                                                  cat("\n")
                                                }


)
)



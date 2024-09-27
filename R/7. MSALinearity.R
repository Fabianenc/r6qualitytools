#################################################################
####################### MSALinearity ############################
#################################################################

# Clase MSALinearity ----
#' @title MSALinearity-class: Class '"MSALinearity"'
#' @description R6 class for performing Measurement System Analysis (MSA) Linearity studies.
#' @field X A data frame containing the independent variable(s) used in the linearity study.
#' @field Y A data frame containing the dependent variable(s) or responses measured in the linearity study.
#' @field model The linear model object resulting from the linearity analysis.
#' @field conf.level A numeric value specifying the confidence level for the linearity analysis. This should be between 0 and 1 (e.g., 0.95 for a 95\% confidence level).
#' @field Linearity A list or data frame containing the results of the linearity study, including the linearity value and associated statistics.
#' @field GageName A character string specifying the name of the gage or measurement system under analysis.
#' @field GageTolerance A numeric value specifying the tolerance of the gage or measurement system.
#' @field DateOfStudy A character string or Date object indicating the date when the linearity study was conducted.
#' @field PersonResponsible A character string specifying the name of the person responsible for the linearity study.
#' @field Comments A character string for additional comments or notes about the linearity study.
#' @field facNames A character vector specifying the names of the factors involved in the study, if any.
MSALinearity <-R6Class("MSALinearity", public = list(X = data.frame(),
                                                     Y = data.frame(),
                                                     model = NULL,
                                                     conf.level = NULL,
                                                     Linearity = NULL,
                                                     GageName = NULL,
                                                     GageTolerance = NULL,
                                                     DateOfStudy = NULL,
                                                     PersonResponsible = NULL,
                                                     Comments = NULL,
                                                     facNames = NULL,

                                                     #' @description Get and set the the \code{response} in an object of class \code{MSALinearity}.
                                                     #' @param value New response, If missing value get the \code{response}.
                                                     response = function(value){
                                                       if (missing(value)) {
                                                         out <- self$Y
                                                         return(out)
                                                       }
                                                       else{
                                                         if (is.vector(value) == TRUE)
                                                           value = data.frame(matrix(value, ncol = ncol(self$Y)))
                                                         if (is.matrix(value) == TRUE)
                                                           value = data.frame(value)
                                                         self$Y = value
                                                         invisible(self)
                                                       }

                                                     },

                                                     #' @description Methods for function \code{summary} in Package \code{base}.
                                                     summary = function(){
                                                       cat("----------------------", fill = TRUE)
                                                       print(self)
                                                       cat("----------------------", fill = TRUE)
                                                       if(!is.null(self$model)){
                                                         print(summary(self$model))
                                                         cat("----------------------", fill = TRUE)
                                                       }
                                                       if(!is.null(self$Linearity)){
                                                         names(self$Linearity) = "Linearity:"
                                                         print(self$Linearity)
                                                       }
                                                     },

                                                     #' @description Plots the measurement system, including individual biases, mean bias, and a regression line with confidence intervals.
                                                     #' @param ylim A numeric vector specifying the limits for the y-axis. If not provided, the limits are automatically calculated based on data.
                                                     #' @param col A vector specifying the colors to be used for different plot elements.
                                                     #' @param pch A numeric vector specifying the plotting characters (symbols) for individual data points and mean bias points.
                                                     #' @param lty A numeric vector specifying the line types for the regression line and its confidence intervals. The default is \code{c(1, 2)}.
                                                     plot = function(ylim, col, pch, lty = c(1, 2)){
                                                       conf.level = self$conf.level
                                                       g = nrow(self$X[2])
                                                       m = ncol(self$Y)
                                                       if (missing(col))
                                                         col = c(1, 2, 1, 4)
                                                       if (missing(pch))
                                                         pch = c(20, 18)
                                                       bias = self$Y
                                                       mbias = numeric(g)
                                                       for (i in 1:g) bias[i, ] = self$Y[i, ] - self$X[i, 2]
                                                       for (i in 1:g) mbias[i] = mean(as.numeric(bias[i, ]))
                                                       if (missing(ylim))
                                                         ylim = c(min(bias, na.rm = TRUE), max(bias, na.rm = TRUE))

                                                       original <- data.frame(x = self$X$Ref, y = mbias, grupo = "Single Bias")
                                                       points_aux <- data.frame(
                                                         x = rep(self$X$Ref[1], length = m),
                                                         y = as.numeric(bias[1,]),
                                                         grupo = "Mean Bias"
                                                       )

                                                       for (i in 2:g) {
                                                         temp_data <- data.frame(
                                                           x = rep(self$X$Ref[i], length = m),
                                                           y = as.numeric(bias[i,]),
                                                           grupo = "Mean Bias"
                                                         )
                                                         points_aux <- rbind(points_aux, temp_data)
                                                       }

                                                       aux <- rbind(original, points_aux)

                                                       BIAS = numeric()
                                                       ref = numeric()
                                                       for (i in 1:g) {
                                                         BIAS = c(BIAS, as.numeric(bias[i, ]))
                                                         ref = c(ref, rep(self$X$Ref[i], length = m))
                                                       }
                                                       lm.1 = lm(formula = BIAS ~ ref)
                                                       a = lm.1[[1]][2]
                                                       names(a) = "slope"
                                                       b = lm.1[[1]][1]
                                                       names(b) = "intercept"
                                                       y.vec = numeric()
                                                       for (i in 1:g) y.vec = c(y.vec, self$Y[i, ])
                                                       pre = predict.lm(lm.1, interval = "confidence", level = conf.level)

                                                       regresion_line <- data.frame(x = ref, y = pre[, 1], grupo = "Regression")
                                                       lower_line <- data.frame(x = ref, y = pre[, 2], grupo = "95% conf.level lower")
                                                       upper_line <- data.frame(x = ref, y = pre[, 3], grupo = "95% conf.level upper")

                                                       lines <- rbind(regresion_line, lower_line)
                                                       lines <- rbind(lines, upper_line)

                                                       p <- ggplot() +
                                                         geom_point(data = aux, aes(x = x, y = y, color = grupo, shape = grupo)) +
                                                         geom_line(data = lines, aes(x = x, y = y, linetype = grupo, color = grupo)) +
                                                         scale_color_manual(values = c("Single Bias" = col[2], "Mean Bias" = col[1],
                                                                                       "Regression" = col[3],"95% conf.level lower" = col[4],
                                                                                       "95% conf.level upper" = col[4])) +
                                                         scale_shape_manual(values = c("Single Bias" = pch[2], "Mean Bias" = pch[1])) +
                                                         scale_linetype_manual(values = c("Regression" = lty[1], "95% conf.level lower" = lty[2],"95% conf.level upper" = lty[2])) +
                                                         geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +  # Línea horizontal en y = 0
                                                         ylim(ylim) +
                                                         labs(y = "Bias", x = "Reference Values") +
                                                         theme_minimal()+
                                                         guides(shape = 'none', linetype = 'none') +
                                                         theme(legend.title = element_blank(),
                                                               legend.background = element_rect(fill = "white"))

                                                       print(p)
                                                     },

                                                     #' @description Methods for function \code{print} in Package \code{base}.
                                                     print = function(){
                                                       print(self$as.data.frame())
                                                     },

                                                     #' @description Return a data frame with the information of the object \code{MSALinearity}.
                                                     as.data.frame = function(){
                                                       return(cbind(self$X,self$Y))
                                                     }
)

)

# Funcion gageLinDesign ----
gageLinDesign <- function(ref, n = 5) {
  #' @title gageLinDesign: Function to create a object of class MSALinearity.
  #' @description Function generates an object that can be used with the function \code{gageLin}.
  #' @param ref A vector and contains the reference values for each group.
  #' @param n A single value and gives the amount of runs.Default value: ‘5’..
  #' @return The function returns an object of class \code{MSALinearity}.
  #' @seealso \code{\link{MSALinearity}}, \code{\link{gageLin}}.
  #' @examples
  #' # results of run A-E
  #' A=c(2.7,2.5,2.4,2.5,2.7,2.3,2.5,2.5,2.4,2.4,2.6,2.4)
  #' B=c(5.1,3.9,4.2,5,3.8,3.9,3.9,3.9,3.9,4,4.1,3.8)
  #' C=c(5.8,5.7,5.9,5.9,6,6.1,6,6.1,6.4,6.3,6,6.1)
  #' D=c(7.6,7.7,7.8,7.7,7.8,7.8,7.8,7.7,7.8,7.5,7.6,7.7)
  #' E=c(9.1,9.3,9.5,9.3,9.4,9.5,9.5,9.5,9.6,9.2,9.3,9.4)
  #'
  #' # create Design
  #' test=gageLinDesign(ref=c(2,4,6,8,10),n=12)
  #' # create data.frame for results
  #' results=data.frame(rbind(A,B,C,D,E))
  #' # enter results in Design
  #' test$response(results)

  numColY = n
  numRowY = length(ref)
  X = data.frame(cbind(Part = 1:length(ref), Ref = ref))
  Y = data.frame(matrix(NA, nrow = numRowY, ncol = numColY))
  for (i in 1:n) names(Y)[i] = paste("n", i, sep = "")
  gageLinDesign = MSALinearity$new()
  gageLinDesign$X = X
  gageLinDesign$Y = Y
  return(gageLinDesign)
}

# Funcion gageLin ----
gageLin <- function(object, conf.level = 0.95, ylim, col, pch, lty = c(1, 2), stats = TRUE, plot = TRUE){
  #' @title gageLin: Function to visualize and calucalte the linearity of a gage.
  #' @description Function visualize the linearity of a gage by plotting the single and mean bias in one plot and intercalate them with a straight line.
  #' Furthermore the function deliver some characteristic values of linearity studies according to MSA (Measurement System Analysis).
  #' @param object An object of class \code{MSALinearity} containing the data and model for the linearity analysis. To create such an object see \code{gageLinDesign}.
  #' @param conf.level A numeric value between ‘0’ and ‘1’, giving the confidence intervall for the analysis.
  #' Default value: ‘0.95’.
  #' @param ylim A numeric vector of length 2 specifying the y-axis limits for the plot. If not specified, the limits are set automatically based on the data.
  #' @param col A vector with four numeric entries. The first gives the color of the single points, the second gives the color of the points for the mean bias, the third gives the color fo the straight interpolation line and the fourth gives the color for the lines representing the confidence interval. If one of the values is missing or negative the points or lines are not plotted. col is by default ‘c(1,2,1,4)’.
  #' @param pch A vector with two numeric or single character entries giving the symbols for the single points (1st entry) and the mean bias (2nd entry). The default vector is “c(20,18)”
  #' @param lty a vector with two entries giving the line-style for the interpolating line and the confidence interval lines. For detailed information to the entries please see par. The default value for lty is ‘c(1,2)’.
  #' @param stats Logical value. If ‘TRUE’ (default) the function returns all calculated information.
  #' @param plot Logical value indicating whether to generate a plot of the linearity analysis. Default is \code{TRUE}.
  #' @return The function returns an object of class \code{MSALinearity} which can be used with e.g. \code{plot} or \code{summary}.
  #' @seealso \code{\link{cg}}, \code{\link{gageRR}}, \code{\link{gageLinDesign}}, \code{\link{MSALinearity}}.
  #' @examples
  #' # Results of single runs
  #' A=c(2.7,2.5,2.4,2.5,2.7,2.3,2.5,2.5,2.4,2.4,2.6,2.4)
  #' B=c(5.1,3.9,4.2,5,3.8,3.9,3.9,3.9,3.9,4,4.1,3.8)
  #' C=c(5.8,5.7,5.9,5.9,6,6.1,6,6.1,6.4,6.3,6,6.1)
  #' D=c(7.6,7.7,7.8,7.7,7.8,7.8,7.8,7.7,7.8,7.5,7.6,7.7)
  #' E=c(9.1,9.3,9.5,9.3,9.4,9.5,9.5,9.5,9.6,9.2,9.3,9.4)
  #'
  #' # create Design
  #' test=gageLinDesign(ref=c(2,4,6,8,10),n=12)
  #' # create data.frame for results
  #' results=data.frame(rbind(A,B,C,D,E))
  #' # enter results in Design
  #' test$response(results)
  #' test$summary()
  #'
  #' # no plot and no return
  #' MSALin=gageLin(test,stats=FALSE,plot=FALSE)
  #'
  #' # plot only
  #' plot(MSALin)
  #' MSALin$plot()
  #'
  #' # summary
  #' MSALin$summary()

  if (class(object)[1]!="MSALinearity")
    stop("object needs to be from class 'MSALinearity'")
  object$conf.level = conf.level
  g = nrow(object$X[2])
  m = ncol(object$Y)
  if (missing(col))
    col = c(1, 2, 1, 4)
  if (missing(pch))
    pch = c(20, 18)
  bias = object$Y
  mbias = numeric(g)
  for (i in 1:g) bias[i, ] = object$Y[i, ] - object$X[i, 2]
  for (i in 1:g) mbias[i] = mean(as.numeric(bias[i, ]))
  if (missing(ylim))
    ylim = c(min(bias, na.rm = TRUE), max(bias, na.rm = TRUE))
  BIAS = numeric()
  ref = numeric()
  for (i in 1:g) {
    BIAS = c(BIAS, as.numeric(bias[i, ]))
    ref = c(ref, rep(object$X$Ref[i], length = m))
  }
  lm.1 = lm(formula = BIAS ~ ref)
  object$model <- lm.1
  a = lm.1[[1]][2]
  names(a) = "slope"
  b = lm.1[[1]][1]
  names(b) = "intercept"
  y.vec = numeric()
  for (i in 1:g) y.vec = c(y.vec, object$Y[i, ])
  test = numeric(g + 1)
  for (i in 1:g) test[i] = t.test(bias[, i], mu = 0, conf.level = conf.level)["p.value"]
  test[g + 1] = t.test(BIAS, mu = 0, conf.level = conf.level)["p.value"]
  Linearity = abs(lm.1[[1]][2]) * 100
  object$Linearity = Linearity
  names(Linearity) = "LINEARITY:"
  if (plot == TRUE) {
    object$plot(ylim = ylim, col = col, pch = pch, lty = lty)
  }
  if (stats == TRUE) {
    cat("----------------------", fill = TRUE)
    cat("BIAS:", fill = TRUE)
    print(bias)
    cat("----------------------", fill = TRUE)
    cat("MEAN OF BIAS:", fill = TRUE)
    temp = mbias
    names(temp) = rownames(object$Y)
    print(temp)
    cat("----------------------", fill = TRUE)
    cat("LINEAR MODEL:", fill = TRUE)
    print(summary(lm.1))
    cat("----------------------", fill = TRUE)
    print(Linearity)
    return(object)
  }
  else return(object)
}




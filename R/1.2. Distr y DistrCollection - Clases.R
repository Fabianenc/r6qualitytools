#########################################################################
######################### DISTRIBUTION - CLASES #########################
#########################################################################

# Class Distr ----
#' @title Distr-class: Class "Distr"
#' @description R6 Class for Distribution Objects
#' @field x Numeric vector of data values.
#' @field name Character string representing the name of the distribution.
#' @field parameters List of parameters for the distribution.
#' @field sd Numeric value representing the standard deviation of the distribution.
#' @field n Numeric value representing the sample size.
#' @field loglik Numeric value representing the log-likelihood.
#' @seealso \code{\link{distribution}}, \code{\link{FitDistr}}, \code{\link{DistrCollection}}
#' @examples
#' # Normal
#' set.seed(123)
#' data1 <- rnorm(100, mean = 5, sd = 2)
#' parameters1 <- list(mean = 5, sd = 2)
#' distr1 <- Distr$new(x = data1, name = "normal", parameters = parameters1, sd = 2, n = 100, loglik = -120)
#' distr1$plot()
#'
#' # Log-normal
#' data2 <- rlnorm(100, meanlog = 1, sdlog = 0.5)
#' parameters2 <- list(meanlog = 1, sdlog = 0.5)
#' distr2 <- Distr$new(x = data2, name = "log-normal", parameters = parameters2, sd = 0.5, n = 100, loglik = -150)
#' distr2$plot()
#'
#' # Geometric
#' data3 <- rgeom(100, prob = 0.3)
#' parameters3 <- list(prob = 0.3)
#' distr3 <- Distr$new(x = data3, name = "geometric", parameters = parameters3, sd = sqrt((1 - 0.3) / (0.3^2)), n = 100, loglik = -80)
#' distr3$plot()
#'
#' # Exponential
#' data4 <- rexp(100, rate = 0.2)
#' parameters4 <- list(rate = 0.2)
#' distr4 <- Distr$new(x = data4, name = "exponential", parameters = parameters4, sd = 1 / 0.2, n = 100, loglik = -110)
#' distr4$plot()
#'
#' # Poisson
#' data5 <- rpois(100, lambda = 3)
#' parameters5 <- list(lambda = 3)
#' distr5 <- Distr$new(x = data5, name = "poisson", parameters = parameters5, sd = sqrt(3), n = 100, loglik = -150)
#' distr5$plot()
#'
#' # Chi-square
#' data6 <- rchisq(100, df = 5)
#' parameters6 <- list(df = 5)
#' distr6 <- Distr$new(x = data6, name = "chi-squared", parameters = parameters6, sd = sqrt(2 * 5), n = 100, loglik = -130)
#' distr6$plot()
#'
#' # Logistic
#' data7 <- rlogis(100, location = 0, scale = 1)
#' parameters7 <- list(location = 0, scale = 1)
#' distr7 <- Distr$new(x = data7, name = "logistic", parameters = parameters7, sd = 1 * sqrt(pi^2 / 3), n = 100, loglik = -140)
#' distr7$plot()
#'
#' # Gamma
#' data8 <- rgamma(100, shape = 2, rate = 0.5)
#' parameters8 <- list(shape = 2, rate = 0.5)
#' distr8 <- Distr$new(x = data8, name = "gamma", parameters = parameters8, sd = sqrt(2 / (0.5^2)), n = 100, loglik = -120)
#' distr8$plot()
#'
#' # f
#' data9 <- rf(100, df1 = 5, df2 = 10)
#' parameters12 <- list(df1 = 5, df2 = 10)
#' df1 = 5
#' df2 = 10
#' distr9 <- Distr$new(x = data9, name = "f", parameters = parameters9, sd = sqrt(((df2^2 * (df1 + df2 - 2)) / (df1 * (df2 - 2)^2 * (df2 - 4)))), n = 100, loglik = -150)
#' distr9$plot()
#'
#' # t
#' data10 <- rt(100, df = 10)
#' parameters10 <- list(df = 10)
#' distr10 <- Distr$new(x = data10, name = "t", parameters = parameters10, sd = sqrt(10 / (10 - 2)), n = 100, loglik = -120)
#' distr10$plot()
#'
#' # negative binomial
#' data11 <- rnbinom(100, size = 5, prob = 0.3)
#' parameters11 <- list(size = 5, prob = 0.3)
#' distr11 <- Distr$new(x = data11, name = "negative binomial", parameters = parameters11, sd = sqrt(5 * (1 - 0.3) / (0.3^2)), n = 100, loglik = -130)
#' distr11$plot()
Distr <- R6Class("Distr",
                 public = list(
                   x = NULL,
                   name = NULL,
                   parameters = NULL,
                   sd = NULL,
                   n = NULL,
                   loglik = NULL,

                   #' @description Initialize the fiels of the `Distribution` object
                   #' @param x Numeric vector of data values.
                   #' @param name Character string representing the name of the distribution.
                   #' @param parameters List of parameters for the distribution.
                   #' @param sd Numeric value representing the standard deviation of the distribution.
                   #' @param n Numeric value representing the sample size.
                   #' @param loglik Numeric value representing the log-likelihood.
                   initialize = function(x, name, parameters, sd, n, loglik) {
                     self$x <- x
                     self$name <- name
                     self$parameters <- parameters
                     self$sd <- sd
                     self$n <- n
                     self$loglik <- loglik
                   },

                   #' @description Plot the distribution with histogram and fitted density curve.
                   #' @param main Character string for the main title of the plot. Defaults to the name of the distribution.
                   #' @param xlab Character string for the x-axis label. Defaults to "x".
                   #' @param xlim Numeric vector specifying the x-axis limits.
                   #' @param xlim.t Logical value specifyind to change the xlim default.
                   #' @param ylab Character string for the y-axis label. Defaults to "Density".
                   #' @param line.col Character string for the color of the plot line. Default is "red".
                   #' @param fill.col Character string for the color of the fill histogram plot line. Default is "lightblue".
                   #' @param border.col Character string for the color of the border of the fill histogram plot line. Default is "black".
                   #' @param box Logical value indicating whether to draw a box with the parameters in the plot. Default is TRUE.
                   #' @param line.width Numeric value specifying the width of the plot line. Default is 1.
                   plot = function(main = NULL, xlab = NULL, xlim = NULL, xlim.t = TRUE, ylab = NULL, line.col = "red",
                                   fill.col = "lightblue", border.col = "black", box=TRUE, line.width = 1)
                   {
                     object <- self
                     xVals = object$x
                     parameters = object$parameters
                     lq = NULL
                     uq = NULL
                     y = NULL

                     if (missing(main)) {
                       main =  object$name
                     }
                     if (missing(xlab)) {
                       xlab = "x"
                     }
                     if (missing(ylab)) {
                       ylab = "Density"
                     }

                     distr <- object$name
                     qFun <- .charToDistFunc(distr, type = "q")
                     dFun <- .charToDistFunc(distr, type = "d")
                     if(object$name %in% c("normal","log-normal", "geometric", "exponential", "poisson")){
                       adTestStats <- .myADTest(xVals, distr)
                       if (adTestStats$class == "adtest") {
                         A <- adTestStats$statistic
                         p <- adTestStats$p.value
                       }
                       else {
                         A <- NA
                         p <- NA
                       }
                     }
                     else {
                       A <- NA
                       p <- NA
                     }

                     if (missing(xlim)) {
                       lq = do.call(qFun, c(list(1e-04), as.list(parameters)))
                       uq = do.call(qFun, c(list(0.9999), as.list(parameters)))
                       xlim = range(lq, uq, xVals)
                     }

                     # Histograma
                     p1 <- ggplot(data.frame(x = xVals), aes(x = x)) +
                       geom_histogram(aes(y = after_stat(density)),
                                      binwidth = 1,  # Ajusta el ancho del bin
                                      colour = border.col, fill = fill.col) +
                       geom_density(colour = line.col, lwd = line.width ) +
                       labs(y = ylab, x = xlab, title = main) +
                       theme_minimal() + theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
                       guides(color = guide_legend(title.position = "top", title.hjust = 0.5))

                     if(xlim.t){
                       p1 <- p1 + xlim(xlim)
                     }

                     # Caja de Info
                     if (box==FALSE) {
                       suppressWarnings(print(p1))
                     }
                     else {
                       p2 <- ggplot(data = data.frame(x = 0, y = 0), aes(x, y)) +
                         theme_bw() +
                         theme(
                           axis.text = element_blank(),
                           axis.ticks = element_blank(),
                           axis.title = element_blank(),
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank()
                         ) +
                         xlim(c(0.25,0.26))
                       {
                         # n y A
                         if(is.na(A)){
                           p2 <- p2 +
                             annotate('text', x = 0.25, y = 0.25,
                                      label = "A = NA", size = 3, hjust = 0)
                         }
                         else{
                           p2 <- p2 +
                             annotate('text', x = 0.25, y = 0.25,
                                      label = paste("A==", round(as.numeric(A), digits = 3)),
                                      parse = TRUE, size = 3, hjust = 0)
                         }

                         # p
                         if(object$name %in% c("normal","log-normal", "geometric", "exponential", "poisson")){
                           if (!is.null(adTestStats$smaller) && adTestStats$smaller){
                             p2 <- p2 +
                               annotate('text',x = 0.25,y = 0.20,
                                        label = paste("p<", round(as.numeric(p), digits =3)),
                                        parse = TRUE,size = 3,hjust = 0)
                           }
                           if (!is.null(adTestStats$smaller) && !adTestStats$smaller){
                             p2 <- p2 +
                               annotate('text',x = 0.25, y = 0.20,
                                        label = paste("p>=", round(as.numeric(p),digits = 3)),
                                        parse = TRUE,size = 3,hjust = 0)
                           }
                           if (is.null(adTestStats$smaller)){
                             if (is.na(p)){
                               p2 <- p2 +
                                 annotate('text',x = 0.25,y = 0.20,
                                          label = "p = NA",
                                          parse = F,size = 3,hjust = 0)
                             }
                             else{
                               p2 <- p2 +
                                 annotate('text',x = 0.25,y = 0.20,
                                          label = paste("p==", round(as.numeric(p), digits = 3)),
                                          parse = TRUE,size = 3,hjust = 0)
                             }

                           }
                         }
                         else{
                           p2 <- p2 +
                               annotate('text',x = 0.25,y = 0.20,
                                        label = "p = NA",
                                        parse = F,size = 3,hjust = 0)
                         }

                         if(length(self$parameters) == 1){
                           p2 <- p2 + annotate('text', x = 0.25, y = 0.30,
                                               label = paste(names(self$parameters[1]),"==", round(self$parameters[[1]], digits = 3)),
                                               parse = TRUE, size = 3, hjust = 0) + ylim(c(0.19, 0.31))
                         }
                         if(length(self$parameters) == 2){
                           p2 <- p2 + annotate('text', x = 0.25, y = 0.35,
                                               label = paste(names(self$parameters[1]),"==", round(self$parameters[[1]], digits = 3)),
                                               parse = TRUE, size = 3, hjust = 0) +
                             annotate('text', x = 0.25, y = 0.30,
                                      label = paste(names(self$parameters[2]),"==", round(self$parameters[[2]], digits = 3)),
                                      parse = TRUE, size = 3, hjust = 0) + ylim(c(0.19, 0.36))
                         }
                         }
                       suppressWarnings(print(p1 + inset_element(p2, left = 0.7, right = 1, top = 1, bottom = 0.60)))
                     }
                   }
                 )
)



# Class DistrCollection ----
#' @title DistrCollection-class: Class "DistrCollection"
#' @description R6 Class for Managing a Collection of Distribution Objects
#' @field distr List of \code{\link{Distr}} objects.
#' @seealso \code{\link{Distr}}, \code{\link{distribution}}, \code{\link{FitDistr}}

#' @examples
#' set.seed(123)
#' data1 <- rnorm(100, mean = 5, sd = 2)
#' parameters1 <- list(mean = 5, sd = 2)
#' distr1 <- Distr$new(x = data1, name = "normal", parameters = parameters1, sd = 2, n = 100, loglik = -120)
#'
#' data2 <- rpois(100, lambda = 3)
#' parameters2 <- list(lambda = 3)
#' distr2 <- Distr$new(x = data2, name = "poisson", parameters = parameters2, sd = sqrt(3), n = 100, loglik = -150)
#'
#' collection <- DistrCollection$new()
#' collection$add(distr1)
#' collection$add(distr2)
#' collection$summary()
#' collection$plot()
DistrCollection <- R6::R6Class("DistrCollection",
                               public = list(
                                 distr = NULL,

                                 #' @description Initialize the fields of the \code{DistrCollection} object.
                                 initialize = function() {
                                   self$distr <- list()
                                 },

                                 #' @description Add a \code{Distr} object to the collection.
                                 #' @param distr A \code{Distr} object to add to the collection.
                                 add = function(distr) {
                                   self$distr <- append(self$distr, list(distr))
                                 },

                                 #' @description Get a \code{Distr} object from the collection by its index.
                                 #' @param i Integer index of the \code{Distr} object to retrieve.
                                 #' @return A \code{Distr} object.
                                 get = function(i) {
                                   self$distr[[i]]
                                 },

                                 #' @description Print the summary of all distributions in the collection.
                                 print = function() {
                                   cat("\n")
                                   for (i in seq_along(self$distr)) {
                                     temp <- self$distr[[i]]
                                     cat("\n")
                                     cat("fitted distribution is", temp$name, ":\n")
                                     print(temp$parameters)
                                     cat("\n")
                                   }
                                 },

                                 #' @description Summarize the goodness of fit for all distributions in the collection.
                                 #' @return A data frame with distribution names, Anderson-Darling test statistics, and p-values.
                                 summary = function() {
                                   numDist <- length(self$distr)
                                   gofMatrix <- data.frame(matrix(nrow = numDist, ncol = 3))
                                   names(gofMatrix) <- c("Distribution", "A", "p.value")
                                   cat("\n------ Fitted Distribution and estimated parameters ------\n")
                                   for (i in seq_along(self$distr)) {
                                     distrObj <- self$distr[[i]]
                                     x <- distrObj$x
                                     distribution <- distrObj$name
                                     parameters <- distrObj$parameters
                                     statistic <- NA
                                     p.value <- NA
                                     temp <- .myADTest(x, distribution)
                                     try(statistic <- as.numeric(temp$statistic), silent = TRUE)
                                     try(p.value <- as.numeric(temp$p.value), silent = TRUE)
                                     gofMatrix[i, ] <- c(distribution, as.numeric(statistic), as.numeric(p.value))
                                     cat("\n")
                                     cat("fitted distribution is", distribution, ":\n")
                                     print(parameters)
                                   }
                                   cat("\n")
                                   cat("\n------ Goodness of Fit - Anderson Darling Test ------\n")
                                   cat("\n")
                                   gofMatrixPrint <- gofMatrix
                                   gofMatrixPrint[, 2] <- signif(as.numeric(gofMatrixPrint[, 2]), 4)
                                   gofMatrixPrint[, 3] <- signif(as.numeric(gofMatrixPrint[, 3]), 4)
                                   print(gofMatrixPrint)
                                 },

                                 #' @description Plot all distributions in the collection.
                                 #' @param xlab Character string for the x-axis label.
                                 #' @param ylab Character string for the y-axis label.
                                 #' @param xlim Numeric vector specifying the x-axis limits.
                                 #' @param ylim Numeric vector specifying the y-axis limits.
                                 #' @param line.col Character string for the color of the plot line. Default is "red".
                                 #' @param fill.col Character string for the color of the histogram fill. Default is "lightblue".
                                 #' @param border.col Character string for the color of the histogram border. Default is "black".
                                 #' @param line.width Numeric value specifying the width of the plot line. Default is 1.
                                 #' @param box Logical value indicating whether to draw a box with the parameters in the plot. Default is TRUE.
                                 plot = function(xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL,
                                                 line.col = "red", fill.col = "lightblue", border.col = "black",
                                                 line.width = 1, box = TRUE) {
                                   distrList <- self$distr
                                   numDist <- length(self$distr)
                                   numColWin <- ceiling(numDist/2)
                                   if (missing(line.col)) {
                                     line.col <- "red"
                                   }
                                   if (missing(line.width)) {
                                     line.width <- 1
                                   }
                                   p <- distrList[[1]]$plot(xlab = xlab, ylab = ylab, line.col = line.col, fill.col = fill.col, border.col = border.col,line.width = line.width, box = box)
                                   for (i in 2:length(distrList)) {
                                     p <- p+distrList[[i]]$plot(xlab = xlab, ylab = ylab, line.col = line.col, fill.col = fill.col, border.col = border.col, line.width = line.width, box = box)
                                   }
                                   suppressWarnings(print(p))
                                 }
                               )
)

##############################################################
##################### gageRR - Clase #########################
##############################################################

# gageRR.c ----

#' @title gageRR-class: Class "gageRR"
#' @description R6 Class for Gage R&R (Repeatability and Reproducibility) Analysis
#' @field X Data frame containing the measurement data.
#' @field ANOVA List containing the results of the Analysis of Variance (ANOVA) for the gage study.
#' @field RedANOVA List containing the results of the reduced ANOVA.
#' @field method Character string specifying the method used for the analysis (e.g., \code{"crossed"}, \code{"nested"}).
#' @field Estimates List of estimates including variance components, repeatability, and reproducibility.
#' @field Varcomp List of variance components.
#' @field Sigma Numeric value representing the standard deviation of the measurement system.
#' @field GageName Character string representing the name of the gage.
#' @field GageTolerance Numeric value indicating the tolerance of the gage.
#' @field DateOfStudy Character string representing the date of the gage R&R study.
#' @field PersonResponsible Character string indicating the person responsible for the study.
#' @field Comments Character string for additional comments or notes about the study.
#' @field b Factor levels for operator.
#' @field a Factor levels for part.
#' @field y Numeric vector or matrix containing the measurement responses.
#' @field facNames Character vector specifying the names of the factors (e.g., \code{"Operator"}, \code{"Part"}).
#' @field numO Integer representing the number of operators.
#' @field numP Integer representing the number of parts.
#' @field numM Integer representing the number of measurements per part-operator combination.
#' @examples
#' #create gageRR-object
#' gdo <- gageRRDesign(Operators = 3, Parts = 10, Measurements = 3, randomize = FALSE)
#' #vector of responses
#' y <- c(0.29,0.08, 0.04,-0.56,-0.47,-1.38,1.34,1.19,0.88,0.47,0.01,0.14,-0.80,
#'       -0.56,-1.46, 0.02,-0.20,-0.29,0.59,0.47,0.02,-0.31,-0.63,-0.46,2.26,
#'       1.80,1.77,-1.36,-1.68,-1.49,0.41,0.25,-0.11,-0.68,-1.22,-1.13,1.17,0.94,
#'       1.09,0.50,1.03,0.20,-0.92,-1.20,-1.07,-0.11, 0.22,-0.67,0.75,0.55,0.01,
#'       -0.20, 0.08,-0.56,1.99,2.12,1.45,-1.25,-1.62,-1.77,0.64,0.07,-0.15,-0.58,
#'       -0.68,-0.96,1.27,1.34,0.67,0.64,0.20,0.11,-0.84,-1.28,-1.45,-0.21,0.06,
#'       -0.49,0.66,0.83,0.21,-0.17,-0.34,-0.49,2.01,2.19,1.87,-1.31,-1.50,-2.16)
#'
#' #appropriate responses
#' gdo$response(y)
#' # perform and gageRR
#' gdo <- gageRR(gdo)
#'
#' # Using the plots
#' gdo$plot()
#' gdo$errorPlot()
gageRR.c <- R6Class("gageRR",
                    public = list(
                      X = NULL,
                      ANOVA = NULL,
                      RedANOVA = NULL,
                      method = NULL,
                      Estimates = NULL,
                      Varcomp = NULL,
                      Sigma = NULL,
                      GageName = NULL,
                      GageTolerance = NULL,
                      DateOfStudy = NULL,
                      PersonResponsible = NULL,
                      Comments = NULL,
                      b = NULL,
                      a = NULL,
                      y = NULL,
                      facNames = NULL,
                      numO = NULL,
                      numP = NULL,
                      numM = NULL,

                      #' @description Initialize the fiels of the \code{gageRR} object
                      #' @param X Data frame containing the measurement data.
                      #' @param ANOVA List containing the results of the Analysis of Variance (ANOVA) for the gage study.
                      #' @param RedANOVA List containing the results of the reduced ANOVA.
                      #' @param method Character string specifying the method used for the analysis (e.g., "crossed", "nested").
                      #' @param Estimates List of estimates including variance components, repeatability, and reproducibility.
                      #' @param Varcomp List of variance components.
                      #' @param Sigma Numeric value representing the standard deviation of the measurement system.
                      #' @param GageName Character string representing the name of the gage.
                      #' @param GageTolerance Numeric value indicating the tolerance of the gage.
                      #' @param DateOfStudy Character string representing the date of the gage R&R study.
                      #' @param PersonResponsible Character string indicating the person responsible for the study.
                      #' @param Comments Character string for additional comments or notes about the study.
                      #' @param b Factor levels for operator.
                      #' @param a Factor levels for part.
                      #' @param y Numeric vector or matrix containing the measurement responses.
                      #' @param facNames Character vector specifying the names of the factors (e.g., "Operator", "Part").
                      #' @param numO Integer representing the number of operators.
                      #' @param numP Integer representing the number of parts.
                      #' @param numM Integer representing the number of measurements per part-operator combination.
                      initialize = function(X, ANOVA = NULL, RedANOVA = NULL, method = NULL, Estimates = NULL, Varcomp = NULL,
                                            Sigma = NULL, GageName = NULL, GageTolerance = NULL, DateOfStudy = NULL,
                                            PersonResponsible = NULL, Comments = NULL, b = NULL, a = NULL, y = NULL,
                                            facNames = NULL, numO = NULL, numP = NULL, numM = NULL) {
                        self$X <- X
                        self$ANOVA <- ANOVA
                        self$RedANOVA <- RedANOVA
                        self$method <- method
                        self$Estimates <- Estimates
                        self$Varcomp <- Varcomp
                        self$Sigma <- Sigma
                        self$GageName <- GageName
                        self$GageTolerance <- GageTolerance
                        self$DateOfStudy <- DateOfStudy
                        self$PersonResponsible <- PersonResponsible
                        self$Comments <- Comments
                        self$b <- b
                        self$a <- a
                        self$y <- y
                        self$facNames <- facNames
                        self$numO <- numO
                        self$numP <- numP
                        self$numM <- numM
                      },

                      #' @description Return the data frame containing the measurement data (\code{X})
                      print = function() {
                        print(as.data.frame(self$X))
                      },

                      #' @description Return a subset of the data frame that containing the measurement data (\code{X})
                      #' @param i The i-position of the row of \code{X}.
                      #' @param j The j-position of the column of \code{X}.
                      subset = function(i, j) {
                        return(self$X[i, j])
                      },

                      #' @description Summarize the information of the fields of the \code{gageRR} object.
                      summary = function() {
                        if (all(is.na(self$X$Measurement))) {
                          cat("Gage R&R Summary\n")
                          cat("-----------------\n")
                          cat("Method: ", self$method, "\n")
                          cat("Sigma: ", self$Sigma, "\n")
                          cat("Gage Name: ", self$GageName, "\n")
                          cat("Gage Tolerance: ", self$GageTolerance, "\n")
                          cat("Date of Study: ", self$DateOfStudy, "\n")
                          cat("Person Responsible: ", self$PersonResponsible, "\n")
                          cat("Comments: ", self$Comments, "\n")
                          cat("Operators: ", self$numO, "\n")
                          cat("Parts: ", self$numP, "\n")
                          cat("Measurements per Part: ", self$numM, "\n")
                        } else {
                          cat("\n")
                          cat("Operators:\t", self$numO, "\tParts:\t", self$numP, "\n")
                          cat("Measurements:\t", self$numM, "\tTotal:\t", nrow(self$X), "\n")
                          cat("----------")
                          cat("\n")
                          gageRR(self, method = self$method)
                        }
                        return(invisible(self))
                      },

                      #' @description Get or get the response for a \code{gageRRDesign} object.
                      get.response = function() {
                        return(self$X$Measurement)
                      },

                      #' @description Set the response for a \code{gageRRDesign} object.
                      #' @param value New response vector.
                      response = function(value) {
                        self$X$Measurement = value
                      },

                      #' @description Methods for function \code{names} in Package \code{base}.
                      names = function() {
                        return(names(as.data.frame(self$X)))
                      },

                      #' @description Methods for function \code{as.data.frame} in Package \code{base}.
                      as.data.frame = function() {
                        return(as.data.frame(self$X))
                      },

                      #' @description Get the \code{tolerance} for an object of class \code{gageRR}.
                      get.tolerance = function() {
                        return(unlist(self$GageTolerance))
                      },

                      #' @description Set the \code{tolerance} for an object of class \code{gageRR}.
                      #' @param value A data.frame or vector for the new value of tolerance.
                      set.tolerance = function(value) {
                        if (!is.numeric(value))
                          stop("GageTolerance needs to be numeric")
                        self$GageTolerance = value
                        return(self)
                      },

                      #' @description Get the \code{sigma} for an object of class \code{gageRR}.
                      get.sigma = function() {
                        return(unlist(self$Sigma))
                      },

                      #' @description Set the \code{sigma} for an object of class \code{gageRR}.
                      #' @param value Valor of \code{sigma}
                      set.sigma = function(value) {
                        if (!is.numeric(value))
                          stop("Sigma needs to be numeric")
                        self$Sigma = value
                        return(self)
                      },

                      #' @description This function creates a customized plot using the data from the \code{gageRR.c} object.
                      #' @param main Character string specifying the title of the plot.
                      #' @param xlab A character string for the x-axis label.
                      #' @param ylab A character string for the y-axis label.
                      #' @param col A character string or vector specifying the color(s) to be used for the plot elements.
                      #' @param lwd A numeric value specifying the line width of plot elements
                      #' @param fun Function to use for the calculation of the interactions (e.g., \code{mean}, \code{median}). Default is \code{mean}.
                      #' @examples
                      #' # Create gageRR-object
                      #' gdo = gageRRDesign(Operators = 3, Parts = 10, Measurements = 3, randomize = FALSE)
                      #' # Vector of responses
                      #' y = c(0.29,0.08, 0.04,-0.56,-0.47,-1.38,1.34,1.19,0.88,0.47,0.01,0.14,-0.80,
                      #'       -0.56,-1.46, 0.02,-0.20,-0.29,0.59,0.47,0.02,-0.31,-0.63,-0.46,2.26,
                      #'       1.80,1.77,-1.36,-1.68,-1.49,0.41,0.25,-0.11,-0.68,-1.22,-1.13,1.17,0.94,
                      #'       1.09,0.50,1.03,0.20,-0.92,-1.20,-1.07,-0.11, 0.22,-0.67,0.75,0.55,0.01,
                      #'       -0.20, 0.08,-0.56,1.99,2.12,1.45,-1.25,-1.62,-1.77,0.64,0.07,-0.15,-0.58,
                      #'       -0.68,-0.96,1.27,1.34,0.67,0.64,0.20,0.11,-0.84,-1.28,-1.45,-0.21,0.06,
                      #'       -0.49,0.66,0.83,0.21,-0.17,-0.34,-0.49,2.01,2.19,1.87,-1.31,-1.50,-2.16)
                      #'
                      #' # Appropriate responses
                      #' gdo$response(y)
                      #' # Perform and gageRR
                      #' gdo <- gageRR(gdo)
                      #'
                      #' gdo$plot()
                      plot = function(main=NULL, xlab=NULL, ylab=NULL, col, lwd, fun = mean){
                        gdo <- self
                        yName <- gdo$facNames[1]
                        aName <- gdo$facNames[2]
                        bName <- gdo$facNames[3]
                        abName <- paste(aName, ":", bName, sep = "")
                        if (missing(col))
                          col <- 2:(length(unique(gdo$b)) + 1)
                        if (missing(lwd))
                          lwd <- 1
                        temp <- NULL
                        Source <- names(gdo$Varcomp)
                        VarComp <- round(as.numeric(gdo$Varcomp), 3)
                        Contribution <- round(as.numeric(gdo$Varcomp) / as.numeric(gdo$Varcomp[length(gdo$Varcomp)]), 3)
                        VarComp <- t(data.frame(gdo$Varcomp))
                        VarCompContrib <- VarComp / gdo$Varcomp$totalVar
                        Stdev <- sqrt(VarComp)
                        StudyVar <- Stdev * gdo$Sigma
                        StudyVarContrib <- StudyVar / StudyVar["totalVar", ]
                        if ((length(gdo$GageTolerance) > 0) && (gdo$GageTolerance > 0)) {
                          ptRatio <- StudyVar / gdo$GageTolerance
                          temp <- data.frame(VarComp, VarCompContrib, Stdev, StudyVar, StudyVarContrib, ptRatio)
                          contribFrame <- data.frame(VarCompContrib, StudyVarContrib, ptRatio)
                          names(temp)[6] <- c("P/T Ratio")
                          row.names(temp) <- c(Source)
                          SNR <- sqrt(2 * (temp["bTob", "VarComp"] / temp["totalRR", "VarComp"]))
                        } else {
                          temp <- data.frame(VarComp, VarCompContrib, Stdev, StudyVar, StudyVarContrib)
                          contribFrame <- data.frame(VarCompContrib, StudyVarContrib)
                        }
                        bTob <- paste(bName, "To", bName, sep = "")
                        Source[Source == "bTob"] <- bTob
                        row.names(contribFrame) <- Source
                        if (gdo$method == "crossed")
                          contribFrame <- contribFrame[-match(c("totalVar", "a", "a_b"), row.names(temp)), ]
                        else contribFrame <- contribFrame[-match(c("totalVar"), row.names(temp)), ]
                        numBars <-ncol(contribFrame)

                        contrib_df <- as.data.frame(contribFrame)
                        contrib_df$component <- rownames(contribFrame)
                        contrib_df <- contrib_df %>% rownames_to_column(var = "Source") %>% filter(Source != "totalVar")
                        ymax <- max(max(contribFrame))
                        main1 <- NA

                        contribFrame_long <- as.data.frame(contribFrame)
                        contribFrame_long$Component <- rownames(contribFrame_long)
                        contribFrame_long <- tidyr::gather(contribFrame_long, key = "Metric", value = "Value", -Component)
                        # 1. Components of Variation --------------------------------------------------
                        p1 <- ggplot(contribFrame_long, aes(x = Component, y = Value, fill = Metric)) +
                          geom_bar(stat = "identity", position = "dodge") +
                          labs(title = ifelse(is.null(main[1]), "Components of Variation", main[1]),
                               x = ifelse(is.null(xlab[1]), "Component", xlab[1]),
                               y = ifelse(is.null(ylab[1]), "", ylab[1])) +
                          theme_bw() +
                          scale_fill_manual(values = col[1:nlevels(factor(contribFrame_long$Metric))]) +
                          theme(legend.position = "top",
                                legend.background = element_rect(fill = "white", colour = "black"),

                                legend.key.size = unit(0.3, "cm"), # reduce the size of the legend key
                                legend.text = element_text(size = 6), # reduce the size of the legend text
                                legend.box.spacing = unit(0.1, 'cm'),
                                legend.title = element_blank(),
                                plot.title = element_text(hjust = 0.5)
                          )


                        # ----------------------
                        if (gdo$method == "crossed") {
                          # 2. Measurement by part ------------------------------------------------------
                          main2 <- NA
                          if (missing(main) || is.null(main[2]))
                            main2 <- paste(yName, "by", bName)
                          else main2 <- main[2]
                          xlab2 <- NA
                          if (missing(xlab) || is.null(xlab[2]))
                            xlab2 <- bName
                          else xlab2 <- xlab[2]
                          ylab2 <- NA
                          if (missing(ylab) || is.null(ylab[2]))
                            ylab2 <- yName
                          else ylab2 <- ylab[2]

                          p2 <- suppressWarnings(
                            ggplot(gdo$X, aes_string(x = bName, y = yName)) +
                              geom_boxplot() +
                              stat_summary(fun = median, geom = "line", aes(group = 1), color = "red", linewidth = lwd) +
                              stat_summary(fun = median, geom = "point", color = "red") +
                              labs(title = ifelse(is.null(main2), paste(yName, "by", bName), main2),
                                   x = ifelse(is.null(xlab2), bName, xlab2),
                                   y = ifelse(is.null(ylab2), yName, ylab2)) +
                              theme_bw() +
                              theme(plot.title = element_text(hjust = 0.5))
                          )


                          # 3. Measurement by operator --------------------------------------------------
                          main3 = NA
                          if (missing(main) || is.null(main[3]))
                            main3 = paste(yName, "by", aName)
                          else main3 = main[3]
                          xlab3 = NA
                          if (missing(xlab) || is.null(xlab[3]))
                            xlab3 = aName
                          else xlab3 = xlab[3]
                          ylab3 = NA
                          if (missing(ylab) || is.null(ylab[3]))
                            ylab3 = yName
                          else ylab3 = ylab[3]
                          col_op<-2:(length(unique(gdo$a)) + 1)
                          p3 <- ggplot(gdo$X, aes_string(x = aName, y = yName)) +
                            geom_boxplot(aes(fill = factor(gdo$X[, 3]))) +
                            stat_summary(fun = median, geom = "line", aes(group = 1), color = "red", linewidth = lwd) +
                            stat_summary(fun = median, geom = "point", color = "red", size = 3) +
                            labs(title = ifelse(is.null(main[3]), paste(yName, "by", aName), main[3]),
                                 x = ifelse(is.null(xlab[3]), aName, xlab[3]),
                                 y = ifelse(is.null(ylab[3]), yName, ylab[3]),
                                 fill = "Factor") +
                            theme_bw() +
                            scale_fill_manual(values = col_op) +
                            theme(plot.title = element_text(hjust = 0.5), legend.position='none')

                          # 4. X_mean Chart -------------------------------------------------------------------------------
                          agg = aggregate(gdo$X[, yName], list(gdo$X[, aName], gdo$X[, bName]), FUN = mean)
                          tab = table(agg[, 2])
                          sgSize = tab[1]
                          aggSd = aggregate(gdo$X[, yName], list(gdo$X[, bName], gdo$X[, aName]), FUN = sd)
                          tab = table(aggSd[, 2])
                          sm = mean(aggSd[, 3])
                          aggMean = aggregate(gdo$X[, yName], list(gdo$X[, bName], gdo$X[, aName]), FUN = mean)
                          xm = mean(agg[, 3])
                          UCL = xm + ((3 * sm)/(.c4(sgSize) * sqrt(sgSize)))
                          LCL = xm - ((3 * sm)/(.c4(sgSize) * sqrt(sgSize)))
                          values = c(UCL, xm, LCL)

                          p4 <- ggplot(data.frame(x = 1:length(aggMean[, 3]), y = aggMean[, 3]), aes(x, y)) +
                            geom_point(shape = 1) +
                            geom_line() +
                            geom_hline(yintercept = xm, color = 3) +                          # lC-nea xm
                            geom_hline(yintercept = UCL, color = "red") +                     # lC-nea UCL
                            geom_hline(yintercept = LCL, color = "red") +                     # lC-nea LCL
                            labs(x = aName, y = expression(bar(x))) +                         # tC-tulos ejes
                            ggtitle(expression(paste(bar(x), " Chart"))) +                    # tC-tulo
                            geom_vline(xintercept = cumsum(tab) - 0.5, linetype = "dashed") + # divide categorC-as A,B C
                            scale_x_continuous(breaks = cumsum(tab), labels = names(tab)) +   # y pone los nombres de las secciones
                            theme_bw() +
                            theme(plot.title = element_text(hjust = 0.5)) +
                            scale_y_continuous(expand = c(0, 0),
                                               sec.axis = sec_axis(~ ., breaks = c(UCL, xm, LCL),
                                                                   labels= c(paste("UCL =", round(UCL, 2)),
                                                                             paste("X_mean =", round(xm,2)),
                                                                             paste("UCL =", round(UCL, 2))
                                                                   ) )) +
                            theme(axis.text.y.right = element_text(size = 8))

                          # 5. Interaction Operator  ------------------------------------------------------------------
                          main4 <- NA
                          if (missing(main) || is.null(main[4]))
                            main4 <- paste("Interaction", abName)
                          else main4 <- main[4]
                          xlab4 <- NA
                          if (missing(xlab) || is.null(xlab[4]))
                            xlab4 <- colnames(gdo$X)[4]
                          else xlab4 <- xlab[4]
                          ylab4 <- NA
                          if (missing(ylab) || is.null(ylab[4]))
                            ylab4 <- paste(as.character(body(match.fun(fun)))[2], "of", colnames(gdo$X)[5])
                          else ylab4 <- ylab[4]
                          p5 <- .aip(gdo$X[, 4], gdo$X[, 3], response = gdo$X[, 5], xlab = xlab4, ylab = ylab4, title = "Interaction Operator: Part", legend = TRUE,col = col, type = "b", plot = FALSE)

                          # 6. R chart -------------------------------------------------
                          D3 <- c(0, 0, 0, 0, 0, 0.076, 0.136, 0.184, 0.223, 0.256, 0.284, 0.308, 0.329, 0.348)
                          D4 <- c(0, 3.267, 2.574, 2.282, 2.115, 2.004, 1.924, 1.864, 1.816, 1.777, 1.744, 1.716, 1.692, 1.671, 1.652)
                          helpRange = function(x) {
                            return(diff(range(x)))
                          }
                          aggForLimits <- aggregate(gdo$X[, yName], list(gdo$X[, aName], gdo$X[, bName]), FUN = helpRange)
                          Rm <- mean(aggForLimits[, 3])
                          sgSize <- length(unique(gdo$X[, bName]))
                          UCL <- D4[sgSize] * Rm
                          LCL <- D3[sgSize] * Rm
                          agg <- aggregate(gdo$X[, yName], list(gdo$X[, bName], gdo$X[, aName]), FUN = helpRange)
                          agg$Group.1 <- factor(agg$Group.1, levels = unique(agg$Group.1))
                          tab = table(agg[, 2])
                          sgSize = tab[1]

                          p6 <-  ggplot(data.frame(x = 1:length(agg[, 3]), y = agg[, 3]), aes(x, y)) +
                            geom_point(shape = 1) +
                            geom_line() +
                            geom_hline(yintercept = Rm, color = 3) +                          # lC-nea xm
                            geom_hline(yintercept = UCL, color = "red") +                     # lC-nea UCL
                            geom_hline(yintercept = LCL, color = "red") +                     # lC-nea LCL
                            labs(x = aName, y = "R") +                                        # tC-tulos ejes
                            ggtitle("R Chart") +                                              # tC-tulo
                            geom_vline(xintercept = cumsum(tab) - 0.5, linetype = "dashed") + # divide categorC-as A,B C
                            scale_x_continuous(breaks = cumsum(tab), labels = names(tab)) +   # y pone los nombres de las secciones
                            theme_bw() +
                            theme(plot.title = element_text(hjust = 0.5)) +
                            scale_y_continuous(expand = c(0, 0),
                                               sec.axis = sec_axis(~ ., breaks = c(UCL, Rm, LCL),
                                                                   labels= c(paste("UCL =", round(UCL, 2)),
                                                                             paste("R_mean =", round(Rm,2)),
                                                                             paste("UCL =", round(UCL, 2))
                                                                   ) )) +
                            theme(axis.text.y.right = element_text(size = 8))

                          # UNION PLOTS -------------
                          p <- p1 + p3 + p5$plot + p2 + p4 + p6 + plot_layout(nrow = 3, byrow = FALSE)

                        }

                        if(gdo$method == "nested"){
                          # 2. Measurement by Part within operator --------------
                          main2 = NA
                          if (missing(main) || is.null(main[2]))
                            main2 = paste(yName, "By", bName, "Within", aName)
                          else main2 = main[2]
                          xlab2 = NA
                          if (missing(xlab) || is.na(xlab[2]))
                            xlab2 = NA
                          else xlab2 = xlab[2]
                          ylab2 = NA
                          if (missing(ylab) || is.na(ylab[2]))
                            ylab2 = yName
                          else ylab2 = ylab[2]
                          agg = aggregate(gdo$X[, yName], list(gdo$X[, bName], gdo$X[, aName]), FUN = function(x){return(x)})

                          plot(1:nrow(agg), main = main2, xlab = xlab2, ylab = ylab2, ylim = range(agg[, 3]), axes = FALSE)
                          axis(2)
                          box()
                          label2 = ""
                          for (i in 1:nrow(agg)) {
                            points(rep(i, length(agg[i, 3])), agg[i, 3])
                            axis(1, at = i, labels = agg[i, 1])
                            if (agg[i, 2] != label2) {
                              axis(1, at = i, labels = agg[i, 2], line = 1, tick = FALSE)
                              label2 = agg[i, 2]
                            }
                          }
                          aggm = aggregate(gdo$X[, yName], list(gdo$X[, bName], gdo$X[, aName]), FUN = mean)
                          lines(aggm[, 3])
                          points(aggm[, 3], pch = 13, cex = 2)

                          # 3. Box Blot Measurement by Operator -----------------
                          main3 = NA
                          if (missing(main) || is.na(main[3]))
                            main3 = paste(yName, "by", aName)
                          else main3 = main[3]
                          xlab3 = NA
                          if (missing(xlab) || is.na(xlab[3]))
                            xlab3 = aName
                          else xlab3 = xlab[3]
                          ylab3 = NA
                          if (missing(ylab) || is.na(ylab[3]))
                            ylab3 = yName
                          else ylab3 = ylab[3]
                          col_op<-2:(length(unique(gdo$a)) + 1)
                          p3 <- ggplot(gdo$X, aes_string(x = aName, y = yName)) +
                            geom_boxplot(aes(fill = factor(gdo$X[, 3]))) +
                            stat_summary(fun = median, geom = "line", aes(group = 1), color = "red", linewidth = lwd) +
                            stat_summary(fun = median, geom = "point", color = "red", size = 3) +
                            labs(title = ifelse(is.null(main[3]), paste(yName, "by", aName), main[3]),
                                 x = ifelse(is.null(xlab[3]), aName, xlab[3]),
                                 y = ifelse(is.null(ylab[3]), yName, ylab[3]),
                                 fill = "Factor") +
                            theme_bw() +
                            scale_fill_manual(values = col_op) +
                            theme(plot.title = element_text(hjust = 0.5), legend.position='none')




                          # 4. X_mean Chart ----------------------
                          agg = aggregate(gdo$X[, yName], list(gdo$X[, aName], gdo$X[, bName]), FUN = mean)
                          tab = table(agg[, 2])
                          sgSize = tab[1]
                          aggSd = aggregate(gdo$X[, yName], list(gdo$X[, bName], gdo$X[, aName]), FUN = sd)
                          tab = table(aggSd[, 2])
                          sm = mean(aggSd[, 3])
                          aggMean = aggregate(gdo$X[, yName], list(gdo$X[, bName], gdo$X[, aName]), FUN = mean)
                          xm = mean(agg[, 3])
                          UCL = xm + ((3 * sm)/(.c4(sgSize) * sqrt(sgSize)))
                          LCL = xm - ((3 * sm)/(.c4(sgSize) * sqrt(sgSize)))
                          values = c(UCL, xm, LCL)

                          p4 <- ggplot(data.frame(x = 1:length(aggMean[, 3]), y = aggMean[, 3]), aes(x, y)) +
                            geom_point(shape = 1) +
                            geom_line() +
                            geom_hline(yintercept = xm, color = 3) +                          # lC-nea xm
                            geom_hline(yintercept = UCL, color = "red") +                     # lC-nea UCL
                            geom_hline(yintercept = LCL, color = "red") +                     # lC-nea LCL
                            labs(x = aName, y = expression(bar(x))) +                         # tC-tulos ejes
                            ggtitle(expression(paste(bar(x), " Chart"))) +                    # tC-tulo
                            geom_vline(xintercept = cumsum(tab) - 0.5, linetype = "dashed") + # divide categorC-as A,B C
                            scale_x_continuous(breaks = cumsum(tab), labels = names(tab)) +   # y pone los nombres de las secciones
                            theme_minimal() +
                            theme(plot.title = element_text(hjust = 0.5)) +
                            scale_y_continuous(expand = c(0, 0),
                                               sec.axis = sec_axis(~ ., breaks = c(UCL, xm, LCL),
                                                                   labels= c(paste("UCL =", round(UCL, 2)),
                                                                             paste("X_mean =", round(xm,2)),
                                                                             paste("UCL =", round(UCL, 2))
                                                                   ) )) +
                            theme(axis.text.y.right = element_text(size = 8))
                          # 5. R Chart --------------------------------------
                          D3 = c(0, 0, 0, 0, 0, 0.076, 0.136, 0.184, 0.223, 0.256, 0.284, 0.308, 0.329, 0.348)
                          D4 = c(0, 3.267, 2.574, 2.282, 2.115, 2.004, 1.924, 1.864, 1.816, 1.777, 1.744, 1.716, 1.692, 1.671, 1.652)
                          helpRange = function(x) {
                            return(diff(range(x)))
                          }
                          aggForLimits = aggregate(gdo$X[, yName], list(gdo$X[, aName], gdo$X[, bName]), FUN = helpRange)
                          Rm = mean(aggForLimits[, 3])
                          UCL = D4[sgSize] * Rm
                          LCL = D3[sgSize] * Rm
                          agg = aggregate(gdo$X[, yName], list(gdo$X[, bName], gdo$X[, aName]), FUN = helpRange)
                          tab = table(agg[, 2])
                          sgSize = tab[1]

                          p5 <-  ggplot(data.frame(x = 1:length(agg[, 3]), y = agg[, 3]), aes(x, y)) +
                            geom_point(shape = 1) +
                            geom_line() +
                            geom_hline(yintercept = Rm, color = 3) +                          # lC-nea xm
                            geom_hline(yintercept = UCL, color = "red") +                     # lC-nea UCL
                            geom_hline(yintercept = LCL, color = "red") +                     # lC-nea LCL
                            labs(x = aName, y = "R") +                                        # tC-tulos ejes
                            ggtitle("R Chart") +                                              # tC-tulo
                            geom_vline(xintercept = cumsum(tab) - 0.5, linetype = "dashed") + # divide categorC-as A,B C
                            scale_x_continuous(breaks = cumsum(tab), labels = names(tab)) +   # y pone los nombres de las secciones
                            theme_minimal() +
                            theme(plot.title = element_text(hjust = 0.5)) +
                            scale_y_continuous(expand = c(0, 0),
                                               sec.axis = sec_axis(~ ., breaks = c(UCL, Rm, LCL),
                                                                   labels= c(paste("UCL =", round(UCL, 2)),
                                                                             paste("R_mean =", round(Rm,2)),
                                                                             paste("UCL =", round(UCL, 2))
                                                                   ) )) +
                            theme(axis.text.y.right = element_text(size = 8))

                          # UNION PLOTS -----------------------
                          p <- p1 / (p2 + p3) / (p4 + p5) # p2 falta cambiar a ggplot2
                        }

                        show(p)
                        invisible(list(plot = p))
                      },


                      #' @description The data from an object of class \code{gageRR} can be analyzed by running “Error Charts” of the individual deviations from the accepted rference values. These “Error Charts” are provided by the function \code{errorPlot}.
                      #' @param main a main title for the plot.
                      #' @param xlab A character string for the x-axis label.
                      #' @param ylab A character string for the y-axis label.
                      #' @param col Plotting color.
                      #' @param pch An integer specifying a symbol or a single character to be used as the default in plotting points.
                      #' @param ylim The y limits of the plot.
                      #' @param legend A logical value specifying whether a legend is plotted automatically. By default legend is set to ‘TRUE’.
                      #' @examples
                      #' # Create gageRR-object
                      #' gdo = gageRRDesign(Operators = 3, Parts = 10, Measurements = 3, randomize = FALSE)
                      #' # Vector of responses
                      #' y = c(0.29,0.08, 0.04,-0.56,-0.47,-1.38,1.34,1.19,0.88,0.47,0.01,0.14,-0.80,
                      #'       -0.56,-1.46, 0.02,-0.20,-0.29,0.59,0.47,0.02,-0.31,-0.63,-0.46,2.26,
                      #'       1.80,1.77,-1.36,-1.68,-1.49,0.41,0.25,-0.11,-0.68,-1.22,-1.13,1.17,0.94,
                      #'       1.09,0.50,1.03,0.20,-0.92,-1.20,-1.07,-0.11, 0.22,-0.67,0.75,0.55,0.01,
                      #'       -0.20, 0.08,-0.56,1.99,2.12,1.45,-1.25,-1.62,-1.77,0.64,0.07,-0.15,-0.58,
                      #'       -0.68,-0.96,1.27,1.34,0.67,0.64,0.20,0.11,-0.84,-1.28,-1.45,-0.21,0.06,
                      #'       -0.49,0.66,0.83,0.21,-0.17,-0.34,-0.49,2.01,2.19,1.87,-1.31,-1.50,-2.16)
                      #'
                      #' # Appropriate responses
                      #' gdo$response(y)
                      #' # Perform and gageRR
                      #' gdo <- gageRR(gdo)
                      #'
                      #' gdo$errorPlot()
                      errorPlot = function(main, xlab, ylab, col, pch, ylim, legend = TRUE){
                        x <- self
                        ops = length(unique(x$subset(j=3)))
                        pts = length(unique(x$subset(j=4)))
                        n = length(x$subset(j=5))/pts/ops
                        ref = mean(x$get.response())
                        val = x$as.data.frame()
                        dat = val
                        if (missing(xlab))
                          xlab = paste(names(val)[4], "s", sep = "")
                        if (missing(ylab))
                          ylab = "Error"
                        if (missing(main))
                          main = paste("Error plot")
                        if (missing(pch))
                          pch = 1:ops
                        if (length(pch) < ops)
                          pch = rep(pch, ops)
                        if (missing(col))
                          col = rainbow(ops)
                        if (length(col) < ops)
                          col = rep(col, ops)
                        if (missing(ylim)) {
                          max_y = numeric(pts)
                          min_y = numeric(pts)
                          for (i in 1:pts) {
                            dat = subset(val, val[, 4] == unique(x$subset(j=4))[i])
                            max_y[i] = max(dat[, 5] - mean(dat[, 5]))
                            min_y[i] = min(dat[, 5] - mean(dat[, 5]))
                          }
                          ylim = c(-1.25 * abs(min(min_y)), abs(max(max_y)))
                        }

                        j <- 0
                        plot_data <- data.frame()

                        legend_labels <- unique(x$subset(j = 3))
                        names(legend_labels) <- paste0("Group ", 1:length(legend_labels))

                        for (i in 1:pts) {
                          dat <- subset(val, val[, 4] == unique(x$subset(j=4))[i])
                          dat <- dat[order(dat[, 3]), ]
                          j <- j + 1
                          for (k in 1:ops) {
                            subset_data <- data.frame(
                              x = (j + 1):(j + n),
                              y = (dat[(((k * n) - n + 1):(k * n)), 5] - mean(dat[, 5])),
                              group = rep(i, n),
                              shape_color = factor(paste0("Group ", k), levels = paste0("Group ", 1:ops))  # Mantener el orden de los grupos
                            )
                            plot_data <- rbind(plot_data, subset_data)
                            j <- j + n
                          }
                        }

                        p <- ggplot(plot_data, aes(x = x, y = y, color = shape_color, shape = shape_color, group = group)) +
                          geom_point() +
                          geom_line() +
                          labs(
                            x = xlab,
                            y = ylab,
                            title = main,
                            color = paste(names(val)[3], "(s):", sep = ""),  # Título de la leyenda
                            shape = paste(names(val)[3], "(s):", sep = "")
                          ) +
                          scale_color_manual(values = col, labels = as.vector(legend_labels)) +  # Etiquetas personalizadas
                          scale_shape_manual(values = pch, labels = as.vector(legend_labels)) +  # Etiquetas personalizadas
                          theme_classic() +
                          scale_x_continuous(breaks = seq(1 + (n * ops / 2), pts * n * ops + pts + 1 - (n * ops / 2), length = pts),
                                             labels = unique(x$subset(j=4))) +
                          ylim(ylim) +
                          geom_vline(xintercept = seq(1, pts * n * ops + pts + 1, by = n * ops + 1), col = "gray") +
                          geom_hline(yintercept = 0, col = "gray", linetype = "dashed") +
                          theme(panel.border = element_rect(colour = "black", fill = NA),
                                plot.title = element_text(hjust = 0.5,face = "bold"))
                        if (legend){
                          p <- p + theme(legend.position="right",legend.background = element_rect(fill = "white", colour = "black"))
                        }
                        else{
                          p <- p + theme(legend.position = "none")
                        }
                        show(p)
                        invisible(list(plot = p))
                      },


                      #' @description In a Whiskers Chart, the high and low data values and the average (median) by part-by-operator are plotted to provide insight into the consistency between operators, to indicate outliers and to discover part-operator interactions. The Whiskers Chart reminds of boxplots for every part and every operator.
                      #' @param main a main title for the plot.
                      #' @param xlab A character string for the x-axis label.
                      #' @param ylab A character string for the y-axis label.
                      #' @param col Plotting color.
                      #' @param ylim The y limits of the plot.
                      #' @param legend A logical value specifying whether a legend is plotted automatically. By default legend is set to ‘TRUE’.
                      #' @examples
                      #' # Create gageRR-object
                      #' gdo = gageRRDesign(Operators = 3, Parts = 10, Measurements = 3, randomize = FALSE)
                      #' # Vector of responses
                      #' y = c(0.29,0.08, 0.04,-0.56,-0.47,-1.38,1.34,1.19,0.88,0.47,0.01,0.14,-0.80,
                      #'       -0.56,-1.46, 0.02,-0.20,-0.29,0.59,0.47,0.02,-0.31,-0.63,-0.46,2.26,
                      #'       1.80,1.77,-1.36,-1.68,-1.49,0.41,0.25,-0.11,-0.68,-1.22,-1.13,1.17,0.94,
                      #'       1.09,0.50,1.03,0.20,-0.92,-1.20,-1.07,-0.11, 0.22,-0.67,0.75,0.55,0.01,
                      #'       -0.20, 0.08,-0.56,1.99,2.12,1.45,-1.25,-1.62,-1.77,0.64,0.07,-0.15,-0.58,
                      #'       -0.68,-0.96,1.27,1.34,0.67,0.64,0.20,0.11,-0.84,-1.28,-1.45,-0.21,0.06,
                      #'       -0.49,0.66,0.83,0.21,-0.17,-0.34,-0.49,2.01,2.19,1.87,-1.31,-1.50,-2.16)
                      #'
                      #' # Appropriate responses
                      #' gdo$response(y)
                      #' # Perform and gageRR
                      #' gdo <- gageRR(gdo)
                      #'
                      #' gdo$whiskersPlot()
                      whiskersPlot = function(main, xlab, ylab, col, ylim, legend = TRUE){
                        x <- self

                        ops = length(unique(x$subset(j=3)))
                        pts = length(unique(x$subset(j=4)))
                        n = length(x$subset(j=5))/pts/ops
                        val = x$as.data.frame()
                        dat = val
                        if (missing(xlab))
                          xlab = paste(names(val)[4], "s", sep = "")
                        if (missing(ylab))
                          ylab = "Value"
                        if (missing(main))
                          main = paste("Whiskers Plot")
                        if (missing(col))
                          col = heat.colors(ops)
                        if (length(col) < ops)
                          col = rep(col, ops)
                        if (missing(ylim)) {
                          max_y = numeric(pts)
                          min_y = numeric(pts)
                          for (i in 1:pts) {
                            dat = subset(val, val[, 4] == unique(x$subset(j=4))[i])
                            max_y[i] = max(dat[, 5])
                            min_y[i] = min(dat[, 5])
                          }
                          ylim = c(-1.25 * abs(min(min_y)), abs(max(max_y)))
                        }
                        j <- 0
                        plot_data <- data.frame()

                        legend_labels <- unique(x$subset(j = 3))

                        for (i in 1:pts) {
                          dat <- subset(val, val[, 4] == unique(x$subset(j=4))[i])
                          dat <- dat[order(dat[, 3]), ]
                          j <- j + 1
                          for (k in 1:ops) {
                            subset_data <- data.frame(
                              x_min = j + 1,
                              x_max = j + n,
                              y_min = min(dat[(((k * n) - n + 1):(k * n)), 5]),
                              y_max = max(dat[(((k * n) - n + 1):(k * n)), 5]),
                              y_mean = median(dat[(((k * n) - n + 1):(k * n)), 5]),
                              group = rep(i, n),
                              fill_color = factor(legend_labels[k], levels = legend_labels)  # Asignar directamente las etiquetas correctas
                            )
                            plot_data <- rbind(plot_data, subset_data)
                            j <- j + n
                          }
                        }

                        p <- ggplot(plot_data) +
                          geom_rect(aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = fill_color), color = "black", alpha = 0.5) +
                          geom_segment(aes(x = x_min, xend = x_max, y = y_mean, yend = y_mean), color = "black", size = 1) +
                          labs(
                            x = xlab,
                            y = ylab,
                            title = main,
                            fill = paste(names(val)[3], "(s):", sep = "")
                          ) +
                          scale_fill_manual(values = col, labels = legend_labels) +  # Etiquetas personalizadas
                          theme_classic() +
                          scale_x_continuous(breaks = seq(1 + (n * ops / 2), pts * n * ops + pts + 1 - (n * ops / 2), length = pts),
                                             labels = unique(x$subset(j=4))) +
                          ylim(ylim) +
                          geom_vline(xintercept = seq(1, pts * n * ops + pts + 1, by = n * ops + 1), col = "gray") +
                          geom_hline(yintercept = 0, col = "gray", linetype = "dashed") +
                          theme(panel.border = element_rect(colour = "black", fill = NA),
                                plot.title = element_text(hjust = 0.5,face = "bold"))

                        if (legend) {
                          p <- p + theme(legend.position = "right", legend.background = element_rect(fill = "white", colour = "black"))
                        } else {
                          p <- p + theme(legend.position = "none")
                        }
                        show(p)
                        invisible(list(plot = p))

                      },


                      #' @description \code{averagePlot} creates all x-y plots of averages by size out of an object of class \code{gageRR}. Therfore the averages of the multiple readings by each operator on each part are plotted with the reference value or overall part averages as the index.
                      #' @param main a main title for the plot.
                      #' @param xlab A character string for the x-axis label.
                      #' @param ylab A character string for the y-axis label.
                      #' @param col Plotting color.
                      #' @param single A logical value.If ‘TRUE’ a new graphic device will be opened for each plot. By default \code{single} is set to ‘FALSE’.
                      #' @examples
                      #' # Create gageRR-object
                      #' gdo = gageRRDesign(Operators = 3, Parts = 10, Measurements = 3, randomize = FALSE)
                      #' # Vector of responses
                      #' y = c(0.29,0.08, 0.04,-0.56,-0.47,-1.38,1.34,1.19,0.88,0.47,0.01,0.14,-0.80,
                      #'       -0.56,-1.46, 0.02,-0.20,-0.29,0.59,0.47,0.02,-0.31,-0.63,-0.46,2.26,
                      #'       1.80,1.77,-1.36,-1.68,-1.49,0.41,0.25,-0.11,-0.68,-1.22,-1.13,1.17,0.94,
                      #'       1.09,0.50,1.03,0.20,-0.92,-1.20,-1.07,-0.11, 0.22,-0.67,0.75,0.55,0.01,
                      #'       -0.20, 0.08,-0.56,1.99,2.12,1.45,-1.25,-1.62,-1.77,0.64,0.07,-0.15,-0.58,
                      #'       -0.68,-0.96,1.27,1.34,0.67,0.64,0.20,0.11,-0.84,-1.28,-1.45,-0.21,0.06,
                      #'       -0.49,0.66,0.83,0.21,-0.17,-0.34,-0.49,2.01,2.19,1.87,-1.31,-1.50,-2.16)
                      #'
                      #' # Appropriate responses
                      #' gdo$response(y)
                      #' # Perform and gageRR
                      #' gdo <- gageRR(gdo)
                      #'
                      #' gdo$averagePlot()
                      averagePlot = function(main, xlab, ylab, col, single = FALSE){
                        x <- self
                        ops = length(unique(x$subset(j=3)))
                        pts = length(unique(x$subset(j=4)))
                        n = length(x$subset(j=5))/pts/ops
                        ref = numeric(pts * n)
                        val = x$as.data.frame()
                        dat = val
                        if (missing(xlab))
                          xlab = "reference"
                        if (missing(ylab))
                          ylab = "values"
                        if (missing(col))
                          col = 1
                        if (missing(main))
                          main = as.vector(paste(names(val)[3], unique(x$subset(j=3))))
                        scr = TRUE
                        if (identical(par("mfrow"), as.integer(c(1, 1))) == TRUE)
                          scr = FALSE
                        k = 0
                        m = 1
                        for (j in 1:pts) {
                          dat = subset(val, val[, 4] == unique(x$subset(j=4))[j])
                          ref[(k + 1):(k + n)] = mean(dat[, 5])
                          k = k + n
                        }

                        if (single == TRUE){
                          plots <- list()

                          for (i in 1:ops) {
                            dat <- subset(val, val[, 3] == unique(x$subset(j=3))[i])
                            dat <- dat[order(dat[, 4]), ]

                            p <- ggplot(data = dat, aes(x = ref, y = dat[, 5])) +
                              geom_point(color = col) +
                              labs(title = main[i], x = xlab, y = ylab) +
                              theme_classic() +
                              theme(panel.border = element_rect(colour = "black", fill = NA),
                                    plot.title = element_text(hjust = 0.5,face = "bold"))

                            plots[[i]] <- p
                            print(p)
                          }

                          invisible(list(plot = plots))
                        }
                        else{
                          plots <- list()

                          for (i in 1:ops) {
                            dat <- subset(val, val[, 3] == unique(x$subset(j=3))[i])
                            dat <- dat[order(dat[, 4]), ]

                            p <- ggplot(data = dat, aes(x = ref, y = dat[, 5])) +
                              geom_point(color = col) +
                              labs(title = main[i], x = xlab, y = ylab) +
                              theme_classic() +
                              theme(panel.border = element_rect(colour = "black", fill = NA),
                                    plot.title = element_text(hjust = 0.5,face = "bold"))

                            plots[[i]] <- p
                          }

                          final_plot <- wrap_plots(plots, nrow = .splitDev(ops)[[2]][1], ncol = .splitDev(ops)[[2]][2])

                          print(final_plot)
                          invisible(list(plot = final_plot))
                        }
                      },


                      #' @description \code{compPlot} creates comparison x-y plots of an object of class \code{gageRR}. The averages of the multiple readings by each operator on each part are plotted against each other with the operators as indices. This plot compares the values obtained by one operator to those of another.
                      #' @param main a main title for the plot.
                      #' @param xlab A character string for the x-axis label.
                      #' @param ylab A character string for the y-axis label.
                      #' @param col Plotting color.
                      #' @param cex.lab The magnification to be used for x and y labels relative to the current setting of cex.
                      #' @param fun Optional function that will be applied to the multiple readings of each part. fun should be an object of class \code{function} like \code{mean},\code{median}, \code{sum}, etc. By default, \code{fun} is set to ‘NULL’ and all readings will be plotted.
                      #' @examples
                      #' # Create gageRR-object
                      #' gdo = gageRRDesign(Operators = 3, Parts = 10, Measurements = 3, randomize = FALSE)
                      #' # Vector of responses
                      #' y = c(0.29,0.08, 0.04,-0.56,-0.47,-1.38,1.34,1.19,0.88,0.47,0.01,0.14,-0.80,
                      #'       -0.56,-1.46, 0.02,-0.20,-0.29,0.59,0.47,0.02,-0.31,-0.63,-0.46,2.26,
                      #'       1.80,1.77,-1.36,-1.68,-1.49,0.41,0.25,-0.11,-0.68,-1.22,-1.13,1.17,0.94,
                      #'       1.09,0.50,1.03,0.20,-0.92,-1.20,-1.07,-0.11, 0.22,-0.67,0.75,0.55,0.01,
                      #'       -0.20, 0.08,-0.56,1.99,2.12,1.45,-1.25,-1.62,-1.77,0.64,0.07,-0.15,-0.58,
                      #'       -0.68,-0.96,1.27,1.34,0.67,0.64,0.20,0.11,-0.84,-1.28,-1.45,-0.21,0.06,
                      #'       -0.49,0.66,0.83,0.21,-0.17,-0.34,-0.49,2.01,2.19,1.87,-1.31,-1.50,-2.16)
                      #'
                      #' # Appropriate responses
                      #' gdo$response(y)
                      #' # Perform and gageRR
                      #' gdo <- gageRR(gdo)
                      #'
                      #' gdo$compPlot()
                      compPlot = function(main, xlab, ylab, col, cex.lab, fun = NULL){

                        x <- self
                        if (missing(xlab))
                          xlab = ""
                        if (missing(ylab))
                          ylab = ""
                        if (missing(col))
                          col = 1
                        ops = length(unique(x$subset(j=3)))
                        pts = length(unique(x$subset(j=4)))
                        n = length(x$subset(j=5))/pts/ops
                        comp = unique(x$subset(j=4))
                        val = x$as.data.frame()
                        dat = val
                        if (identical(fun, NULL) == FALSE)
                          means = matrix(data = NA, nrow = ops, ncol = pts)
                        if (identical(fun, NULL))
                          means = matrix(data = NA, nrow = ops, ncol = n * pts)
                        if (missing(main))
                          main = paste("Comparison Plot for", names(val)[3])
                        if (missing(cex.lab))
                          cex.lab = 10/ops

                        plots <- list()

                        for (i in 1:ops) {
                          dat <- subset(val, val[, 3] == unique(x$subset(j=3))[i])
                          if (!is.null(fun)) {
                            for (j in 1:pts) means[i, j] <- fun(subset(dat, dat[, 4] == unique(dat[, 4])[j])[, 5])
                          } else {
                            means[i, ] <- dat[, 5]
                          }

                          for (k in 1:ops) {
                            if (k == i) {
                              p <- ggplot() +
                                geom_blank() +
                                annotate("text", x = 5.5, y = 5, label = unique(x$subset(j=3))[i], size = cex.lab, fontface = "bold") +
                                theme_void()
                              plots[[length(plots) + 1]] <- p
                            } else if (k < i) {
                              plot_data <- data.frame(x = means[k, ], y = means[i, ])
                              p <- ggplot(plot_data, aes(x = x, y = y)) +
                                geom_point(color = col) +
                                labs(x = xlab, y = ylab) +
                                theme_classic() +
                                theme(axis.title.x = element_blank(), axis.title.y = element_blank())
                              plots[[length(plots) + 1]] <- p
                            } else {
                              p <- ggplot() +
                                geom_blank() +
                                theme_void()
                              plots[[length(plots) + 1]] <- p
                            }
                          }
                        }

                        final_plot <- wrap_plots(plots, nrow = ops, ncol = ops) +
                          plot_annotation(
                            title = main,
                            theme = theme(
                              plot.title = element_text(hjust = 0.5,face = "bold") # Centrar el título
                            )
                          )

                        print(final_plot)
                        invisible(list(plot = final_plot))
                      }
                    )
)


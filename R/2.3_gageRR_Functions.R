##################################################################
##################### gageRR - FUNCIONES #########################
##################################################################

# gageRRDesign ----
gageRRDesign = function(Operators = 3, Parts = 10, Measurements = 3,
                        method = "crossed", sigma = 6, randomize = TRUE){
  #' @title gageRRDesign: Gage R&R - Gage Repeatability and Reproducibility
  #' @description Function to Creates a Gage R&R design.
  #' @param Operators Numeric value giving a number or a character vector defining the Operators.
  #' By default \code{Operators} is set to `3`.
  #' @param Parts A number or character vector defining the Parts.
  #' By default \code{parts} is set to `10`.
  #' @param Measurements A number defining the measurements per part. By default \code{Measurements} is set to `3`.
  #' @param method Character string specifying the Gage R&R method. \code{`crossed`} which is the typical design for performing a Measurement Systems Analysis using Gage Repeatability and Reproducibility or \code{`nested`} which is used for destructive testing (i.e. the same part cannot be measured twice). Operators measure each a different sample of parts under the premise that the parts of each batch are alike.
  #' By default \code{method} is set to \code{`crossed`}.
  #' @param sigma For \code{sigma=6} this relates to 99.73 percent representing the full spread of a normal distribution function (i.e. \code{pnorm(3) - pnorm(-3)}).
  #' Another popular setting \code{sigma=5.15} relates to 99 percent (i.e. \code{pnorm(2.575) - pnorm(-2.575)}). By default \code{sigma} is set to `6`.
  #' @param randomize Logical value. \code{TRUE} (default) randomizes the gageRR design.
  #' @return The function \code{gageRRDesign} returns an object of class \code{gageRR}.
  #' @seealso \code{\link{gageRR.c}}, \code{\link{gageRR}}.
  #' @examples
  #' design <- gageRRDesign(Operators = 3, Parts = 10, Measurements = 3,
  #'                        method = "crossed", sigma = 6, randomize = TRUE)

  if (!is.numeric(sigma))
    stop("sigma needs to be numeric")
  if (method != "nested" && method != "crossed")
    stop("Unknown method specified. Use 'method = nested' or 'method = crossed'.")
  Measurements <- as.integer(Measurements)
  if (!is.numeric(Measurements) || Measurements <= 0)
    stop("Number of Measurements per Part must be a positive integer.")


  opvec <- factor()
  partvec <- factor()

  yName <- "Measurement"
  aName <- "Operator"
  bName <- "Part"
  abName <- "Operator:Part"

  Operators <- unique(Operators)
  Parts <- unique(Parts)

  if (is.numeric(Operators))
    opvec <- factor(LETTERS[1:Operators[1]])
  if (is.character(Operators))
    opvec <- factor(Operators)

  if (length(unique(opvec)) > 26)
    stop("Too many Operators!")
  if (length(unique(opvec)) < 2)
    stop("Not enough Operators")

  if (is.numeric(Parts))
    partvec <- factor(LETTERS[1:Parts[1]])
  if (is.character(Parts))
    partvec <- factor(Parts)

  if (length(unique(partvec)) > 26)
    stop("Too many Parts!")
  if (length(unique(partvec)) < 2)
    stop("Too few Parts")

  Measurement <- rep(NA, (length(opvec) * length(partvec) * Measurements))
  outFrame <- data.frame()

  if (method == "crossed") {
    temp <- expand.grid(opvec, partvec)
    o <- rep(temp[, 1], Measurements)
    p <- rep(temp[, 2], Measurements)
  } else {
    p <- rep(sort(rep(partvec, length(opvec))), Measurements)
    o <- (rep(opvec, length(Measurement) / length(opvec)))
    p <- p[order(o,p)]
    o <- o[order(o,p)]
  }

  if (randomize)
    outFrame <- data.frame(StandardOrder = 1:length(Measurement), RunOrder = sample(1:length(Measurement), length(Measurement)), Operator = factor(o), Part = factor(p), Measurement)
  else
    outFrame <- data.frame(StandardOrder = 1:length(Measurement), RunOrder = 1:length(Measurement), Operator = factor(o), Part = factor(p), Measurement)

  outFrame <- outFrame[order(outFrame$RunOrder), ]
  # Valores predeterminados
  gageRRObj <- gageRR.c$new(
    X = outFrame,
    ANOVA = NULL,
    RedANOVA = NULL,
    method = method,
    Estimates = NULL,
    Varcomp = NULL,
    Sigma = sigma,
    GageName = NULL,
    GageTolerance = NULL,
    DateOfStudy = Sys.Date(),
    PersonResponsible = NULL,
    Comments = NULL,
    b = factor(p),
    a = factor(o),
    y = as.numeric(Measurement),
    facNames = c(yName, aName, bName, abName),
    numO = length(unique(opvec)),  # NC:mero de operadores
    numP = length(unique(partvec)),  # NC:mero de partes
    numM = Measurements  # NC:mero de mediciones
  )

  return(gageRRObj)
}

# gageRR ----
gageRR <- function(gdo, method = "crossed", sigma = 6, alpha = 0.25,
                  tolerance = NULL, dig = 3) {
  #' @title gageRR: Gage R&R - Gage Repeatability and Reproducibility
  #' @description Performs a Gage R&R analysis for an object of class \code{\link{gageRR.c}}.
  #' @param gdo Needs to be an object of class \code{gageRR.c}.
  #' @param method Character string specifying the Gage R&R method. \code{`crossed`} which is the typical design for performing a Measurement Systems Analysis using Gage Repeatability and Reproducibility or \code{`nested`} which is used for destructive testing (i.e. the same part cannot be measured twice). Operators measure each a different sample of parts under the premise that the parts of each batch are alike.
  #' By default \code{method} is set to \code{`crossed`}.
  #' @param sigma Numeric value giving the number of sigmas.
  #' For \code{sigma=6} this relates to 99.73 percent representing the full spread of a normal distribution function (i.e. \code{pnorm(3) - pnorm(-3)}).
  #' Another popular setting \code{sigma=5.15} relates to 99 percent (i.e. \code{pnorm(2.575) - pnorm(-2.575)}). By default \code{sigma} is set to `6`.
  #' @param alpha Alpha value for discarding the interaction Operator:Part and fitting a non-interaction model. By default \code{alpha} is set to `0.25`.
  #' @param tolerance Mumeric value giving the tolerance for the measured parts. This is required to calculate the Process to Tolerance Ratio.
  #' By default \code{tolerance} is set to \code{NULL}.
  #' @param dig numeric value giving the number of significant digits for \code{format}.
  #' By default \code{dig} is set to `3`.
  #' @return The function \code{gageRR} returns an object of class \code{gageRR.c} and shows typical Gage Repeatability and Reproducibility Output including Process to Tolerance Ratios and the number of distinctive categories (i.e. ndc) the measurement system is able to discriminate with the tested setting.
  #' @seealso \code{\link{gageRR.c}}, \code{\link{gageRRDesign}}, \code{\link{gageLin}}, \code{\link{cg}}.
  #' @examples
  #' # Create de gageRR Design
  #' design <- gageRRDesign(Operators = 3, Parts = 10, Measurements = 3,
  #'                        method = "crossed", sigma = 6, randomize = TRUE)
  #' design$response(rnorm(nrow(design$X), mean = 10, sd = 2))
  #'
  #' # Results of de Design
  #' result <- gageRR(gdo = design, method = "crossed", sigma = 6, alpha = 0.25)
  #' class(result)
  #' result$plot()

  yName <- "Measurement"
  aName <- "Operator"
  bName <- "Part"

  abName <- if(method == "crossed") paste(aName, ":", bName, sep = "")
  else if(method == "nested") paste(bName, "(", aName, ")", sep = "")
  else NA

  bTobName <- paste(bName, "to", bName, sep = " ")

  if (!is.null(tolerance)) gdo$set.tolerance(tolerance)

  y <- gdo$X[[yName]]
  a <- gdo$X[[aName]]
  b <- gdo$X[[bName]]

  nestedFormula <- as.formula(paste(yName, "~", aName, "/", bName))
  crossedFormula <- as.formula(paste(yName, "~", aName, "*", bName))
  reducedFormula <- as.formula(paste(yName, "~", aName, "+", bName))

  if (method == "nested") {
    numA <- nlevels(a)
    numB <- nlevels(b)
    numMPP <- length(y) / (numB * numA)

    gdo$numO <- numA
    gdo$numP <- numB
    gdo$numM <- numMPP

    fit <- aov(nestedFormula, data = gdo$X)
    meanSq <- anova(fit)[, 3]

    gdo$ANOVA <- fit
    gdo$method <- "nested"

    MSa <- meanSq[1]
    MSab <- meanSq[2]
    MSe <- meanSq[3]

    Cerror <- MSe
    Cb <- (MSab - MSe) / numMPP
    Ca <- (MSa - MSab) / (numB * numMPP)

    if (Ca <= 0) Ca <- 0
    if (Cb <= 0) Cb <- 0

    Cab <- 0
    totalRR <- Ca + Cab + Cerror
    repeatability <- Cerror
    reproducibility <- Ca
    bTob <- Cb
    totalVar <- Cb + Ca + Cab + Cerror

    estimates <- list(Cb = Cb, Ca = Ca, Cab = Cab, Cerror = Cerror)
    varcomp <- list(totalRR = totalRR, repeatability = repeatability, reproducibility = reproducibility, bTob = bTob, totalVar = totalVar)

    gdo$Estimates <- estimates
    gdo$Varcomp <- varcomp
  }

  if (method == "crossed") {
    numA <- nlevels(a)
    numB <- nlevels(b)
    numMPP <- length(a) / (numA * numB)

    gdo$numO <- numA
    gdo$numP <- numB
    gdo$numM <- numMPP

    fit <- aov(crossedFormula, data = gdo$X)
    model <- anova(fit)

    gdo$ANOVA <- fit
    gdo$method <- "crossed"

    MSb <- MSa <- MSab <- MSe <- 0

    if (bName %in% row.names(model)) MSb <- model[bName, "Mean Sq"]
    else warning(paste("missing factor", bName, "in model"))

    if (aName %in% row.names(model)) MSa <- model[aName, "Mean Sq"]
    else warning(paste("missing factor", aName, "in model"))

    if (abName %in% row.names(model)) MSab <- model[abName, "Mean Sq"]
    else warning(paste("missing interaction", abName, "in model"))

    if ("Residuals" %in% row.names(model)) MSe <- model["Residuals", "Mean Sq"]
    else warning("missing Residuals in model")

    Cb <- Ca <- Cab <- Cerror <- 0

    Cb <- (MSb - MSab) / (numA * numMPP)
    Ca <- (MSa - MSab) / (numB * numMPP)
    Cab <- (MSab - MSe) / numMPP
    Cerror <- (MSe)

    gdo$RedANOVA <- gdo$ANOVA

    if ((Cab < 0) || (model[abName, "Pr(>F)"] >= alpha)) {
      redFit <- aov(reducedFormula, data = gdo$X)
      model <- anova(redFit)

      MSb <- MSa <- MSab <- MSe <- 0

      if (bName %in% row.names(model)) MSb <- model[bName, "Mean Sq"]
      else warning(paste("missing factor", bName, "in model"))

      if (aName %in% row.names(model)) MSa <- model[aName, "Mean Sq"]
      else warning(paste("missing factor", aName, "in model"))

      if ("Residuals" %in% row.names(model)) MSe <- model["Residuals", "Mean Sq"]
      else warning("missing Residuals in model")

      Cb <- Ca <- Cab <- Cerror <- 0

      Cb <- (MSb - MSe) / (numA * numMPP)
      Ca <- (MSa - MSe) / (numB * numMPP)
      Cab <- 0
      Cerror <- (MSe)

      gdo$RedANOVA <- redFit
    }

    gdo$method <- "crossed"
    Ca <- max(0, Ca)
    Cb <- max(0, Cb)
    Cab <- max(0, Cab)

    totalRR <- Ca + Cab + Cerror
    repeatability <- Cerror
    reproducibility <- Ca + Cab
    bTob <- max(0, Cb)
    totalVar <- Cb + Ca + Cab + Cerror

    estimates <- list(Cb = Cb, Ca = Ca, Cab = Cab, Cerror = Cerror)
    varcomp <- list(totalRR = totalRR, repeatability = repeatability, reproducibility = reproducibility, a = Ca, a_b = Cab, bTob = bTob, totalVar = totalVar)

    gdo$Estimates <- estimates
    gdo$Varcomp <- varcomp
  }

  cat("\n")
  cat(paste("AnOVa Table - ", gdo$method, "Design\n"))
  print(summary(gdo$ANOVA))
  cat("\n")
  cat("----------\n")

  if (!identical(gdo$RedANOVA, gdo$ANOVA) && gdo$method == "crossed") {
    cat(paste("AnOVa Table Without Interaction - ", gdo$method, "Design\n"))
    print(summary(gdo$RedANOVA))
    cat("\n")
    cat("----------\n")
  }

  Source <- names(gdo$Varcomp)
  Source[Source == "repeatability"] <- " repeatability"
  Source[Source == "reproducibility"] <- " reproducibility"
  Source[Source == "a_b"] <- paste("  ", abName)
  Source[Source == "a"] <- paste("  ", aName)
  Source[Source == "bTob"] <- bTobName

  VarComp <- round(as.numeric(gdo$Varcomp[c(1:length(gdo$Varcomp))]), 3)
  Contribution <- round(as.numeric(gdo$Varcomp[c(1:length(gdo$Varcomp))]) / as.numeric(gdo$Varcomp[length(gdo$Varcomp)]), 3)
  VarComp <- t(data.frame(gdo$Varcomp))
  VarCompContrib <- VarComp / gdo$Varcomp$totalVar
  Stdev <- sqrt(VarComp)
  StudyVar <- Stdev * gdo$Sigma
  StudyVarContrib <- StudyVar / StudyVar["totalVar", ]
  SNR <- 1
  ptRatio <- NULL
  temp <- NULL

  if ((length(gdo$GageTolerance) > 0) && (gdo$GageTolerance > 0)) {
    ptRatio <- StudyVar / gdo$GageTolerance
    temp <- data.frame(VarComp, VarCompContrib, Stdev, StudyVar, StudyVarContrib, ptRatio)
    names(temp)[6] <- c("P/T Ratio")
    row.names(temp) <- c(Source)
  } else {
    temp <- data.frame(VarComp, VarCompContrib, Stdev, StudyVar, StudyVarContrib)
    row.names(temp) <- c(Source)
  }

  cat("\n")
  cat("Gage R&R\n")
  tempout <- temp
  print(format(tempout, digits = dig))
  cat("\n")
  cat("---\n")
  cat(" * Contrib equals Contribution in %\n")

  SNRTemp <- sqrt(2) * (temp[bTobName, "Stdev"] / temp["totalRR", "Stdev"])
  if (SNRTemp > 1) SNR <- SNRTemp

  cat(paste(" **Number of Distinct Categories (truncated signal-to-noise-ratio) =", floor(SNR), "\n"))
  cat("\n")
  invisible(gdo)
}

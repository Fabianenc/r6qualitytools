.curvTest = function(fdo, response, DB = FALSE) {
  fullString = character(0)
  interString = character(0)
  quadString = character(0)
  quadString2 = character(0)
  y = character(0)
  nameVec = fdo$names()
  pValue = NA
  if (!(try(is.character(response), silent = TRUE) == TRUE)) {
    y = deparse(substitute(response))
  }
  else if (response %in% names(fdo$.response()))
    y = response
  if (length(nameVec) < 1) {
    cat("\n")
    invisible("curvTest: not enough names (factors)!")
  }
  if (nrow(fdo$centerCube) <= 1) {
    cat("\n")
    invisible("curvTest: not enough centerPoints for a test for curvature")
  }
  if (nrow(fdo$star) > 0) {
    cat("\n")
    invisible("curvTest: star portion exists; nothing to do")
  }
  if (length(nameVec) == 1) {
    cat("\n")
    invisible(nameVec[1])
  }
  for (i in seq(along = nameVec)) {
    if (i == 1) {
      fullString = nameVec[i]
      if (DB)
        print(fullString)
    }
    if (length(nameVec) >= 2) {
      fullString = paste(fullString, "+", nameVec[i + 1])
      if (DB)
        print(fullString)
    }
    if ((i + 1) >= length(nameVec)) {
      if (DB)
        print("break")
      break
    }
  }
  for (k in 2:length(nameVec)) {
    if (DB)
      print(k)
    temp = combn(nameVec, k, simplify = TRUE)
    if (DB)
      print(temp)
    for (i in 1:ncol(temp)) {
      interString = character(0)
      for (j in 1:k) {
        if (j == 1)
          interString = temp[j, i]
        else interString = paste(interString, ":", temp[j, i])
      }
      fullString = paste(fullString, "+", interString)
    }
  }
  for (i in seq(along = nameVec)) {
    if (i == 1) {
      quadString = paste("I(", nameVec[i], "^2)", sep = "")
      quadString2 = paste("I(", nameVec[i], "^2)", sep = "")
      if (DB)
        print(quadString)
    }
    if (length(nameVec) >= 2) {
      quadString = paste(quadString, "+I(", nameVec[i + 1], "^2)", sep = "")
      quadString2 = c(quadString2, paste("I(", nameVec[i + 1], "^2)", sep = ""))
      if (DB) {
        print(quadString2)
        print(quadString)
      }
    }
    if ((i + 1) >= length(nameVec)) {
      if (DB)
        print("break")
      break
    }
  }
  fullString = paste(fullString, "+", quadString)
  fullString = paste(y, "~", fullString)
  if (DB)
    print(fullString)
  aov.1 = aov(formula = as.formula(fullString), data = fdo$as.data.frame())
  if (DB) {
    print(summary(aov.1))
    print(pmatch(quadString2, row.names(summary(aov.1)[[1]])))
  }
  rows = pmatch(quadString2, row.names(summary(aov.1)[[1]]))
  if (DB)
    print(rows)
  rows = row.names(summary(aov.1)[[1]])[rows[!is.na(rows)]]
  if (DB)
    print(rows)
  cols = "Pr(>F)"
  tempFrame = data.frame(summary(aov.1)[[1]][rows, cols])
  if (nrow(tempFrame) > 0) {
    row.names(tempFrame) = rows
    names(tempFrame) = cols
    pValue = format(tempFrame[1, 1], digits = 3)
  }
  out = paste("Test for Curvature:  p =", pValue)
  cat("\n")
  cat(out)
  cat("\n")
  invisible(pValue)
}
summaryFits = function(fdo, lmFit = TRUE, curvTest = TRUE, origFit = TRUE) {
  #' @title summaryFits: Fit Summary
  #' @description
  #' Function to provide an overview of fitted linear models for objects of class \code{\link{facDesign.c}}.
  #' @usage
  #' summaryFits(fdo, lmFit = TRUE, curvTest = TRUE, origFit = TRUE)
  #' @param fdo An object of class \code{\link{facDesign.c}}.
  #' @param lmFit A logical value deciding whether the fits from the object \code{fdo} should be included or not. By default, \code{lmFit} is set to \code{TRUE}.
  #' @param curvTest A logical value deciding whether curvature tests should be performed or not. By default, \code{curvTest} is set to \code{TRUE}.
  #' @param origFit A logical value. If \code{TRUE} (default), the original values of the fits will be displayed.
  #' @return A summary output of the fitted linear models, which may include the linear fits, curvature tests, and original fit values, depending on the input parameters.
  #'
  #' @examples
  #' dfac <- facDesign(k = 3)
  #' dfac$.response(data.frame(y = rnorm(8), y2 = rnorm(8)))
  #' dfac$set.fits(lm(y ~ A + B , data = dfac$as.data.frame()))
  #' dfac$set.fits(lm(y2 ~ A + C, data = dfac$as.data.frame()))
  #' summaryFits(dfac)

  summaryList = vector(mode = "list", length = 0)
  origFrame = fdo$as.data.frame()
  for (i in fdo$names()) origFrame[, i] = code2real(fdo$lows()[[i]], fdo$highs()[[i]], origFrame[, i])
  for (f in names(fdo$.response())) {
    if (!is.null(fdo$fits[[f]])) {
      cat(paste("----------- Summary for response '", f, "' -----------", sep = ""))
      cat("\n")
      print(summary(fdo$fits[[f]]))
      cat("-----------")
      cat("\n")
      cat("\n")
      cat("Regression in non coded form:")
      cat("\n")
      lm.f = (lm(formula(fdo$fits[[f]]), data = origFrame))
      coefs = coefficients(lm.f)
      coefsI = coefs[pmatch("(Intercept)", names(coefs))]
      coefsW = coefs[-pmatch("(Intercept)", names(coefs))]
      coefsW = coefsW[!is.na(coefsW)]
      temp = character(length(coefsW))
      temp[coefsW >= 0] = "+"
      temp[coefsW < 0] = "-"
      firstString = ""
      firstString = paste(firstString, format(coefsI, digits = 4))
      restString = paste(format(abs(coefsW), digits = 4), names(coefsW), sep = "*")
      restString = paste(temp, restString)
      restString = paste(restString, collapse = " ")
      fullString = paste(firstString, restString)
      fullString = paste(paste(f, " ="), fullString)
      cat("\n")
      cat(paste("  ", fullString))
      cat("\n")
      cat("\n")
      cat("-----------")
      cat("\n")
      .curvTest(fdo, f)
      cat("\n")
      cat("\n")
    }
  }
  invisible()
}


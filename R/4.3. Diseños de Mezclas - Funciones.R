##########################################################################
################ DISEÑOS DE MEZCLAS - FUNCIONES ##########################
##########################################################################

###Funcion mixDesign####

mixDesign <- function(p, n = 3, type = "lattice",
                      center = TRUE, axial = FALSE, delta,
                      replicates = 1, lower, total = 1,
                      randomize, seed=1234) {
  #' @title mixDesign: Mixture Designs
  #' @description Function to generate simplex lattice and simplex centroid mixture designs with optional center points and axial points.
  #' @param p Numerical value giving the amount of factors.
  #' @param n Numerical value specifying the degree (ignored if type = “centroid”).
  #' @param type Character string giving the type of design. \code{type} can be “lattice” or “centroid” (referencing to the first source under the section references].
  #' By default \code{type} is set to “lattice”.
  #' @param center Logical value specifying whether (optional) center points will be added.
  #' By default `center` is set to ‘TRUE’.
  #' @param axial Logical value specifying whether (optional) axial points will be added.
  #' By default `axial` is set to ‘FALSE’.
  #' @param delta Numerical value giving the delta (see references) for axial runs. No default setting.
  #' @param replicates Vector with the number of replicates for the different design points i.e. c(center = 1, axial = 1, pureBlend = 1, BinaryBlend = 1, p-3 blend, p-2 blend, p-1 blend).
  #' By default `replicates` is set to ‘1’.
  #' @param lower Numeric vector of lower-bound constraints on the component proportions (i.e. must be given in percent).
  #' @param total Numeric vector with
  #' \itemize{
  #' \item {[1] the percentage of the mixture made up by the q - components (e.g. q = 3 and x1 + x2 + x3 = 0.8 –> total = 0.8 with 0.2 for the other factors being held constant)}
  #' \item {[2] overall total in corresponding units (e.g. 200ml for the overall mixture)}
  #' }
  #' @param randomize Logical value. If ‘TRUE’ the RunOrder of the mixture design will be randomized (default).
  #' @param seed Nmerical value giving the input for set.seed.
  #' @return The function \code{mixDesig()} returns an object of class \code{mixDesig()}.
  #' @note
  #' In this version the creation of (augmented) lattice, centroid mixture designs is fully supported. Getters and Setter methods for the mixDesign object exist just as for objects of class \code{facDesign} (i.e. factorial designs).
  #'
  #' The creation of constrained component proportions is partially supported but don't rely on it. Visualization (i.e. ternary plots) for some of these designs can be done with the help of the \code{wirePlot3} and \code{contourPlot3} function.
  #'
  #' @seealso \code{\link{mixDesign.c}}, \code{\link{facDesign.c}}, \code{\link{facDesign}}, \code{\link{fracDesign}}, \code{\link{rsmDesign}}, \code{\link{wirePlot3}}, \code{\link{contourPlot3}}.
  #' @examples
  #' # Example usage of mixDesign
  #' mdo <- mixDesign(3, 2, center = FALSE, axial = FALSE, randomize = FALSE, replicates = c(1, 1, 2, 3))
  #'
  #' mdo$names(c("polyethylene", "polystyrene", "polypropylene"))
  #' elongation <- c(11.0, 12.4, 15.0, 14.8, 16.1, 17.7, 16.4, 16.6, 8.8, 10.0, 10.0, 9.7, 11.8, 16.8, 16.0)
  #' mdo$.response(elongation)
  #'
  #' mdo$units()
  #' mdo$summary()


  frameOut = NA
  out = mixDesign.c$new()
  if (missing(p))
    stop("the number of factors p must be given")
  if (p <= 1 | !is.numeric(p))
    stop("invalid value for p")
  if (!(type %in% c("lattice", "centroid")))
    stop("type needs to be \"lattice\" or \"centroid\"")
  out$designType = type
  if (missing(delta))
    delta = (p - 1)/(2 * p)
  if (missing(randomize))
    randomize = TRUE
  set.seed(seed)
  if (!is.numeric(total))
    stop("total needs to be a numeric vector with <= 2 arguments")
  else {
    if (total[1] > 1 || total[1] <= 0)
      stop("total[1] needs to be within (0,1]")
    if (is.na(total[2]))
      total[2] = 1
    if (total[2] <= 0)
      stop("total[2] needs to be > 0")
  }
  out$total = total
  if (!is.numeric(replicates))
    stop("replicates need to be numeric")
  if (delta > (p - 1)/p) {
    delta = (p - 1)/(2 * p)
    warning(paste("delta was reset to:", delta))
  }
  if (missing(lower))
    lower = 0
  if (length(lower) == 1)
    lower = rep(lower, p)
  if (length(lower) > 1) {
    initial = rep(0, p)
    if (length(lower) < p)
      lower[, (length(lower) + 1):p] = 0
  }
  out$lower = lower
  repTemp = list(center = 1, axial = 1)
  for (i in 1:(p - 1)) repTemp[[as.character(i)]] = 1
  if (length(replicates) > 1) {
    for (i in 1:length(replicates)) repTemp[[i]] = replicates[[i]]
    replicates = repTemp
  }
  if (length(replicates) == 1) {
    for (i in 1:length(repTemp)) repTemp[[i]] = replicates
    replicates = repTemp
  }

  N = factorial(p + n - 1)/(factorial(n) * factorial(p - 1))
  if (identical(type, "lattice")) {
    j = 1
    x = numeric(p)
    j = 1
    x[1] = n
    for (i in 1:N) {
      x[j + 1] = n - (sum(x[1:j]))
      if (j < (p - 1))
        x[(j + 2):p] = 0
      if (i == 1)
        frameOut = data.frame(matrix(x, ncol = p, nrow = 1))
      else frameOut = rbind(frameOut, x)

      logVec = rep(FALSE, p)
      logVec[1:(p - 1)] = x[1:p - 1] > 0
      if (any(logVec)) {
        j = max((1:p)[logVec])
        x[j] = x[j] - 1
      }
    }
    frameOut = (frameOut/(n))
    names(frameOut) = LETTERS[1:p]
  }
  if (identical(type, "centroid")) {
    frameOut = .simplexCentroid(p)
  }
  frameOutCopy = frameOut[1, ]
  Type = data.frame(Type = "1")
  temp = apply(ceiling(frameOut), 2, ">", 0) * 1
  temp = apply(temp, 1, sum)
  for (i in 1:nrow(frameOut)) {
    typ = as.character(temp[i])
    times = replicates[[typ]]
    if (is.null(times))
      times = 0
    else times = times - 1
    repFrame = frameOut[i, ]
    if (all(frameOut[i, ] == frameOut[i, 1]))
      typFrame = data.frame(Type = "center")
    else typFrame = data.frame(Type = paste(typ, "-blend", sep = ""))
    if (times >= 1) {
      for (j in 1:times) {
        repFrame = rbind(repFrame, frameOut[i, ])
        typFrame = rbind(typFrame, data.frame(Type = paste(typ, "-blend", sep = "")))
      }
    }
    frameOutCopy = rbind(frameOutCopy, repFrame)
    Type = rbind(Type, typFrame)
    if (i == 1) {
      frameOutCopy = frameOutCopy[c(-1), ]
      Type = data.frame(Type = Type[c(-1), ])
    }
  }
  frameOut = frameOutCopy

  keepIndex = (1:nrow(frameOut))[!apply(Type, 1, "==", "center")]
  Type = data.frame(Type = Type[keepIndex, ])
  frameOut = frameOut[keepIndex, ]
  if (center) {
    center = data.frame(matrix(1/p, nrow = 1, ncol = p))

    times = replicates$center
    if (is.null(times))
      times = 0
    else times = times - 1
    if (n == p)
      times = times - 1
    if ((n%%p) == 0) {
      numCenter = n%/%p
    }
    temp = center
    if (times >= 1) {
      for (i in 1:times) center = rbind(center, temp)
    }
    names(center) = names(frameOut)
    frameOut = rbind(frameOut, center)
    Type = rbind(Type, data.frame(Type = rep("center", times + 1)))

  }
  if (axial) {
    temp = rep(NA, p)
    axial = data.frame(matrix(NA, ncol = p, nrow = p))
    for (i in 1:p) {
      temp[i] = delta + 1/p
      temp[c(-i)] = (1 - temp[i])/(p - 1)
      axial[i, ] = temp
    }
    times = replicates$axial
    if (is.null(times))
      times = 0
    else times = times - 1
    temp = axial
    if (times >= 1) {
      for (i in 1:times) axial = rbind(axial, temp)
    }
    names(axial) = names(frameOut)
    frameOut = rbind(frameOut, axial)
    Type = rbind(Type, data.frame(Type = rep("axial", (times + 1) * p)))

  }
  StandOrder = 1:nrow(frameOut)
  RunOrder = StandOrder
  if (randomize) {
    RunOrder = sample(1:nrow(frameOut), nrow(frameOut), replace = FALSE, prob = NULL)
  }
  frameOut = frameOut[order(RunOrder), ]
  row.names(frameOut) = frameOut$RunOrder
  out$pseudo = frameOut
  out$runOrder = data.frame(RunOrder = RunOrder)
  out$standardOrder = data.frame(StandOrder = StandOrder)
  out$Type = data.frame(Type = Type[order(RunOrder), ])
  out$response = data.frame(y = rep(NA, nrow(out$pseudo)))
  design = frameOut
  design[, ] = NA
  for (i in 1:ncol(frameOut)) {
    design[, i] = frameOut[, i] * (total[1] - sum(lower)) + lower[i]
  }
  out$design = design

  listFac = vector("list", p)
  for (i in seq(along = listFac)) listFac[[i]] = doeFactor$new()
  names(listFac) = LETTERS[1:p]
  out$.factors(listFac)
  if (out$total[2] != 1) {
    out$lows(lower * out$total[2])
    out$highs(1 * out$total[2])
    out$units("NA")
  }
  else if (any(out$lower != 0)) {
    out$lows(out$lower)
    out$highs(1 * out$total[1])
    out$units("%")
  }
  else {
    out$lows(0)
    out$highs(1 * out$total[1])
    out$units("%")
  }
  return(out)
}


# contourPlot3 ----
contourPlot3 <- function(x, y, z, response, data = NULL, main, xlab, ylab, zlab, form = "linear", col = 1,
                         col.text, axes = TRUE, steps, factors, plot = TRUE, show.scale = TRUE) {
  #' @title contourPlot3: Ternary plot
  #' @description This function creates a ternary plot (contour plot) for mixture designs (i.e. object of class \code{mixDesign}).
  #' @param x Factor 1 of the \code{mixDesign} object.
  #' @param y Factor 2 of the \code{mixDesign} object.
  #' @param z Factor 3 of the \code{mixDesign} object.
  #' @param response the response of the \code{mixDesign} object.
  #' @param data The \code{mixDesign} object from which x,y,z and the response are taken.
  #' @param main Character string specifying the main title of the plot.
  #' @param xlab Character string specifying the label for the x-axis.
  #' @param ylab Character string specifying the label for the y-axis.
  #' @param zlab Character string specifying the label for the z-axis.
  #' @param border Numeric or character (for example “red”) value specifying the color of the surroundimg the ternary plot.
  #' By default `border` is set to “white”
  #' @param form A character string or a formula with the syntax “y ~ A + B + C”.
  #' If form is a character string, it has to be one of the following:
  #' \itemize{
  #' \item{“linear”}
  #' \item{“quadratic”}
  #' \item{“fullCubic”}
  #' \item{“specialCubic”}
  #' }
  #' How the form influences the output is described in the reference listed below.
  #' By default, \code{form} is set to “linear”.
  #' @param col A predefined value (1, 2, 3, or 4) or a self-defined \code{colorRampPalette} specifying the colors to be used in the plot.
  #' @param col.text A numeric value or a character string specifying the color of the axis labels.
  #' The default value \code{col.text} is '1'.
  #' @param cex.axis A numeric value specifying the size of the axis labels.
  #' The default value \code{cex.axis} is '1'.
  #' @param axes A logical value specifying whether the axes should be plotted.
  #' By default, \code{axes} is set to \code{TRUE}.
  #' @param steps A numeric value specifying the resolution of the plot, i.e., the number of rows for the square matrix, which also represents the number of grid points per factor.
  #' By default, \code{steps} is set to 25.
  #' @param factors A list of factors for categorizing with specific settings, applicable if there are more than 3 factors (not yet implemented).
  #' @param plot Logical value indicating whether to display the plot. Default is \code{TRUE}.
  #' @param show.scale Logical value indicating whether to display the color scale on the plot. Default is \code{TRUE}.
  #' @return The function \code{contourPlot3} returns an invisible list containing:
  #' \itemize{
  #'  \item{mat - A matrix containing the response values as NA's and numerics.}
  #'  \item{plot - The generated plot.}
  #' }
  #' @seealso \code{\link{mixDesign.c}}, \code{\link{mixDesign}}, \code{\link{wirePlot3}}.
  #' @examples
  #' mdo = mixDesign(3,2, center = FALSE, axial = FALSE, randomize = FALSE,
  #'                 replicates  = c(1,1,2,3))
  #' mdo$names(c("polyethylene", "polystyrene", "polypropylene"))
  #' mdo$units("percent")
  #' elongation = c(11.0, 12.4, 15.0, 14.8, 16.1, 17.7, 16.4, 16.6, 8.8, 10.0, 10.0,
  #'                9.7, 11.8, 16.8, 16.0)
  #' mdo$.response(elongation)
  #' contourPlot3(A, B, C, elongation, data = mdo, form = "linear")
  #' contourPlot3(A, B, C, elongation, data = mdo, form = "quadratic", col = 2)
  #' contourPlot3(A, B, C, elongation, data = mdo, form = "elongation ~ I(A^2) - B:A + I(C^2)", col = 3, axes = FALSE)
  #' contourPlot3(A, B, C, elongation, data = mdo, form = "quadratic", col = c("yellow", "white", "red"), axes = F)

  out = list()
  mdo = data
  x.c = deparse(substitute(x))
  y.c = deparse(substitute(y))
  z.c = deparse(substitute(z))
  r.c = deparse(substitute(response))
  if (missing(col))
    col = 1
  if (missing(col.text))
    col.text = 1
  if (missing(main))
    main = paste("Response Surface for", r.c)
  if (missing(ylab))
    ylab = y.c
  if (missing(xlab))
    xlab = x.c
  if (missing(zlab))
    zlab = z.c
  if (missing(factors))
    factors = NULL
  if (missing(steps))
    steps = 100
  col.axis = par("col.axis")
  if (is.numeric(col)) {
    if (identical(col, 1)) {
      col_palette = list(c(0, "#00007F"),c(0.125, "blue"),c(0.25, "#007FFF"),c(0.375, "cyan"),c(0.5, "#7FFF7F"),c(0.625, "yellow"),c(0.75, "#FF7F00"),c(0.875, "red"),c(1, "#7F0000"))
    }
    else if (identical(col, 2)) {
      col_palette = list(c(0, "blue"), c(0.5, "white"), c(1, "red"))
    }
    else if (identical(col, 3)) {
      col_palette = list(c(0, "blue"), c(0.5, "white"), c(1, "orange"))
    }
    else if (identical(col, 4)) {
      col_palette = list(c(0, "gold"), c(0.5, "white"), c(1, "firebrick"))
    }
  }
  else{
    if(is.vector(col) & length(col) > 1){
      create_palette <- function(colors) {
        n <- length(colors)
        escala <- seq(0, 1, length.out = n)
        lista_colores <- lapply(1:n, function(i) {
          c(escala[i], colors[i])
        })
        return(lista_colores)
      }
      col_palette = create_palette(col)
    }
  }

  nameVec = names(mdo$names())
  linStrings = "-1"
  for (i in seq(along = nameVec)) linStrings = paste(linStrings, "+", nameVec[i])

  combList = combn(nameVec, 2, simplify = FALSE)
  quadStrings = character(length = length(combList))
  for (i in seq(along = combList)){
    if (i == 1){
      quadStrings[i] = paste(combList[[i]][1], ":", combList[[i]][2])
    }
    else{
      quadStrings[i] = paste("+", combList[[i]][1], ":", combList[[i]][2])
    }
  }
  quadStrings = paste(quadStrings, collapse = "")

  if (identical(form, "linear")) {
    form = paste(r.c, "~", linStrings)
  }
  if (identical(form, "quadratic")) {
    form = paste(r.c, "~", linStrings, "+", quadStrings)

  }
  lm.1 = lm(formula = form, data = mdo$as.data.frame())

  dcList = vector(mode = "list", length = length(mdo$names()))
  names(dcList) = names(mdo$names())
  dcList[1:length(mdo$names())] = 0
  if (!is.null(factors)) {
    for (i in names(factors)) dcList[[i]] = factors[[i]][1]
  }

  help.predict = function(a, b, x.c, y.c, lm.1) {
    dcList[[x.c]] = 2 * b/sqrt(3)
    dcList[[y.c]] = 1 - (2 * b/sqrt(3)) - (a - b/sqrt(3))
    dcList[[z.c]] = a - b/sqrt(3)
    temp = do.call(data.frame, dcList)
    invisible(predict(lm.1, temp))
  }
  a = seq(0, 1, length = steps)
  b = seq(0, sqrt(3)/2, length = steps)
  mat = outer(a, b, help.predict, x.c, y.c, lm.1)
  acc = nrow(mat)
  v = seq(acc, 1, length = acc)
  w = c(seq(2, acc, length = acc/2), seq(acc, 2, length = acc/2))
  mat[outer(w, v, `+`) <= acc] = NA

  p <- plot_ly(z = t(mat), type = "contour", autocontour = TRUE, line = list(smoothing = 0),
               contours = list(coloring = 'heatmap'), showscale = show.scale, colorscale = col_palette) %>%
    layout(
      title = main,
      xaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE, ticks = ""),
      yaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE, ticks = ""),
      showlegend = FALSE
    ) %>%
    add_annotations(text = ylab, x = -1.5, y = -1.5, showarrow = FALSE, font = list(size = 16, color = col.text), xref = "x", yref = "y") %>%
    add_annotations(text = zlab, x = (steps+1), y = -1.5, showarrow = FALSE, font = list(size = 16, color = col.text), xref = "x", yref = "y") %>%
    add_annotations(text = xlab, x = ((steps/2)-0.5), y = (steps+2), showarrow = FALSE, font = list(size = 16, color = col.text), xref = "x", yref = "y")

  if(axes){
    A = c(steps / 2, steps)
    B = c(0, 0)
    C = c(steps, 0)

    AC = c((A[1]+C[1])/2, (A[2]+C[2])/2)
    AB = c((A[1]+B[1])/2, (A[2]+B[2])/2)
    BC = c((B[1]+C[1])/2, (B[2]+C[2])/2)

    p <- p %>%
      add_lines(x = c(A[1], BC[1]), y = c(A[2], BC[2]), line = list(color = 'black', width = 1), inherit = FALSE) %>% # A - BC
      add_lines(x = c(B[1], AC[1]), y = c(B[2], AC[2]), line = list(color = 'black', width = 1), inherit = FALSE) %>% # B - AC
      add_lines(x = c(C[1], AB[1]), y = c(C[2], AB[2]), line = list(color = 'black', width = 1), inherit = FALSE)     # C - AB

    for (i in seq(0.8, 0.1, by = -0.1)) {
      p <- p %>%
        add_annotations(text = paste0(i+0.1), x = A[1] + (BC[1] - A[1]) * (1 - i/0.9), y = A[2] + (BC[2] - A[2]) * (1 - i/0.9), showarrow = FALSE, font = list(size = 12), xref = "x", yref = "y") %>%
        add_annotations(text = paste0(i+0.1), x = B[1] + (AC[1] - B[1]) * (1 - i/0.9), y = B[2] + (AC[2] - B[2]) * (1 - i/0.9), showarrow = FALSE, font = list(size = 12), xref = "x", yref = "y") %>%
        add_annotations(text = paste0(i+0.1), x = C[1] + (AB[1] - C[1]) * (1 - i/0.9), y = C[2] + (AB[2] - C[2]) * (1 - i/0.9), showarrow = FALSE, font = list(size = 12), xref = "x", yref = "y")
    }

    i = 0
    p <- p %>%
      add_annotations(text = paste0(0.1), x = A[1] + (BC[1] - A[1]) * (1 - i/0.9), y = A[2] + (BC[2] - A[2]) * (1 - i/0.9) + 2, showarrow = FALSE, font = list(size = 12), xref = "x", yref = "y") %>%
      add_annotations(text = paste0(0.1), x = B[1] + (AC[1] - B[1]) * (1 - i/0.9), y = B[2] + (AC[2] - B[2]) * (1 - i/0.9), showarrow = FALSE, font = list(size = 12), xref = "x", yref = "y") %>%
      add_annotations(text = paste0(0.1), x = C[1] + (AB[1] - C[1]) * (1 - i/0.9), y = C[2] + (AB[2] - C[2]) * (1 - i/0.9), showarrow = FALSE, font = list(size = 12), xref = "x", yref = "y")

  }
  if(plot){
    print(p)
  }
  invisible(list(mat = t(mat), plot = p))
}

# wirePlot3 ----
wirePlot3 = function(x, y, z, response, data = NULL, main, xlab, ylab, zlab, form = "linear", col = "Rainbow", steps, factors, plot = TRUE) {
  #' @title wirePlot3: function to create a ternary plot (3D wire plot)
  #' @description This function creates a ternary plot for mixture designs (i.e. object of class \code{mixDesign}).
  #' @param x Factor 1 of the \code{mixDesign} object.
  #' @param y Factor 2 of the \code{mixDesign} object.
  #' @param z Factor 3 of the \code{mixDesign} object.
  #' @param response the response of the \code{mixDesign} object.
  #' @param data The \code{mixDesign} object from which x,y,z and the response are taken.
  #' @param main Character string specifying the main title of the plot.
  #' @param xlab Character string specifying the label for the x-axis.
  #' @param ylab Character string specifying the label for the y-axis.
  #' @param zlab Character string specifying the label for the z-axis.
  #' @param form A character string or a formula with the syntax “y ~ A + B + C”.
  #' If form is a character string, it has to be one of the following:
  #' \itemize{
  #' \item{“linear”}
  #' \item{“quadratic”}
  #' }
  #' How the form influences the output is described in the reference listed below.
  #' By default, \code{form} is set to “linear”.
  #' @param col Character string specifying the color palette to use for the plot (e.g., \code{"Rainbow"}, \code{"Jet"}, \code{"Earth"}, \code{"Electric"}). Default is \code{"Rainbow"}.
  #' @param steps A numeric value specifying the resolution of the plot, i.e., the number of rows for the square matrix, which also represents the number of grid points per factor.
  #' By default, \code{steps} is set to 25.
  #' @param factors A list of factors for categorizing with specific settings, applicable if there are more than 3 factors (not yet implemented).
  #' @param plot Logical value indicating whether to display the plot. Default is \code{TRUE}.
  #' @return The function \code{wirePlot3} returns an invisible matrix containing the response values as NA's and numerics.
  #' @seealso \code{\link{mixDesign.c}}, \code{\link{mixDesign}}, \code{\link{contourPlot3}}.
  #' @examples
  #' #Example 1
  #' mdo <- mixDesign(3, 2, center = FALSE, axial = FALSE, randomize = FALSE, replicates = c(1, 1, 2, 3))
  #' elongation <- c(11.0, 12.4, 15.0, 14.8, 16.1, 17.7, 16.4, 16.6, 8.8, 10.0, 10.0, 9.7, 11.8, 16.8, 16.0)
  #' mdo$.response(elongation)
  #' wirePlot3(A, B, C, elongation, data = mdo, form = "quadratic")
  #'
  #' #Example 2
  #' mdo <- mixDesign(3,2, center = FALSE, axial = FALSE, randomize = FALSE, replicates  = c(1,1,2,3))
  #' mdo$names(c("polyethylene", "polystyrene", "polypropylene"))
  #' mdo$units("percent")
  #' elongation <- c(11.0, 12.4, 15.0, 14.8, 16.1, 17.7, 16.4, 16.6, 8.8, 10.0, 10.0, 9.7, 11.8, 16.8, 16.0)
  #' mdo$.response(elongation)
  #' wirePlot3(A, B, C, elongation, data = mdo, form = "linear")
  #' wirePlot3(A, B, C, elongation, data = mdo, form = "quadratic", col = "Jet")
  #' wirePlot3(A, B, C, elongation, data = mdo, form = "elongation ~ I(A^2) - B:A + I(C^2)", col = "Electric")
  #' wirePlot3(A, B, C, elongation, data = mdo, form = "quadratic", col = "Earth")

  out = list()
  mdo = data
  x.c = deparse(substitute(A))
  y.c = deparse(substitute(B))
  z.c = deparse(substitute(C))
  r.c = deparse(substitute(response))
  if (missing(main))
    main = paste("Response Surface for", r.c)
  if (missing(ylab))
    ylab = y.c
  if (missing(xlab))
    xlab = x.c
  if (missing(zlab))
    zlab = z.c
  if (missing(factors))
    factors = NULL
  if (missing(steps))
    steps = 100
  nameVec = names(mdo$names())
  linStrings = "-1"
  for (i in seq(along = nameVec)) linStrings = paste(linStrings, "+", nameVec[i])

  combList = combn(nameVec, 2, simplify = FALSE)
  quadStrings = character(length = length(combList))
  for (i in seq(along = combList)){
    if (i == 1){
      quadStrings[i] = paste(combList[[i]][1], ":", combList[[i]][2])
    }
    else {
      quadStrings[i] = paste("+", combList[[i]][1], ":", combList[[i]][2])
    }
  }
  quadStrings = paste(quadStrings, collapse = "")

  if (identical(form, "linear")) {
    form = paste(r.c, "~", linStrings)

  }
  if (identical(form, "quadratic")) {
    form = paste(r.c, "~", linStrings, "+", quadStrings)

  }
  lm.1 = lm(formula = form, data = mdo$as.data.frame())

  dcList = vector(mode = "list", length = length(mdo$names()))
  names(dcList) = names(mdo$names())
  dcList[1:length(mdo$names())] = 0
  if (!is.null(factors)) {
    for (i in names(factors)) dcList[[i]] = factors[[i]][1]
  }

  help.predict = function(a, b, x.c, y.c, lm.1) {
    dcList[[x.c]] = 2 * b/sqrt(3)
    dcList[[y.c]] = 1 - (2 * b/sqrt(3)) - (a - b/sqrt(3))
    dcList[[z.c]] = a - b/sqrt(3)
    temp = do.call(data.frame, dcList)
    invisible(predict(lm.1, temp))
  }
  a = seq(0, 1, length = steps)
  b = seq(0, sqrt(3)/2, length = steps)
  mat = outer(a, b, help.predict, x.c, y.c, lm.1)
  acc = nrow(mat)
  sca = sin(1/3 * pi)
  ncmat = ncol(mat)
  v = seq(acc, 1, length = acc)
  w = c(seq(2, acc, length = acc/2), seq(acc, 2, length = acc/2))
  mat[outer(w, v, `+`) <= acc] = NA
  if (is.function(col)) {
    nrMat <- nrow(mat)
    ncMat <- ncol(mat)
    nbcol <- 100
    color <- col(nbcol)
    matFacet = mat[-1, -1] + mat[-1, -ncmat] + mat[-acc, -1] + mat[-acc, -ncmat]
    facetcol <- cut(matFacet, nbcol)
  } else {
    color = col
    facetcol = 1
  }
  maxim = max(mat, na.rm = TRUE) * acc
  minim = min(mat, na.rm = TRUE) * acc

  p <- plot_ly(x =seq(0, acc * sca, length = ncmat)  , y = -seq(0, acc, length = acc), z = mat * acc, colorscale=col)%>%
    add_surface(
      contours = list(z = list(show = TRUE, usecolormap = TRUE, highlightcolor = "#ff0000", project = list(z = TRUE)))
    ) %>%
    layout(
      title = main,
      scene = list(
        xaxis = list(title = ylab,
                     showticklabels = FALSE,
                     zeroline = FALSE),
        yaxis = list(title = xlab,
                     showticklabels = FALSE,
                     zeroline = FALSE),
        zaxis = list(title = zlab,
                     showticklabels = FALSE,
                     zeroline = FALSE)

      )
    )

  if(plot){
    show(p)
  }
  invisible(mat)
}



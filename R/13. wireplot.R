
# response=elongation, data = mdo, form = "quadratic", theta = -170
wirePlot3 = function(x, y, z, response, data = NULL, main, xlab, ylab, zlab, form = "linear", phi, theta, col = 1, steps, factors) {

  out = list()
  mdo = data
  x.c = deparse(substitute(A))
  y.c = deparse(substitute(B))
  z.c = deparse(substitute(C))
  r.c = deparse(substitute(response))
  if (missing(col))
    col = 1
  if (missing(main))
    main = paste("Response Surface for", r.c)
  if (missing(ylab))
    ylab = y.c
  if (missing(xlab))
    xlab = x.c
  if (missing(zlab))
    zlab = z.c
  if (missing(phi))
    phi = 30
  if (missing(theta))
    theta = 30
  if (missing(factors))
    factors = NULL
  if (missing(steps))
    steps = 100
  if (!is.function(col)) {
    if (identical(col, 1))
      col = colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
    if (identical(col, 2))
      col = colorRampPalette(c("blue", "white", "red"), space = "Lab")
    if (identical(col, 3))
      col = colorRampPalette(c("blue", "white", "orange"))
    if (identical(col, 4))
      col = colorRampPalette(c("gold", "white", "firebrick"))
  }
  phi = phi%%360
  .phi = phi
  theta = theta%%360
  .theta = theta
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
  ############
  p <- plot_ly(x = ~x_seq, y = ~y_seq, z = ~z_mat) %>%
    add_surface(
      contours = list(
        z = list(show = TRUE, usecolormap = TRUE, highlightcolor = "#ff0000", project = list(z = TRUE))
      ),
      colorscale = list(c(0, "blue"), c(1, "red")),  # Puedes personalizar la escala de colores
      showscale = TRUE  # Mostrar la escala de colores
    ) %>%

    # Añadir el título y etiquetas de los ejes
    layout(
      title = main,
      scene = list(
        xaxis = list(title = xlab),
        yaxis = list(title = ylab),
        zaxis = list(title = zlab),
        camera = list(eye = list(x = 1.25, y = 1.25, z = 1.25))  # Ajuste de la cámara para vista en perspectiva
      )
    )
  ############
  per = persp(x = seq(0, acc, length = acc), y = seq(0, acc * sca, length = ncmat), mat * acc, phi = .phi, theta = .theta, scale = TRUE, col = "transparent",
              border = FALSE, box = FALSE, main = main, xlab = xlab, ylab = ylab)
  lineList = contourLines(x = seq(0, acc, length = acc), y = seq(0, acc * sca, length = ncmat), mat)
  for (i in seq(along = lineList)) lines(trans3d(lineList[[i]]$x, lineList[[i]]$y, z = minim, pmat = per))
  if (.phi < 90) {
    lines(trans3d(x = seq(0, acc/2, length = 10), y = seq(0, acc * sca, length = 10), z = maxim, pmat = per), lty = 2)
    lines(trans3d(x = seq(acc, acc/2, length = 10), y = seq(0, acc * sca, length = 10), z = maxim, pmat = per), lty = 2)
    lines(trans3d(x = 0:acc, y = 0, z = maxim, pmat = per), lty = 2)
  }
  if (.theta > 323 || .theta < 37) {
    lines(trans3d(x = acc/2, y = acc * sca, z = minim:maxim, pmat = per), lty = 2)
    lines(trans3d(x = 0, y = 0, z = minim:maxim, pmat = per), lty = 2)
    lines(trans3d(x = acc, y = 0, z = minim:maxim, pmat = per), lty = 2)
  }
  if (.theta > 37 && .theta < 156)
    lines(trans3d(x = 0, y = 0, z = minim:maxim, pmat = per), lty = 2)
  if (.theta > 156 && .theta < 323) {
    lines(trans3d(x = acc, y = 0, z = minim:maxim, pmat = per), lty = 2)
  }
  lines(trans3d(x = seq(0, acc/2, length = 10), y = seq(0, acc * sca, length = 10), z = minim, pmat = per), lty = 1, lwd = 2)
  lines(trans3d(x = seq(acc, acc/2, length = 10), y = seq(0, acc * sca, length = 10), z = minim, pmat = per), lty = 1, lwd = 2)
  lines(trans3d(x = 0:acc, y = 0, z = minim, pmat = per), lty = 1, lwd = 2)
  text(trans3d(x = acc/2 + acc/50, y = acc * sca + acc * sca/50, z = minim, pmat = per), labels = xlab, lwd = 2)
  text(trans3d(x = -acc/50, y = -acc * sca/50, z = minim, pmat = per), labels = ylab, lwd = 2)
  text(trans3d(x = acc + acc/50, 0, z = minim, pmat = per), labels = zlab, cex = 1, lwd = 2)
  par(new = TRUE)
  persp(x = seq(0, acc, length = acc), y = seq(0, acc * sca, length = ncmat), mat * acc, phi = .phi, theta = .theta, scale = TRUE, col = color[facetcol],
        border = FALSE, box = FALSE)
  if (.phi > 0) {
    lines(trans3d(x = seq(0, acc/2, length = 10), y = seq(0, acc * sca, length = 10), z = maxim, pmat = per), lty = 2)
    lines(trans3d(x = seq(acc, acc/2, length = 10), y = seq(0, acc * sca, length = 10), z = maxim, pmat = per), lty = 2)
    lines(trans3d(x = 0:acc, y = 0, z = maxim, pmat = per), lty = 2)
  }
  if (.theta > 37 && .theta < 156) {
    lines(trans3d(x = acc/2, y = acc * sca, z = minim:maxim, pmat = per), lty = 2)
    lines(trans3d(x = acc, y = 0, z = minim:maxim, pmat = per), lty = 2)
  }
  if (.theta > 156 && .theta < 323) {
    lines(trans3d(x = acc/2, y = acc * sca, z = minim:maxim, pmat = per), lty = 2)
    lines(trans3d(x = 0, y = 0, z = minim:maxim, pmat = per), lty = 2)
  }
  if (TRUE) {
    zlim = range(mat, finite = TRUE, na.rm = TRUE)
    leglevel = pretty(zlim, 6)
    legcol = col(length(leglevel))
    legpretty = as.character(abs(leglevel))
    temp = character(length(leglevel))
    temp[leglevel > 0] = "+"
    temp[leglevel < 0] = "-"
    temp[leglevel == 0] = " "
    legpretty = paste(temp, legpretty, sep = "")
    if (.theta <= 180)
      legend("topright", inset = 0.02, legend = paste(">", legpretty), col = legcol, bg = "white", pt.cex = 1.5, cex = 0.75, pch = 15)
    if (.theta > 180)
      legend("topleft", inset = 0.02, legend = paste(">", legpretty), col = legcol, bg = "white", pt.cex = 1.5, cex = 0.75, pch = 15)
  }
  invisible(mat)
}

# mdo <- mixDesign(3, 2, center = FALSE, axial = FALSE, randomize = FALSE, replicates = c(1, 1, 2, 3))
#
# elongation <- c(11.0, 12.4, 15.0, 14.8, 16.1, 17.7, 16.4, 16.6, 8.8, 10.0, 10.0, 9.7, 11.8, 16.8, 16.0)
# mdo$.response(elongation)
#
# wirePlot3(A, B, C, elongation, data = mdo, form = "quadratic", theta = -170)
#
#
# set.seed(1234)
# dfac <- facDesign(k = 3, centerCube = 4)
# dfac$names(c('Factor 1', 'Factor 2', 'Factor 3'))
# dfac$lows(c(80,120,1))
# dfac$highs( c(120,140,2))
# #valores completos
# rend = c(simProc(120,140,1),simProc(80,140,1),simProc(120,140,2),simProc(120,120,1),simProc(90,130,1.5),simProc(90,130,1.5),simProc(80,120,2),simProc(90,130,1.5),simProc(90,130,1.5),simProc(120,120,2),simProc(80,140,2),simProc(80,120,1))
# dfac$.response(rend)
# wirePlot(A,B,rend,data=dfac)

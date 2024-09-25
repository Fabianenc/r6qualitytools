##########################################################################################
####################### DISEÑOS FACTORIALES - AUXILIARES (.r) ############################
##########################################################################################

# .replace2s ----
.replace2s = function(x) {
  if (!is.data.frame(x))
    stop(paste(deparse(substitute(x)), "needs to be a data.frame"))
  for (i in 1:ncol(x)) x[x[, i] == 2, i] = -1
  return(x)
}
# .helpAliasTable ----
.helpAliasTable = function(fdo, k, degree = 3) {
  if (degree > k) {
    degree = k
  }
  if (class(fdo)[1] == "facDesign")
    X = unique(fdo$cube)
  if (class(fdo)[1] == "taguchiDesign") {
    X = unique(fdo$design)
    X = .replace2s(X)
  }
  N = nrow(X)
  columns = names(X[, 1:k])
  X1 = matrix(1, nrow = N, ncol = 1)
  nameVec = c("Identity")
  for (i in 1:degree) {
    temp = combn(columns, i)
    for (j in 1:ncol(temp)) {
      if (class(fdo)[1] == "facDesign")
        index = names(X) %in% temp[, j]
      if (class(fdo)[1] == "taguchiDesign")
        index = names(X) %in% temp[, j]
      if (length((1:length(index))[index]) == 1) {
        X1 = cbind(X1, X[, index])
        nameVec = c(nameVec, temp[, j])
      }
      else {
        X1 = cbind(X1, apply(X[, index], 1, prod))
        nameVec = c(nameVec, paste(temp[, j], sep = "", collapse = ""))
      }
    }
    X1 = data.frame(X1)
    names(X1) = nameVec
  }
  return(X1)
}
# .fdoOrth ----
.fdoOrth = vector(mode = "list", length = 3)
.fdoOrth[[1]] = list(k = 3, gen = "C=AB", p = 1)
.fdoOrth[[2]] = list(k = 4, gen = "D=ABC", p = 1)
.fdoOrth[[3]] = list(k = 5, gen = c("D=AB","E=AC"), p = 2)
.fdoOrth[[4]] = list(k = 6, gen = c("D=AB","E=AC","F=BC"), p = 3)
.fdoOrth[[5]] = list(k = 7, gen = c("D=AB","E=AC","F=BC","G=ABC"), p = 4)
.fdoOrth[[6]] = list(k = 5, gen = "E=ABCD", p = 1)
.fdoOrth[[7]] = list(k = 6, gen = c("E=ABC","F=BCD"), p = 2)
.fdoOrth[[8]] = list(k = 7, gen = c("E=ABC","F=BCD","G=ACD"), p = 3)
.fdoOrth[[9]] = list(k = 8, gen = c("E=BCD","F=ACD","G=ABC","H=ABD"), p = 4)
.fdoOrth[[10]] = list(k = 9, gen = c("E=ABC","F=BCD","G=ACD","H=ABD","J=ABCD"), p = 5)
.fdoOrth[[11]] = list(k = 10, gen = c("E=ABC","F=BCD","G=ACD","H=ABD","J=ABCD","K=AB"), p = 6)
.fdoOrth[[12]] = list(k = 11, gen = c("E=ABC","F=BCD","G=ACD","H=ABD","J=ABCD","K=AB","L=AC"), p = 7)
.fdoOrth[[13]] = list(k = 6, gen = "F=ABCDE", p = 1)
.fdoOrth[[14]] = list(k = 7, gen = c("F=ABCD","G=ABDE"), p = 2)
.fdoOrth[[15]] = list(k = 8, gen = c("F=ABC","G=ABD","H=BCDE"), p = 3)
.fdoOrth[[16]] = list(k = 9, gen = c("F=BCDE","G=ACDE","H=ABDE","J=ABCE"), p = 4)
.fdoOrth[[17]] = list(k = 10, gen = c("F=ABCD","G=ABCE","H=ABDE","J=ACDE","K=BCDE"), p = 5)
.fdoOrth[[18]] = list(k = 11, gen = c("F=ABC","G=BCD","H=CDE","J=ACD","K=AEF","L=ADEF"), p = 6)
.fdoOrth[[19]] = list(k = 7, gen = "G=ABCDEF", p = 1)
.fdoOrth[[20]] = list(k = 8, gen = c("G=ABCD","H=ABEF"), p = 2)
.fdoOrth[[21]] = list(k = 9, gen = c("G=ABCD","H=ABEF","J=CDEF"), p = 3)
.fdoOrth[[22]] = list(k = 10, gen = c("G=BCDF","H=ACDF","J=ABDE","K=ABCE"), p = 4)
.fdoOrth[[23]] = list(k = 11, gen = c("G=CDE","H=ABCD","J=ABF","K=BDEF","L=ADEF"), p = 5)
.fdoOrth[[24]] = list(k = 8, gen = "H=ABCDEFG", p = 1)
.fdoOrth[[25]] = list(k = 9, gen = c("H=ACDFG","J=BCEFG"), p = 2)
.fdoOrth[[26]] = list(k = 10, gen = c("H=ABCG","J=BCDE","K=ACDF"), p = 3)
.fdoOrth[[27]] = list(k = 11, gen = c("H=ABCG","J=BCDE","K=ACDF","L=ABCDEFG"), p = 4)

# .NAMES ----
.NAMES = LETTERS[c(1:8, 10:26)]
# .generate_double_letters ----
.generate_double_letters <- function(n) {
  letters_single <- LETTERS[c(1:8, 10:26)]
  letters_double <- c()
  for (i in letters_single) {
    for (j in letters_single) {
      letters_double <- c(letters_double, paste0(i, j))
    }
  }
  return(letters_double[1:n])
}
# .m.interaction.plot ----
.m.interaction.plot <- function(x.factor, trace.factor, response, fun = mean, type = c("l", "p", "b"), legend = TRUE, trace.label = deparse(substitute(trace.factor)),
                                fixed = FALSE, xlab = deparse(substitute(x.factor)), ylab = ylabel, ytitle = TRUE, ylim = range(cells, na.rm = TRUE), lty = nc:1, col = 1, pch = c(1L:9, 0, letters), xpd = NULL,
                                leg.bg = par("bg"), leg.bty = "n", xtick = FALSE, xaxt = par("xaxt"), axes.x = TRUE, axes.y = TRUE, main, ...) {
  ylabel <- paste(deparse(substitute(fun)), "of ", deparse(substitute(response)))
  type <- match.arg(type)
  cells <- tapply(response, list(x.factor, trace.factor), fun)
  nr <- nrow(cells)
  nc <- ncol(cells)
  xvals <- 1L:nr
  xvals = as.numeric(rownames(cells))
  if (is.ordered(x.factor)) {
    wn <- getOption("warn")
    options(warn = -1)
    xnm <- as.numeric(levels(x.factor))
    options(warn = wn)
    if (!any(is.na(xnm)))
      xvals <- xnm
  }
  if (missing(main)) {
    main = paste("Effect Plot")
  }
  xlabs <- rownames(cells)
  ylabs <- colnames(cells)
  nch <- max(sapply(ylabs, nchar, type = "width"))
  if (is.null(xlabs))
    xlabs <- as.character(xvals)
  if (is.null(ylabs))
    ylabs <- as.character(1L:nc)
  xlim <- range(xvals)
  xleg <- xlim[2L] + 0.05 * diff(xlim)
  xlim <- xlim + c(-0.2/nr, if (legend) 0.2 + 0.02 * nch else 0.2/nr) * diff(xlim)

  df <- data.frame(x = xvals, y = c(cells))

  # PLOT
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_line(na.rm = TRUE) +
    ylim(ylim) + labs(x = xlab, y = ylab, title = main) + theme_bw() +

    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5))

  if (axes.x){
    p <- p +
      theme(axis.ticks.x = element_line(),
            axis.text.x = element_text()) +
      scale_x_continuous(breaks=c(-1, 1))
  }
  if (axes.y){
    p <- p +
      theme(axis.ticks.y = element_line(),
            axis.text.y = element_text())
  }
  if (ytitle == FALSE){
    p <-  p + theme(axis.title.y = element_blank())
  }
  if(main == ""){
    p <- p + labs(title = NULL)
  }
  if (deparse(substitute(mean)) == "mean"){
    p <- p + geom_hline(yintercept = median(df$y, na.rm = TRUE), linetype = "dashed", col = "#324B7A")
  }

  invisible(list(xVals = df$x, yVals = df$y, plot = p))
}
# .rsm ----
.rsm = vector(mode = "list", length = 7)
.rsm[[1]] = list(k = 3, blocks = 2, gen = c("ABC"))
.rsm[[2]] = list(k = 3, blocks = 4, gen = c("AB", "AC"))
.rsm[[3]] = list(k = 4, blocks = 2, gen = c("ABCD"))
.rsm[[4]] = list(k = 4, blocks = 4, gen = c("ABC", "ACD"))
.rsm[[5]] = list(k = 4, blocks = 8, gen = c("AB", "BC", "CD"))
.rsm[[6]] = list(k = 5, blocks = 2, gen = c("ABCDE"))
.rsm[[7]] = list(k = 5, blocks = 4, gen = c("ABC", "CDE"))
.rsm[[8]] = list(k = 5, blocks = 8, gen = c("ABE", "BCE", "CDE"))
.rsm[[9]] = list(k = 5, blocks = 16, gen = c("AB", "AC", "CD", "DE"))
.rsm[[10]] = list(k = 6, blocks = 2, gen = c("ABCDEF"))
.rsm[[11]] = list(k = 6, blocks = 4, gen = c("ABCF", "CDEF"))
.rsm[[12]] = list(k = 6, blocks = 8, gen = c("ABEF", "ABCD", "ACE"))
.rsm[[13]] = list(k = 6, blocks = 16, gen = c("ABF", "ACF", "BDF", "DEF"))
.rsm[[14]] = list(k = 6, blocks = 32, gen = c("AB", "BC", "CD", "DE", "EF"))
.rsm[[15]] = list(k = 7, blocks = 2, gen = c("ABCDEFG"))
.rsm[[16]] = list(k = 7, blocks = 4, gen = c("ABCFG", "CDEFG"))
.rsm[[17]] = list(k = 7, blocks = 8, gen = c("ABC", "DEF", "AFG"))
.rsm[[18]] = list(k = 7, blocks = 16, gen = c("ABD", "EFG", "CDE", "ADG"))
.rsm[[19]] = list(k = 7, blocks = 32, gen = c("ABG", "BCG", "CDG", "DEG", "EFG"))
.rsm[[20]] = list(k = 7, blocks = 64, gen = c("AB", "BC", "CD", "DE", "EF", "FG"))


# .numFac ----
.numFac = function(fdo) {
  return(length(fdo$names()))
}
# .confoundings ----
.confoundings = function(blockGenVec, lSet) {
  biVec = character(0)
  for (i in 2:length(blockGenVec)) {
    mat = combn(blockGenVec, i)
    temp = apply(mat, 2, strsplit, split = "")
    comb = lapply(temp, unlist)
    comb = lapply(comb, c, lSet)

    combFreq = sapply(comb, table)%%2
    combBool = !apply(combFreq, 2, as.logical)
    chars = row.names(combFreq)

    biTemp = character(0)
    for (j in 1:ncol(combBool)) {
      biTemp = c(biTemp, paste(chars[combBool[, j]], collapse = ""))
    }

    biVec = c(biVec, biTemp)
  }
  return(c(blockGenVec, biVec))
}
# .lociv ----
.lociv = function(charVec) {
  lenVec = numeric(length = length(charVec))
  for (i in seq(along = charVec)) {
    lenVec[i] = length(strsplit(charVec[i], split = "")[[1]])
  }
  return(lenVec)
}

# .blockInteractions ----
.blockInteractions = function(fdo, blocks = 2, useTable = "rsm") {

  if (!(blocks %in% c(0, 1, 2, 4, 8, 16, 32, 64)))
    stop("blocks needs to be a power of 2 up to 64!")
  gen = NULL
  if (blocks %in% c(0, 1)) {

    return(gen)
  }
  if (length(useTable) > 0) {
    if (!(nrow(unique(fdo$cube)) >= 2^.numFac(fdo)))
      stop("no blocking of a fractional factorial Design --> block on replicates instead!")
    if (identical(useTable, "rsm")) {
      for (i in seq(along = .rsm)) {
        if (.rsm[[i]]$k == .numFac(fdo) & .rsm[[i]]$blocks == blocks)
          return(.rsm[[i]]$gen)
      }
    }
    return(gen)
  }
  bgaci = matrix(nrow = 0, ncol = blocks - 1)
  if (!is.numeric(blocks))
    stop("blocks must be an integer")
  numCol = log2(blocks)
  blockGen = character(3)
  lSet = fdo$names()
  sSet = vector(mode = "list")
  for (i in length(lSet):2) {
    sSet = c(sSet, combn(lSet, i, simplify = FALSE))
  }
  if (blocks == 2) {
    index = order(sapply(sSet, length), decreasing = TRUE)[1]
    sSet = sapply(sSet, paste, collapse = "")
    return(sSet[index])
  }
  sSet = sapply(sSet, paste, collapse = "")

  possGen = combn(sSet, numCol, simplify = FALSE)
  for (i in seq(along = possGen)) {
    blockGenVec = unlist(possGen[[i]])

    newRow = .confoundings(blockGenVec, lSet)
    if (!any(newRow %in% c(lSet, "")))
      bgaci = rbind(bgaci, .confoundings(blockGenVec, lSet))
  }
  mat = unique(t(apply(bgaci, 1, sort)))
  temp = t(apply(mat, 1, .lociv))
  temp = t(apply(temp, 1, sort))
  ref = temp[1, ]
  index = 1
  for (i in 1:nrow(temp)) {
    if (any((ref - temp[i, ]) < 0)) {
      ref = temp[i, ]
      index = i
    }
  }
  for (i in 1:nrow(temp)) {
    if (!(any(ref - temp[i, ] > 0) | any(ref - temp[i, ] < 0))) {
      index = c(index, i)
    }
  }
  temp = unique((mat[index, ]))
  cat("\nSuggested Effects for Blocking:")
  cat("\n")
  cat(temp[1, 1:numCol])
  cat("\n")
  cat("\nInteractions Confounded with blocks:")
  cat("\n")
  cat(unique(temp[1, ]))
  cat("\n")
  cat("\n Alternate Effects for Blocking:")
  cat(temp[c(-1), 1:numCol])
  cat("\n")
  gen = temp[1, 1:numCol]
  return(gen)
}
# .blockGenCol ----
.blockGenCol = function(gen, fdo) {
  blockVec = NULL
  .blockCol = NULL
  genList = gen
  genList = strsplit(genList, split = "")
  .fdo = fdo
  for (i in seq(along = genList)) {
    gen = genList[[i]]
    for (j in seq(along = gen)) {
      genTemp = .fdo$get(,gen[j])
      if (j == 1)
        blockVec = rep(1, length = length(genTemp))
      blockVec = blockVec * genTemp
    }
    if (i == 1)
      .blockCol = data.frame(B1 = blockVec)
    else .blockCol = cbind(.blockCol, blockVec)
  }
  names(.blockCol) = paste("B", 1:ncol(.blockCol), sep = "")
  return(.blockCol)
}
# .blockCol ----
.blockCol = function(.blockGenCol) {
  .blockCol = numeric(nrow(.blockGenCol))
  uniCol = unique(.blockGenCol)
  for (i in 1:nrow(uniCol)) {
    if (ncol(uniCol) == 1)
      .blockCol[apply(t(as.data.frame(apply(.blockGenCol, 1, "==", uniCol[i, ]))), 2, all)] = i
    else .blockCol[apply(apply(.blockGenCol, 1, "==", uniCol[i, ]), 2, all)] = i
  }
  return(data.frame(Block = .blockCol))
}


# .norm2d ----
.norm2d <- function(x1, x2, mu1 = 160, mu2 = 165, rho = 0.7, sigma1 = 45, sigma2 = 22.5) {
  z = 1/(2 * pi * sigma1 * sigma2 * sqrt(1 - rho^2)) * exp(-1/(2 * (1 - rho^2)) * (((x1 - mu1)/sigma1)^2 - 2 * rho * (x1 - mu1)/sigma1 * (x2 - mu2)/sigma2 +
                                                                                     ((x2 - mu2)/sigma2)^2))
  return(z)
}
# .letterPos .testFun ----
.letterPos <- function(LETTER) {
  if (!(nchar(LETTER) == 1))
    stop("factor names should be single characters only")
  return((1:26)[LETTERS[1:26] == LETTER])
}
.testFun <- function(x.factor, trace.factor, response, fun = mean, type = c("l", "p", "b"), legend = TRUE, trace.label = deparse(substitute(trace.factor)),
                     fixed = FALSE, xlab = deparse(substitute(x.factor)), ylab = ylabel, ylim = range(cellNew, na.rm = TRUE), lty = nc:1, col = 1, pch = c(1L:9, 0, letters),
                     xpd = NULL, leg.bg = par("bg"), leg.bty = "o", xtick = FALSE, xaxt = par("xaxt"), axes = TRUE, title = "", ...) {
  ylabel <- paste(deparse(substitute(fun)), "of ", deparse(substitute(response)))
  type <- match.arg(type)
  cellNew <- tapply(response, list(x.factor, trace.factor), fun)
  nr <- nrow(cellNew)
  nc <- ncol(cellNew)
  xvals <- 1L:nr
  if (is.ordered(x.factor)) {
    wn <- getOption("warn")
    options(warn = -1)
    xnm <- as.numeric(levels(x.factor))
    options(warn = wn)
    if (!any(is.na(xnm)))
      xvals <- xnm
  }
  xlabs <- rownames(cellNew)
  ylabs <- colnames(cellNew)
  nch <- max(sapply(ylabs, nchar, type = "width"))
  if (is.null(xlabs))
    xlabs <- as.character(xvals)
  if (is.null(ylabs))
    ylabs <- as.character(1L:nc)
  xlim <- range(xvals)
  xleg <- xlim[2L] + 0.05 * diff(xlim)
  xlim <- xlim + c(-0.2/nr, if (legend) 0.2 + 0.02 * nch else 0.2/nr) * diff(xlim)
  matplot(xvals, cellNew, ..., type = type, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, axes = axes, xaxt = "n", col = col, lty = lty, pch = pch)
  if (axes && xaxt != "n") {
    axisInt <- function(x, main, sub, lwd, bg, log, asp, ...) axis(1, x, ...)
    mgp. <- par("mgp")
    if (!xtick)
      mgp.[2L] <- 0
    axisInt(1, at = xvals, labels = xlabs, tick = xtick, mgp = mgp., xaxt = xaxt, ...)
  }
  if (legend) {
    legpretty = ylabs
    legend("topright", legend = legpretty, title = title, col = col, pch = if (type %in% c("p", "b"))
      pch, lty = if (type %in% c("l", "b"))
        lty, bty = leg.bty, bg = leg.bg, inset = 0.02)
  }
  return(list(xvals, xlabs))
}



# .splitDev ----
.splitDev = function(x) {
  if (x > 6)
    dev = TRUE
  else dev = FALSE
  if (x == 1)
    mfrow = c(1, 1)
  if (x == 2)
    mfrow = c(1, 2)
  if (x == 3)
    mfrow = c(2, 2)
  if (x == 4)
    mfrow = c(2, 2)
  if (x == 5)
    mfrow = c(2, 3)
  if (x == 6)
    mfrow = c(2, 3)
  if (x >= 7)
    mfrow = c(3, 3)
  return(list(dev, mfrow))
}
# .lfkp
.lfkp = function(wholeList, filterList) {
  if (!is.list(wholeList))
    stop(paste(deparse(substitute(wholeList)), "is not a list!"))
  if (length(wholeList) == 0)
    return(wholeList)
  if (!is.list(filterList))
    stop(paste(deparse(substitute(filterList)), "is not a list!"))
  if (length(filterList) == 0)
    return(filterList)
  logVec = lapply(names(wholeList), "%in%", names(filterList))
  filteredList = wholeList[unlist(logVec)]
  return(filteredList)
}
# .desireFun ----
.desireFun = function(low, high, target = "max", scale = c(1, 1), importance = 1) {

  if (importance > 10 | importance < 0.1)
    stop("importance needs to be in [0.1, 10]")
  if (low >= high)
    stop("the lower bound must be greater than the high bound!")
  if (any(scale <= 0))
    stop("the scale parameter must be greater than zero!")
  if (is.numeric(target)) {
    out = function(y) {
      flush.console()
      d = rep(0, length(y))
      d[y >= low & y <= target] = ((y[y >= low & y <= target] - low)/(target - low))^scale[1]
      d[y >= target & y <= high] = ((y[y >= target & y <= high] - high)/(target - high))^scale[2]
      return(d^importance)
    }
    return(out)
  }
  if (identical(tolower(target), "min")) {
    out = function(y) {

      d = rep(0, length(y))
      d[y > high] = 0
      d[y < low] = 1
      d[y >= low & y <= high] = ((y[y >= low & y <= high] - high)/(low - high))^scale[1]
      return(d^importance)
    }
    return(out)
  }
  if (identical(tolower(target), "max")) {
    out = function(y) {

      d = rep(0, length(y))
      d[y < low] = 0
      d[y > high] = 1
      d[y >= low & y <= high] = ((y[y >= low & y <= high] - low)/(high - low))^scale[1]
      return(d^importance)
    }
    return(out)
  }
}


# .nblock ----
.nblock <- function(fdo) {
  if (class(fdo)[1] != "facDesign")
    stop(paste(deparse(substitute(fdo)), "needs to be an object of class 'facDesign'"))
  return(length(unique(fdo$block[[1]])))
}
# .starFrame ----
.starFrame = function(k, alpha) {
  if (!is.numeric(k))
    stop("k must be numeric")
  if (!is.numeric(alpha))
    stop("alpha must be numeric")
  .starFrame = as.data.frame(matrix(0, nrow = k * 2, ncol = k))
  for (j in 1:k) {
    for (i in (2 * (j - 1) + 1):(2 * (j - 1) + 2)) {
      .starFrame[i, j] = ((-1)^i) * alpha
    }
  }
  return(.starFrame)
}
# .alphaOrth, .alphaRot,  ----
.alphaOrth <- function(k, p = 0, cc, cs) {
  alpha = sqrt((2^(k - p) * (2 * k + cs))/(2 * (2^(k - p) + cc)))
  return(alpha)
}
.alphaRot <- function(k, p = 0) {
  alpha = (2^(k - p))^0.25
  return(alpha)
}
# .rsmOrth ----
.rsmOrth = vector(mode = "list", length = 7)
.rsmOrth[[1]] = list(k = 2, p = 0, col = 1, row = 2, blocks = 2, cc = 3, cs = 3)
.rsmOrth[[2]] = list(k = 2, p = 0, col = 1, row = 1, blocks = 1, cc = 0, cs = 0)
.rsmOrth[[3]] = list(k = 3, p = 0, col = 2, row = 3, blocks = 3, cc = 2, cs = 2)
.rsmOrth[[4]] = list(k = 3, p = 0, col = 2, row = 2, blocks = 2, cc = 2, cs = 2)
.rsmOrth[[5]] = list(k = 3, p = 0, col = 2, row = 1, blocks = 1, cc = 0, cs = 0)
.rsmOrth[[6]] = list(k = 4, p = 0, col = 3, row = 4, blocks = 5, cc = 2, cs = 2)
.rsmOrth[[7]] = list(k = 4, p = 0, col = 3, row = 3, blocks = 3, cc = 2, cs = 2)
.rsmOrth[[8]] = list(k = 4, p = 0, col = 3, row = 2, blocks = 2, cc = 2, cs = 2)
.rsmOrth[[9]] = list(k = 4, p = 0, col = 3, row = 1, blocks = 1, cc = 0, cs = 0)
.rsmOrth[[10]] = list(k = 5, p = 0, col = 4, row = 5, blocks = 9, cc = 2, cs = 4)
.rsmOrth[[11]] = list(k = 5, p = 0, col = 4, row = 4, blocks = 5, cc = 2, cs = 4)
.rsmOrth[[12]] = list(k = 5, p = 0, col = 4, row = 3, blocks = 3, cc = 2, cs = 4)
.rsmOrth[[13]] = list(k = 5, p = 0, col = 4, row = 2, blocks = 2, cc = 2, cs = 4)
.rsmOrth[[14]] = list(k = 5, p = 0, col = 4, row = 1, blocks = 1, cc = 0, cs = 0)
.rsmOrth[[15]] = list(k = 5, p = 1, col = 5, row = 4, blocks = 5, cc = 6, cs = 1)
.rsmOrth[[16]] = list(k = 5, p = 1, col = 5, row = 3, blocks = 3, cc = 6, cs = 1)
.rsmOrth[[17]] = list(k = 5, p = 1, col = 5, row = 2, blocks = 2, cc = 6, cs = 1)
.rsmOrth[[18]] = list(k = 5, p = 1, col = 5, row = 1, blocks = 1, cc = 0, cs = 0)
.rsmOrth[[19]] = list(k = 6, p = 0, col = 6, row = 6, blocks = 17, cc = 1, cs = 6)
.rsmOrth[[20]] = list(k = 6, p = 0, col = 6, row = 5, blocks = 9, cc = 1, cs = 6)
.rsmOrth[[21]] = list(k = 6, p = 0, col = 6, row = 4, blocks = 5, cc = 1, cs = 6)
.rsmOrth[[22]] = list(k = 6, p = 0, col = 6, row = 3, blocks = 3, cc = 1, cs = 6)
.rsmOrth[[23]] = list(k = 6, p = 0, col = 6, row = 2, blocks = 2, cc = 1, cs = 6)
.rsmOrth[[24]] = list(k = 6, p = 0, col = 6, row = 1, blocks = 1, cc = 0, cs = 0)
.rsmOrth[[25]] = list(k = 6, p = 1, col = 7, row = 5, blocks = 9, cc = 4, cs = 2)
.rsmOrth[[26]] = list(k = 6, p = 1, col = 7, row = 4, blocks = 5, cc = 4, cs = 2)
.rsmOrth[[27]] = list(k = 6, p = 1, col = 7, row = 3, blocks = 3, cc = 4, cs = 2)
.rsmOrth[[28]] = list(k = 6, p = 1, col = 7, row = 2, blocks = 2, cc = 4, cs = 2)
.rsmOrth[[29]] = list(k = 6, p = 1, col = 7, row = 1, blocks = 1, cc = 0, cs = 0)
.rsmOrth[[30]] = list(k = 7, p = 0, col = 8, row = 6, blocks = 17, cc = 1, cs = 11)
#.rsmOrth[[31]] = list(k = 7, p = 0, col = 8, row = 6, blocks = 17, cc = 1, cs = 11) ###
.rsmOrth[[31]] = list(k = 7, p = 0, col = 8, row = 5, blocks = 9, cc = 1, cs = 11)
.rsmOrth[[32]] = list(k = 7, p = 0, col = 8, row = 4, blocks = 5, cc = 1, cs = 11)
.rsmOrth[[33]] = list(k = 7, p = 0, col = 8, row = 3, blocks = 3, cc = 1, cs = 11)
.rsmOrth[[34]] = list(k = 7, p = 0, col = 8, row = 2, blocks = 2, cc = 1, cs = 11)
.rsmOrth[[35]] = list(k = 7, p = 0, col = 8, row = 1, blocks = 1, cc = 0, cs = 0)
.rsmOrth[[36]] = list(k = 7, p = 1, col = 9, row = 5, blocks = 9, cc = 1, cs = 4)
# .validizeConstraints ----
.validizeConstraints = function(fdo, constraints) {
  X = fdo$as.data.frame()
  csOut = vector(mode = "list")
  aux <- list()
  for (i in 1:length(fdo$names())) {
    aux[[fdo$names()[i]]] <-.NAMES[i]
  }
  for (i in aux) {
    csOut[[i]] = c(min(X[, i]), max(X[, i]))
  }
  if (missing(constraints))
    return(csOut)
  cs2 = constraints[fdo$names()]
  cs2 = cs2[!unlist(lapply(cs2, is.null))]
  cs2 = cs2[(unlist(lapply(cs2, length)) == 2)]
  csOut[names(cs2)] = cs2[names(cs2)]
  return(csOut)
}
# .desHelp ----
.desHelp = function(fdo, factors, ...) {
  if (length(factors) != length(fdo$names()))
    stop("not enough factors specified in factors")
  if (any(is.na(factors)))
    stop("factors contain NA")
  yCharSet = intersect(names(fdo$desires()), names(fdo$fits))
  desList = fdo$desires()
  fitList = fdo$fits
  yDes = vector(mode = "list")
  aux <- list()
  for (i in 1:length(fdo$names())) {
    aux[[fdo$names()[i]]] <-.NAMES[i]
  }
  names(factors)<-unlist(aux)
  for (y in yCharSet) {
    obj = desList[[y]]
    dFun = .desireFun(obj$low, obj$high, obj$target, obj$scale, obj$importance)
    lm.y = fitList[[y]]
    yHat = predict(lm.y, newdata = data.frame(factors))
    yDes[[y]] = dFun(yHat)
  }
  return(yDes)
}
# .dHelp ----
.dHelp = function(model, dFun) {
  lm1 = model
  d1 = dFun
  out = function(newdata) {
    return(d1(predict(lm1, newdata = newdata)))
  }
  return(out)
}

# .curvTest ----
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

dotPlot <- function(x, group, xlim, ylim, col, xlab, ylab, pch, cex, breaks, stacked = TRUE, main, showPlot=TRUE) {
  #' @title dotPlot: Function to create a dot plot
  #' @description Creates a dot plot. For data in groups, the dot plot can be displayed stacked or in separate regions.
  #' @param x A numeric vector containing the values to be plotted.
  #' @param group (Optional) A vector for grouping the values. This determines the grouping of the data points in the dot plot.
  #' @param xlim A numeric vector of length 2 specifying the limits of the x-axis (lower and upper limits).
  #' @param ylim A numeric vector of length 2 specifying the limits of the y-axis (lower and upper limits).
  #' @param col A vector containing numeric values or strings specifying the colors for the different groups in the dot plot.
  #' @param xlab A title for the x-axis.
  #' @param ylab A title for the y-axis.
  #' @param pch A vector of integers specifying the symbols or a single character to be used for plotting points for the different groups in the dot plot.
  #' @param cex The amount by which points and symbols should be magnified relative to the default.
  #' @param breaks A numeric vector specifying the breakpoints for binning the values in \code{x}.
  #' @param stacked A logical value indicating whether the groups should be plotted in a stacked dot plot (default is \code{TRUE}).
  #' @param main A title for the plot.
  #' @param showPlot A logical value indicating whether to display the plot. Default is \code{TRUE}.
  #' @details Values in \code{x} are assigned to the bins defined by \code{breaks}. The binning is performed using \code{hist}.
  #' @return A list cointaining:
  #' \itemize{
  #' \item {An invisible matrix containing \code{NA}s and numeric values representing values in a bin. The number of bins is given by the number of columns of the matrix.}
  #' \item {The graphic.}
  #' }

  #' @examples
  #' # Create some data and grouping
  #' set.seed(1)
  #' x <- rnorm(28)
  #' g <- rep(1:2, 14)
  #'
  #' # Dot plot with groups and no stacking
  #' dotPlot(x, group = g, stacked = FALSE, pch = c(19, 20), main = "Non stacked dot plot")
  #'
  #' # Dot plot with groups and stacking
  #' dotPlot(x, group = g, stacked = TRUE, pch = c(19, 20), main = "Stacked dot plot")


  pch.size = "O"
  grouped = TRUE
  if(missing(main))
    main="Dot Plot"
  if (missing(xlab))
    xlab = deparse(substitute(x))
  x = x[!is.na(x)]
  if (missing(xlim))
    xlim = range(x)
  if (missing(ylim))
    ylim = c(0, 1)
  if (missing(ylab))
    ylab = ""
  if (missing(cex))
    cex = 1.5
  if (missing(group))
    group = rep(1, length(x))
  if (length(unique(group)) == 1)
    grouped = FALSE
  if (missing(pch) || length(unique(group)) > length(pch))
    pch = 1:length(unique(group))
  if (missing(col) || length(unique(group)) > length(col))
    col = 1:length(unique(group))
  if (missing(breaks)) {
    text_grob <- textGrob(pch.size, gp = gpar(cex = cex))
    text_width_npc <- convertWidth(grobWidth(text_grob), "npc", valueOnly = TRUE)
    slotSizeX <- text_width_npc * diff(xlim)*1.459287
    span = diff(range(x))
    temp1 = ppoints(2 * ceiling(span/slotSizeX))
    temp2 = numeric(length(temp1) + 2)
    temp2[2:(length(temp1) + 1)] = temp1
    temp2[1] = temp2[1] - 1.01 * diff(c(temp1[1], temp1[2]))
    temp2[length(temp2)] = rev(temp1)[1] + 1.01 * diff(c(temp1[1], temp1[2]))
    temp2 = temp2 * span + min(x)
    temp = min(x) + ppoints(span/slotSizeX) * span
    breaks = numeric(length(temp) + 2)
    breaks[2:(length(temp) + 1)] = temp
    breaks[1] = temp[1] - diff(c(temp[1], temp[2])) * 1.001
    breaks[length(breaks)] = rev(temp)[1] + diff(c(temp[1], temp[2])) * 1.001
    breaks = temp2
  }
  slotSizeY = 0.05607925
  span = diff(ylim)
  temp1 = ppoints(2 * ceiling(span/slotSizeY))
  temp2 = numeric(length(temp1) + 2)
  temp2[2:(length(temp1) + 1)] = temp1
  temp2[1] = temp2[1] - 1.01 * diff(c(temp1[1], temp1[2]))
  temp2[length(temp2)] = rev(temp1)[1] + 1.01 * diff(c(temp1[1], temp1[2]))
  yVec = temp2 * span + min(ylim)
  if (yVec[1] < 0)
    yVec = yVec + abs(yVec[1])
  else yVec = yVec - yVec[1]

  histObj = hist(x, breaks = breaks, right = FALSE, plot = FALSE)
  hMids = histObj$mids
  hCounts = histObj$counts
  hMids = histObj$mids
  mat = matrix(NA, nrow = length(x), ncol = length(hMids))
  colMat = mat
  groupmat = mat
  numVec = 1:nrow(mat)
  cutOff = 1
  groupList = vector(mode = "list", length = length(unique(group)))
  for (k in unique(group)) {
    histObj = hist(x[group == k], breaks = breaks, plot = FALSE)
    hMids = histObj$mids
    hCounts = histObj$counts
    hMids = histObj$mids
    for (i in seq(along = hMids)) {
      value = pch[k]
      colValue = col[k]
      from = 0
      from = numVec[is.na(mat[, i])][1]
      to = from
      if (hCounts[i] == 0)
        value = NA
      if (hCounts[i] >= 1)
        to = to + hCounts[i] - 1
      if (to > cutOff)
        cutOff = to
      mat[from:to, i] = value
      colMat[from:to, i] = colValue
    }
    groupList[[k]] = groupmat
  }
  if (grouped && !stacked) {
    groupIndex = unique(group)
    plot<-list()
    for (i in groupIndex){
      plot[[i]]<-dotPlot(x[group == i], xlim = xlim, breaks = breaks, cex = cex, xlab = xlab, ylab = ylab, col = col, pch = pch[i], showPlot=FALSE,main=main)[[2]]
    }
  }
  else {
    mat = mat[1:cutOff, ]
    if (!is.matrix(mat))
      mat = matrix(mat, nrow = 1)
    X<-c()
    Y<-c()
    Col<-c()
    Pch<-c()
    for (i in 1:nrow(mat)) {
      x = hMids[!is.na(mat[i, ])]
      y = rep(i * 0.3, times = length(x))
      y = rep(yVec[i], times = length(x))
      col = colMat[i, !is.na(mat[i, ])]
      pch = mat[i, !is.na(mat[i, ])]
      X<-c(X,x)
      Y<-c(Y,y)
      Col<-c(Col,col)
      Pch<-c(Pch,pch)
    }
    if(stacked){
      plot<-ggplot(data=data.frame(X,Y,Col,Pch), aes(x = X, y = Y, colour = factor(Col) , shape = factor(Pch))) + geom_point(size=cex) +
        scale_color_manual(values = setNames(unique(Col), unique(Col)))+
        scale_shape_manual(values = setNames(unique(Pch), unique(Pch)))+
        scale_x_continuous(limits = xlim+c(-0.05,0.05))+
        scale_y_continuous(limits = ylim)+
        labs(x=xlab, y=ylab, title=main)+
        theme_classic() +
        theme(panel.border = element_rect(colour = "black", fill = NA),plot.title = element_text(hjust = 0.5,face = "bold"),legend.position = "none")

    }
    else{
    plot<-ggplot(data=data.frame(X,Y), aes(x = X, y = Y)) + geom_point(colour=unique(col),size=cex,shape=unique(pch)) +
      scale_x_continuous(limits = xlim+c(-0.05,0.05))+
      scale_y_continuous(limits = ylim)+
      labs(x=xlab, y=ylab, title=main)+
      theme_classic() +
      theme(panel.border = element_rect(colour = "black", fill = NA),plot.title = element_text(hjust = 0.5,face = "bold"))
    }
  }
  if(showPlot){
    if(stacked){
      print(plot)
    }
    else{
      print(wrap_plots(plot,nrow=length(unique(group))))
    }

    invisible(list(mat,plot))
  } else{
    invisible(list(mat,plot))
  }

}



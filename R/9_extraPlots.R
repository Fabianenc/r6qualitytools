### dotPlot ----
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
        scale_x_continuous(limits = xlim+c(-0.08,0.08))+
        scale_y_continuous(limits = ylim)+
        labs(x=xlab, y=ylab, title=main)+
        theme_classic() +
        theme(panel.border = element_rect(colour = "black", fill = NA),plot.title = element_text(hjust = 0.5,face = "bold"),legend.position = "none")

    }
    else{
    plot<-ggplot(data=data.frame(X,Y), aes(x = X, y = Y)) + geom_point(colour=unique(col),size=cex,shape=unique(pch)) +
      scale_x_continuous(limits = xlim+c(-0.08,0.08))+
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
### mvPlot ----
mvPlot <- function(response,fac1,fac2,fac3,fac4,sort=TRUE,col,pch,labels=FALSE,quantile=TRUE,FUN=NA){
  #' @title mvPlot: Function to create a multi-variable plot
  #' @description Creates a plot for visualizing the relationships between a response variable and multiple factors.
  #' @param response The values of the \code{response} in a vector.response must be declared.
  #' @param fac1 Vector providing factor 1 as shown in the example.\code{fac1} must be declared.
  #' @param fac2 Vector providing factor 1 as shown in the example.\code{fac2} must be declared.
  #' @param fac3 Optional vector providing factor 3 as shown in the example.
  #' @param fac4 Optional vector providing factor 4 as shown in the example.
  #' @param sort Logical value indicating whether the sequence of the factors given by \code{fac1} - \code{fac4} should be reordered to minimize the space needed to visualize the Multi-Vari-Chart. By default \code{sort} is set to `TRUE`.
  #' @param col Graphical parameter. Vector containing numerical values or character strings giving the colors for the different factors. By default \code{col} starts with the value `3` and is continued as needed.
  #' @param pch Graphical parameter. Vector containing numerical values or single characters giving plotting points for the different factors. See \code{points} for possible values and their interpretation. Note that only integers and single-character strings can be set as a graphics parameter (and not \code{NA} nor \code{NULL}). By default \code{pch} starts with the value `1` and is continued as needed.
  #' @param labels Logical value indicating whether the single points should be labels with the row-number of the \code{data.frame} invisibly returned by the function \code{mvPlot}. By default \code{labels} is set to `FALSE`.
  #' @param quantile A logical value indicating whether the quanitiles (0.00135, 0.5 & 0.99865) should be visualized for the single groups. By default \code{quantile} is set to `TRUE`.
  #' @param FUN An optional function to be used for calculation of \code{response} for unique settings of the factors e.g. the \code{mean}. By default \code{FUN} is set to `NA` and therfore omitted.
  #' @return \code{mvPlot} returns an invisible list cointaining: a data.frame in which all plotted points are listed and the final plot. The option labels can be used to plot the row-numbers at the single points and to ease the identification.
  #' @examples
  #' #Example I
  #' examp1 = expand.grid(c("Engine1","Engine2","Engine3"),c(10,20,30,40))
  #' examp1 = as.data.frame(rbind(examp1, examp1, examp1))
  #' examp1 = cbind(examp1, rnorm(36, 1, 0.02))
  #' names(examp1) = c("factor1", "factor2", "response")
  #' mvPlot(response = examp1[,3], fac1 = examp1[,2],fac2 = examp1[,1],sort=FALSE,FUN=mean)


  n=4
  if(missing(fac3)){
    fac3 = NA
  }
  if(missing(fac4)){
    fac4 = NA
  }
  if(all(is.na(fac3)) || all(is.na(fac4))){
    n=3
  }
  if(all(is.na(fac3)) && all(is.na(fac4))){
    n=2
  }
  if(is.na(fac3[1])){
    fac3=0
  }
  if(is.na(fac4[1])){
    fac4=0
  }

  entire_data=data.frame(fac1,fac2,fac3,fac4,response)
  names(entire_data)=c(deparse(substitute(fac1)),deparse(substitute(fac2)),
                       deparse(substitute(fac3)),deparse(substitute(fac4)),
                       deparse(substitute(response)))
  original_entire_data=entire_data
  original_names=names(entire_data)

  if(sort==TRUE){
    fac_order=c(1,2,3,4)
    names(fac_order)=c(length(unique(fac1)),length(unique(fac2)),
                       length(unique(fac3)),length(unique(fac4)))
    fac_order=fac_order[order(names(fac_order),decreasing=TRUE)]
    entire_data[,1:4]=entire_data[,fac_order]
    names(entire_data)[1:4]=names(entire_data)[fac_order]
    original_names=names(entire_data)
  }

  xvalues = as.factor(entire_data[,1])
  xlabels = levels(xvalues)
  labels_legend = as.vector(unique(entire_data[,2]))
  number_of_plots=length(unique(entire_data[,3]))*length(unique(entire_data[,4]))
  xaxis_lab=c(xlabels,rep(c(NA,NA,xlabels),
                          times=length(unique(unique(entire_data[,2])))-1))
  levels(xvalues)=1:nlevels(xvalues)

  entire_data=cbind(entire_data,xvalues)
  subdata=list(); subsubdata=list(); temp=list() ; plotdata=list(); m=0
  names(entire_data)=c("x_lab","legend","a","b","y","x")

  for(i in 1:length(unique(entire_data[,4]))){
    subdata[[i]]=subset(entire_data,entire_data$b==unique(entire_data$b)[i])
    for(k in 1:length(unique(entire_data[,3]))){
      subsubdata[[k]]=subset(subdata[[i]],subdata[[i]]$a==unique(subdata[[i]]$a)[k])
      for(j in 1:length(unique(entire_data[,2])))
      {
        m=m+1
        temp[[m]]=subsubdata[[k]]
        plotdata[[m]]=subset(subsubdata[[k]],
                             subsubdata[[k]]$legend==unique(entire_data$legend)[j])
        plotdata[[m]]=plotdata[[m]][order(plotdata[[m]][,1]),]
      }
    }
  }

  if(n==2){
    ncol = 1
    nrow = 1
  }
  if(n==3){
    if (length(as.character(unique(entire_data[, 3]))) == 1) {
      ncol = 1
      nrow = 1
    }
    if (length(as.character(unique(entire_data[, 3]))) == 2) {
      ncol = 2
      nrow = 1
    }
    if (length(as.character(unique(entire_data[, 3]))) == 3) {
      ncol = 3
      nrow = 1
    }
    if (length(as.character(unique(entire_data[, 3]))) == 4) {
      ncol = 2
      nrow = 2
    }
    if (length(as.character(unique(entire_data[, 3]))) == 5) {
      ncol = 3
      nrow = 2
    }
    if (length(as.character(unique(entire_data[, 3]))) >= 6) {
      ncol = 1
      nrow = length(as.character(unique(entire_data[, 3])))
    }
  }
  if (n == 4) {
    ncol = length(unique(entire_data[, 4]))
    nrow = length(unique(entire_data[, 3]))
  }
  if(missing(col)){
    col=c(seq(3,length(unique(fac2))+2))
  }
  if(missing(pch)){
    pch=c(seq(1,length(unique(fac2))),8,16)
  }

  plots <- list()
  plot_counter <- 1

  m=0
  for(i in 1:(length(plotdata)/length(unique(entire_data[,2])))){
    p <- ggplot(entire_data, aes(x = x_lab, y = y, color = legend, group = legend)) +
      facet_wrap(~legend, scales = "free_x", nrow = 1) +
      theme_bw() +
      labs(x = "", y = "") +
      ylim(c(min(entire_data[,5],na.rm=TRUE),max(entire_data[,5],na.rm=TRUE)+0.1*(max(entire_data[,5],na.rm=TRUE)-min(entire_data[,5],na.rm=TRUE))))

    if(is.function(FUN)){
      table <- entire_data %>%
        group_by(legend) %>%
        summarise(x_median = mean(x_lab, na.rm = TRUE),
                  y_FUN = FUN(y)
        )
      table <- as.data.frame(table)

      entire_data_aux <- merge(entire_data, table, by.x = "legend", by.y = "legend", all.x = TRUE)

      p <- p + geom_point(data = entire_data_aux, aes(x = x_median, y = y_FUN), color = "darkred", shape = 15, size = 2) +
        geom_line(data = entire_data_aux, aes(x = x_median, y = y_FUN, group = 1), color = "darkred", linetype = "solid")
    }

    if(n==4){
      p <- p + labs(title = paste(names(original_entire_data[3]),"=",as.character(unique(temp[[i*length(unique(entire_data[,2]))]][,3]))," & ",
                                  names(original_entire_data[4]),"=",as.character(unique(temp[[i*length(unique(entire_data[,2]))]][,4])))) +
        theme(plot.title = element_text(hjust = 0.5,face = "bold"))

    }

    if(n==3){
      p <- p + labs(title = paste(names(original_entire_data[3]),"=",as.character(unique(temp[[i*length(unique(entire_data[,2]))]][,3])))) +
        theme(plot.title = element_text(hjust = 0.5,face = "bold"))

    }
    for(j in 1:length(unique(entire_data[,2]))){
      m=m+1
      p <- p + geom_point(data = plotdata[[m]], aes(x = x_lab, y = y), color = col[j], shape = pch[j])

      if(labels==TRUE && length(row.names(plotdata[[m]]))!=0)                                             #labels
      {
        p <- p + geom_text(data = plotdata[[m]], aes(x = x_lab, y = y, label = row.names(plotdata[[m]])),
                           vjust = -1, size = 3, col ="black")
      }
      if(quantile==TRUE)                                                                                  #quantiles
      {
        # median
        p <- p + geom_segment(
          data = plotdata[[m]],
          aes(x = min(plotdata[[m]]$x_lab), xend = max(plotdata[[m]]$x_lab),
              y = as.numeric(quantile(plotdata[[m]]$y, probs = 0.5)), yend = as.numeric(quantile(plotdata[[m]]$y, probs = 0.5))),
          linetype = "dashed", color = "gray"
        )
        # lower
        p <- p + geom_segment(
          data = plotdata[[m]],
          aes(x = min(plotdata[[m]]$x_lab), xend = max(plotdata[[m]]$x_lab),
              y = as.numeric(quantile(plotdata[[m]]$y, probs = 0.00135)), yend = as.numeric(quantile(plotdata[[m]]$y, probs = 0.00135))),
          linetype = "dashed", color = "gray"
        )
        # upper
        p <- p + geom_segment(
          data = plotdata[[m]],
          aes(x = min(plotdata[[m]]$x_lab), xend = max(plotdata[[m]]$x_lab),
              y = as.numeric(quantile(plotdata[[m]]$y, probs = 0.99865)), yend = as.numeric(quantile(plotdata[[m]]$y, probs = 0.99865))),
          linetype = "dashed", color = "gray"
        )
      }
      if(is.function(FUN)){
        fun_val=numeric()
        for(k in 1:length(unique(plotdata[[m]][,1]))){
          fun_val[k] = FUN(subset(plotdata[[m]],plotdata[[m]][,1]==unique(plotdata[[m]][,1])[k])[,5])
        }
        mean_points = data.frame(x = unique(plotdata[[m]]$x_lab), means = fun_val)
        plotdata[[m]] <- merge(plotdata[[m]], mean_points, by.x = "x_lab", by.y = "x", all.x = TRUE)

        p <- p + geom_point(data = plotdata[[m]], aes(x = x_lab, y = means), color = "red") +
          geom_line(data = plotdata[[m]], aes(x = x_lab, y = means), color = "red", linetype = "dashed")

      }


    }

    plots[[plot_counter]] <- p
    plot_counter <- plot_counter + 1
  }

  combined_plot <- wrap_plots(plots, ncol = ncol, nrow = nrow)

  print(combined_plot)
  invisible(list(data=original_entire_data, plot = combined_plot))
}


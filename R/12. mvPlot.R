mvPlot <- function(response,fac1,fac2,fac3,fac4,sort=TRUE,col,pch,labels=FALSE,quantile=TRUE,FUN=NA){
  #' @title mvPlot: Function to create a multi-variable plot
  #' @description Creates a plot for visualizing the relationships between a response variable and multiple factors.
  #' @param response The values of the \code{response} in a vector.response must be declared.
  #' @param fac1 Vector providing factor 1 as shown in the example.\code{fac1} must be declared.
  #' @param fac2 Vector providing factor 1 as shown in the example.\code{fac2} must be declared.
  #' @param fac1 Optional vector providing factor 3 as shown in the example.
  #' @param fac1 Optional vector providing factor 4 as shown in the example.
  #' @param sort Logical value indicating whether the sequence of the factors given by \code{fac1} - \code{fac4} should be reordered to minimize the space needed to visualize the Multi-Vari-Chart. By default \code{sort} is set to ‘TRUE’.
  #' @param col Graphical parameter. Vector containing numerical values or character strings giving the colors for the different factors. By default \code{col} starts with the value ‘3’ and is continued as needed.
  #' @param pch Graphical parameter. Vector containing numerical values or single characters giving plotting points for the different factors. See \code{points} for possible values and their interpretation. Note that only integers and single-character strings can be set as a graphics parameter (and not \code{NA} nor code{NULL}). By default \code{pch} starts with the value ‘1’ and is continued as needed.
  #' @param labels Logical value indicating whether the single points should be labels with the row-number of the \code{data.frame} invisibly returned by the function \code{mvPlot}. By default \code{labels} is set to ‘FALSE’.
  #' @param quantile A logical value indicating whether the quanitiles (0.00135, 0.5 & 0.99865) should be visualized for the single groups. By default \code{quantile} is set to ‘TRUE’.
  #' @param FUN An optional function to be used for calculation of \code{response} for unique settings of the factors e.g. the \code{mean}. By default \code{FUN} is set to ‘NA’ and therfore omitted.
  #' @return \code{mvPlot} returns an invisible list cointaining: a data.frame in which all plotted points are listed and the final plot. The option labels can be used to plot the row-numbers at the single points and to ease the identification.
  #' @examples
  #' #Example I
  #' examp1 = expand.grid(c("Engine1","Engine2","Engine3"),c(10,20,30,40))
  #' examp1 = as.data.frame(rbind(examp1, examp1, examp1))
  #' examp1 = cbind(examp1, rnorm(36, 1, 0.02))
  #' names(examp1) = c("factor1", "factor2", "response")
  #' mvPlot(response = examp1[,3], fac1 = examp1[,2],fac2 = examp1[,1],sort=FALSE,FUN=mean)
  #'
  #' #Example II
  #' examp2=expand.grid(c("Op I","Op II","Op III"),c(1,2,3,4),
  #'                    c("20.11.1987","21.11.1987"))
  #' examp2=as.data.frame(rbind(examp2, examp2, examp2))
  #' examp2=cbind(examp2, rnorm(72, 22, 2))
  #' names(examp2) = c("factor1", "factor2", "factor3", "response")
  #' mvPlot(response = examp2[,4], fac1 = examp2[,1],
  #'        fac2 = examp2[,2], fac3 = examp2[,3], sort=TRUE, FUN=mean, labels=TRUE)
  #'
  #' #Example III
  #' examp3 = expand.grid(c("A","B","C"),c("I","II","III","IV"),c("H","I"),
  #'                      c(1,2,3,4,5))
  #' examp3 = as.data.frame(rbind(examp3, examp3, examp3))
  #' examp3 = cbind(examp3, rnorm(360, 0, 2))
  #' names(examp3) = c("factor1", "factor2", "factor3", "factor4", "response")
  #' mvPlot(response = examp3[,5], fac1 = examp3[,1],
  #'              fac2 = examp3[,2], fac3 = examp3[,3], fac4 = examp3[,4], sort=TRUE,
  #'              quantile=TRUE, FUN=mean)

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


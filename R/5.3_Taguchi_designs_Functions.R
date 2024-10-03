###############################################################################
####################### DISEÑO TAGUCHI - FUNCIONES ############################
###############################################################################

# Funcion taguchiDesign ----
taguchiDesign <- function(design, randomize = TRUE, replicates = 1) {

  #' @title taguchiDesign: Taguchi Designs
  #' @description Function to create a taguchi design.
  #' @param design A character string specifying the orthogonal array of the Taguchi design. The available options are:
  #' \itemize{
  #'   \item {`L4_2" for three two-level factors.}
  #'   \item {`L8_2" for seven two-level factors.}
  #'   \item {`L9_3" for four three-level factors.}
  #'   \item {`L12_2" for 11 two-level factors.}
  #'   \item {`L16_2" for 16 two-level factors}
  #'   \item {`L16_4" for 16 four-level factors.}
  #'   \item {`L18_2_3" for one two-level and seven three-level factors.}
  #'   \item {`L25_5" for six five-level factors.}
  #'   \item {`L27_3" for 13 three-level factors.}
  #'   \item {`L32_2" for 32 two-level factors.}
  #'   \item {`L32_2_4" for one two-level factor and nine four-level factors.}
  #'   \item {`L36_2_3_a" for 11 two-level factors and 12 three-level factors.}
  #'   \item {`L36_2_3_b" for three two-level factors and 13 three-level factors.}
  #'   \item {`L50_2_5" for one two-level factor and eleven five-level factors.}
  #'   \item {`L8_4_2" for one four-level factor and four two-level factors.}
  #'   \item {`L16_4_2_a" for one four-level factor and 12 two-level factors.}
  #'   \item {`L16_4_2_b" for two four-level factors and nine two-level factors.}
  #'   \item {`L16_4_2_c" for three four-level factors and six two-level factors.}
  #'   \item {`L16_4_2_d" for five four-level factors and two two-level factors.}
  #'   \item {`L18_6_3" for one six-level factor and six three-level factors.}
  #' }
  #' @param randomize A logical value (\code{TRUE}/\code{FALSE}) that specifies whether to randomize the RunOrder of the design.
  #' By default, \code{randomize} is set to \code{TRUE}.
  #' @param replicates An integer specifying the number of replicates for each run in the design.
  #' @return A \code{taguchiDesign} returns an object of class \code{taguchiDesign}.
  #' @details An overview of possible taguchi designs is possible with \code{taguchiChoose}.
  #' @seealso
  #' \itemize{
  #' \item{\code{\link{facDesign}}: for 2^k factorial designs.}
  #' \item{\code{\link{rsmDesign}}: for response surface designs.}
  #' \item{\code{\link{fracDesign}}: for fractional factorial design.}
  #' \item{\code{\link{pbDesign}}: for response surface designs.}
  #' \item{\code{\link{gageRRDesign}}: for gage designs.}
  #' }
  #' @examples
  #' tdo <- taguchiDesign("L9_3")
  #' tdo$values(list(A = c("material 1", "material 2", "material 3"), B = c(29, 30, 35)))
  #' tdo$names(c("Factor 1", "Factor 2", "Factor 3", "Factor 4"))
  #' tdo$.response(rnorm(9))
  #' tdo$summary()


  odo = NA
  type = "single"
  for (i in seq(along = .oaList)) {
    #pmatch(design, .oaList[[i]]$id)
    if (!is.na(pmatch(design, .oaList[[i]]$id))) {

      temp = .oaList[[i]]
      design = temp$design
      repVec = rep(1, nrow(design))
      if (replicates > 1) {
        X = temp$design
        for (i in 1:(replicates - 1)) {
          design = rbind(design, X)
          repVec = c(repVec, rep(i + 1, times = nrow(X)))
        }
      }
      Replicate = data.frame(Replicate = as.numeric(repVec))

      odo = taguchiDesign.c$new()
      odo$design = design
      if(ncol(odo$design)>25){
        names(odo$design) = c(.NAMES,.generate_double_letters(ncol(odo$design)-25))
      } else{
        names(odo$design) = .NAMES[1:ncol(design)]
      }
      odo$name = temp$id
      odo$designType = temp$type
      odo$replic = Replicate
      StandOrder = 1:nrow(odo$design)
      RunOrder = StandOrder
      if (randomize) {
        RunOrder = sample(1:nrow(odo$design), nrow(odo$design), replace = FALSE, prob = NULL)
      }
      odo$design = odo$design[order(RunOrder), ]
      odo$replic = data.frame(Replicate = odo$replic[order(RunOrder), 1])
      row.names(odo$design) = odo$design$RunOrder
      odo$runOrder = data.frame(RunOrder = data.frame(RunOrder = RunOrder)[order(RunOrder), ])
      odo$standardOrder = data.frame(StandOrder = data.frame(StandOrder = StandOrder)[order(RunOrder), ])
      odo$response = data.frame(y = rep(NA, nrow(odo$design)))
      tfList = vector("list", ncol(design))
      for (i in seq(along = tfList)) tfList[[i]] = taguchiFactor$new()
      names(tfList) = names(odo$design)
      odo$.factors(tfList)
      valList = list(length = length(odo$names()))
      for (i in names(odo$names())) valList[[i]] = sort(unique(odo$design[, i]))
      odo$values(valList)
      return(odo)
    }
  }
  stop(paste("Please provide a valid design."))
}
# Funcion oaChoose ----
oaChoose <- function(factors1, factors2, level1, level2, ia) {
  #' @title oaChoose: Taguchi Designs
  #' @description Shows a matrix of possible taguchi designs.
  #' @param factors1 Number of factors on level1.
  #' @param factors2 Number of factors on level2.
  #' @param level1 Number of levels on level1.
  #' @param level2 Number of levels on level2.
  #' @param ia Number of interactions.
  #' @details \code{oaChoose} returns possible taguchi designs. Specifying the number of factor1 factors with level1 levels (factors1 = 2, level1 = 3 means 2 factors with 3 factor levels) and factor2 factors with level2 levels and desired interactions one or more taguchi designs are suggested.
  #' If all parameters are set to `0`, a matrix of possible taguchi designs is shown.
  #' @return \code{oaChoose} returns an object of class \code{taguchiDesign}.
  #' @examples
  #' oaChoose()
  #' @seealso
  #' \itemize{
  #' \item{\code{\link{facDesign}}: for 2^k factorial designs.}
  #' \item{\code{\link{rsmDesign}}: for response surface designs.}
  #' \item{\code{\link{fracDesign}}: for fractional factorial design.}
  #' \item{\code{\link{gageRRDesign}}: for gage designs.}
  #' }

  params = list(factors1 = 0, factors2 = 0, level1 = 0, level2 = 0, ia = 0)
  if (!missing(ia))
    params$ia = ia
  if (!missing(factors2))
    params$factors2 = factors2
  if (!missing(level2))
    params$level2 = level2
  do.call(taguchiChoose, params)
}

# Funcion taguchiChoose ----
taguchiChoose <- function(factors1 = 0, factors2 = 0, level1 = 0, level2 = 0, ia = 0, col = 2, randomize = TRUE, replicates = 1) {
  #' @title taguchiChoose: Taguchi Designs
  #' @description Shows a matrix of possible taguchi designs
  #' @param factors1 Integer number of factors on level1. By default set to `0`.
  #' @param factors2 Integer number of factors on level2. By default set to `0`.
  #' @param level1 Integer number of levels on level1. By default set to `0`.
  #' @param level2 Integer number of levels on level2. By default set to `0`.
  #' @param ia Integer number of interactions. By default set to `0`.
  #' @param col Select the color scheme for the selection matrix: use \code{1} for blue, \code{2} for pink (default), and \code{3} for a variety of colors.
  #' @param randomize A logical value (\code{TRUE}/\code{FALSE}) that specifies whether to randomize the RunOrder of the design.
  #' By default, \code{randomize} is set to \code{TRUE}.
  #' @param replicates An integer specifying the number of replicates for each run in the design.
  #' @details \code{taguchiChoose} returns possible taguchi designs.
  #' Specifying the number of factor1 factors with level1 levels (factors1 = 2, level1 = 3 means 2 factors with 3 factor levels) and factor2 factors with level2 levels and desired interactions one or more taguchi designs are suggested.
  #' If all parameters are set to 0, a matrix of possible taguchi designs is shown.
  #' @return \code{taguchiChoose} returns an object of class \code{taguchiDesign}.
  #' @examples
  #' tdo1 <- taguchiChoose()
  #' tdo1 <- taguchiChoose(factors1 = 3, level1 = 2)
  #' @seealso
  #' \itemize{
  #' \item{\code{\link{facDesign}}: for 2^k factorial designs.}
  #' \item{\code{\link{rsmDesign}}: for response surface designs.}
  #' \item{\code{\link{fracDesign}}: for fractional factorial design.}
  #' \item{\code{\link{gageRRDesign}}: for gage designs.}
  #' }

  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  if(col == 1){
    col<-c("#0091EA", "#00A3E0", "#00B0FF", "#26C6DA", "#4DD0E1", "#80DEEA", "#B2EBF2", "#E0F7FA", "#F2F2F2")
  } else if(col == 2){
    col<-c("#C2185B", "#D81B60", "#E91E63", "#EC407A", "#F06292", "#F48FB1", "#F8BBD0", "#FCE4EC", "#F2F2F2")
  } else if (col == 3){
    col<-c("#FBB4AE", "#B5CCE1", "#CAE9C6", "#DDCBE3", "#FCD8A8", "#FEFECB", "#E5D8BD", "#FCDAEC", "#F2F2F2")
  } else{
    stop(paste("Invalid value for 'col' argument. It must be 1, 2, or 3."))
  }

  if (factors1 == 0 & factors2 == 0 & level1 == 0 & level2 == 0 & ia == 0) {
    temp = vector(mode = "character", length = length(.oaList))
    for (i in 1:length(.oaList)) temp[i] = .oaList[[i]]$id
    temp = c(temp, rep(" ", (length(temp)%/%6 + 1) * 6 - length(temp)))
    mat = data.frame(matrix(temp, ncol = 6, byrow = TRUE))
    names(mat) = rep(" ", ncol(mat))
    colList<-.colList(mat,col)
    par(mfrow = c(4, 6))
    par(mar = c(0, 0, 0, 0))
    par(oma = c(4, 4, 4, 4))
    for (i in seq(nrow(mat))) {
      for (j in seq(ncol(mat))) {
        plot(0, 0, xaxs = "i", yaxs = "i", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, type = "n", xlab = "", ylab = "", bg = "red", fg = "green")
        box()
        rect(0, 0, 1, 1, col = colList[i,j])
        yPos = 0.04
        xPos = 0.6
        text(0.5, 0.5 , mat[i,j], adj = c(0.5, 0.5), cex = 1)
      }
    }
    cat("\nChoose a taguchi design by clicking into the appropriate field")
    cat("\nWaiting for your selection:")
    cat("\n\n")
    flush.console()
    mtext("Choose a Taguchi Design:", side = 3, outer = TRUE, line = 1, cex = 1.5, font = 2)
    xyList = NULL
    xyList = try(locator(1), silent = TRUE)
    x = 1
    y = 1
    if (!is.null(xyList)) {
      x = ceiling(xyList$x + 5)
      y = ceiling(4 - xyList$y)
    }
    if(mat[y,x]!=" "){
      return(taguchiDesign(mat[y,x],randomize = randomize, replicates = replicates))
    }
    else{
      stop(paste("Please select a non-empty grid."))
    }

  }
  else {

    if (factors2 <= 0)
      level2 = 0

    Anzahl_Spalten = factors1 + factors2 + ia
    ss = list()
    for (i in seq(along = .oaList)) {
      li = .oaList[[i]]
      if (li$factors1 >= factors1 & li$factors2 >= factors2 & (li$levels1 == level1 | li$levels1 == level2) & (li$levels2 == level2 | li$levels2 == level1) &
          li$anzahl_spalten >= Anzahl_Spalten)
        ss[i] = li$id
    }
    out = as.character(ss)
    out = out[out != "NULL"]
    if (length(out) > 0) {
      cat(paste(factors1, "factors on", level1, "levels and", factors2, "factors on", level2, "levels with", ia, "desired interactions to be estimated\n"))
      cat("\n")
      naux <- ceiling(length(out)/floor(sqrt(length(out))))*floor(sqrt(length(out)))-length(out)
      out <- c(out,rep(" ",naux))
      mat = data.frame(matrix(out, nrow = floor(sqrt(length(out))), byrow = TRUE))
      names(mat) = rep(" ", ncol(mat))
      colList<-.colList(mat,col)
      par(mfrow = c(nrow(mat),ncol(mat)))
      par(mar = c(0, 0, 0, 0))
      par(oma = c(4, 4, 4, 4))
      k = 1
      for (i in seq(nrow(mat))) {
        for (j in seq(ncol(mat))) {
          plot(0, 0, xaxs = "i", yaxs = "i", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, type = "n", xlab = "", ylab = "", bg = "red", fg = "green")
          box()
          rect(0, 0, 1, 1, col = colList[i,j])
          yPos = 0.04
          xPos = 0.6
          text(0.5, 0.5 , mat[i,j], adj = c(0.5, 0.5), cex = 1)
          k = k+1
        }
      }

      cat("\nChoose a possible taguchi design by clicking into the appropriate field")
      cat("\nWaiting for your selection:")
      cat("\n\n")
      flush.console()
      mtext("Choose a Taguchi Design:", side = 3, outer = TRUE, line = 1, cex = 1.5, font = 2)
      xyList = NULL
      xyList = try(locator(1), silent = TRUE)
      x = 1
      y = 1
      if (!is.null(xyList)) {
        x = ceiling(xyList$x + ncol(mat) - 1)
        y = ceiling(nrow(mat) - xyList$y)
      }
      if(mat[y,x]!=" "){
        return(taguchiDesign(mat[y,x],randomize = randomize, replicates = replicates))
      }
      else{
        stop(paste("Please select a non-empty grid."))
      }

    }
    else {
      message("No Design Found\n")
      out = NA
      invisible(out)
    }
  }
}

# snPlot ----
snPlot<-function(object, type="nominal" , factors, fun = mean, response = NULL,
                points = FALSE, classic = FALSE,
                lty, xlab, ylab, main, ylim, l.col, p.col, ld.col, pch)
{
  #' @title snPlot: Signal-to-Noise-Ratio Plots
  #' @description Creates a Signal-to-Noise Ratio plot for designs of type \code{taguchiDesign.c} with at least two replicates.
  #' @usage
  #' snPlot(object, type = "nominal", factors, fun = mean, response = NULL,
  #'        points = FALSE, classic = FALSE, lty, xlab, ylab,
  #'        main, ylim, l.col, p.col, ld.col, pch)
  #' @param object An object of class \code{\link{taguchiDesign.c}}.
  #' @param type A character string specifying the type of the Signal-to-Noise Ratio plot. Possible values are:
  #' \itemize{
  #'   \item \code{`nominal`}: Nominal-the-best plot to equalize observed values to a nominal value.
  #'   \item \code{`smaller`}: Smaller-the-better plot to minimize observed values.
  #'   \item \code{`larger`}: Larger-the-better plot to maximize observed values.
  #' }
  #' Default is \code{`nominal`}.
  #' @param factors The factors for which the effect plot is to be created.
  #' @param fun A function for constructing the effect plot such as \code{mean}, \code{median}, etc. Default is \code{mean}.
  #' @param response A character string specifying the response variable. If \code{object} contains multiple responses, this parameter selects one column to plot. Default is \code{NULL}.
  #' @param points A logical value. If \code{TRUE}, points are shown in addition to values derived from \code{fun}. Default is \code{FALSE}.
  #' @param classic A logical value. If \code{TRUE}, creates an effect plot as depicted in most textbooks. Default is \code{FALSE}.
  #' @param lty A numeric value specifying the line type to be used.
  #' @param xlab A title for the x-axis.
  #' @param ylab A title for the y-axis.
  #' @param main An overall title for the plot.
  #' @param ylim A numeric vector of length 2 specifying the limits of the y-axis.
  #' @param l.col A color for the lines.
  #' @param p.col A color for the points.
  #' @param ld.col A color for the dashed line.
  #' @param pch The symbol for plotting points.
  #' @details The Signal-to-Noise Ratio (SNR) is calculated based on the type specified:
  #' \itemize{
  #'   \item \code{`nominal`}: \deqn{SN = 10 \cdot log(mean(y) / var(y))}
  #'   \item \code{`smaller`}: \deqn{SN = -10 \cdot log((1 / n) \cdot  sum(y^2))}
  #'   \item \code{`larger`}: \deqn{SN = -10 \cdot log((1 / n) \cdot sum(1 / y^2))}
  #' }
  #' Signal-to-Noise Ratio plots are used to estimate the effects of individual factors and to judge the variance and validity of results from an effect plot.
  #' @return An invisible \code{data.frame} containing all the single Signal-to-Noise Ratios.
  #' @examples
  #' tdo <- taguchiDesign("L9_3", replicates = 3)
  #' tdo$.response(rnorm(27))
  #' snPlot(tdo, points = TRUE, l.col = 2, p.col = 2, ld.col = 2, pch = 16, lty = 3)

  Debugging=FALSE
  if(class(object)[1]!="taguchiDesign.c")
    stop("object needs to be of class taguchiDesign")
  Length=dim(object$as.data.frame())[1]
  resLength=dim(object$.response())[2]
  temp=data.frame(object$design)
  comp=unique(temp)
  SNi=numeric();SN=data.frame()
  m=numeric();y=numeric()
  if(missing(main))
  {
    for(k in 1:resLength)
      m[k]=paste("Effect Plot for S/N ratios of",names(object$.response())[k])
    main=m
  }
  if(missing(ylab))
  {
    for(k in 1:resLength)
      y[k]=paste("means of S/N ratios for ",names(object$.response())[k])
    ylab=y
  }
  if(identical(comp,temp))
    stop("taguchi design has no replicates! S/N can not be calculated!")
  for(k in 1:resLength)
  {
    for(j in 1:dim(comp)[1])
    {  val=numeric()
    for(i in 1:Length)
    {
      if(identical(as.numeric(comp[j,]),as.numeric(temp[i,])))
      {
        val[i]=object$.response()[i,k]
      }
      else
        val[i]=NA
    }
    n=Length/(dim(comp)[1])
    if(type=="nominal")
      SNi[j]=10*log10((mean(val,na.rm=TRUE)^2)/(sd(val,na.rm=TRUE)^2))
    if(type=="smaller")
      SNi[j]=-10*log10((1/n)*sum(val^2,na.rm=TRUE))
    if(type=="larger")
      SNi[j]=-10*log10((1/n)*sum(1/(val^2),na.rm=TRUE))
    if(Debugging==TRUE)
      print(SNi)
    for(i in 1:Length)
    {
      if(identical(as.numeric(comp[j,]),as.numeric(temp[i,])))
      {
        SN[i,k]=SNi[j]
      }
    }
    }
    tdo=object$clone()
    tdo$.response(SN[k])
    if(k>1)
      dev.new()
    tdo$effectPlot(factors=factors, fun = mean, response = response,
               points = points, lty = lty, xlab = xlab, ylab =ylab[k],
               main = main[k], ylim = ylim, l.col=l.col, p.col=p.col, ld.col=ld.col, pch = pch)
  }
  names(SN)=paste("S/N",names(object$.response()))
  invisible(SN)
}


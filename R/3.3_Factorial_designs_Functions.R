#################################################################################################
############################# DISEÑOS FACTORIALES - FUNCIONES ###################################
#################################################################################################

# as.data.frame_facDesign ----
as.data.frame_facDesign <- function(dfac) {
  #' @title as.data.frame_facDesign: Coerce to a data.frame
  #' @description Converts an object of class \code{\link{facDesign.c}} into a data frame.
  #' @usage as.data.frame_facDesign(dfac)
  #' @param dfac An object of class \code{\link{facDesign.c}} that you want to convert to a data frame.
  #' @return The function \code{as.data.frame_facDesign} returns a data frame.
  #' @examples
  #' fdo <- fracDesign(k = 2, replicates = 3, centerCube = 4)
  #' as.data.frame_facDesign(fdo)

  if (nrow(dfac$cube)>0) {
    frameOut = dfac$cube
    names(frameOut) = dfac$names()
  }
  else return(NULL)
  if (nrow(dfac$centerCube)>0){
    faux <- dfac$centerCube
    names(faux) <- dfac$names()
    frameOut = rbind(frameOut, faux)
  }
  if (nrow(dfac$star)>0)
    frameOut = rbind(frameOut, dfac$star)
  if (nrow(dfac$centerStar)>0)
    frameOut = rbind(frameOut, dfac$centerStar)
  aux <- list()
  for (i in 1:length(dfac$names())) {
    aux[[dfac$names()[i]]] <-.NAMES[i]
  }
  if (!is.null(dfac$factors) && length(dfac$factors) == dim(frameOut)[2]) {
    names(frameOut) = as.character(aux)
  }
  if (!is.null(dfac$blockGen) && nrow(dfac$blockGen) > 0) {
    frameOut = cbind(dfac$blockGen, frameOut)
  }
  if (!is.null(dfac$block) && nrow(dfac$block) > 0) {
    frameOut = cbind(dfac$block, frameOut)
  }
  if (!is.null(dfac$runOrder) && nrow(dfac$runOrder) > 0) {
    frameOut = cbind(dfac$runOrder, frameOut)
  }
  if (!is.null(dfac$standardOrder) && nrow(dfac$standardOrder) > 0) {
    frameOut = cbind(dfac$standardOrder, frameOut)
  }
  if (!is.null(dfac$response) && nrow(frameOut) == nrow(dfac$response))
    frameOut = cbind(frameOut, dfac$response)
  else {
    temp = as.data.frame(matrix(NA, nrow = nrow(frameOut), ncol = ncol(dfac$response)))
    names(temp) = names(dfac$response)
    frameOut = cbind(frameOut, temp)
  }
  runIndex = order(dfac$runOrder[,1])
  out = frameOut[runIndex, ]
  return(out)
}
# aliasTable ----
aliasTable <- function (fdo, degree, print = TRUE)
{
  #' @title aliasTable: Display an alias table
  #' @description This function generates an alias table for a factorial design object.
  #' @param fdo An object of class \code{\link{facDesign.c}}.
  #' @param degree Numeric value specifying the degree of interaction i.e. degree=3 means up to threeway interactions.
  #' @param print If \code{TRUE}, the alias table will be printed. By default \code{print} is set to \code{TRUE}.
  #' @return The function \code{aliasTable} returns a matrix indicating the aliased effects.
  #' @seealso \code{\link{fracDesign}}, \code{\link{fracChoose}}
  #' @examples
  #' # Create a fractional factorial design
  #' dfrac <- fracDesign(k = 3, gen = "C = AB")
  #' # Display the alias table for the fractional factorial design
  #' aliasTable(dfrac)

  if (class(fdo)[1] == "facDesign.c") {
    X = unique(fdo$cube)
    N = nrow(X)
    k = log2(N)
    kPlusP = ncol(X)
    if (missing(degree))
      degree = min(c(4, k + 1))
    X1 = .helpAliasTable(fdo, k, degree = degree - 1)
    X2 = .helpAliasTable(fdo, k = kPlusP, degree)
  }
  if (class(fdo)[1] == "taguchiDesign.c") {
    if (length(table(as.numeric(as.matrix(fdo$design)))) !=
        2)
      stop("calculation of an alias table for mixed designs is not supported")
    k = ncol(fdo$design)
    if (missing(degree))
      degree = min(c(3, k))
    X1 = unique(fdo$design)
    X1 = .replace2s(X1)
    X2 = .helpAliasTable(fdo, k, degree)
    X1 = cbind(data.frame(Identity = rep(1, times = nrow(X1))),
               X1)
  }
  logVec = !(names(X2) %in% names(X1))
  X2 = X2[, logVec]
  X1 = as.matrix(X1)
  X2 = as.matrix(X2)
  alias.matrix = solve(t(X1) %*% X1) %*% t(X1) %*% X2
  if (print)
    print(round(alias.matrix, 2))
  invisible(alias.matrix)
}
# randomize ----
randomize <- function (fdo, random.seed, so = FALSE)
{
  #' @title randomize: Randomization
  #' @description Function to do randomize the run order of factorial designs.
  #' @param fdo An object of class \code{\link{facDesign.c}}.
  #' @param random.seed Seed for randomness.
  #' @param so Logical value specifying whether the standard order should be used or not. By default \code{so} is set to \code{FALSE}.
  #' @return An object of class \code{\link{facDesign.c}} with the run order randomized.
  #' @examples
  #' dfrac <- fracDesign(k = 3)
  #' randomize(dfrac)

  if (missing(random.seed))
    set.seed(93275938)
  else set.seed(random.seed)
  j = 1
  temp = fdo$runOrder
  for (i in sort(unique(fdo$block[, 1]))) {
    pos = !is.na(match(fdo$block[, 1], i))
    count = sum(as.numeric(pos))
    if (so) {
      temp[pos, 1] = j:(j + (count - 1))
    }
    else {
      temp[pos, 1] = sample(j:(j + (count - 1)), count)
    }
    j = j + count
  }
  fdo$runOrder = temp
  return(fdo)
}
# blocking ----
blocking <- function (fdo, blocks, random.seed, useTable = "rsm", gen){
  #' @title blocking: Blocking
  #' @description Blocks a given factorial or response surface design.
  #' @param fdo An object of class \code{\link{facDesign.c}}.
  #' @param blocks Numeric value giving the number of blocks.
  #' @param random.seed Numeric value to generate repeatable results for randomization within blocks.
  #' @param useTable Character indicating which table to use. The following options will be accepted:
  #' \itemize{
  #'    \item \code{`rms`}: table from reference
  #'    \item \code{`calc`}: table calculated by package
  #' }
  #' @param gen Giving the generator that will be used.
  #' @return The function \code{blocking} returns an object of class \code{\link{facDesign.c}} with blocking structure.
  #' @seealso \code{\link{facDesign}}.
  #' @examples
  #' # Example 1
  #' #Create a 2^3 full factorial design
  #' fdo <- facDesign(k = 3)
  #' # Apply blocking to the design with 2 blocks
  #' blocking(fdo, 2)
  #'
  #' # Example 2
  #' #Create a response surface design for 3 factors
  #' fdo <- rsmDesign(k = 3)
  #' # Apply blocking to the design with 3 blocks (1 block for star part and 2 blocks for the cube part)
  #' blocking(fdo, 3)

  override = FALSE
  Block = data.frame(Block = rep(1, fdo$nrow()))
  fdo$.block(Block)
  fdo = randomize(fdo, so = TRUE)
  if (missing(random.seed)) {
    runif(1)
    random.seed = .Random.seed[sample(1:626, 1)]
  }
  if (missing(gen))
    gen = NULL
  if (blocks <= 1) {
    Block = data.frame(Block = rep(1, fdo$nrow()))
    fdo$.block(Block)
    fdo = randomize(fdo, random.seed = random.seed)
    return(fdo)
  }
  if (nrow(fdo$star) > 0 | nrow(fdo$centerStar) > 0) {
    if (blocks == 2) {
      override = TRUE
      fdo = randomize(fdo, so = TRUE)
      numB1 = nrow(fdo$cube) + nrow(fdo$centerCube)
      numB2 = fdo$nrow() - numB1
      fdo$.block(data.frame(Block = c(rep(1, numB1),
                                      rep(2, numB2))))
      fdo$.blockGen(data.frame(B1 = rep(NA, fdo$nrow())))
    }
    if (blocks %in% c(2, 3, 5, 9, 17))
      blocks = blocks - 1
    else stop("Blocking not possible")
  }
  else {
    if (!(blocks %in% c(1, 2, 4, 8, 16, 32, 64, 128)))
      stop("Blocking not possible")
  }
  if (is.null(gen))
    gen = .blockInteractions(fdo, blocks, useTable)
  if (is.null(gen) & !override) {
    cat("\n")
    cat(paste("Blocking in", blocks, "blocks not possible!"))
    cat("\n")
    return(fdo)
  }
  if (!override) {
    .blockGenCol = .blockGenCol(gen, fdo)
    .blockCol = .blockCol(.blockGenCol)
    Block = .blockCol
    BlockGenCol = .blockGenCol
    fdo$.block(Block)
    fdo$.blockGen(BlockGenCol)
  }
  numCC = nrow(fdo$centerCube)
  if (numCC > 0) {
    ccFrame = as.data.frame(matrix(0, nrow = numCC, ncol = ncol(fdo$cube)))
    names(ccFrame) = fdo$names()
    fdo$.centerCube(ccFrame)
  }
  fdo = randomize(fdo, random.seed = random.seed)
  return(fdo)
}
# Función fracDesign ----
fracDesign <- function (k = 3, p = 0, gen = NULL, replicates = 1, blocks = 1,
                        centerCube = 0, random.seed = 1234)
{
  #' @title fracDesign
  #' @description Generates a 2^k-p fractional factorial design.
  #' @param k Numeric value giving the number of factors. By default \code{k} is set to `3`.
  #' @param p Numeric integer between `0` and `7`. p is giving the number of additional factors in the response surface design by aliasing effects.
  #' A 2^k-p factorial design will be generated and the generators of the standard designs available in fracChoose() will be used.
  #' By default p is set to `0`. Any other value will cause the function to omit the argument gen given by the user and replace it by the one out of the table of standard designs (see: \code{\link{fracChoose}}).
  #' Replicates and blocks can be set anyway!
  #' @param gen One or more defining relations for a fractional factorial design, for example:  \code{`C=AB`}. By default gen is set to \code{NULL}.
  #' @param replicates Numeric value giving the number of replicates per factor combination. By default \code{replicates} is set to `1`.
  #' @param blocks Numeric value giving the number of blocks. By default blocks is set to `1`.
  #' @param centerCube Numeric value giving the number of center points within the 2^k design. By default \code{centerCube} is set to `0`.
  #' @param random.seed Seed for randomization of the design
  #' @return The function \code{fracDesign} returns an object of class \code{\link{facDesign.c}}.
  #' @seealso \code{\link{facDesign}}, \code{\link{fracChoose}}, \code{\link{rsmDesign}}, \code{\link{pbDesign}}, \code{\link{taguchiDesign}}
  #' @examples
  #' #Example 1
  #' #Returns a 2^4-1 fractional factorial design. Factor D will be aliased with
  #' vp.frac = fracDesign(k = 4, gen = "D=ABC")
  #' #the three-way-interaction ABC (i.e. I = ABCD)
  #' vp.frac$.response(rnorm(2^(4-1)))
  #' # summary of the fractional factorial design
  #' vp.frac$summary()
  #'
  #' #Example 2
  #' #Returns a full factorial design with 3 replications per factor combination and 4 center points
  #' vp.rep = fracDesign(k = 3, replicates = 3, centerCube = 4)
  #' #Summary of the replicated fractional factorial design
  #' vp.rep$summary()
  STDfdo = FALSE
  if (p < 0 || p > 7)
    stop("p needs to be an integer between 0 and 7!")
  if (abs(p - round(p)) > .Machine$double.eps^0.5) {
    warning(paste("p needs to be an integer but is real! p was rounded to",
                  round(p)))
    p = round(p)
  }
  if (p != 0) {
    gen = NULL
    for (i in 1:length(.fdoOrth)) {
      if (k == .fdoOrth[[i]]$k && p == .fdoOrth[[i]]$p) {
        STDfdo = TRUE
        return(fracDesign(k = .fdoOrth[[i]]$k, gen = .fdoOrth[[i]]$gen,
                          replicates = replicates, blocks = blocks,
                          centerCube = centerCube, random.seed = random.seed))
      }
    }
    if (STDfdo == FALSE)
      stop("No standard Design for the choosen combination of k and p (see: fracChoose())!")
  }
  if (!is.numeric(random.seed))
    stop("random.seed needs to be numeric")
  if (!is.numeric(blocks))
    stop("blocks needs to be numeric!")
  if (!is.numeric(replicates)){
    stop("replicates needs to be numeric!")
  } else {
    if (replicates < 0){
      stop("replicates need to >= 0")
    }
  }
  N <- 2^k
  X <- matrix(NA, nrow = N, ncol = k)
  for (j in 1:k) X[, j] <- rep(sort(rep(c(-1, 1), N/2^j)),
                               2^(j - 1))
  X <- X[, ncol(X):1]
  if (is.null(gen)) {
    X = as.data.frame(X)
    names(X) = .NAMES[1:k]
  }
  origX = X
  if (replicates > 1) {
    for (i in 2:replicates) {
      X = rbind(X, origX)
    }
  }
  frameOut = data.frame(X)

  if (!is.null(gen)) {
    listGen = vector("list", length(gen))
    .numFactors = numeric(0)
    charFactors = character(0)

    temp = character(0)
    for (i in seq(along = gen)) {

      if (!is.character(gen[i]))
        stop("Defining Relations should contain characters only!")
      chars = strsplit(gen[i], split = character(0))[[1]]

      checkDupl = character(0)
      for (j in 1:length(chars)) {
        if (chars[j] %in% toupper(c(.NAMES[1:26], letters[1:26]))) {
          if (chars[j] %in% checkDupl)
            stop("Defining relations contain one or more duplicates!")
          checkDupl = c(checkDupl, chars[j])
          temp = c(temp, chars[j])
        }
      }
    }
    temp = sort(unique(temp))
    numCharVec = 1:length(temp)
    names(numCharVec) = temp

    for (i in seq(along = gen)) {

      if (!is.character(gen[i]))
        stop("Defining Relations should contain characters only!")
      chars = strsplit(gen[i], split = character(0))[[1]]
      numVec = numeric(0)
      charVec = character(0)
      allowedChars = c(.NAMES[1:26], letters[1:26], "=")
      for (j in 1:length(chars)) {
        if (chars[j] %in% allowedChars) {
          if ((chars[j] == "=") & (length(numVec) !=
                                   1))
            stop("check position of \"=\" in generators!")
          if (chars[j] != "=") {
            charVec = c(charVec, toupper(chars[j]))
            numVec = c(numVec, numCharVec[names(numCharVec) ==
                                            toupper(chars[j])])
          }
        }
      }

      listGen[[i]] = numVec
      .numFactors = c(.numFactors, numVec)
      charFactors = c(charFactors, charVec)
    }

    names(.numFactors) = charFactors
    if (length(unique(.numFactors)) > k)
      stop("number of distinct Factors in generators greater than k!")

    for (i in seq(along = listGen)) {
      ind <- trunc(listGen[[i]])
      if (any(abs(ind) > k))
        stop(paste("generator:", paste(ind[1], "=",
                                       paste(ind[-1], collapse = "*")), "includes undefined columns"))
      x <- rep(sign(ind[1]), N)
      for (j in ind[-1]) x <- x * X[, j]
      X[, abs(ind[1])] <- x
    }
    X <- unique(X)
    origX = X
    if (replicates > 1) {
      for (i in 2:replicates) {
        X = rbind(X, origX)
      }
    }
    frameOut = as.data.frame(X)
    names(frameOut) = names(numCharVec)
    if (k > length(temp)) {
      charsLeft = (.NAMES[1:26])[-match(charFactors, .NAMES[1:26])]
      naIndex = (1:k)[is.na(names(frameOut))]
      names(frameOut)[naIndex] = charsLeft[1:length(naIndex)]
    }
  }
  DesignOut <- facDesign.c$new()
  DesignOut$generator <-  gen
  DesignOut$cube <-  frameOut
  listFac <-  vector("list", ncol(frameOut))
  for (i in seq(along = listFac)){
    listFac[[i]] = doeFactor$new()
    listFac[[i]]$name = names(frameOut)[i]
  }


  DesignOut$factors = listFac

  if (centerCube >= 1) {
    temp = data.frame(matrix(rep(0, centerCube * k), ncol = k,
                             nrow = centerCube))
    names(temp) = names(frameOut)
    DesignOut$centerCube = temp
  }
  numRows = nrow(DesignOut$cube) + nrow(DesignOut$centerCube) + nrow(DesignOut$star) +
    nrow(DesignOut$centerStar)

  DesignOut$response = data.frame(y = rep(NA, numRows))

  standardOrder = data.frame(matrix(data = 1:numRows, nrow = numRows,
                                    ncol = 1))
  names(standardOrder) = "StandOrder"
  DesignOut$standardOrder <-  standardOrder

  set.seed(random.seed)
  runOrder = as.data.frame(standardOrder[sample(1:numRows),])

  names(runOrder) = "RunOrder"
  DesignOut$runOrder <- runOrder

  temp = try(blocking(DesignOut, blocks = blocks, random.seed = random.seed))
  if (inherits(temp, "try-error"))
    stop("Blocking not possible!")
  return(blocking(DesignOut, blocks = blocks, random.seed = random.seed))
}

# Función facDesign ----
facDesign <- function (k = 3, p = 0, replicates = 1, blocks = 1, centerCube = 0, random.seed = 1234)
{
  #' @title facDesign
  #' @description Generates a 2^k full factorial design.
  #' @param k Numeric value giving the number of factors. By default k is set to `3`.
  #' @param p Numeric integer between `0` and `7`. p is giving the number of additional factors in the response surface design by aliasing effects.
  #' For further information see fracDesign and fracChoose.
  #' By default p is set to `0`.
  #' @param replicates Numeric value giving the number of \code{replicates} per factor combination. By default replicates is set to `1`.
  #' @param blocks Numeric value giving the number of blocks. By default blocks is set to `1`. Blocking is only performed for k greater 2.
  #' @param centerCube Numeric value giving the number of centerpoints within the 2^k design. By default \code{centerCube} is set to `0`.
  #' @param random.seed Numeric value for setting the random seed for reproducibility.
  #' @return The function \code{facDesign} returns an object of class \code{\link{facDesign.c}}.
  #' @seealso \code{\link{fracDesign}}, \code{\link{fracChoose}}, \code{\link{rsmDesign}}, \code{\link{pbDesign}}, \code{\link{taguchiDesign}}
  #' @examples
  #' # Example 1
  #' vp.full <- facDesign(k = 3)
  #' vp.full$.response(rnorm(2^3))
  #' vp.full$summary()
  #'
  #' # Example 2
  #' vp.rep <- facDesign(k = 2, replicates = 3, centerCube = 4)
  #' vp.rep$names(c("Name 1", "Name 2"))
  #' vp.rep$unit(c("min", "F"))
  #' vp.rep$lows(c(20, 40, 60))
  #' vp.rep$highs(c(40, 60, 80))
  #' vp.rep$summary()
  #'
  #' # Example 3
  #' dfac <- facDesign(k = 3, centerCube = 4)
  #' dfac$names(c('Factor 1', 'Factor 2', 'Factor 3'))
  #' dfac$names()
  #' dfac$lows(c(80, 120, 1))
  #' dfac$lows()
  #' dfac$highs(c(120, 140, 2))
  #' dfac$highs()
  #' dfac$summary()
  frameOut = fracDesign(k = k, p = p, gen = NULL, replicates = replicates,
                        blocks = blocks, centerCube = centerCube, random.seed = random.seed)
  return(frameOut)
}






# simProc ----
simProc <- function(x1, x2, x3, noise = TRUE) {
  #' @title simProc: Simulated Process
  #' @description This is a function to simulate a black box process for teaching the use of designed experiments.
  #' The optimal factor settings can be found using a sequential assembly strategy i.e. apply a 2^k factorial design first,
  #' calculate the path of the steepest ascent, again apply a 2^k factorial design and augment a star portion
  #' to find the optimal factor settings. Of course, other strategies are possible.
  #' @param x1 numeric vector containing the values for factor 1.
  #' @param x2 numeric vector containing the values for factor 2.
  #' @param x3 numeric vector containing the values for factor 3.
  #' @param noise logical value deciding whether noise should be added or not. Default setting is \code{TRUE}.
  #' @return \code{simProc} returns a numeric value within the range [0,1].
  #' @examples
  #' simProc(120, 140, 1)
  #' simProc(120, 220, 1)
  #' simProc(160, 140, 1)
  max_z = 0.0002200907
  min_z = 8.358082e-10
  yield = .norm2d(x1 = x1, x2 = x2)
  yield = yield - min_z
  yield = (yield/max_z) * 0.9
  if (noise)
    yield = yield + rnorm(length(yield), mean = 0, sd = 0.007)
  return(yield)
}




# InteractionPlot ----
interactionPlot <- function(dfac, response = NULL, fun = mean, main, col = 1:2) {
  #' @title interactionPlot
  #' @description Creates an interaction plot for the factors in a factorial design to visualize the interaction effects between them.
  #' @param dfac An object of class \code{\link{facDesign.c}}, representing a factorial design.
  #' @param response Response variable. If the response data frame of fdo consists of more then one responses, this variable can be used to choose just one column of the response data frame.
  #' \code{response} Needs to be an object of class character with length of `1`. It needs to be the same character as the name of the response in the response data frame that should be plotted.
  #' @param fun Function to use for the calculation of the interactions (e.g., \code{mean}, \code{median}). Default is \code{mean}.
  #' @param main Character string: title of the plot.
  #' @param col Vector of colors for the plot. Single colors can be given as character strings or numeric values. Default is \code{1:2}.
  #' @details \code{interactionPlot()} displays interactions for an object of class \code{facDesign} (i.e. 2^k full or 2^k-p fractional factorial design).
  #' Parts of the original interactionPlot were integrated.
  #' @return Return an interaction plot for the factors in a factorial design.
  #' @seealso \code{\link{fracDesign}}, \code{\link{facDesign}}
  #' @examples
  #' # Example 1
  #' # Create the facDesign object
  #' dfac <- facDesign(k = 3, centerCube = 4)
  #' dfac$names(c('Factor 1', 'Factor 2', 'Factor 3'))
  #'
  #' # Assign performance to the factorial design
  #' rend <- c(simProc(120,140,1), simProc(80,140,1), simProc(120,140,2),
  #'           simProc(120,120,1), simProc(90,130,1.5), simProc(90,130,1.5),
  #'           simProc(80,120,2), simProc(90,130,1.5), simProc(90,130,1.5),
  #'           simProc(120,120,2), simProc(80,140,2), simProc(80,120,1))
  #' dfac$.response(rend)
  #'
  #' # Create an interaction plot
  #' interactionPlot(dfac, fun = mean, col = c("purple", "red"))
  #'
  #' # Example 2
  #' vp <- fracDesign(k=3, replicates = 2)
  #' y <- 4*vp$get(j=1) -7*vp$get(j=2) + 2*vp$get(j=2)*vp$get(j=1) +
  #'      0.2*vp$get(j=3) + rnorm(16)
  #' vp$.response(y)
  #'
  #' interactionPlot(vp)

  fdo = dfac$clone()
  if (missing(main)){
    mainmiss = TRUE
  }
  else {
    mainmiss = FALSE
  }
  if (missing(fdo) || class(fdo)[1] != "facDesign.c")
    stop("fdo needs to be an object of class facDesign")

  fdoName = deparse(substitute(dfac))
  if (!is.null(response)) {
    temp = fdo$.response()[response]
    fdo$.response(temp)
  }

  x <- fdo$cube
  runIndex <- order(fdo$runOrder[, 1])
  y <- fdo$.response()[1:nrow(x), ]
  numFac <- ncol(x)
  combMat <- combn(names(x), 2)

  plot_list <- list()

  for (r in 1:ncol(fdo$.response())) {
    y = fdo$.response()[1:nrow(x), r]

    for (i in 1:ncol(combMat)) {
      facName1 <- combMat[1, i]
      facName2 <- combMat[2, i]

      df = data.frame(fac1 = x[[facName1]], fac2 = x[[facName2]], response = y)

      p = ggplot(df, aes_string(x = "fac2", y = "response", color = "as.factor(fac1)")) +
        geom_line(stat = "summary", fun = fun,size=1.5) +
        labs(x = " ", y = " ", color = facName1) +
        scale_color_manual(values = c(col[1], col[2])) +
        theme_minimal()

      plot_list[[paste(facName1, facName2)]] = p
    }

    if (mainmiss) {
      main = paste("Interaction plot for", names(fdo$.response())[r], "in", fdoName)
    }

    plot_matrix <- vector("list", numFac * numFac)
    plot_idx <- 1

    for (j in 1:numFac) {
      for (i in 1:numFac) {
        if (i == j) {
          facName <- names(x)[i]
          p_diag <- ggplot() +
            labs(x = facName, y = "") +
            theme_void() +
            theme(
              plot.title = element_text(size = 5, face = "bold", hjust = 0.5),
              axis.title.x = element_text(size = 20, face = "bold", margin = margin(0, 0, 10, 0)),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()
            )
          plot_matrix[[plot_idx]] <- p_diag
        } else if (i < j) {
          plot_matrix[[plot_idx]] <- plot_list[[paste(names(x)[i], names(x)[j])]]
        } else {
          plot_matrix[[plot_idx]] <- ggplot() + theme_void()  # Empty plot
        }
        plot_idx <- plot_idx + 1
      }
    }

    plot_matrix <- matrix(plot_matrix, ncol = numFac, byrow = TRUE)
    final_plot <- wrap_plots(plot_matrix, ncol = numFac)
    final_plot <- final_plot + plot_annotation(
      title = main,
      theme = theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 20)))
    )
    print(final_plot)
  }

  invisible()
}

# paretoPlot ----
paretoPlot <- function(dfac, abs = TRUE, decreasing = TRUE, alpha = 0.05,
                       response = NULL, ylim, xlab, ylab, main, p.col, legend_left = TRUE) {
  #' @title paretoPlot
  #' @description Display standardized effects and interactions of a \code{\link{facDesign.c}} object in a pareto plot.
  #' @param dfac An object of class facDesign.
  #' @param abs Logical. If \code{TRUE}, absolute effects and interactions are displayed. Default is \code{TRUE}.
  #' @param decreasing Logical. If \code{TRUE}, effects and interactions are sorted decreasing. Default is \code{TRUE}.
  #' @param alpha The significance level used to calculate the critical value
  #' @param response Response variable. If the response data frame of fdo consists of more then one responses, this variable can be used to choose just one column of the response data frame. \code{response} needs to be an object of class character with length of `1`.
  #' It needs to be the same character as the name of the response in the response data frame that should be plotted. By default \code{response} is set to \code{NULL}.
  #' @param ylim Numeric vector of length 2: limits for the y-axis. If missing, the limits are set automatically.
  #' @param xlab Character string: label for the x-axis.
  #' @param ylab Character string: label for the y-axis.
  #' @param main Character string: title of the plot.
  #' @param p.col Character string specifying the color palette to use for the plot. Must be one of the following values from the \code{RColorBrewer} package:
  #' \itemize{
  #'   \item{\code{`Set1`}}
  #'   \item{\code{`Set2`}}
  #'   \item{\code{`Set3`}}
  #'   \item{\code{`Pastel1`}}
  #'   \item{\code{`Pastel2`}}
  #'   \item{\code{`Paired`}}
  #'   \item{\code{`Dark2`}}
  #'   \item{\code{`Accent`}}
  #' }
  #' @param legend_left Logical value indicating whether to place the legend on the left side of the plot. Default is \code{TRUE}.
  #' @details \code{paretoPlot} displays a pareto plot of effects and interactions for an object of class facDesign (i.e. 2^k full or 2^k-p fractional factorial design). For a given significance level alpha, a critical value is calculated and added to the plot. Standardization is achieved by dividing estimates with their standard error. For unreplicated fractional factorial designs a Lenth Plot is generated.
  #' @return The function \code{paretoPlot} returns an invisible list containing:
  #' \item{effects}{a list of effects for each response in the \code{facDesign.c} object}
  #' \item{plot}{The generated PP plot.}
  #' @seealso \code{\link{fracDesign}}, \code{\link{facDesign}}
  #' @examples
  #' # Create the facDesign object
  #' dfac <- facDesign(k = 3, centerCube = 4)
  #' dfac$names(c('Factor 1', 'Factor 2', 'Factor 3'))
  #'
  #' # Assign performance to the factorial design
  #' rend <- c(simProc(120,140,1), simProc(80,140,1), simProc(120,140,2),
  #'           simProc(120,120,1), simProc(90,130,1.5), simProc(90,130,1.5),
  #'           simProc(80,120,2), simProc(90,130,1.5), simProc(90,130,1.5),
  #'           simProc(120,120,2), simProc(80,140,2), simProc(80,120,1))
  #' dfac$.response(rend)
  #'
  #' paretoPlot(dfac)
  #' paretoPlot(dfac, decreasing = TRUE, abs = FALSE, p.col = "Pastel1")



  fdo=dfac$clone()
  if(length(fdo$.response())>1 & !is.character(response)){
    stop("If your design have more than one response you should select the response writting the name of the response")
  }

  if(!is.null(response))
  {
    temp=fdo$.response()[response]
    fdo$.response(temp)
  }
  ylimMissing = FALSE
  if (missing(ylim)){
    ylimMissing = TRUE
  }
  if (missing(xlab))
    xlab = ""
  location = "topright"
  if (decreasing == F | abs == F | legend_left == T) {
    location = "topleft"
  }
  xVals = numeric(0)
  sig.neg = NULL
  sig.pos = NULL
  effect.list = vector("list")
  for (j in 1:ncol(fdo$.response())) {
    if (!any(is.na(fdo$.response()[, j]))) {
      if (missing(ylab))
        ylabel = names(fdo$.response())[j]                                ###
      else
        ylabel = ylab                                                   ###
      form = paste("fdo$.response()[,", j, "]~")
      for (i in 1:ncol(fdo$cube)) {
        form = paste(form, names(fdo$cube)[i], sep = "")
        if (i < ncol(fdo$cube))
          form = paste(form, "*", sep = "")
      }
      lm.1 = lm(as.formula(form), data = fdo$as.data.frame())
      coefs = coef(lm.1)[-pmatch("(Intercept)", names(coef(lm.1)))]
      df.resid = df.residual(lm.1)
      num.c = nrow(fdo$centerCube)

      if (df.resid == 0) {
        effect = 2 * coefs
        effect = effect[!is.na(effect)]
        effect.list[[j]] = effect
        if (missing(main))
          main = "Lenth Plot of effects"
        limits = TRUE
        faclab = NULL
        m = length(effect)
        d = m/3
        s0 = 1.5 * median(abs(effect))
        rmedian = effect[abs(effect) < 2.5 * s0]
        PSE = 1.5 * median(abs(rmedian))
        ME = qt(1 - alpha/2, d) * PSE
        Gamma = (1 + (1 - alpha)^(1/m))/2
        SME = qt(Gamma, d) * PSE
        n = length(effect)

        if(missing(p.col)){
          p.col = brewer.pal(length(effect), "Pastel1")
        }
        else{p.col = brewer.pal(length(effect), p.col)} #paste0("Set", p.col))

        if (ylimMissing)
          if (abs)
            ylim = (range(c(0, abs(effect), 1.3 * ME))) * 1.1
        else ylim = (range(c(effect, -1.3 * ME, 1.3 * ME))) * 1.1
        if (abs) {
          effect = effect[order(abs(effect), na.last = TRUE, decreasing = decreasing)]
          effect = round(effect, 3)
          if (missing(ylabel))
            ylabel = ""

          p <- ggplot(data.frame(names = factor(names(effect), levels = names(effect)), effect_ = abs(as.vector(effect))),
                      aes(x = names, y = effect_, fill = names)) +
            geom_bar(stat = "identity", color = "black") +
            scale_fill_manual(values=c(p.col)) +
            theme_minimal()+
            geom_text(aes(label = round(effect,2)), vjust = -1, colour = "black") + # etiquetas sobre las barras
            labs(title = main, x = xlab, y = ylabel) + ylim(c(ylim)) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1),
                  plot.title = element_text(hjust = 0.5),
                  legend.position = "none")

          if(ME >= ylim[1] & ME <= ylim[2] & SME >= ylim[1] & SME <= ylim[2]){
            p <- p +
              geom_hline(yintercept = ME, linetype = "dashed", color = "red") +
              geom_hline(yintercept = SME, linetype = "dashed", color = "red") +
              scale_y_continuous(limits = ylim, expand = c(0, 0),sec.axis = sec_axis(~ ., breaks = c(abs(ME), abs(SME)),
                                                                                     labels = c(paste("ME = ", round(abs(ME), 2)), paste("SME = ", round(abs(SME), 2)))
              ))

          }
          else{
            if(SME >= ylim[1] & SME <= ylim[2]){
              p <- p + geom_hline(yintercept = SME, linetype = "dashed", color = "red") +
                scale_y_continuous(limits = ylim, expand = c(0, 0),sec.axis = sec_axis(~ ., breaks = c(abs(SME)), labels = c(paste("SME = ", round(abs(SME), 2)))))
            }
            if(ME >= ylim[1] & ME <= ylim[2]){
              p <- p + geom_hline(yintercept = ME, linetype = "dashed", color = "red") +
                scale_y_continuous(limits = ylim, expand = c(0, 0),sec.axis = sec_axis(~ ., breaks = c(abs(ME)), labels = c(paste("ME = ", round(abs(ME), 2)))))
            }
          }

        }
        else {
          effect = effect[order((effect), na.last = TRUE, decreasing = decreasing)]
          effect = round(effect, 3)
          if (missing(ylabel))
            ylabel = ""
          p <- ggplot(data.frame(names = names(effect), effect_ = abs(as.vector(effect))),
                      aes(x = names, y = effect_, fill = names)) +
            geom_bar(stat = "identity", color = "black") +
            scale_fill_manual(values=c(p.col)) +
            theme_minimal()+
            geom_text(aes(label = round(effect,2)), vjust = -1, colour = "black") + # etiquetas sobre las barras
            labs(title = main, x = xlab, y = ylabel) + ylim(c(ylim)) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1),
                  plot.title = element_text(hjust = 0.5),
                  legend.position = "none")

          if(ME >= ylim[1] & ME <= ylim[2] & SME >= ylim[1] & SME <= ylim[2]){
            p <- p +
              geom_hline(yintercept = ME, linetype = "dashed", color = "red") +
              geom_hline(yintercept = SME, linetype = "dashed", color = "red")
            scale_y_continuous(limits = ylim, expand = c(0, 0),sec.axis = sec_axis(~ ., breaks = c(ME, SME), labels = c(paste("ME = ", round(abs(ME), 2)), paste("SME = ", round(abs(SME), 2)) )))

          }
          else{
            if(ME >= ylim[1] & ME <= ylim[2]){
              p <- p + geom_hline(yintercept = ME, linetype = "dashed", color = "red") +
                scale_y_continuous(limits = ylim, expand = c(0, 0),sec.axis = sec_axis(~ ., breaks = c(ME), labels = c(paste("ME = ", round(ME, 2)))))

            }
            if(SME >= ylim[1] & SME <= ylim[2]){
              p <- p + geom_hline(yintercept = SME, linetype = "dashed", color = "red") +
                scale_y_continuous(limits = ylim, expand = c(0, 0),sec.axis = sec_axis(~ ., breaks = c(SME), labels = c(paste("SME = ", round(SME, 2)))))
            }
          }



        }
      }

      else {
        if (missing(main))
          main = "Standardized main effects and interactions"
        effect = ((summary(lm.1)$coefficients[-pmatch("(Intercept)", names(coef(lm.1))), 1])/(summary(lm.1)$coefficients[-pmatch("(Intercept)", names(coef(lm.1))),2]))
        if (all(is.na(effect)))
          stop("effects could not be calculated")
        effect = effect[!is.na(effect)]
        effect.list[[j]] = effect
        if ((df.resid) > 0) {
          sig.pos = -qt(alpha/2, df.resid)
          sig.neg = +qt(alpha/2, df.resid)
        }
        # Ylimits ----

        if (ylimMissing)
          if (abs) {
            tempVec = c(effect, sig.pos)
            tempVec = tempVec[!is.na(tempVec)]
            ylim = c(0, 0.3 + max(tempVec))
          }
        else {
          tempVec1 = c(0, effect, sig.neg, sig.pos)
          tempVec1 = tempVec1[!is.na(tempVec1)]
          tempVec2 = c(abs(effect), sig.pos, sig.neg)
          tempVec2 = tempVec2[!is.na(tempVec2)]
          ylim = c(min(tempVec1)-0.3, max(tempVec2)+0.3)
        }

        if(missing(p.col)){
          p.col = brewer.pal(length(effect), "Pastel1")
        }
        else{p.col = brewer.pal(length(effect), p.col)} #paste0("Set", p.col))
        # Plot ---------
        if (abs) {
          effect = effect[order(abs(effect), na.last = TRUE, decreasing = decreasing)]
          effect = round(effect, 3)

          if (missing(ylabel))
            ylabel = ""

          # plot with abs
          df <- data.frame(Names = factor(names(effect), levels = names(effect)),
                           effect_ = abs(as.vector(effect)))

          p <- ggplot(data = df,
                      aes(x = Names, y = effect_, fill = Names)) +
            geom_bar(stat = "identity", color = "black") +
            scale_fill_manual(values=c(p.col)) +
            theme_minimal()+
            labs(title = main, x = xlab, y = ylabel) + ylim(c(ylim)) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1),
                  plot.title = element_text(hjust = 0.5),
                  legend.position = "none") +
            geom_text(aes(label = round(effect,2)), vjust = -1, colour = "black") + # etiquetas sobre las barras
            geom_hline(yintercept = sig.pos, linetype = "dashed", color = "red") +
            annotate("text", x = max(as.numeric(df$Names)), y = sig.pos, label = paste(round(sig.pos, 2)), vjust = -0.5, color = "red")
        }
        else {
          effect = effect[order((effect), na.last = TRUE, decreasing = decreasing)]
          effect = round(effect, 3)

          if (missing(ylabel))
            ylabel = ""

          df <- data.frame(Names = factor(names(effect), levels = names(effect)),
                           effect_ = as.vector(effect))

          # Plot without abs
          p <- ggplot(df,
                      aes(x = Names, y = effect_, fill = Names)) +
            geom_bar(stat = "identity", color = "black") +
            scale_fill_manual(values=c(p.col)) +
            theme_minimal()+
            labs(title = main, x = xlab, y = ylabel) + ylim(c(ylim)) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0),legend.position = "none") +
            geom_text(aes(label = effect), vjust = ifelse(df$effect_ > 0, -0.5, 1.5) , colour = "black") + # etiquetas sobre las barras
            geom_hline(yintercept = sig.pos, linetype = "dashed", color = "red") +
            geom_hline(yintercept = sig.neg, linetype = "dashed", color = "red") +
            annotate("text", x = max(as.numeric(df$Names)), y = sig.pos, label = paste(round(sig.pos, 2)), vjust = -0.5, color = "red") +
            annotate("text", x = max(as.numeric(df$Names)), y = sig.neg, label = paste(round(sig.neg, 2)), vjust = 1.5, color = "red")
        }
        myDelta = diff(range(ylim)) * 0.02
      }
      # Legend ----
      titles <- data.frame(Name_title = paste0(names(fdo$cube),": ",fdo$names()))
      for(i in 1:dim(titles)[1]){
        titles$Pos_title[i] <- 0.95 - (0.05 * i)
      }
      caja <- ggplot(data.frame(x = 0,y = 0), aes(x = x, y = y)) +
        theme_bw() +
        theme(
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, vjust = -0.5,margin = margin(b = -12), size = 10)
        ) + xlim(c(0.24, 0.26)) + ylim(c(min(titles$Pos_title) - 0.05, max(titles$Pos_title) + 0.05))
      for(i in 1:dim(titles)[1]){
        caja <- caja + annotate("text", x = 0.25, y = titles$Pos_title[i], label = titles$Name_title[i], size = 3.5, hjust = 0.5)
      }
      # insert legend -----
      if(location == "topright"){
        p <- p + inset_element(caja, left = 0.75, right = 1, top = 1,  bottom = 0.80)
      }
      else{
        p <- p + inset_element(caja, left = 0.25, right = 0.05, top = 1,  bottom = 0.80)
      }

    }
  }

  print(p)
  invisible(list(effects = effect.list, plot = p))
}
# normalPlot ----
normalPlot <- function(dfac, response = NULL, main, ylim, xlim, xlab, ylab, pch,
                       col, border = "red"){
  #' @title normalPlot: Normal plot
  #' @description Creates a normal probability plot for the effects in a \code{\link{facDesign.c}} object.
  #' @param dfac An object of class \code{\link{facDesign.c}}.
  #' @param response Response variable. If the response data frame of fdo consists of more then one responses, this variable can be used to choose just one column of the \code{response} data frame. response needs to be an object of class character with length of `1`. It needs to be the same character as the name of the response in the response data frame that should be plotted.
  #' By default \code{respons}` is set to \code{NULL}.
  #' @param main Character string specifying the main title of the plot.
  #' @param ylim Graphical parameter. The y limits of the plot.
  #' @param xlim Graphical parameter. The x limits (x1, x2) of the plot. Note that x1 > x2 is allowed and leads to a `reversed axis`.
  #' @param xlab Character string specifying the label for the x-axis.
  #' @param ylab Character string specifying the label for the y-axis.
  #' @param pch Graphical parameter. Vector containing numerical values or single characters giving plotting points for the different factors.
  #' Accepts values from 0 to 25, each corresponding to a specific shape in \code{ggplot2} (e.g., 0: square, 1: circle, 2: triangle point up, 3: plus, 4: cross).
  #' @param col Graphical parameter. Single numerical value or character string giving the color for the points (e.g., 1: black, 2: red, 3: green).
  #' @param border Graphical parameter. Single numerical value or character string giving the color of the border line.
  #' @details If the given \code{facDesign.c} object \code{fdo} contains replicates this function will deliver a normal plot
  #' i.e.: effects divided by the standard deviation (t-value) will be plotted against an appropriate probability
  #' scaling (see: `ppoints`).
  #' If the given \code{facDesign.c} object \code{fdo} contains no replications the standard error can not be calculated.
  #' In that case the function will deliver an effect plot.
  #' i.e.: the effects will be plotted against an appropriate probability scaling. (see: `ppoints`).
  #' @return The function \code{normalPlot} returns an invisible list containing:
  #' \item{effects}{a list of effects for each response in the \code{facDesign.c} object.}
  #' \item{plot}{The generated normal plot.}
  #' @seealso \code{\link{facDesign}}, \code{\link{paretoPlot}}, \code{\link{interactionPlot}}
  #' @examples
  #' # Example 1: Create a normal probability plot for a full factorial design
  #' dfac <- facDesign(k = 3, centerCube = 4)
  #' dfac$names(c('Factor 1', 'Factor 2', 'Factor 3'))
  #'
  #' # Assign performance to the factorial design
  #' rend <- c(simProc(120,140,1), simProc(80,140,1), simProc(120,140,2),
  #'           simProc(120,120,1), simProc(90,130,1.5), simProc(90,130,1.5),
  #'           simProc(80,120,2), simProc(90,130,1.5), simProc(90,130,1.5),
  #'           simProc(120,120,2), simProc(80,140,2), simProc(80,120,1))
  #' dfac$.response(rend)
  #'
  #' normalPlot(dfac)
  #'
  #' # Example 2: Create a normal probability plot with custom colors and symbols
  #' normalPlot(dfac, col = "blue", pch = 4)

  fdo = dfac$clone()
  fdoName = deparse(substitute(dfac))

  if(is.null(response)==FALSE)
  {
    temp=fdo$.response()[response]
    fdo$.response(temp)
  }
  parList = list()

  XLIM=FALSE;YLIM=FALSE
  if (!(class(fdo)[1] == "facDesign.c"))
    stop(paste(deparse(substitute(fdo)), "is not an object of class facDesign"))
  mainmiss = FALSE
  if (missing(main))
    mainmiss = TRUE
  if (missing(ylim))
    YLIM=TRUE
  if (missing(xlim))
    XLIM=TRUE
  if (missing(xlab))
    xlab = "Coefficients"
  if (missing(ylab))
    ylab = "Theoretical Quantiles"
  if (missing(pch))
    pch = 19
  if (missing(col))
    col = "black"

  list_plot = list()
  for(j in 1:ncol(fdo$.response())){
    parList = list()
    params = list()
    leg.col = vector()
    p.col = vector()
    p.pch = vector()
    leg.txt = vector()
    if (missing(main)){
      main = paste("Normal plot for", names(fdo$.response())[j], "in", fdoName)
    }
    if (j > 1)
      dev.new()
    form = paste("fdo$.response()[,", j, "]~")

    for (i in 1:ncol(fdo$cube)) {
      form = paste(form, names(fdo$cube)[i], sep = "")
      if (i < ncol(fdo$cube))
        form = paste(form, "*", sep = "")
    }

    lm.1 = lm(as.formula(form), data = fdo$as.data.frame())
    lm.1s = summary(lm.1)
    effect = coef(lm.1s)[row.names(coef(lm.1s)) != "(Intercept)", "t value"]
    if (all(is.na(effect)))
      effect = 2 * coef(lm.1)[-pmatch("(Intercept)", names(coef(lm.1)))]
    #            stop("effects could not be calculated")
    sig = summary(lm.1)$coefficients[, "Pr(>|t|)"][-pmatch("(Intercept)", names(coef(lm.1)))]
    df.resid = df.residual(lm.1)
    nc = nrow(fdo$centerCube)

    tQ = ppoints(effect)
    index = order(effect)
    sQ = effect[index]
    sig = sig[index]

    if (df.resid > 0) {
      # obtenemos el caracter de la cajita del p_value
      for (k in seq(along = sig)) {
        setted = FALSE
        if (abs(sig)[k] < 0.01) {
          if (!setted) {
            leg.txt = c(leg.txt, "p < 0.01")
            leg.col = c(leg.col)
            setted = TRUE
          }
        }
        if (abs(sig)[k] < 0.05) {
          if (!setted) {
            leg.txt = c(leg.txt, "p < 0.05")
            leg.col = c(leg.col)
            setted = TRUE
          }
        }
        if (abs(sig)[k] < 0.1) {
          if (!setted) {
            leg.txt = c(leg.txt, "p < 0.1")
            leg.col = c(leg.col)
            setted = TRUE
          }
        }
        if (abs(sig)[k] >= 0.1) {
          if (!setted) {
            p.col[k] = col
            p.pch[k] = pch
            leg.txt = c(leg.txt, "p >= 0.1")
            leg.col = c(leg.col, p.col)
            setted = TRUE
          }
        }
      }
      leg.txt = unique(leg.txt)
      leg.col = unique(leg.col)
    }else{p.col=col
    p.pch=pch}

    mid = round(length(tQ)/2)
    last = length(tQ)
    params$p = ppoints(effect)
    estimates = FitDistr(effect, "normal")
    params$mean = estimates$estimate[["mean"]]
    params$sd = estimates$estimate[["sd"]]

    y = do.call(qnorm, params)

    if (XLIM)
      xlim = range(sQ)
    if (YLIM)
      ylim = range(y)

    # PLOT -----------------------
    df <- data.frame(sQ = names(sQ), value = as.numeric(sQ), y = y)

    p <- ggplot(df, aes(x = value, y = y, label = sQ)) +
      geom_point(col = p.col, pch = p.pch) +
      theme_classic() + lims(x = xlim, y = ylim) +
      labs(x = xlab, y = ylab, title = main) +
      geom_text(check_overlap = TRUE, vjust = 1) + theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))

    xp = c(qnorm(0.1), qnorm(0.99))
    yp = c(qnorm(0.1, mean = estimates$estimate[["mean"]], sd = estimates$estimate[["sd"]]), qnorm(0.99, mean = estimates$estimate[["mean"]], sd = estimates$estimate[["sd"]]))
    slope = (yp[2] - yp[1])/(xp[2] - xp[1])
    int = yp[1] - slope * xp[1]

    # line
    p <- p + geom_abline(intercept = int, slope = slope, col = border)

    # legend
    if (df.resid > 0){
      caja <- ggplot(data = data.frame(x = 0, y = 0), aes(x, y)) +
        theme_bw() +
        theme(
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        ) +
        xlim(c(0.25,0.30)) + ylim(c(0.24, 0.31))

      caja <- caja +
        annotate('text', x = 0.275, y = 0.28,
                 label = leg.txt, size = 3, hjust = 0.5, colour = "black")

      p <- p + inset_element(caja, left = 0.01, right = 0.2, top = 1, bottom = 0.85)
    }
    print(p)
    list_plot[[paste0("p",j)]] <- p
  }
  invisible(list(effects = effect, plots = list_plot))
}

# wirePlot ----
wirePlot <- function(x, y, z, data = NULL,
                     xlim, ylim, zlim, main, xlab, ylab,
                     sub, sub.a = TRUE, zlab, form = "fit",
                     col = "Rainbow", steps, fun,
                     plot = TRUE, show.scale = TRUE,
                     n.scene = "scene") {
  #' @title wirePlot: 3D Plot
  #' @description Creates a wireframe diagram for an object of class \code{\link{facDesign.c}}.
  #' @param x Name providing the Factor A for the plot.
  #' @param y Name providing the Factor B for the plot.
  #' @param z Name giving the Response variable.
  #' @param data Needs to be an object of class \code{facDesign} and contains the names of \code{x}, \code{y}, \code{z}.
  #' @param xlim Numeric vector of length 2: limits for the x-axis. If missing, limits are set automatically.
  #' @param ylim Numeric vector of length 2: limits for the y-axis. If missing, limits are set automatically.
  #' @param zlim Numeric vector of length 2: limits for the z-axis. If missing, limits are set automatically.
  #' @param main Character string: title of the plot.
  #' @param xlab Character string: label for the x-axis.
  #' @param ylab Character string: label for the y-axis.
  #' @param zlab Character string: label for the z-axis.
  #' @param sub Character string: subtitle for the plot. Default is \code{NULL}.
  #' @param sub.a Logical value indicating whether to display the subtitle. Default is \code{TRUE}.
  #' @param form Character string specifying the form of the surface to be plotted. Options include
  #' \itemize{
  #'     \item \code{`quadratic`}
  #'     \item \code{`full`}
  #'     \item \code{`interaction`}
  #'     \item \code{`linear`}
  #'     \item \code{`fit`}
  #'     }
  #' Default is \code{`fit`}.
  #' @param col Character string specifying the color palette to use for the plot (e.g., \code{`Rainbow`}, \code{`Jet`}, \code{`Earth`}, \code{`Electric`}). Default is \code{`Rainbow`}.
  #' @param steps Numeric value specifying the number of steps for the grid in the plot. Higher values result in a smoother surface.
  #' @param fun Optional function to be applied to the data before plotting.
  #' @param plot Logical value indicating whether to display the plot. Default is \code{TRUE}.
  #' @param show.scale Logical value indicating whether to display the color scale on the plot. Default is \code{TRUE}.
  #' @param n.scene Character string specifying the scene name for the plot. Default is \code{`scene`}.
  #' @details The \code{wirePlot} function is used to create a 3D wireframe plot that visualizes the relationship between two factors and a response variable. The plot can be customized in various ways, including changing axis labels, adding subtitles, and choosing the color palette.
  #' @return The function \code{wirePlot} returns an invisible list containing:
  #' \item{plot}{The generated wireframe plot.}
  #' \item{grid}{The grid data used for plotting.}
  #' @seealso \code{\link{contourPlot}}, \code{\link{paretoChart}}.
  #' @examples
  #' # Example 1: Basic wireframe plot
  #' x <- seq(-10, 10, length = 30)
  #' y <- seq(-10, 10, length = 30)
  #' z <- outer(x, y, function(a, b) sin(sqrt(a^2 + b^2)))
  #' wirePlot(x, y, z, main = "3D Wireframe Plot", xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis")
  #'
  #' fdo = rsmDesign(k = 3, blocks = 2)
  #' fdo$.response(data.frame(y = rnorm(fdo$nrow())))
  #'
  #' #I - display linear fit
  #' wirePlot(A,B,y, data = fdo, form = "linear")
  #'
  #' #II - display full fit (i.e. effect, interactions and quadratic effects
  #' wirePlot(A,B,y, data = fdo, form = "full")
  #'
  #' #III - display a fit specified before
  #' fdo$set.fits(fdo$lm(y ~ B + I(A^2)))
  #' wirePlot(A,B,y, data = fdo, form = "fit")
  #'
  #' #IV - display a fit given directly
  #' wirePlot(A,B,y, data = fdo, form = "y ~ A*B + I(A^2)")
  #'
  #' #V - display a fit using a different colorRamp
  #' wirePlot(A,B,y, data = fdo, form = "full", col = 2)
  #'


  form = form
  fact = NULL
  if (missing(steps))
    steps = 25
  fdo = data
  fit = NULL
  lm.1 = NULL

  if (is.null(data) | class(data)[1] != "facDesign.c") {
    if(length(x) == length(y)){
      if(dim(z)[1] == length(x) & dim(z)[2] == length(x)){
        x.c = deparse(substitute(x))
        y.c = deparse(substitute(y))
        z.c = deparse(substitute(z))

        if (missing(main))
          main = paste("Response Surface for", z.c)

        if (missing(ylab))
          ylab = y.c
        if (missing(xlab))
          xlab = x.c
        if (missing(zlab))
          zlab = z.c

        if (missing(xlim))
          xlim = c(min(x), max(x))
        if (missing(ylim))
          ylim = c(min(y), max(y))

        if (missing(zlim))
          zlim = range(z)

        p <- plot_ly(x = -y, y = x, z = z, colorscale=col, scene = n.scene) %>%
          add_surface(showscale = show.scale) %>%
          layout(
            title = main,
            scene = list(
              xaxis = list(range = ylim, title = ylab, zeroline = FALSE),
              yaxis = list(range = xlim, title = xlab, zeroline = FALSE),
              zaxis = list(range = zlim, title = zlab, zeroline = FALSE),
              camera = list(eye = list(x=2, y=2, z=0.1))
            ),
            margin = list(l = 10, r = 15, t = 30, b = 20)
          )
        if(!missing(sub)){
          p <- p %>%
            layout(
              annotations = list(
                list(
                  text = sub,
                  x = 0.5,
                  y = -0.1,
                  showarrow = FALSE,
                  font = list(size = 12)
                )
              )
            )
        }
        if (plot) {
          show(p)
        }
        invisible(list(x = x, y = y, z = z, plot = p))

      }
    }
  }
  else{
    x.c = deparse(substitute(x))
    y.c = deparse(substitute(y))
    z.c = deparse(substitute(z))

    if (missing(main))
      main = paste("Response Surface for", z.c)

    aux <- list()
    for (i in 1:length(fdo$names())) {
      aux[[.NAMES[i]]] <-fdo$names()[i]
    }
    if (missing(ylab))
      ylab = paste(y.c, ": ", aux[[y.c]])
    if (missing(xlab))
      xlab = paste(x.c, ": ", aux[[x.c]])
    if (missing(zlab))
      zlab = paste(x.c, ": ", z.c)

    if (missing(xlim))
      xlim = c(min(fdo$get(, x.c)), max(fdo$get(, x.c)))
    if (missing(ylim))
      ylim = c(min(fdo$get(, y.c)), max(fdo$get(, y.c)))

    allVars = c(fdo$names(), names(fdo$.response()))
    isct = intersect(c(aux[[x.c]], aux[[y.c]], z.c), c(fdo$names(), names(fdo$.response())))

    if (length(isct) < length(c(x.c, y.c, z.c))) {
      d = setdiff(isct, allVars)
      stop(paste(d, "could not be found\n"))
    }

    if (missing(fun))
      fun = NULL
    if (!is.function(fun) & !is.null(fun))
      if (!(fun %in% c("overall", "desirability")))
        stop("fun should be a function, \"overall\" or \"desirability\"")
    if (identical(fun, "desirability")) {
      obj = fdo$desires()[[z.c]]
      fun = .desireFun(obj$low, obj$high, obj$target, obj$scale, obj$importance)
    }

    if (form %in% c("fit")) {
      lm.1 = fdo$fits[[z.c]]
      if (is.null(fit))
        form = "full"
    }

    if (form %in% c("quadratic", "full", "interaction", "linear")) {
      if (identical(form, "full")) {
        form = paste(z.c, "~", x.c, "+", y.c, "+", x.c, ":", y.c)
        if (nrow(fdo$star) > 0)
          form = paste(form, "+ I(", x.c, "^2) + I(", y.c, "^2)")
      }
      if (identical(form, "interaction")) {
        form = paste(z.c, "~", x.c, "+", y.c, "+", x.c, ":", y.c)
      }
      if (identical(form, "linear")) {
        form = paste(z.c, "~", x.c, "+", y.c)
      }
      if (identical(form, "quadratic")) {
        form = paste(z.c, "~I(", x.c, "^2) + I(", y.c, "^2)")
      }
    }

    if (is.null(form))
      stop(paste("invalid formula", form))
    if (is.null(lm.1))
      lm.1 = fdo$lm(form)
    if (missing(sub))
      sub = deparse(formula(lm.1))

    dcList = vector(mode = "list", length = length(fdo$names()))
    names(dcList) = names(aux)
    dcList[1:length(fdo$names())] = 0

    help.predict = function(x, y, x.c, y.c, lm.1) {
      dcList[[x.c]] = x
      dcList[[y.c]] = y
      temp = do.call(data.frame, dcList)
      invisible(predict(lm.1, temp))
    }

    xVec = seq(min(xlim), max(xlim), length = steps)
    yVec = seq(min(ylim), max(ylim), length = steps)

    mat = outer(xVec, yVec, help.predict, x.c, y.c, lm.1)

    if (is.function(fun))
      mat = try(apply(mat, c(1, 2), fun))
    if (identical(fun, "overall")) {
      main = "composed desirability"
      mat = matrix(1, nrow = nrow(mat), ncol = ncol(mat))
      for (i in names(fdo$.response())) {
        obj = fdo$desires()[[i]]
        fun = .desireFun(obj$low, obj$high, obj$target, obj$scale, obj$importance)
        temp = outer(xVec, yVec, help.predict, x.c, y.c, fits(fdo)[[i]])
        temp = try(apply(temp, c(1, 2), fun))
        mat = mat * temp
      }
      mat = mat^(1/length(names(fdo$response())))
    }

    if (missing(zlim))
      zlim = range(mat)
    if(sub.a){sub = sub}
    else{sub = ""}

    p <- plot_ly(x = -yVec, y = xVec, z = mat, colorscale=col, scene = n.scene) %>%
      add_surface(showscale = show.scale) %>%
      layout(
        title = list(text=paste0(main,
                                 "<br>",
                                 sub)),
        scene = list(
          xaxis = list(range = ylim, title = ylab, zeroline = FALSE),
          yaxis = list(range = xlim, title = xlab, zeroline = FALSE),
          zaxis = list(range = zlim, title = zlab, zeroline = FALSE),
          camera = list(eye = list(x=2, y=2, z=0.1))
        ),
        margin = list(l = 10, r = 15, t = 30, b = 20)

      )
    if(sub.a){
      p <- p %>%
        layout(

        )
    }

    if (plot) {
      show(p)
    }
    invisible(list(x = xVec, y = yVec, z = mat, plot = p))
  }
}


# contourPlot ----
contourPlot <- function(x, y, z, data = NULL, xlim, ylim, main, xlab, ylab, form = "fit", col = 1, steps,
                        fun, plot = TRUE, show.scale = TRUE) {
  #' @title contourPlot: Contour Plot
  #' @description Creates a contour diagram for an object of class \code{\link{facDesign.c}}.
  #' @param x Name providing the Factor A for the plot.
  #' @param y Name providing the Factor B for the plot.
  #' @param z Name giving the Response variable.
  #' @param data Needs to be an object of class \code{\link{facDesign.c}} and contains the names of x, y, z.
  #' @param xlim Vector giving the range of the x-axis.
  #' @param ylim Vector giving the range of the y-axis.
  #' @param main Character string: title of the plot.
  #' @param xlab Character string: label for the x-axis.
  #' @param ylab Character string: label for the y-axis.
  #' @param form A character string or a formula with the syntax `y~ x+y + x*y`. If form is a character it has to be one out of the following:
  #' \itemize{
  #'    \item \code{`quadratic`}
  #'    \item \code{`full`}
  #'    \item \code{`interaction`}
  #'    \item \code{`linear`}
  #'    \item \code{`fit`}
  #' }
  #' \code{`fit`} takes the formula from the fit in the \code{facDesign.c} object \code{fdo}. Quadratic or higher orders should be given as I(Variable^2).
  #' By default \code{form} is set as \code{`fit`}.
  #' @param col A predefined (1, 2, 3 or 4) or self defined colorRampPalette or color to be used (i.e. \code{`red`}).
  #' @param steps Number of grid points per factor. By default \code{steps} = 25.
  #' @param fun Function to be applied to z \code{desirability}.
  #' @param plot Logical value indicating whether to display the plot. Default is \code{TRUE}.
  #' @param show.scale Logical value indicating whether to display the color scale on the plot. Default is \code{TRUE}.
  #' @return The function \code{contourPlot} returns an invisible list containing:
  #' \itemize{
  #'  \item x - locations of grid lines for x at which the values in z are measured.
  #'  \item y - locations of grid lines for y at which the values in z are measured.
  #'  \item z - a matrix containing the values of z to be plotted.
  #'  \item plot - The generated plot.
  #' }
  #' @seealso \code{\link{wirePlot}}, \code{\link{paretoChart}}
  #' @examples
  #' fdo = rsmDesign(k = 3, blocks = 2)
  #' fdo$.response(data.frame(y = rnorm(fdo$nrow())))
  #'
  #' #I - display linear fit
  #' contourPlot(A,B,y, data = fdo, form = "linear")
  #' #II - display full fit (i.e. effect, interactions and quadratic effects
  #' contourPlot(A,B,y, data = fdo, form = "full")
  #' #III - display a fit specified before
  #' fdo$set.fits(fdo$lm(y ~ B + I(A^2)))
  #' contourPlot(A,B,y, data = fdo, form = "fit")
  #' #IV - display a fit given directly
  #' contourPlot(A,B,y, data = fdo, form = "y ~ A*B + I(A^2)")
  #' #V - display a fit using a different colorRamp
  #' contourPlot(A,B,y, data = fdo, form = "full", col = 2)
  #' #VI - display a fit using a self defined colorRamp
  #' myColour = colorRampPalette(c("green", "gray","blue"))
  #' contourPlot(A,B,y, data = fdo, form = "full", col = myColour)


  fact = NULL
  if (missing(steps))
    steps = 25
  fdo = data
  fit = NULL
  lm.1 = NULL
  if (!is.function(col)) {
    if (identical(col, 1))
      col = colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
    if (identical(col, 2))
      col = colorRampPalette(c("blue", "white", "red"), space = "Lab")
    if (identical(col, 3))
      col = colorRampPalette(c("blue", "white", "orange"))
    if (identical(col, 4))
      col = colorRampPalette(c("gold", "white", "firebrick"))
    if (identical(col, 5))
      col = colorRampPalette(c("blue4", "lightblue1", "lightgreen", "green4"))
  }

  if (is.null(data) | class(data)[1] != "facDesign.c") {
    if(length(x) == length(y)){
      if(dim(z)[1] == length(x) & dim(z)[2] == length(x)){
        if(missing(main))
          main = "Filled Contour"
        if(missing(xlab))
          xlab = ""
        if(missing(ylab))
          ylab = ""

        if (is.function(col)) {
          mat <- z
          nrMat <- nrow(mat)
          ncMat <- ncol(mat)
          nbcol <- 1000
          color <- col(nbcol)
          matFacet <- mat[-1, -1] + mat[-1, -ncMat] + mat[-nrMat, -1] + mat[-nrMat, -ncMat]
          facetcol <- cut(matFacet, nbcol)
        }
        else {
          color = col
          facetcol = 1
        }

        p <- plot_ly(x = x, y = y, type = "contour", z = z, autocontour = TRUE, colors = color,
                     contours = list(coloring = 'heatmap'), line = list(smoothing = 0),
                     showscale = show.scale) %>%
          layout(
            title = main,
            xaxis = list(title = xlab, zeroline = FALSE),
            yaxis = list(title = ylab, zeroline = FALSE)
          )

        if (plot) {
          show(p)
        }

        invisible(list(x = x, y = y, z = z, plot = p))
      }

    }
    else{
      cat("\n defaulting to filled.contour function\n")
      return("persp")
    }
  }
  else{x.c = deparse(substitute(x))
  y.c = deparse(substitute(y))
  z.c = deparse(substitute(z))

  aux <- list()
  for (i in 1:length(fdo$names())) {
    aux[[.NAMES[i]]] <-fdo$names()[i]
  }
  if (missing(plot))
    plot = TRUE
  if (missing(main))
    main = paste("Filled Contour for", z.c)
  if (missing(ylab))
    ylab = paste(y.c, ": ", aux[[y.c]])
  if (missing(xlab))
    xlab = paste(x.c, ": ", aux[[x.c]])
  if (missing(xlim))
    xlim = c(min(fdo$get(, x.c)), max(fdo$get(, x.c)))
  if (missing(ylim))
    ylim = c(min(fdo$get(, y.c)), max(fdo$get(, y.c)))
  allVars = c(fdo$names(), names(fdo$.response()))
  isct = intersect(c(aux[[x.c]], aux[[y.c]], z.c), c(fdo$names(), names(fdo$.response())))

  if (length(isct) < length(c(x.c, y.c, z.c))) {
    d = setdiff(isct, allVars)
    stop(paste(d, "could not be found\n"))
  }
  if (missing(fun))
    fun = NULL
  if (!is.function(fun) & !is.null(fun))
    if (!(fun %in% c("overall", "desirability")))
      stop("fun should be a function, \"overall\" or \"desirability\"")
  if (identical(fun, "desirability")) {
    obj = fdo$desires()[[z.c]]
    fun = .desireFun(obj$low, obj$high, obj$target, obj$scale, obj$importance)
  }
  if (form %in% c("fit")) {
    lm.1 = fdo$fits[[z.c]]
    if (is.null(fit))
      form = "full"
  }
  if (form %in% c("quadratic", "full", "interaction", "linear")) {
    if (identical(form, "interaction")) {
      form = paste(z.c, "~", x.c, "+", y.c, "+", x.c, ":", y.c)
    }
    if (identical(form, "linear")) {
      form = paste(z.c, "~", x.c, "+", y.c)
    }
    if (identical(form, "quadratic")) {
      form = paste(z.c, "~I(", x.c, "^2) + I(", y.c, "^2)")
    }
    if (identical(form, "full")) {
      form = paste(z.c, "~", x.c, "+", y.c, "+", x.c, ":", y.c)
      if (nrow(fdo$star) > 0)
        form = paste(form, "+ I(", x.c, "^2) + I(", y.c, "^2)")
    }
  }

  if (is.null(form))
    stop(paste("invalid formula", form))
  if (is.null(lm.1))
    lm.1 = fdo$lm(form)

  dcList = vector(mode = "list", length = length(fdo$names()))
  names(dcList) = names(aux)
  dcList[1:length(fdo$names())] = 0



  help.predict = function(x, y, x.c, y.c, lm.1) {
    dcList[[x.c]] = x
    dcList[[y.c]] = y
    temp = do.call(data.frame, dcList)
    invisible(predict(lm.1, temp))
  }

  xVec = seq(min(xlim), max(xlim), length = steps)
  yVec = seq(min(ylim), max(ylim), length = steps)
  mat = outer(xVec, yVec, help.predict, x.c, y.c, lm.1)

  if (is.function(col)) {
    nrMat <- nrow(mat)
    ncMat <- ncol(mat)
    nbcol <- 1000
    color <- col(nbcol)
    matFacet <- mat[-1, -1] + mat[-1, -ncMat] + mat[-nrMat, -1] + mat[-nrMat, -ncMat]
    facetcol <- cut(matFacet, nbcol)
  }
  else {
    color = col
    facetcol = 1
  }
  if (is.function(fun))
    mat = try(apply(mat, c(1, 2), fun))
  if (identical(fun, "overall")) {
    main = "composed desirability"
    mat = matrix(1, nrow = nrow(mat), ncol = ncol(mat))
    for (i in names(fdo$.response())) {
      obj = fdo$desires()[[i]]
      fun = .desireFun(obj$low, obj$high, obj$target, obj$scale, obj$importance)
      temp = outer(xVec, yVec, help.predict, x.c, y.c, fdo$fits[[i]])
      temp = try(apply(temp, c(1, 2), fun))
      mat = mat * temp
    }
    mat = mat^(1/length(names(fdo$.response())))
  }

  p <- plot_ly(x = xVec, y = yVec, z = mat, colors = color,
               type = "contour", autocontour = TRUE, line = list(smoothing = 0), contours = list(coloring = 'heatmap'),
               showscale = show.scale) %>%
    layout(
      title = main,
      xaxis = list(range = xlim, title = xlab, zeroline = FALSE),
      yaxis = list(range = ylim, title = ylab, zeroline = FALSE)
    )

  if (plot) {
    show(p)
  }

  invisible(list(x = xVec, y = yVec, z = mat, plot = p))
  }

}


# confounds ----
confounds <- function(x, depth = 2) {
  #' @title confounds: Confounded Effects
  #' @description Function to display confounded effects of a fractional factorial design in a human readable way.
  #' @param x An object of class \code{\link{facDesign.c}}.
  #' @param depth numeric value - up to depth-way confounded interactions are printed
  #' @return The function returns a summary of the factors confounded.
  #' @examples
  #' vp.frac = fracDesign(k = 4, gen = "D=ABC")
  #' confounds(vp.frac,depth=5)

  varName = deparse(substitute(x))
  identityList = x$identity()
  x = x$cube
  if (length(identityList) < 1) {
    print(paste(varName, " contains no defining relations!"))
    invisible()
  }
  effect1 = numeric(0)
  effect2 = numeric(0)

  index = numeric(0)
  for (i in 1:(dim(x)[2])) {
    if (!(TRUE && all(unique(x[, i]) %in% c(-1, 1))))
      index = c(index, i)
  }
  if (length(index) > 0)
    x = x[, -index]

  n = dim(x)[2]
  if (n <= 1)
    stop("Factorial Design contains only one row!")
  for (j in 1:length(identityList)) {
    ident = identityList[[j]]
    for (m in 1:n) {
      combMat = combn(1:n, m)
      for (i in 1:(dim(combMat)[2])) {
        isect = intersect(ident, combMat[, i])
        conf = setdiff(ident, isect)
        conf = sort(c(conf, setdiff(combMat[, i], isect)))
        effect1 = c(effect1, paste(sort(names(x)[as.numeric((combMat[, i]))]), sep = "",
                                   collapse = ""))
        effect2 = c(effect2, paste(sort(names(x)[conf]), sep = "", collapse = ""))

      }
    }
  }

  if (length(effect1) > 0)
    dupIndex = numeric(0)
  for (i in 1:length(effect1)) {
    if (i > length(effect1))
      break
    index = (1:length(effect1))[effect2 == effect1[i]]
    dupIndex = numeric(0)
    for (j in index) {
      if (effect1[j] == effect2[i]) {
        if (i != j)
          dupIndex = c(dupIndex, j)
      }
    }
    if (length(dupIndex > 0)) {
      effect1 = effect1[-dupIndex]
      effect2 = effect2[-dupIndex]
    }
  }
  cat("\nAlias Structure:\n")
  for (i in 1:length(effect1)) {
    if ((length(strsplit(effect1[i], split = character(0))[[1]]) <= depth) && (length(strsplit(effect2[i],
                                                                                               split = character(0))[[1]]) <= depth))
      cat(effect1[i], "\tis confounded with\t", effect2[i], "\n")
    if (identical(depth, "all"))
      cat(effect1[i], "\tis confounded with\t", effect2[i], "\n")
  }
  invisible(effect1)
}
# fracChoose ----
fracChoose <- function() {
  #' @title fracChoose: Choosing a fractional or full factorial design from a table.
  #' @description Designs displayed are the classic minimum abberation designs. Choosing a design is done by clicking with the mouse into the appropriate field.
  #' @return \code{fracChoose} returns an object of class \code{\link{facDesign.c}}.
  #' @examples
  #' fracChoose()
  #' @seealso \code{\link{fracDesign}}, \code{\link{facDesign}}, \code{\link{rsmChoose}}, \code{\link{rsmDesign}}

  genList = list(6 * 9)
  genList = list(c("C = AB"), c(NULL), c(NULL), c(NULL), c(NULL), c(NULL), c(NULL), c(NULL), c(NULL), c(NULL), c("D = ABC"), c("D = AB", "E = AC"), c("D = AB",
                                                                                                                                                      "E = AC", "F = BC"), c("D = AB", "E = AC", "F = BC", "G = ABC"), c(NULL), c(NULL), c(NULL), c(NULL), c(NULL), c(NULL), c("E = ABCD"), c("E = ABC", "F = BCD"),
                 c("E = ABC", "F = BCD", "G = ACD"), c("E = BCD", "F = ACD", "G = ABC", "H = ABD"), c("E = ABC", "F = BCD", "G = ACD", "H = ABD", "J = ABCD"), c("E = ABC",
                                                                                                                                                                 "F = BCD", "G = ACD", "H = ABD", "J = ABCD", "K = AB"), c("E = ABC", "F = BCD", "G = ACD", "H = ABD", "J = ABCD", "K = AB", "L = AC"), c(NULL),
                 c(NULL), c(NULL), c("F = ABCDE"), c("F = ABCD", "G = ABDE"), c("F = ABC", "G = ABD", "H = BCDE"), c("F = BCDE", "G = ACDE", "H = ABDE", "J = ABCE"),
                 c("F = ABCD", "G = ABCE", "H = ABDE", "J = ACDE", "K = BCDE"), c("F = ABC", "G = BCD", "H = CDE", "J = ACD", "K = AEF", "L = ADEF"), c(NULL), c(NULL),
                 c(NULL), c(NULL), c("G = ABCDEF"), c("G = ABCD", "H = ABEF"), c("G = ABCD", "H = ACEF", "J = CDEF"), c("G = BCDF", "H = ACDF", "J = ABDE", "K = ABCE"),
                 c("G = CDE", "H = ABCD", "J = ABF", "K = BDEF", "L = ADEF"), c(NULL), c(NULL), c(NULL), c(NULL), c(NULL), c("H = ABCDEFG"), c("H = ACDFG", "J = BCEFG"),
                 c("H = ABCG", "J = BCDE", "K = ACDF"), c("H = ABCG", "J = BCDE", "K = ACDF", "L = ABCDEFG"))
  resList = list(6 * 9)
  resList = list(c(3), c(NULL), c(NULL), c(NULL), c(NULL), c(NULL), c(NULL), c(NULL), c(NULL), c(NULL), c(4), c(3), c(3), c(3), c(NULL), c(NULL), c(NULL),
                 c(NULL), c(NULL), c(NULL), c(5), c(4), c(4), c(4), c(3), c(3), c(3), c(NULL), c(NULL), c(NULL), c(6), c(4), c(4), c(4), c(4), c(4), c(NULL), c(NULL),
                 c(NULL), c(NULL), c(7), c(5), c(4), c(4), c(4), c(NULL), c(NULL), c(NULL), c(NULL), c(NULL), c(8), c(6), c(5), c(5))
  facMat = matrix(rep(3:11, 6), ncol = 9, byrow = TRUE)
  runMat = matrix(c(rep(2^2, 9), rep(2^3, 9), rep(2^4, 9), rep(2^5, 9), rep(2^6, 9), rep(2^7, 9)), ncol = 9, byrow = TRUE)
  par(mfrow = c(6, 9))
  par(mar = c(0, 0, 0, 0))
  par(oma = c(4, 4, 4, 4))
  colList = vector(mode = "list")
  colList[3] = "red"
  colList[4] = "yellow"
  colList[5] = "green"
  colList[6] = "green"
  colList[7] = "green"
  colList[8] = "green"
  k = 3
  N = 2^2
  m = 0
  for (i in seq(along = genList)) {
    res = unlist(resList[[i]])
    plot(0, 0, xaxs = "i", yaxs = "i", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, type = "n", xlab = "", ylab = "", bg = "red", fg = "green")
    box()
    if (!is.null(res))
      rect(0, 0, 1, 1, col = colList[[res]])
    yPos = 0.04
    xPos = 0.6
    item = rev(genList[[i]])
    for (j in seq(along = item)) {
      text(xPos, yPos + (j - 1) * 0.125, item[j], adj = c(0, 0), cex = 0.8)
    }
    p = log2((2^k)/N)
    if (!is.null(res)) {
      romNum = as.character(as.roman(res))
      text(0.1, 0.9, do.call("expression", list(substitute(2[romNum]^(k - p), list(k = k, p = p, romNum = romNum)))), adj = c(0, 1), cex = 1.5)
    }
    k = k + 1
    if ((i%%9) == 0) {
      N = N * 2
      k = 3
    }
  }
  cat("\nChoose a fractional factorial design by clicking into the appropriate field")
  cat("\nWaiting for your selection:")
  cat("\n\n")
  flush.console()
  mtext("number of runs N", side = 2, line = 2.5, outer = TRUE)
  mtext("number of variables k", side = 3, line = 2.5, outer = TRUE)
  for (numFac in 1:9) {
    mtext(numFac + 2, at = c(-0.05 + (1/9) * numFac), outer = TRUE, line = 0.5)
  }
  for (k in 1:6) {
    mtext(2^(k + 1), at = (7 - k)/6 - (0.5 * (1/6)), side = 2, outer = TRUE, line = 0.5)
  }

  xyList = NULL
  xyList = try(locator(1), silent = TRUE)
  x = 1
  y = 1
  if (!is.null(xyList)) {
    x = ceiling(xyList$x + 8)
    y = ceiling(6 - xyList$y)
  }
  mat = matrix(1:54, ncol = 9, byrow = TRUE)
  fdo = NULL
  if (!(x %in% 1:ncol(mat)) || !(y %in% 1:nrow(mat)))
    return(fracDesign(k = 3, gen = NULL, replicates = 1))
  else index = mat[y, x]
  k = facMat[y, x]
  generator = genList[[index]]
  N = runMat[y, x]
  if (!is.null(generator)) {
    fdo = try(do.call("fracDesign", list(k = k, gen = generator)), silent = TRUE)
  }
  if (N >= 2^k & is.null(generator)) {
    replicates = N/(2^k)
    fdo = try(fracDesign(k = k, gen = NULL, replicates = replicates), silent = TRUE)
  }
  if (class(fdo)[1] == "facDesign.c")
    return(fdo)
  else return(genList[[mat[y, x]]])
}


# code2real ----
code2real = function(low, high, codedValue) {
  #' @title code2real: Coding
  #' @description Function to calculate the real value of a coded value.
  #' @param low Numeric value giving the lower boundary.
  #' @param high Numeric value giving the higher boundary.
  #' @param codedValue Numeric value giving the coded value that will be calculated.
  #' @return The function return a real value of a coded value
  #' @examples
  #' code2real(160, 200, 0)
  return((diff(c(low, high))/2) * codedValue + mean(c(low, high)))
}
# Función steepAscent ----
steepAscent <- function(factors, response, size = 0.2, steps = 5, data) {
  #' @title steepAscent: Steepest Ascent
  #' @description \code{steepAscent} is a method to calculate the steepest ascent for a \code{\link{facDesign.c}} object.
  #' @param factors List containing vector of factor names (coded) to be included in calculation, first factor is the reference factor.
  #' @param response A character of response given in data.
  #' @param size Numeric integer value giving the step size in coded units for the first factor given in factors.
  #' By default size is set to \code{0.2}.
  #' @param steps Numeric integer value giving the number of steps.
  #' By default step is set to `5`.
  #' @param data An object of class \code{\link{facDesign.c}}.
  #' @return \code{steepAscent} returns an object of class \code{\link{steepAscent.c}}.
  #' @seealso \code{\link{optimum}}, \code{\link{desirability}}
  #' @examples
  #' # Example 1
  #' fdo = facDesign(k = 2, centerCube = 5)
  #' fdo$lows(c(170, 150))
  #' fdo$highs(c(230, 250))
  #' fdo$names(c("temperature", "time"))
  #' fdo$unit(c("C", "minutes"))
  #' yield = c(32.79, 24.07, 48.94, 52.49, 38.89, 48.29, 29.68, 46.5, 44.15)
  #' fdo$.response(yield)
  #' fdo$summary()
  #'
  #' sao = steepAscent(factors = c("B", "A"), response = "yield", size = 1,
  #'                   data = fdo)

  if(missing(response))
    stop("Choose a response")
  if (missing(data))
    stop("missing an object of class 'facDesign'")
  else fdo = data
  if (missing(factors) | length(factors) < 1)
    stop("missing factors")
  if (!is.character(factors))
    stop("factors needs to be a character")
  #names(names(fdo))
  model = data$fits[[response]]
  if (is.null(model)) {
    form = c(response, "~")
    for (i in seq(along = factors)) {
      if (i == 1)
        form = c(form, factors[i])
      else form = c(form, "+", factors[i])
    }
    form = paste(form, collapse = "")
    model = fdo$lm(form)
  }
  b = numeric(length = length(factors))
  x = numeric(length = length(factors))
  names(x) = factors
  for (i in seq(along = factors)) {
    b[i] = coef(model)[factors[i]]
    if (i == 1) {
      x[i] = size * sign(b[i])
    }
    else {
      x[i] = (x[1]/b[1]) * b[i]
    }
  }

  Run = 1:(steps + 1)
  Delta = 0:steps
  frameOut = data.frame(Run, Delta)
  initial = ncol(frameOut)
  for (i in seq(along = factors)) {
    frameOut[, i + initial] = x[i] * 0:steps
    names(frameOut)[i + initial] = paste(factors[i], ".coded", collapse = "", sep = "")
  }
  initial = ncol(frameOut)
  aux <- list()
  for (i in 1:length(fdo$names())) {
    aux[[.NAMES[i]]] <-fdo$names()[i]
  }
  for (i in seq(along = factors)) {
    frameOut[, i + initial] = code2real(fdo$lows()[[aux[[factors[i]]]]], fdo$highs()[[aux[[factors[i]]]]], x[i] * 0:steps)
    names(frameOut)[i + initial] = paste(factors[i], ".real", collapse = "", sep = "")
  }
  soa = steepAscent.c$new()
  soa$X = frameOut
  soa$.response(rep(NA, times = nrow(frameOut)))
  names(soa$response) = deparse(substitute(response))
  cat("\n")
  cat(paste("Steepest Ascent for", deparse(substitute(data)), "\n"))
  cat("\n")
  print(format(frameOut, digits = 3))
  invisible(soa)
}






# Función starDesign ----
starDesign <- function(k, p = 0, alpha = c("both", "rotatable", "orthogonal"), cs, cc, data) {
  #' @title starDesign: Axial Design
  #' @description \code{starDesign} is a function to create the star portion of a response surface design. The starDesign function can be used to create a star portion of a response surface design for a sequential assembly strategy.
  #' One can either specify k and p and alpha and cs and cc OR simply simply pass an object of class \code{\link{facDesign.c}} to the data. In the latter an object of class \code{\link{facDesign.c}} otherwise a list containing the axial runs and centerpoints is returned.
  #' @param k Integer value giving number of factors.
  #' @param p Integer value giving the number of factors via aliasing.
  #' By default set to `0`.
  #' @param alpha If no numeric value is given defaults to \code{`both`} i.e. \code{`orthogonality`} and \code{`rotatibility`} which can be set as character strings too.
  #' @param cs Integer value giving the number of centerpoints in the star portion of the design.
  #' @param cc Integer value giving the number of centerpoints in the cube portion of the design.
  #' @param data Optional. An object of class \code{\link{facDesign.c}}.
  #' @return \code{starDesign} returns a \code{facDesign.c} object if an object of class \code{facDesign.c} is given or a list containing entries for axial runs and center points in the cube and the star portion of a design.
  #' @seealso \code{\link{facDesign}}, \code{\link{fracDesign}}, \code{\link{rsmDesign}}, \code{\link{mixDesign}}
  #' @examples
  #' # Example 1: sequential assembly
  #' # Factorial design with one center point in the cube portion
  #' fdo = facDesign(k = 3, centerCube = 1)
  #' # Set the response via generic response method
  #' fdo$.response(1:9)
  #' # Sequential assembly of a response surface design (rsd)
  #' rsd = starDesign(data = fdo)
  #'
  #' # Example 2: Returning a list of star point designs
  #' starDesign(k = 3, cc = 2, cs = 2, alpha = "orthogonal")
  #' starDesign(k = 3, cc = 2, cs = 2, alpha = "rotatable")
  #' starDesign(k = 3, cc = 2, cs = 2, alpha = "both")

  fdo = NULL
  csFrame = NULL
  ccFrame = NULL
  starFrame = NULL
  blocks = 1
  alpha = alpha[1]
  if (missing(cc))
    cc = 1
  if (missing(cs))
    cs = 1
  if (!missing(k)) {
    nameVec = LETTERS[1:k]
  }
  if (!missing(data)) {
    fdo = data$clone()
    k = ncol(fdo$cube)
    if (class(fdo)[1] != "facDesign.c") {
      stop(paste(deparse(substitute(data)), "needs to be an object of class 'facDesign'"))
    }
    if (nrow(fdo$star) > 0)
      stop(paste("star portion of", deparse(substitute(data)), "not empty"))
    k = length(fdo$names())
    nameVec = fdo$names()
    cc = nrow(fdo$centerCube)
    p = ncol(fdo$cube) - log(nrow(unique(fdo$cube)), 2)
    blocks = .nblock(fdo) + 1
  }
  if (is.numeric(alpha))
    a = alpha
  if (alpha == "rotatable")
    a = .alphaRot(k, p)
  if (alpha == "orthogonal")
    a = .alphaOrth(k, p, cc = cc, cs = cs)
  if (alpha == "both") {
    found = FALSE
    for (i in seq(along = .rsmOrth)) {
      if (.rsmOrth[[i]]$k == k)
        if (.rsmOrth[[i]]$blocks == blocks)
          if (.rsmOrth[[i]]$p == p) {
            found = TRUE
            cc = .rsmOrth[[i]]$cc
            cs = .rsmOrth[[i]]$cs
            p = .rsmOrth[[i]]$p
            a = .alphaOrth(k, p, cc, cs)
            break
          }
    }
    if (!found) {
      return("no starDesign with approximate rotatability and orthogonality available")
    }
  }
  starFrame = .starFrame(k, alpha = a)
  names(starFrame) = nameVec
  if (!missing(data))
    fdo$.star(starFrame)
  if (cs > 0) {
    csFrame = as.data.frame(matrix(0, nrow = cs, ncol = k))
    names(csFrame) = nameVec
    if (!missing(data)) {
      fdo$.centerStar(csFrame)
    }
  }
  if (cc > 0) {
    ccFrame = as.data.frame(matrix(0, nrow = cc, ncol = k))
    names(ccFrame) = nameVec
    if (!missing(data)) {
      fdo$.centerCube(ccFrame)
    }
  }
  if (!missing(data))
    return(fdo)
  else return(list(star = starFrame, centerStar = csFrame, centerCube = ccFrame))
}




# Función rsmDesign ----
rsmDesign <- function(k = 3, p = 0, alpha = "rotatable", blocks = 1, cc = 1, cs = 1, fp = 1,
                      sp = 1, faceCentered = FALSE) {
  #' @title rsmDesign: Generate a response surface design.
  #' @description Generates a response surface design containing a cube, centerCube, star, and centerStar portion.
  #' @param k Integer value giving the number of factors. By default, \code{k} is set to `3`.
  #' @param p Integer value giving the number of additional factors in the response surface design by aliasing effects. Default is `0`.
  #' @param alpha Character string indicating the type of star points to generate. Should be \code{`rotatable`}(default), \code{`orthogonal`}, or \code{`both`}. If \code{`both`}, values for \code{cc} and \code{cs} will be discarded.
  #' @param blocks Integer value specifying the number of blocks in the response surface design. Default is `1`.
  #' @param cc Integer value giving the number of centerpoints (per block) in the cube portion (i.e., the factorial 2^k design) of the response surface design. Default is `1`.
  #' @param cs Integer value specifying the number of centerpoints in the star portion. Default is `1`.
  #' @param fp Integer value giving the number of replications per factorial point (i.e., corner points). Default is `1`.
  #' @param sp Integer value specifying the number of replications per star point. Default is `1`.
  #' @param faceCentered Logical value indicating whether to use a faceCentered response surface design (i.e., \code{alpha} = `1`). Default is \code{FALSE}.
  #' @details Generated designs consist of a cube, centerCube, star, and centerStar portion. The replication structure can be set with the parameters \code{cc} (centerCube), \code{cs} (centerStar), \code{fp} (factorialPoints), and \code{sp} (starPoints).
  #' @return The function returns an object of class \code{\link{facDesign.c}}.
  #' @seealso \code{\link{facDesign}}, \code{\link{fracDesign}}, \code{\link{fracChoose}}, \code{\link{pbDesign}}, \code{\link{rsmChoose}}
  #' @examples
  #' # Example 1: Central composite design for 2 factors with 2 blocks, alpha = 1.41,
  #' # 5 centerpoints in the cube portion and 3 centerpoints in the star portion:
  #' rsmDesign(k = 2, blocks = 2, alpha = sqrt(2), cc = 5, cs = 3)
  #'
  #' # Example 2: Central composite design with both, orthogonality and near rotatability
  #' rsmDesign(k = 2, blocks = 2, alpha = "both")
  #'
  #' # Example 3: Central composite design with:
  #' # 2 centerpoints in the factorial portion of the design (i.e., 2)
  #' # 1 centerpoint in the star portion of the design (i.e., 1)
  #' # 2 replications per factorial point (i.e., 2^3*2 = 16)
  #' # 3 replications per star point (i.e., 3*2*3 = 18)
  #' # Makes a total of 37 factor combinations
  #' rsdo = rsmDesign(k = 3, blocks = 1, alpha = 2, cc = 2, cs = 1, fp = 2, sp = 3)

  if (blocks > 2^(k - 1) + 1)
    stop("Blocking not possible")
  if (alpha == "rotatable")
    alpha = .alphaRot(k, p)
  if (alpha == "orthogonal")
    alpha = .alphaOrth(k, p, cc = cc, cs = cs)
  if (alpha == "both") {
    found = FALSE
    for (i in seq(along = .rsmOrth)) {
      if (.rsmOrth[[i]]$k == k)
        if (.rsmOrth[[i]]$blocks == blocks)
          if (.rsmOrth[[i]]$p == p) {
            cc = .rsmOrth[[i]]$cc
            cs = .rsmOrth[[i]]$cs
            p = .rsmOrth[[i]]$p
            alpha = .alphaOrth(k, p, cc, cs)
            found = TRUE
            break
          }
    }
    if (!found) {
      return("no design available")
    }
  }
  if(faceCentered==TRUE){
    alpha = 1
  }

  fdo = facDesign(k = k, p = p, replicates = fp)                              ###
  if (cc > 0) {
    temp = as.data.frame(matrix(0, nrow = cc, ncol = ncol(fdo$cube)))
    names(temp) = names(fdo$cube)
    fdo$.centerCube(temp)
  }

  temp = .starFrame(k, alpha)
  starportion = data.frame()
  for (i in 1:sp) {
    starportion = rbind(temp, starportion)
  }
  names(starportion) = names(fdo$cube)
  fdo$.star(starportion)
  if (cs > 0) {
    temp = as.data.frame(matrix(0, nrow = cs, ncol = ncol(fdo$cube)))
    names(temp) = names(fdo$cube)
    fdo$.centerStar(temp)
  }
  #    return(fdo)
  fdo = blocking(fdo, blocks)
  return(fdo)
}


# rsmChoose() ----
rsmChoose <- function() {
  #' @title rsmChoose: Choosing a response surface design from a table
  #' @description Designs displayed are central composite designs with orthogonal blocking and near rotatability. The function allows users to choose a design by clicking with the mouse into the appropriate field.
  #' @return Returns an object of class \code{\link{facDesign.c}}.
  #' @examples
  #' rsmChoose()
  #' @seealso \code{\link{fracChoose}}, \code{\link{rsmDesign}}

  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  colFun = colorRampPalette(c("yellow", "red"), space = "rgb")
  colPalette = colFun(169)
  numRows = 6
  numCol = 9
  blockVals = c(1, 2, 3, 5, 9, 17)
  factorVals = c(2, 3, 4, 5, 5, 6, 6, 7, 7)
  rsmList = .rsmOrth
  plot.new()
  par(mfrow = c(6, 9))
  par(mar = c(0, 0, 0, 0))
  par(oma = c(4, 4, 4, 4))
  for (i in 1:6) for (j in 1:9) {
    par(mfg = c(i, j))
    plot(0, 0, xaxs = "i", yaxs = "i", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, type = "n",
         xlab = "", ylab = "", bg = "red", fg = "green")
    box()
  }
  for (i in seq(along = rsmList)) {
    temp = rsmList[[i]]
    par(mfg = c(temp$row, temp$col))
    par(mfg = c(temp$row, temp$col))
    plot(0, 0, xaxs = "i", yaxs = "i", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, type = "n",
         xlab = "", ylab = "", bg = "red", fg = "green")
    rect(0, 0, 1, 1, col = colPalette[2^((temp$k) - (temp$p))])
    text(0.1, 0.9, paste("N =", 2^((temp$k) - (temp$p)) + temp$cc * (temp$blocks - 1) + temp$cs +
                           (temp$k + temp$p) * 2), adj = c(0, 1), cex = 1.25)
    text(0.1, 0.75, paste("k =", temp$k), adj = c(0, 1), cex = 1.25)
    text(0.1, 0.6, paste("p =", temp$p), adj = c(0, 1), cex = 1.25)
    text(0.1, 0.45, "CenterPoints:", adj = c(0, 1), cex = 1.25)
    text(0.1, 0.3, paste("Cube:", temp$cc), adj = c(0, 1), cex = 1.25)
    text(0.1, 0.15, paste("Axial:", temp$cs), adj = c(0, 1), cex = 1.25)
    box()
  }
  x = 1/18 + (0:8) * 2/18
  mtext(factorVals, at = x, side = 3, line = 0.5, outer = TRUE)
  mtext("number of factors k", at = 0.5, side = 3, line = 2.5, outer = TRUE)
  x = 1/12 + (5:0) * 2/12
  mtext(blockVals, at = x, side = 2, line = 0.5, outer = TRUE, las = 2)
  mtext("number of blocks", at = 0.5, side = 2, line = 2.5, outer = TRUE)
  cat("\nChoose a response surface design by clicking into the appropriate field")
  cat("\nWaiting for your selection:")
  cat("\n\n")
  flush.console()
  x = numeric(0)
  y = numeric(0)
  xyList = locator(1)                                                         ###
  #print(xyList)
  x = ceiling(xyList$x + 8)
  y = ceiling(5 - xyList$y)

  if (length(x) < 1)
    return(rsmDesign(k = 2, p = 0, blocks = 2, alpha = "both"))
  if (length(y) < 1)
    return(rsmDesign(k = 2, p = 0, blocks = 2, alpha = "both"))
  #    if (!(x %in% factorVals) || !(y %in% blockVals))                           ###
  #        return(rsmDesign(k = 2, p = 0, blocks = 2, alpha = "both"))            ###
  blocks = blockVals[y]
  k = factorVals[x]
  if (x==5 || x==7 || x==9 )                                                  ###
    p = 1                                                                      ###
  else                                                                        ###
    p = 0                                                                      ###

  for (i in seq(along = rsmList)) {
    if (rsmList[[i]]$k == k)
      if (rsmList[[i]]$blocks == blocks)
        if (rsmList[[i]]$p == p)
          return(rsmDesign(k = k, p = rsmList[[i]]$p, blocks = blocks, ###
                           alpha = "both", cc = rsmList[[i]]$cc,                 ###
                           cs = rsmList[[i]]$cs))                                ###
  }
  return(cat("\nno selection recognized\n"))
}





# Funcion desirability ----
desirability = function(response, low, high, target = "max", scale = c(1, 1), importance = 1) {
  #' @title desirability: Desirability Function.
  #' @description Creates desirability functions for use in the optimization of multiple responses.
  #' @param response Name of the response.
  #' @param low Lowest acceptable value for the response.
  #' @param high Highest acceptable value for the response.
  #' @param target Desired target value of the response. \code{target} can be \code{`max`}, \code{`min`}, or any specific numeric value.
  #' @param scale Numeric value giving the scaling factors for one and two-sided transformations. Default is \code{c(1, 1)}.
  #' @param importance A value ranging from 0.1 to 10, used to calculate a weighted importance, i.e., with importances 1, 2, and 4, D = [(d1)^1, (d2)^2, (d3)^4]^(1/7). Default is `1`.
  #' @details For a product to be developed, different values of responses are desired, leading to multiple response optimization. Minimization, maximization, as well as a specific target value, are defined using desirability functions. A desirability function transforms the values of a response into [0,1], where 0 stands for a non-acceptable value of the response and 1 for values where higher/lower (depending on the direction of the optimization) values of the response have little merit. This function builds upon the desirability functions specified by Harrington (1965) and the modifications by Derringer and Suich (1980) and Derringer (1994). Castillo, Montgomery, and McCarville (1996) further extended these functions, but these extensions are not implemented in this version.
  #' @return This function returns a \code{\link{desirability.c}} object.
  #' @seealso \code{\link{overall}}, \code{\link{optimum}}
  #' @examples
  #' # Example 1: Maximization of a response
  #' # Define a desirability for response y where higher values of y are better
  #' # as long as the response is smaller than high
  #' d = desirability(y, low = 6, high = 18, target = "max")
  #' # Show and plot the desirability function
  #' d
  #' plot(d)
  #'
  #' # Example 2: Minimization of a response including a scaling factor
  #' # Define a desirability for response y where lower values of y are better
  #' # as long as the response is higher than low
  #' d = desirability(y, low = 6, high = 18, scale = c(2), target = "min")
  #' # Show and plot the desirability function
  #' d
  #' plot(d)
  #'
  #' # Example 3: Specific target of a response is best including a scaling factor
  #' # Define a desirability for response y where desired value is at 8 and
  #' # values lower than 6 as well as values higher than 18 are not acceptable
  #' d = desirability(y, low = 6, high = 18, scale = c(0.5, 2), target = 12)
  #' # Show and plot the desirability function
  #' d
  #' plot(d)
  #'
  #' # Example 4:
  #' y1 <- c(102, 120, 117, 198, 103, 132, 132, 139, 102, 154, 96, 163, 116, 153,
  #'         133, 133, 140, 142, 145, 142)
  #' y2 <- c(470, 410, 570, 240, 640, 270, 410, 380, 590, 260, 520, 380, 520, 290,
  #'         380, 380, 430, 430, 390, 390)
  #' d1 <- desirability(y1, 120, 170, scale = c(1, 1), target = "max")
  #' d3 <- desirability(y2, 400, 600, target = 500)
  #' d1
  #' plot(d1)
  #' d3
  #' plot(d3)


  if (low >= high)
    stop("the lower bound must be greater than the high bound!")
  if (any(scale <= 0))
    stop("the scale parameter must be greater than zero!")
  if (!is.numeric(target) & !identical(tolower(target), "min") & !identical(tolower(target), "max"))
    stop("target needs to be \"min\", \"max\" or a numeric value")
  return(desirability.c$new(response = deparse(substitute(response)), low = low, high = high, target = target, scale = scale, importance = importance))
}



# overall ----
overall <- function(fdo, steps = 20, constraints, ...) {
  #' @title overall: Overall Desirability.
  #' @description This function calculates the desirability for each response as well as the overall desirability. The resulting \code{data.frame} can be used to plot the overall desirability as well as the desirabilities for each response. This function is designed to visualize the desirability approach for multiple response optimization.
  #' @param fdo An object of class \code{\link{facDesign.c}} containing \code{fits} and \code{desires}.
  #' @param steps A numeric value indicating the number of points per factor to be evaluated, which also specifies the grid size. Default is `20`.
  #' @param constraints A list of constraints for the factors in coded values, such as \code{list(A > 0.5, B < 0.2)}.
  #' @param ... Further arguments passed to other methods.
  #' @return A \code{data.frame} with a column for each factor, the desirability for each response, and a column for the overall desirability.
  #' @seealso \code{\link{facDesign}}, \code{\link{rsmDesign}}, \code{\link{desirability}}.
  #' @examples
  #' #Example 1: Arbitrary example with random data
  #' rsdo = rsmDesign(k = 2, blocks = 2, alpha = "both")
  #' rsdo$.response(data.frame(y = rnorm(rsdo$nrow()), y2 = rnorm(rsdo$nrow())))
  #' rsdo$set.fits(rsdo$lm(y ~ A*B + I(A^2) + I(B^2)))
  #' rsdo$set.fits(rsdo$lm(y2 ~ A*B + I(A^2) + I(B^2)))
  #' rsdo$desires(desirability(y, -1, 2, scale = c(1, 1), target = "max"))
  #' rsdo$desires(desirability(y2, -1, 0, scale = c(1, 1), target = "min"))
  #' dVals = overall(rsdo, steps = 10, constraints = list(A = c(-0.5,1), B = c(0, 1)))

  importances = list()
  cs = list()
  if (!missing(constraints))
    cs = constraints
  l = vector(mode = "list", length = 0)
  fitList = fdo$fits
  if (length(fitList) < 1)
    stop(paste("no fits found in fits(", deparse(substitute(fdo)), ")"), sep = "")
  desList = fdo$desires()
  if (length(desList) < 1)
    stop(paste("no desirabilities found in desires(", deparse(substitute(fdo)), ")"), sep = "")
  X = fdo$cube
  newdata = NULL
  aux <- list()
  for (i in 1:length(fdo$names())) {
    aux[[fdo$names()[i]]] <-.NAMES[i]
  }
  aux<-unlist(aux)
  for (i in aux) {
    seqList = vector(mode = "list", length = 0)
    seqList[["length"]] = steps
    seqList[["from"]] = min(X[, i])
    seqList[["to"]] = max(X[, i])
    minC = NULL
    maxC = NULL
    if (!is.null(cs[[i]])) {
      if (length(cs[[i]]) < 2)
        stop("length of ", names(cs[i]), "=", cs[i], " < 2 in constraints")
      minC = min(cs[[i]])
      if (!is.null(minC) & !is.na(minC))
        seqList[["from"]] = minC
      maxC = max(cs[[i]])
      if (!is.null(maxC) & !is.na(maxC))
        seqList[["to"]] = maxC
      if (maxC == minC)
        stop(paste("equal values in constraints ", names(cs[i]), "=", cs[i]))
    }
    l[[i]] = do.call(seq, seqList)
  }

  newdata = expand.grid(l)
  names(newdata) = names(X)
  out = newdata
  yCharSet = intersect(names(fdo$desires()), names(fdo$fits))
  dFrame = data.frame(matrix(NA, nrow = nrow(newdata), ncol = length(yCharSet) + 1))
  names(dFrame) = c(yCharSet, "overall")
  dFrame[, "overall"] = 1
  for (y in yCharSet) {
    obj = desList[[y]]
    dFun = .desireFun(obj$low, obj$high, obj$target, obj$scale, obj$importance)
    lm.y = fitList[[y]]
    importances[[y]] = fdo$desires()[[y]]$importance
    yHat = predict(lm.y, newdata = newdata, ...)
    yDes = dFun(yHat)
    dFrame[, y] = yDes
  }
  geomFac = 1/sum(unlist(importances))
  overall = apply(dFrame, 1, prod)^geomFac
  dFrame[, "overall"] = overall
  dFrame = cbind(out, dFrame)
  invisible(dFrame)
}

# optimum ----
optimum <- function(fdo, constraints, steps = 25, type = "grid", start) {
  #' @title optimum: Optimal factor settings
  #' @description This function calculates the optimal factor settings based on defined desirabilities and constraints. It supports two approaches: (I) evaluating all possible factor settings via a grid search and (II) using optimization methods such as \code{`optim`} or \code{`gosolnp`} from the Rsolnp package. Using \code{`optim`} initial values for the factors to be optimized over can be set via start.
  #' The optimality of the solution depends critically on the starting parameters which is why it is recommended to use \code{type=`gosolnp`} although calculation takes a while.
  #' @param fdo An object of class \code{\link{facDesign.c}} with \code{fits} and \code{desires} set.
  #' @param constraints A list specifying the constraints for the factors, e.g., \code{list(A = c(-2,1), B = c(0, 0.8))}.
  #' @param steps Number of grid points per factor if \code{type = `grid`}. Default is `25`.
  #' @param type The type of search to perform. Supported values are \code{`grid`}, \code{`optim`}, and \code{`gosolnp`}. See Details for more information.
  #' @param start A numeric vector providing the initial values for the factors when using \code{type = `optim`}.
  #' @details The function allows you to optimize the factor settings either by evaluating a grid of possible settings (\code{type = `grid`}) or by using optimization algorithms (\code{type = `optim` or `gosolnp`}). The choice of optimization method may significantly affect the result, especially for desirability functions that lack continuous first derivatives. When using \code{type = `optim`}, it is advisable to provide \code{start} values to avoid local optima. The \code{`gosolnp`} method is recommended for its robustness, although it may be computationally intensive.
  #' @return Return an object of class \code{\link{desOpt}}.
  #' @seealso \code{\link{overall}}, \code{\link{desirability}},
  #' @examples
  #' #Example 1: Simultaneous Optimization of Several Response Variables
  #' #Define the response surface design as given in the paper and sort via Standard Order
  #' fdo = rsmDesign(k = 3, alpha = 1.633, cc = 0, cs = 6)
  #' fdo = randomize(fdo, so = TRUE)
  #' #Attaching the 4 responses
  #' y1 = c(102, 120, 117, 198, 103, 132,
  #'         132, 139, 102, 154, 96, 163,
  #'         116, 153, 133, 133, 140, 142,
  #'         145, 142)
  #'
  #' y2 = c(900, 860, 800, 2294, 490, 1289,
  #'         1270, 1090, 770, 1690, 700, 1540,
  #'         2184, 1784, 1300, 1300, 1145, 1090,
  #'         1260, 1344)
  #'
  #' y3 = c(470, 410, 570, 240, 640, 270,
  #'         410, 380, 590, 260, 520, 380,
  #'         520, 290, 380, 380, 430, 430,
  #'         390, 390)
  #'
  #' y4 = c(67.5, 65, 77.5, 74.5, 62.5, 67,
  #'         78, 70, 76, 70, 63, 75,
  #'         65, 71, 70, 68.5, 68, 68,
  #'         69, 70)
  #' fdo$.response(data.frame(y1, y2, y3, y4)[c(5,2,3,8,1,6,7,4,9:20),])
  #' #Setting names and real values of the factors
  #' fdo$names(c("silica", "silan", "sulfur"))
  #' fdo$highs(c(1.7, 60, 2.8))
  #' fdo$lows(c(0.7, 40, 1.8))
  #' #Setting the desires
  #' fdo$desires(desirability(y1, 120, 170, scale = c(1,1), target = "max"))
  #' fdo$desires(desirability(y2, 1000, 1300, target = "max"))
  #' fdo$desires(desirability(y3, 400, 600, target = 500))
  #' fdo$desires(desirability(y4, 60, 75, target = 67.5))
  #' #Setting the fits
  #' fdo$set.fits(fdo$lm(y1 ~ A + B + C + A:B + A:C + B:C + I(A^2) + I(B^2) + I(C^2)))
  #' fdo$set.fits(fdo$lm(y2 ~ A + B + C + A:B + A:C + B:C + I(A^2) + I(B^2) + I(C^2)))
  #' fdo$set.fits(fdo$lm(y3 ~ A + B + C + A:B + A:C + B:C + I(A^2) + I(B^2) + I(C^2)))
  #' fdo$set.fits(fdo$lm(y4 ~ A + B + C + A:B + A:C + B:C + I(A^2) + I(B^2) + I(C^2)))
  #' #Calculate the best factor settings using type = "optim"
  #' optimum(fdo, type = "optim")
  #' #Calculate the best factor settings using type = "grid"
  #' optimum(fdo, type = "grid")

  if (missing(fdo))
    stop("missing fdo!")
  X = fdo$as.data.frame()
  numFac = length(fdo$names())
  if (!(type %in% c("grid", "optim", "gosolnp"))) {
    warning(paste("type =", deparse(substitute(type)), "not found --> using type = \"grid\""))
    type = "grid"
  }
  constraints = .validizeConstraints(fdo, constraints)
  if (missing(start))
    start = as.numeric(lapply(constraints, mean))
  lower = numeric(length(constraints))
  upper = numeric(length(constraints))
  for (i in seq(along = constraints)) {
    lower[i] = min(constraints[[i]])
    upper[i] = max(constraints[[i]])
  }

  desOpt = desOpt$new()
  desOpt$fdo = fdo
  facCoded = NA
  desirabilities = NA
  overall = NA
  setList = list()
  dList = list()
  importances = list()
  yCharSet = intersect(names(fdo$desires()), names(fdo$fits))
  for (y in yCharSet) {
    obj = fdo$desires()[[y]]
    dFun = .desireFun(obj$low, obj$high, obj$target, obj$scale, obj$importance)
    lm.y = fdo$fits[[y]]
    importances[[y]] = fdo$desires()[[y]]$importance
    dList[[y]] = .dHelp(lm.y, dFun)
  }
  geomFac = 1/sum(unlist(importances))
  dAll = function(X) {
    newdata = data.frame(t(X))
    names(newdata) = LETTERS[1:ncol(newdata)]
    return(prod(unlist(lapply(dList, do.call, list(newdata = newdata))))^geomFac)
  }
  dAllRsolnp = function(X) {
    newdata = data.frame(t(X))
    names(newdata) = LETTERS[1:ncol(newdata)]
    return(-prod(unlist(lapply(dList, do.call, list(newdata = newdata))))^geomFac)
  }
  if (type == "optim") {
    #        print(lower)
    #       print(upper)
    temp = optim(par = start, dAll, method = "L-BFGS-B", lower = lower, upper = upper, control = list(fnscale = -1, maxit = 1000))
    facCoded = as.list(temp$par)
    names(facCoded) = fdo$names()
    desOpt$facCoded = facCoded
    overall = temp$value
    desirabilities = .desHelp(fdo, desOpt$facCoded)
  }
  if (type == "gosolnp") {
    #if (!require(Rsolnp, quietly = TRUE))
    #    stop("Package Rsolnp needs to be installed!")
    temp = Rsolnp::gosolnp(fun = dAllRsolnp, LB = lower, UB = upper)
    facCoded = as.list(temp$pars)
    names(facCoded) = fdo$names()
    desOpt$facCoded = facCoded
    overall = -rev(temp$values)[1]
    desirabilities = .desHelp(fdo, desOpt$facCoded)
  }
  if (type == "grid") {
    dVals = overall(fdo = fdo, constraints = constraints, steps = steps)
    index = order(dVals[, "overall"], decreasing = TRUE)[1]
    desOpt$all = dVals
    aux <- list()
    for (i in 1:length(fdo$names())) {
      aux[[fdo$names()[i]]] <-.NAMES[i]
    }
    aux<-unlist(aux)
    desOpt$facCoded = as.list(dVals[index, aux])
    names(desOpt$facCoded)<-fdo$names()
    desirabilities = as.list(dVals[index, names(fdo$.response())])
    names(desirabilities) = names(fdo$.response()) #fix for the case of having just one response
    overall = dVals[index, "overall"]
  }
  for (i in names(desOpt$facCoded)) {
    desOpt$facReal[[i]] = code2real(fdo$lows()[[i]], fdo$highs()[[i]], desOpt$facCoded[[i]])
  }
  desOpt$desirabilities = desirabilities
  desOpt$overall = overall
  newData = do.call(data.frame, desOpt$facCoded)
  aux <- list()
  for (i in 1:length(fdo$names())) {
    aux[[fdo$names()[i]]] <-.NAMES[i]
  }
  names(newData)<-unlist(aux)
  for (i in names(desOpt$desirabilities)) {
    desOpt$responses[[i]] = predict(fdo$fits[[i]], newData)
  }
  return(desOpt)
}














# summaryFits ----
summaryFits = function(fdo, lmFit = TRUE, curvTest = TRUE) {
  #' @title summaryFits: Fit Summary
  #' @description
  #' Function to provide an overview of fitted linear models for objects of class \code{\link{facDesign.c}}.
  #' @usage
  #' summaryFits(fdo, lmFit = TRUE, curvTest = TRUE)
  #' @param fdo An object of class \code{\link{facDesign.c}}.
  #' @param lmFit A logical value deciding whether the fits from the object \code{fdo} should be included or not. By default, \code{lmFit} is set to \code{TRUE}.
  #' @param curvTest A logical value deciding whether curvature tests should be performed or not. By default, \code{curvTest} is set to \code{TRUE}.
  #' @return A summary output of the fitted linear models, which may include the linear fits, curvature tests, and original fit values, depending on the input parameters.
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
      if(lmFit){
        print(summary(fdo$fits[[f]]))
      }
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
      if(curvTest){
        .curvTest(fdo, f)
      }
      cat("\n")
      cat("\n")
    }
  }
  invisible()
}


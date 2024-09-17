#########################################################################
################ DISEÑOS DE MEZCLAS - AUXILIARES ########################
#########################################################################

# .npp ----
.npp = function(mdo) {
  pseudo = mdo$pseudo
  Type = mdo$Type
  temp = as.character(mdo$as.data.frame()$Type)
  tab = table(temp)
  nums = data.frame(matrix(tab, nrow = 4, ncol = length(tab), byrow = TRUE))
  nums[1:2, ] = NA
  nums[4, ] = c(nrow(Type), rep("", length(tab) - 1))
  row.names(nums) = c("Unique", "Replicates", "Sub Total", "Total")
  names(nums) = names(tab)
  for (i in names(tab)) {
    sSet = pseudo[Type == i, ]
    usSet = unique(sSet)
    nums["Unique", i] = nrow(usSet)
    if (nrow(usSet) == 1) {
      nums["Replicates", i] = nrow(sSet)
    }
    else {
      for (j in 1:nrow(usSet)) {
        uCount1 = sum(apply(apply(sSet, 1, "==", usSet[j, ]), 2, "all") * 1)
        if (j == 1) {
          uCount2 = uCount1
          nums["Replicates", i] = uCount1
        }
        if (uCount2 != uCount1)
          nums["Replicates", i] = -1
      }
    }
  }
  cat("Information about the Design Points:")
  cat("\n")
  cat("\n")
  print(nums)
}
# .permut ----
.permut <- function(x) {
  if (any(is.na(x)))
    stop(paste(deparse(substitute(x)), "contains NA"))
  x = sort(x, decreasing = FALSE)
  n = length(x)
  num = 1:n
  frameOut = matrix(NA, nrow = 1, ncol = n)
  frameOut[1, ] = x
  while (TRUE) {
    highest = NA
    for (j in 1:(n - 1)) {
      if (x[j] < x[j + 1])
        highest = j
    }
    if (is.na(highest))
      return(frameOut)
    else {
      l = max((num)[x[highest] < x])
      temp = x[l]
      x[l] = x[highest]
      x[highest] = temp
      x[(highest + 1):n] = rev(x[(highest + 1):n])
    }
    frameOut = rbind(frameOut, x, deparse.level = 2)
  }
}
# .simplexCentroid ----
.simplexCentroid = function(p) {
  if (p <= 1 | !is.numeric(p))
    stop("invalid value for p")
  frameOut = NA
  for (i in 1:p) {
    initial = rep(0, times = p)
    initial[1:i] = 1/i
    mat = .permut(initial)
    if (i == 1)
      frameOut = mat
    else frameOut = rbind(frameOut, mat, deparse.level = 2)
  }
  frameOut = data.frame(frameOut, row.names = 1:(2^p - 1))
  names(frameOut) = LETTERS[1:p]
  return(frameOut)
}

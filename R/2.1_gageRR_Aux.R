###################################################################
##################### gageRR - AUXILIARES #########################
###################################################################

# .aip ----
.aip <- function(x.factor, trace.factor, response, fun = mean, type = c("l", "p", "b"), legend = TRUE, trace.label = NULL,fixed = FALSE, xlab = deparse(substitute(x.factor)), ylab ="Measurement" , ylim = NULL, lty = 1:length(unique(trace.factor)),col = 1, pch = c(1L:9, 0, letters), xpd = NULL, leg.bg = par("bg"), leg.bty = "o", xtick = FALSE, xaxt = par("xaxt"), axes = TRUE, title = "", plot = TRUE, ...) {
  ylabel <- paste(ylab )
  type <- match.arg(type)

  # Asegurarse de que los factores son realmente factores
  x.factor <- factor(x.factor)
  trace.factor <- factor(trace.factor)

  # Calcular los valores de celda
  cellNew <- tapply(response, list(x.factor, trace.factor), fun)
  cellNew <- as.data.frame(as.table(cellNew))
  colnames(cellNew) <- c("x.factor", "trace.factor", "response")

  # Convertir x.factor a numC)rico sC3lo para la visualizaciC3n, pero mantenerlo como factor en los datos
  cellNew$x.numeric <- as.numeric(cellNew$x.factor)

  # Crear el grC!fico
  p <- ggplot(cellNew, aes(x = x.numeric, y = response, group = trace.factor, color = trace.factor, shape = trace.factor, linetype = trace.factor)) +
    geom_line() +
    geom_point(size = 2) +
    # geom_text(aes(label = round(response, 2)), vjust = -0.5) +  # Comentar o eliminar esta lC-nea para ocultar nC:meros
    scale_x_continuous(breaks = unique(cellNew$x.numeric), labels = levels(cellNew$x.factor)) +  # Etiquetas de x.factor
    labs(x = xlab, y = ylabel, title = title, color = trace.label, shape = trace.label, linetype = trace.label) +
    theme_bw() +
    theme(legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5))

  if (!is.null(ylim)) {
    p <- p + ylim(ylim)
  }

  if (legend) {
    p <- p + theme(legend.position="right",
                   legend.background = element_rect(fill = "white", colour = "black"))
  } else {
    p <- p + theme(legend.position = "none")
  }
  if(plot){
    print(p)
  }
  invisible(list(plot = p))
}
# .c4 ----
.c4 = function(n) {
  if (n > 1 && n < 342)
    sqrt(2/(n - 1)) * (factorial(n/2 - 1)/factorial((n - 1)/2 - 1))
  else stop("n needs to be bigger than 1 and smaller than 342")
}
# .mapping ----
.mapping = function(x, oldValues, newValues) {
  if (length(oldValues) != length(newValues)) {
    print("old and new")
    print(oldValues)
    print(newValues)
    warning(paste("unequal length of", deparse(substitute(oldValues)), "and", deparse(substitute(newValues))))
  }
  out = numeric(length(x))
  for (i in seq(along = newValues)) {
    index = (x == oldValues[i])
    out[index] = newValues[i]
  }
  return(out)
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

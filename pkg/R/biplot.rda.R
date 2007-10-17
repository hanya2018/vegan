## biplot.rda
##
## draws pca biplots with species as arrows
##

biplot.rda <- function(x, choices = c(1, 2), scaling = 2,
                       display = c("sites", "species"),
                       type, xlim, ylim, ...) {
  if(!inherits(x, "rda"))
    stop("'biplot.rda' is only for objects of class 'rda'")
  if(!is.null(x$CCA))
    stop("'biplot.rda' not suitable for models with constraints")
  g <- scores(x, choices = choices, display = display,
              scaling = scaling)
  if (!is.list(g)) 
    g <- list(default = g)
  if (missing(type)) {
    nitlimit <- 80
    nit <- max(nrow(g$species), nrow(g$sites))
    if (nit > nitlimit) 
      type <- "points"
    else type <- "text"
  }
  else type <- match.arg(type, TYPES)
  if (missing(xlim)) 
    xlim <- range(g$species[, 1], g$sites[, 1])
  if (missing(ylim)) 
    ylim <- range(g$species[, 2], g$sites[, 2])
  plot(g[[1]], xlim = xlim, ylim = ylim, type = "n", asp = 1, 
       ...)
  abline(h = 0, lty = 3)
  abline(v = 0, lty = 3)
  if (!is.null(g$species)) {
    arrows(0, 0, g$species[,1] * 0.85, g$species[, 2] * 0.85,
           col = "red", length = 0.05)
    text(g$species, rownames(g$species),
         col = "red", cex = 0.7)
  }
  if (!is.null(g$sites)) {
    if (type == "text") 
      text(g$sites, rownames(g$sites), cex = 0.7)
    else if (type == "points") 
      points(g$sites, pch = 1, cex = 0.7)
  }
  class(g) <- "ordiplot"
  invisible(g)
}

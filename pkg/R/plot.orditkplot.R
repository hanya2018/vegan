`plot.orditkplot` <-
    function(x, ...)
{
    op <- par(x$par)
    on.exit(par(op))
    plot(x$points, pch = 21, cex = x$args$pcex, col = x$args$col,
         bg = x$args$bg, asp=1)
    text(x$labels, rownames(x$labels), cex = x$args$cex)
    invisible(x)
}

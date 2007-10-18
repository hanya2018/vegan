"plot.nestedtemp" <-
function(x, kind = c("temperature", "incidendce"), col=rev(heat.colors(100)),  ...)
{
    kind <- match.arg(kind)
    if (kind == "temperature")
        z <- x$u
    else
        z <- x$comm
    z <- t(z[nrow(z):1,])
    image(z, axes=FALSE, col = col,  ...)
    box()
    lines(c(0, x$fill, 1), c(0, 1-x$fill, 1))
}


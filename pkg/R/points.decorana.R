"points.decorana" <-
    function (x, display=c("sites", "species"), choices=1:2, origin = TRUE, select, ...)
{
    localPoints <- function(..., shrink, origin, scaling, triangular)
        points(...)
    display <- match.arg(display)
    x <- scores(x, display = display, choices = choices, origin = origin, ...)
    if (!missing(select))
        x <- x[select,,drop=FALSE]
    localPoints(x, ...)
    invisible()
}


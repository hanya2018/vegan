"ordihull" <-
    function (ord, groups, display = "sites", draw = c("lines", "polygon"),
              show.groups, ...)
{
    localLines <- function(..., shrink, origin, scaling, triangular) lines(...)
    localPolygon <- function(..., shrink, origin, scaling, triangular)
        polygon(...)
    draw <- match.arg(draw)
    pts <- scores(ord, display = display, ...)
    if (!missing(show.groups)) {
        take <- groups %in% show.groups
        pts <- pts[take, , drop = FALSE]
        groups <- groups[take]
    }
    out <- seq(along = groups)
    inds <- names(table(groups))
    for (is in inds) {
        gr <- out[groups == is]
        if (length(gr) > 1) {
            X <- pts[gr, ]
            hpts <- chull(X)
            hpts <- c(hpts, hpts[1])
            if (draw == "lines")
                localLines(X[hpts, ], ...)
            else localPolygon(X[hpts,], ...)
        }
    }
    invisible()
}

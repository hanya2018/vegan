"lines.spantree" <-
    function (x, ord, display = "sites", ...)
{
    localSegments <- function(..., shrink, origin, scaling, triangular)
        segments(...)
    ord <- scores(ord, display = display, ...)
    tree <- x$kid
    #segments(ord[-1, 1], ord[-1, 2], ord[tree, 1], ord[tree,
    #                                                   2], ...)
    localSegments(ord[-1, 1], ord[-1, 2], ord[tree, 1], ord[tree,
                                                            2], ...)
    invisible()
}

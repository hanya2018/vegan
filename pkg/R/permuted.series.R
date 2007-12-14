`permuted.series` <- function(inds, mirror = FALSE, start = NULL)
{
    n <- length(inds)
    if(is.null(start))
        start <- .Internal(sample(n, 1, FALSE, NULL))
    out <- seq(start, length = n) %% n + inds[1]
    if(mirror && runif(1) < 0.5)
        out <- rev(out)
    out
}
